

download_DK_daily_contests_MLB <- function(contest_info) { 
  original_wd <- getwd()
  
  library(RSelenium)
  library(XML)
  
  ### Local Function
  
  # make sure you have the server
  checkForServer <- function (dir = NULL, update = FALSE, rename = TRUE, 
                              beta = FALSE) 
  {
    selURL <- "http://selenium-release.storage.googleapis.com"
    selXML <- xmlParse(paste0(selURL), "/?delimiter=")
    selJAR <- 
      xpathSApply(selXML,
                  "//s:Key[contains(text(),'selenium-server-standalone')]", 
                  namespaces = c(s = "http://doc.s3.amazonaws.com/2006-03-01"), 
                  xmlValue
      )
    
    # get the most up-to-date jar
    selJAR <- if(!beta){
      grep("^.*-([0-9\\.]*)\\.jar$", selJAR, value = TRUE)
    }else{
      selJAR
    }
    
    selJARdownload <- selJAR[order(gsub(".*-(.*).jar$", "\\1", selJAR), 
                                   decreasing = TRUE)][1]
    selDIR <- ifelse(is.null(dir), file.path(find.package("RSelenium"), 
                                             "bin"), dir)
    selFILE <- if(rename){
      file.path(selDIR, "selenium-server-standalone.jar")
    }else{
      file.path(selDIR, gsub(".*(selenium-server-standalone.*)", "\\1", 
                             selJARdownload))
    }
    
    if (update || !file.exists(selFILE)) {
      dir.create(selDIR, showWarnings=FALSE)
      message("DOWNLOADING STANDALONE SELENIUM SERVER. THIS MAY TAKE 
              SEVERAL MINUTES")
      download.file(paste0( selURL, "/", selJARdownload), selFILE, 
                    mode = "wb")
    }
  }
  
  # use default server 
  startServer <- function (dir = NULL, args = NULL, javaargs = NULL, 
                           log = TRUE,  ...) 
  {
    selDIR <-  ifelse(is.null(dir), file.path(find.package("RSelenium"), 
                                              "bin"), dir)
    selFILE <- file.path(selDIR, "selenium-server-standalone.jar")
    if (!file.exists(selFILE)) {
      possFiles <- list.files(selDIR, "selenium-server-standalone")
      if(length(possFiles) == 0){
        stop("No Selenium Server binary exists. Run checkForServer or start 
             server manually.")
      }
      # pick most recent driver
      selFILE <- possFiles[order(gsub(".*-(.*).jar$", "\\1", possFiles), 
                                 decreasing = TRUE)][1]
      selFILE <- file.path(selDIR, selFILE)
      }
    logFILE <- file.path(selDIR, "sellog.txt")
    selArgs <- c(paste("-jar", shQuote(selFILE)))
    if(log){
      write("", logFILE)
      selArgs <- c(selArgs, paste("-log", shQuote(logFILE)))
    }
    selArgs <- c(javaargs, selArgs, args)
    userArgs <- list(...)
    if (.Platform$OS.type == "unix") {
      initArgs <- list(command = "java", args = selArgs, wait = FALSE, 
                       stdout = FALSE, stderr = FALSE)
    }
    else {
      initArgs <- list(command = "java",args = selArgs, wait = FALSE, 
                       invisible = TRUE)
    }
    initArgs[names(userArgs)] <- userArgs 
    do.call(system2, initArgs)
    if (.Platform$OS.type == "windows"){
      wmicOut <- tryCatch({
        system2("wmic",
                args = c("path win32_process get Caption,Processid,Commandline"
                         , "/format:htable")
                , stdout=TRUE, stderr=NULL)
      }, error = function(e)e)
      selPID <- if(inherits(wmicOut, "error")){
        wmicArgs <- paste0(c("path win32_process where \"commandline like '%",
                             selFILE, "%'\" get Processid"))
        wmicOut <- system2("wmic", 
                           args = wmicArgs
                           , stdout = TRUE)
        as.integer(gsub("\r", "", wmicOut[2]))
      }else{
        wmicOut <- readHTMLTable(htmlParse(wmicOut), header = TRUE, 
                                 stringsAsFactors = FALSE)[[1]]
        wmicOut[["ProcessId"]] <- as.integer(wmicOut[["ProcessId"]])
        idx <- grepl(selFILE, wmicOut$CommandLine)
        if(!any(idx)) stop("Selenium binary error: Unable to start Selenium 
                           binary. Check if java is installed.")
        wmicOut[idx,"ProcessId"]
      }
    }else{
      if(Sys.info()["sysname"] == "Darwin"){
        sPids <- system('ps -Ao"pid"', intern = TRUE)
        sArgs <- system('ps -Ao"args"', intern = TRUE)
      }else{
        sPids <- system('ps -Ao"%p"', intern = TRUE)
        sArgs <- system('ps -Ao"%a"', intern = TRUE)
      }
      idx <- grepl(selFILE, sArgs)
      if(!any(idx)) stop("Selenium binary error: Unable to start Selenium 
                         binary. Check if java is installed.")
      selPID <- as.integer(sPids[idx])
    }
    
    list(
      stop = function(){
        tools::pskill(selPID)
      },
      getPID = function(){
        return(selPID)
      }
    )
    }
  
  #My Code Begins
  
  checkForServer()
  startServer(invisible = FALSE, log = FALSE)
  
  # Load webpage
  mybrowser <- remoteDriver(browser = "chrome")
  mybrowser$open(silent = TRUE)
  mybrowser$getStatus()
  
  url <- paste0('https://www.draftkings.com/lobby#/MLB/0/Tournament')
  mybrowser$navigate(url)
  
  username = mybrowser$findElement(using = "css", "[name = 'username']")
  username$sendKeysToElement(list('chigorin'))
  password = mybrowser$findElement(using = "css", "[name = 'password']")
  password$sendKeysToElement(list('pizza1995', key = "enter"))

  Sys.sleep(3) # sleep system for 3 seconds
  mybrowser$navigate(url)
  
  
  #webElem <- mybrowser$findElement(using = 'css selector', value = "._1zpPDEgsQJYHz7UOkFa1TK:nth-child(3) a")
  #webElem$clickElement()
  
  ########## THIS PART DOESNT WORK
  script <- "return packagedContests;"
  dk_contests_json <- mybrowser$executeScript(script, args = list())
  
  # Close out of Browser
  mybrowser$quit()
  
  indx <- sapply(dk_contests_json, length)
  full_table <- as.data.frame(do.call(rbind,lapply(dk_contests_json, `length<-`,
                                            max(indx))))
  colnames(full_table) <- names(dk_contests_json[[which.max(indx)]])
  
  # attr is a list, remove list from df and append it as vectors
  attr_list <-  do.call(rbind, full_table$attr)
  attr_list <- as.data.frame(attr_list)
  for(i in 1:ncol(attr_list)) {
    print(i)
    attr_list[,i] <- unlist(attr_list[,i])
  }
  
  full_table$attr <- NULL
  
  # pd is a list, remove list from df and append it as vectors
  pd_list <-  do.call(rbind, full_table$pd)
  pd_list <- as.data.frame(pd_list)
  for(i in seq(ncol(pd_list))) {
    print(i)
    pd_list[,i] <- unlist(pd_list[,i])
  }
  
  full_table$pd <- NULL
  
  df <- cbind(full_table, attr_list, pd_list)
  df <- df[df$sport == 'MLB',]
  df <- as.data.frame(t(apply(df, 1, unlist)))
  df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
  
  df$Cash <- as.numeric(df$Cash)
  df$mec <- as.numeric(df$mec)
  df <- df[df$mec > 10,]

  
  todays_day <- format(as.Date(Sys.Date()), '%a')
  
  
  df <- df[grepl(as.character(todays_day), df$sdstring),]
  vars_needed <- c('id','n', 'mec', "a")
  df <- df[, vars_needed]
  df$Contest_Date <- as.Date(Sys.Date())
  names(df) <- c('Contest_ID', 'Contest_Name','Max_Entry', 'Entry_Fee', 'Contest_Date')
  df$Entry_Fee <- as.numeric(df$Entry_Fee)
  df$Entry_Fee <- sprintf("%.2f", as.numeric(df$Entry_Fee))
  df$Entry_Fee <- paste0('$',df$Entry_Fee)
  

  return_df <- rbind(contest_info, df)
  write.csv(return_df, file = 'MLB/data_warehouse/contests.csv', row.names = F)
  return(return_df)
}

