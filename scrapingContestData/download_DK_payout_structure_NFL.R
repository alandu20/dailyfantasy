

download_DK_payout_structure_NFL <- function(contest_number, date, contest_name) { 
  original_wd <- getwd()
  
  library(RSelenium)
  library(XML)
  
  ### Local Function
  
  strEndsWith <- function(haystack, needle)
  {
    hl <- nchar(haystack)
    nl <- nchar(needle)
    if(nl>hl)
    {
      return(F)
    } else
    {
      return(substr(haystack, hl-nl+1, hl) == needle)
    }
  }
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
  startServer()
  
  # Load webpage
  mybrowser <- remoteDriver(browser = "chrome")
  mybrowser$open(silent = TRUE)
  mybrowser$getStatus()
  
  url <- paste0('https://www.draftkings.com/draft/contest/', contest_number)
  mybrowser$navigate(url)
  
  #New UI
  Sys.sleep(1.5)
  webElem <- mybrowser$findElement(using = 'css selector', value = ".ContestInformation_text span")
  webElem$clickElement()
  
  # Parse HTML Table
  Sys.sleep(1.5)
  doc <- htmlParse(mybrowser$getPageSource()[[1]])
  
  # Close out of Browser
  mybrowser$quit()
  
  payout_structure <- readHTMLTable(doc)[[3]]
  payout_structure$Payout <- 0
  names(payout_structure) <- c('Place_lo', 'Place_hi', 'Payout')
  
  
  
  
  
  
  payout_structure$Payout <- payout_structure$Place_hi
  
  payout_structure$Place_lo <- gsub(" ", "", payout_structure$Place_lo, fixed = TRUE)
  
  # Clean Payout Column
  payout_structure$Payout <- gsub(",", "", payout_structure$Payout, fixed = TRUE)
  payout_structure$Payout <- gsub("$", "", payout_structure$Payout, fixed = TRUE)
  payout_structure$Payout <- as.numeric(payout_structure$Payout)
  
  # Change data.frame from factors into characters
  payout_structure[] <- lapply(payout_structure, as.character)
  for(index in 1:length(payout_structure$Place_lo)) {
    if (nchar(payout_structure$Place_lo[index]) == 3) {
      payout_structure$Place_hi[index] <- substr(payout_structure$Place_lo[index], 1, 1)
      payout_structure$Place_lo[index] <- substr(payout_structure$Place_lo[index], 1, 1)
    } else {
      temp <- strsplit(payout_structure$Place_lo[index], "-")[[1]]
      payout_structure$Place_lo[index] <- substr(temp[1],1,nchar(temp[1])-2)
      payout_structure$Place_hi[index] <- substr(temp[2],1,nchar(temp[2])-2)
    }
  }
  
  # Save file in correct Directory 
  setwd(paste0('NFL/data_warehouse/', date))
  file.sources = list.files()
  cleaned_files <- toupper(file.sources)
  cleaned_files <- gsub(" ", "", cleaned_files, fixed = TRUE)
  
  folder <- grep(toupper(get('contest_name')), cleaned_files, value=TRUE)
  
  correct_index = 0  
  for(index in 1:length(cleaned_files)) {
    if(strEndsWith(cleaned_files[index], toupper(get('contest_name')))) {
      correct_index = index
    }
  }
  
  setwd(file.sources[correct_index])
  
  
  write.csv(payout_structure, file = 'payout_structure.csv', row.names = F)
  
  setwd(original_wd)
  }

