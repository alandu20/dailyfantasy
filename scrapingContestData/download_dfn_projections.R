#setwd("~/Projects/DFS/")
#setwd("~/Documents/PrincetonFall16/fantasyfootball/DFS/")

library(dplyr)
library(rvest)


download_dfn_projections <- function() {
  original_wd <- getwd()
  
  library(RSelenium)
  library(XML)
  library(lubridate)
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
  startServer()
  
  # Load webpage
  mybrowser <- remoteDriver(browser = "chrome")
  mybrowser$open(silent = TRUE)
  mybrowser$getStatus()
  
  url <- paste0('https://dailyfantasynerd.com/login')
  mybrowser$navigate(url)
  
  
  username = mybrowser$findElement(using = "css selector", "#input-username")
  username$sendKeysToElement(list('dfsoptimizers2017'))
  password = mybrowser$findElement(using = "css selector", "#input-password")
  password$sendKeysToElement(list('pizza1995', key = "enter"))
  Sys.sleep(2)
  mybrowser$navigate('https://dailyfantasynerd.com/optimizer/draftkings/mlb')
  Sys.sleep(2)
  
  # Download Hitters
  download_hitters_button = mybrowser$findElement(using = 'name', value = "Hitters")
  download_hitters_button$clickElement()
  
  #Switch to Pitchers
  download_pitchers_button = mybrowser$findElement(using = 'name', value = "Pitchers")
  download_pitchers_button$clickElement()

  # Close out of Browser
  mybrowser$quit()
  
  d <- Sys.Date()
  date <- as.character(paste(month(d), day(d), sep="-"))

  ### Pick up csv's in download folder
  setwd('~/Downloads')
  pitchers_file_name <- paste0('DFN MLB Pitchers DK ', date, ".csv")
  hitters_file_name <- paste0('DFN MLB Hitters DK ', date, ".csv")
  
  while(!file.exists(pitchers_file_name)){
    Sys.sleep(1)
  }
  
  dfn_hitters <- read.csv(hitters_file_name, stringsAsFactors = F)
  dfn_pitchers <- read.csv(pitchers_file_name, stringsAsFactors = F)
  
  file.remove(hitters_file_name)
  file.remove(pitchers_file_name)
  #setwd(paste0(original_wd, 'MLB/data_warehouse/', date)
  dfn_projections_path <- file.path(original_wd,'MLB/data_warehouse/projections/dailyfantasynerd')
  
  setwd(dfn_projections_path)
  pitchers_file_name <- paste0('pitchers_', Sys.Date(), ".csv")
  hitters_file_name <- paste0('hitters_', Sys.Date(), ".csv")
  write.csv(dfn_pitchers, file = pitchers_file_name, row.names = F)
  write.csv(dfn_hitters, file = hitters_file_name, row.names = F)
  
  setwd(original_wd)
  return(0)
  }


