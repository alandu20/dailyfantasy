####### Description #######
# Function that returns a vector of dates where data is missing (either didn't collect data
# or no games that day).

listMissingDates <- function() {
  missing_dates <- as.Date(c("2017-12-03","2017-12-07","2017-12-08","2017-12-09","2017-12-10","2017-12-11",
                             "2017-12-15","2017-12-17","2017-12-18","2017-12-21","2017-12-24","2017-12-27",
                             "2017-12-31"))
  return(missing_dates)
}