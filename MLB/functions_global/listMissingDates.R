####### Description #######
# Function that returns a vector of dates where data is missing (either didn't collect data
# or no games that day).

# TODO: download DFN 7/10 (esp. pitcher file), regenerate pitcher.csv, and remove from list

listMissingDates <- function() {
  missing_dates <- as.Date(c("2017-06-28", "2017-07-09", "2017-07-10", "2017-07-11",
                             "2017-07-12", "2017-07-13", "2017-07-16", "2017-08-08", "2017-08-17"))
  # note: "2017-07-10" was all-star day contest so no pitchers
  return(missing_dates)
}