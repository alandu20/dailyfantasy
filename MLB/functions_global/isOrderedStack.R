
isOrderedStack <- function(vector_of_batting_order) { 
  if(any(is.na(vector_of_batting_order))) {
    return(FALSE)
  }
  if(typeof(type.convert(vector_of_batting_order, as.is = TRUE)) == 'character') {
    return(FALSE)
  }
  sorted <- sort(type.convert(vector_of_batting_order, as.is = TRUE))
  has_batter_1 <- sorted[1] == 1
  
  current_streak <- 1
  
  while(sorted[current_streak]+1 == sorted[current_streak+1]) {
    current_streak <- current_streak + 1
    if(current_streak == length(sorted)){
      break()
    }
  }
  
  if(has_batter_1) {
    check_number <- 9 
    index_check <- length(sorted)
    while(sorted[index_check] == check_number) {
      check_number <- check_number - 1 
      current_streak <- current_streak + 1
      index_check <- index_check - 1
    }
  }
  
  return(current_streak == length(sorted))
}


