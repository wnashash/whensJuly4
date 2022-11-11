#' @title Days Until Birthday
#' @description A simple function to determine how many days are left until July 4th.
#' @param date (Character) The target date in "YYYY-MM-DD" format. Defaults to the current date.
#' @details The description says it all!
#' @return Returns a message with number of days left until July 4th.
#' @author Walid Nashashibi (\url{https://github.com/wnashash/})
#' @examples
#' library(whensJuly4)
#' Sys.Date() # for reference
#' whensJuly4()
#' @importFrom lubridate days today year ymd
#' @export
whensJuly4 <- function(date=NULL){
  if(is.null(date)){
    date <- lubridate::today()
  } else {
    date <- tryCatch(
      lubridate::ymd(date),
      warning = function(w) {
        stop("Could not parse the provided date.
             Is it in \"YYYY-MM-DD\" format?", call. = FALSE)
      }
    )
  }

  current_year <- lubridate::year(date)
  current_bday <- lubridate::ymd(paste0(current_year,"-07-04"))

  if(date < current_bday){
    next_bday <- current_bday
  } else if(date > current_bday) {
    next_bday <- lubridate::ymd(paste0(current_year+1, "-07-04"))
  } else {
    next_bday <- current_bday
  }

  bday_diff <- lubridate::days(next_bday - date)

  if(date == current_bday){
    bday_text <- "Your birthday is today!"
  } else {
    bday_text <- paste0("Only ", bday_diff@day, " days unitl your birthday!")
  }

  message(bday_text)

}
