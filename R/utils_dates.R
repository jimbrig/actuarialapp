#' End of Month
#'
#' @param date character or date representation of a date.
#'
#' @return date
#' @export
#'
#' @importFrom lubridate is.Date ceiling_date
#'
#' @examples
#' # character input
#' end_of_month("2020-08-13")
#' # date input
#' end_of_month(as.Date("2020-08-13"))
end_of_month <- function(date) {
  if (is.character(date)) date <- as.Date(date)
  stopifnot(lubridate::is.Date(date))
  lubridate::ceiling_date(date, unit = "months") - 1
}

#' Beginning of Month
#'
#' @param date character or date representation of a date.
#'
#' @return date
#' @export
#'
#' @importFrom lubridate is.Date floor_date
#'
#' @examples
#' # character input
#' beg_of_month("2020-08-13")
#' # date input
#' beg_of_month(as.Date("2020-08-13"))
beg_of_month <- function(date) {
  if (is.character(date)) date <- as.Date(date)
  stopifnot(lubridate::is.Date(date))
  lubridate::floor_date(date, unit = "months")
}
