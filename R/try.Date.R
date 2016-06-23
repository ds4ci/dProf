#' Look for dates in a character vector.
#'
#' \code{try.Date} tries common date formats on elements of a character vector
#' and returns the maximum number of dates found.
#'
#' Implicit assumption is Dates in vector have same format.
#'
#' @param try_this character Vector to test for Date compatible elements
#' @return integer maximum number of Date compatible elements found
#' @examples
#' try.Date(c("1/1/1999", "12/1/2020", "abcd"))
#' try.Date(c("12/30/1999", "30/12/1999", "1999-12-30"))
try.Date <- function(try_this){
  formats <- c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%Y")
  nDates <- integer(length(formats))
  for(ifmt in seq_along(formats)){
    nDates[ifmt] <- sum(!is.na(as.Date(try_this,
                                       format = formats[ifmt])))
  }
  max(nDates)
}