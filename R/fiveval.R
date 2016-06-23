#' Calculate the five values of a vector.
#'
#' \code{fiveval} is a generalization of \code{fivenum} to allow non-numeric input
#'
#' Unlike \code{fivenum}, \code{fiveval} returns a character vector and will
#' accept any data type. For non-numerics, each value is just picked out of the
#' ordered list of values using the \code{fivenum} indexing scheme. Instead of "median", we use the "2nd Qtl" to make it
#' clear this is not \code{fivenum}.
#'
#'
#' @param x vector On which the five values will be calculated.
#' @return a 5-element character vector with elements named: Min, 1st Qtl,
#'   2nd Qtl, 3rd Qtl, and Max.
#' @examples
#'   fiveval(-100:100)
#'   fiveval(LETTERS)
#'   fiveval(seq(as.Date("1990-01-01"), as.Date("2009-12-31"), by = 1))
fiveval <- function(x) {
  xna <- is.na(x)
  if (all(xna)) return(rep.int(NA, 5))
  x <- x[!xna]
  x <- sort(x)
  n <- length(x)
  if (n == 0)
    rep.int(NA, 5)
  else {
    n4 <- floor((n + 3)/2)/2
    d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
    if (is.numeric(x))
      fv <- prettyNum(0.5 * (x[floor(d)] + x[ceiling(d)]),
                      big.mark = ",", nsmall = 3)
    else
      fv <- as.character(x[floor(d)])
  }
  names(fv) <- c("Min", "1st Qtl", "2nd Qtl", "3rd Qtl", "Max")
  fv
}