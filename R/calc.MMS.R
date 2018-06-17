#' Calculate min-max scaling
#'
#' @param x Indicator values.
#' @param refval Refrence value.
#' @param reverse Reverse direction of indicator values.

calc.MMS <- function(x, refval, reverse = FALSE)
{
    if(reverse){
        x <- 1-x
        refval <- 1-refval
    }
    ((x-min(x))/(max(x)-min(x))) / ((refval-min(x))/(max(x)-min(x)))
}
