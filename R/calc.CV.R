#' Calculate coefficient of variation
#'
#' @param x Vector of indicator values.

calc.CV <-
    function(x)
{
    sqrt(calc.VAR(x))/mean(x)
}
