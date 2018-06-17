#' Calculate standard deviation
#'
#' @param x Vector of indicator values.


calc.S <-
    function(x)
{
    sqrt( (1/length(x)) * (sum((x - mean(x))^2)) )
}
