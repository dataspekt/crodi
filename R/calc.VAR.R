#' Calculate variance
#'
#' @param x Vector of indicator values.

calc.VAR <-
    function(x)
{
    (1/length(x)) * (sum((x - mean(x))^2))
}
