#' Calculate Z-score
#'
#' @param x Numeric variable.
#' @param reverse Should standardized variable be reversed?

calc.Z <-
    function(x, reverse = FALSE)
{
    if(reverse)
        Z <- 100 - (x - mean(x))/calc.S(x) * 10
    else
        Z <- 100 + (x - mean(x))/calc.S(x) * 10
    return(Z)
}

