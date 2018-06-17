#' Calculate penalization
#'
#' @param mf Data frame with normalized indicator values
#' 
#' @details Function will calculate penalty as defined in Mazziotta-Pareto method.
#' 
#' @export

penalize <-
    function(mf)
{
    apply(mf, 1, function(x) calc.S(x) * calc.CV(x))
}
