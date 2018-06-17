#' Calculate development index
#'
#' @param formula Index variables
#' @param data Dataset
#' @param penalize Penalize composite index.
#' @param wts Indicator weights.
#' @param aggregate Method of aggregation of indicator values.
#'
#' @details By default, function will calculate composite index penalized using Mazziotta-Pareto method. Setting \code{penalize = FALSE} will calculate composite index as a simple non-weighted mean of indicator values.
#'
#' To use indicator weights specify \code{wts} as a named list.
#'
#' Method of aggregation of indicator values is set via \code{aggregate} argument. Default is \code{mean}, but it can be a name of any function (typical alternative is \code{sum}).
#'
#' By setting both \code{penalize = TRUE} and \code{wts} it is possible to use weighted indicators with penalization.
#' 
#' @export

devindex <-
    function(formula, data, penalize = TRUE, wts = NULL, aggregate = "mean")
{
    mf <- stats::get_all_vars(formula,data)
    # apply 'wts' to 'mf'
    if(is.list(wts) & length(wts) == ncol(mf))
    {
        mf <- sapply(names(wts), function(x){
            data[,x]*wts[[x]]
        })
    }
    # calculate composite index
    i <- do.call("apply", list(X = mf, MARGIN = 1, FUN = aggregate))
    # penalize composite index
    if(penalize)
    {
        i <- i - penalize(mf)
    }
    # return
    return(i)
}
