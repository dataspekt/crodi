#' Normalize indicator
#'
#' @param x Vector of indicator values.
#' @param method Method of normalization.
#' @param reverse Reversing direction of indicator values.
#' @param refval If method is \code{mms}, a reference value used in normalization of indicator values.
#'
#' @details Two methods of normalization are available. Method \code{zscore} will standardize indicator values to z-scores with mean of 100 and standard deviation of 10. Method \code{mms} will use minimum-maximum normalization with respect to the lowest sample value, further divided by normalized value on the sample level.
#'
#' @references Perisic & Wagner, Denona-Bogovic, Drezgic & Cegar.
#' 
#' @export

normalize <-
    function(x, method = c("zscore","mms"), reverse = FALSE, refval = NULL)
{
    switch(method[1],
           zscore = do.call("calc.Z", list(x = x, reverse = reverse)),
           mms = do.call("calc.MMS", list(x = x, reverse = reverse, refval = refval))
           )
}
