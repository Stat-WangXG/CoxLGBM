#' @title Model Predictions
#'
#' @param object a trained CoxLGBM model
#' @param newx the new prediction variables
#' @param time the new individual observation times
#' @param status the new censoring indicators
#' @param times.eval evaluation time points, The default is \code{NULL}
#' @param centered the default is \code{FALSE}
#'
#' @return a \code{list} of time points, the values of cumulative baseline
#'     hazard function and baseline survival function at these time points,
#'     and the predicted log-hazard function
#' @export
coxlgbm.predict <- function(object, time, status, newx, times.eval = NULL, centered = FALSE){
  preds <- predict(object = object, data = newx)
  obj <- basesurv(time = time, status = status, preds = preds, times.eval = times.eval, centered = centered)
  obj$preds <- preds
  return(obj)
}
