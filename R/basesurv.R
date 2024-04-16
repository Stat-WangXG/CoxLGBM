#' @title Compute the estimations of values of the survival related functions
#' @description Creates a \code{list} of values of cumulative baseline hazard
#'     function and baseline survival function at evaluation time points.
#' @param time the prediction variables
#' @param status individual observation times
#' @param preds the predictions of the log-hazard function in CoxLGBM model
#' @param times.eval evaluation time points, The default is \code{NULL}
#' @param centered the default is \code{FALSE}
#'
#' @return a \code{list} of time points, and the values of cumulative baseline
#'     hazard function and baseline survival function at these time points
#' @export
basesurv <- function(time, status, preds, times.eval = NULL, centered = FALSE){
  if(is.null(times.eval)){
    times.eval <- sort(unique(time))
  }

  t.unique <- sort(unique(time[status == 1]))
  alpha    <- length(t.unique)

  for(i in 1:length(t.unique)){
    alpha[i] <- sum(time[status == 1] == t.unique[i])/sum(exp(preds[time >=  t.unique[i]]))
  }

  obj <- approx(t.unique, cumsum(alpha), yleft=0, xout = times.eval, rule=2)

  if(centered){
    obj$y <- obj$y * exp(mean(preds))
  }
  obj$z <- exp(-obj$y)
  names(obj) <- c("times","cumBaseHaz","BaseSurv")
  return(obj)
}
