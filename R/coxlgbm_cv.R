#' @title Main CV logic for CoxLGBM
#'
#' @param time individual observation times
#' @param status censoring indicators
#' @param x the prediction variables
#' @param obj objective function, It should be set to \code{lossobj} here
#' @param eval evaluation metric, It should be set to \code{cox.nloglik} here
#' @param ... other parameters for \code{lightgbm}
#'
#' @return a trained model \code{lgb.CVBooster}
#' @export
coxlgbm.cv <- function(time, status, x, obj = lossobj, eval = cox.nloglik, ...){
  dtrain <- list(data = data.matrix(x), label = time * (-(-1)^(as.numeric(status))))
  Dtrain <- lightgbm::lgb.Dataset(dtrain$data,label=dtrain$label)

  model <- lightgbm::lgb.cv(
    data = Dtrain,
    obj = obj,
    eval = eval,
    ...
  )
  return(model)
}
