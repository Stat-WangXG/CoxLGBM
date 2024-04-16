#' @title Calculate the value of loss function
#'
#' @param preds the predictions of the log-hazard function in CoxLGBM model
#' @param dtrain a \code{\link[lightgbm]{lgb.Dataset}} object
#'
#' @return the value of loss function
#' @export
cox.nloglik <- function(preds, dtrain){
  labels <- lightgbm::get_field(dtrain, "label")
  status <- ifelse(labels > 0, 1, 0)
  mh <- as.numeric(exp(preds))
  survtime <- abs(labels)
  SR <- c()
  for(i in 1:length(labels)){
    SR[i] <- sum(mh[survtime >= survtime[i]])
  }
  err <- -2 * sum(status * (preds - log(SR))) / length(labels)
  return(list(name = "cox-nloglik",value = err,higher_better = FALSE))
}
