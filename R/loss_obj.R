#' @title Calculate the first and second order gradients
#'
#' @param preds the predictions of the log-hazard function in CoxLGBM model
#' @param dtrain a \code{\link[lightgbm]{lgb.Dataset}} object
#'
#' @return a list of the first and second order gradients
#' @export
lossobj <- function(preds, dtrain){
  labels <- lightgbm::get_field(dtrain, "label")
  status <- ifelse(labels > 0, 1, 0)
  mh <- as.numeric(exp(preds))
  survtime <- abs(labels)
  g <- c()
  h <- c()
  SR <- c()
  for(i in 1:length(labels)){
    SR[i] <- sum(mh[survtime >= survtime[i]])
  }
  for(j in 1:length(labels)){
    c1 <- sum(SR[survtime <= survtime[j] & status == 1]^(-1))
    c2 <- sum(SR[survtime <= survtime[j] & status == 1]^(-2))
    g[j] <- mh[j] * c1 - status[j]
    h[j] <- mh[j] * c1 - mh[j]^2 * c2
  }
  return(list(grad = g, hess = h))
}
