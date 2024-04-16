#' @title Train a CoxLGBM model
#'
#' @param time individual observation times
#' @param status censoring indicators
#' @param x the prediction variables
#' @param obj objective function, It should be set to \code{lossobj} here
#' @param eval evaluation metric, It should be set to \code{cox.nloglik} here
#' @param ... other parameters for \code{lightgbm}
#'
#' @return a trained \code{lgb.Booster}
#' @export
#' @examples
#' # Generate some survival data with 10 informative covariates
#' n <- 200; p <- 20
#' beta <- c(rep(1,10),rep(0,p-10))
#' x <- matrix(rnorm(n*p),n,p)
#' real.time <- -(log(runif(n)))/(10*exp(drop(x %*% beta)))
#' cens.time <- rexp(n,rate=1/10)
#' status <- ifelse(real.time <= cens.time,1,0)
#' obs.time <- ifelse(real.time <= cens.time,real.time,cens.time)
#' # Fit a CoxLGBM model
#' model <- coxlgbm.train(time = obs.time, status = status, x = x, obj = lossobj, eval = cox.nloglik,
#'                        params = list(min_data = 5, learning_rate = 0.05), eval_freq = 0, nrounds = 100)
coxlgbm.train <- function(time, status, x, obj = lossobj, eval = cox.nloglik, ...){
  dtrain <- list(data = data.matrix(x), label = time * (-(-1)^(as.numeric(status))))
  Dtrain <- lightgbm::lgb.Dataset(dtrain$data,label=dtrain$label)

  model <- lightgbm::lightgbm(
    data = Dtrain,
    obj = obj,
    eval = eval,
    ...
  )
  return(model)
}


