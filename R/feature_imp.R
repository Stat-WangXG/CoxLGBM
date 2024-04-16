#' @title Compute feature importance in a model
#'
#' @param model object of class \code{lgb.Booster}
#' @param percentage whether to show importance in relative percentage
#'
#' @return a \code{data.table} of feature importances in a model
#' @export
coxlgbm.importance <- function(model, percentage = TRUE){
  return(lightgbm::lgb.importance(model = model, percentage = percentage))
}
