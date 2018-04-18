#' Function to get data to plot a roc curve
#' @param score A numeric vector of predictions.
#' @param target A binary variable same length as \code{score}.
roc_data <- function(score, target) {
  pred <- ROCR::prediction(score, target)
  perf <- ROCR::performance(pred, "tpr", "fpr")
  df <- data.frame(x = unlist(perf@"x.values") , y = unlist(perf@"y.values"))
  df
}
