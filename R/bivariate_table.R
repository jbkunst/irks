#' Calculate bivariate table from a vector and a response variable
#' @param x A variable.
#' @param target A binary variable same length as \code{x}.
#'
#' @examples
#'
#' data(german_credit)
#'
#' bivariate_table(german_credit$personal_status_and_sex, german_credit$good_bad)
#'
#' @export
bivariate_table <- function(x, target){

  tot_target <- sum(target)
  tot_non_target <- length(target) - tot_target

  bt <- dplyr::data_frame(x = x, target) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      n = length(target),
      percent = n/nrow(.),
      target_n = sum(target),
      target_rate = target_n/n,
      target_percent = target_n/tot_target,
      non_target_n = (n - target_n),
      non_target_percent = (n - target_n)/tot_non_target,
      odds = target_n/(n - target_n),
      woe = log(target_percent/non_target_percent),
      iv = (target_percent - non_target_percent) * woe) %>%
    dplyr::ungroup()

  bt

}
