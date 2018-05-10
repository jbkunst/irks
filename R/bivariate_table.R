#' Calculate bivariate table from a vector and a response variable
#' @param x A variable.
#' @param target A binary variable same length as \code{x}.
#' @param add.prop.test Boolean to add or not the \code{prop.test} by
#'   categories.
#' @param conf.level Confidence level of the returned confidence interval.
#'
#' @examples
#'
#' data(german_credit)
#'
#' bivariate_table(german_credit$purpose, german_credit$good_bad)
#'
#'
#' x <- cut(german_credit$duration_in_month, c(0, 6, 12, 24, 36, Inf))
#' target <- german_credit$good_bad
#'
#' bivariate_table(x, target)
#'
#' @export
bivariate_table <- function(x, target, add.prop.test = TRUE, conf.level = 0.95){

  tot_target <- sum(target)
  tot_non_target <- length(target) - tot_target

  bt <- dplyr::data_frame(x, target) %>%
    dplyr::group_by(!!sym("x")) %>%
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

  if(add.prop.test) {
    bt <- bt %>%
      dplyr::mutate(
        test = purrr::map2(target_n, n, ~ dplyr::as_data_frame(t(prop.test(.x, .y, conf.level = conf.level)$conf.int)) %>% purrr::set_names(c("lower", "upper")))
      ) %>%
      tidyr::unnest()
  }

  bt

}
