#' Perform a very raw Feature Selection
#' @param data A data frame to perform the feature selection.
#' @param target The target.
#' @param max_concentration_limit A maximum concentration limit.
#' @param min_iv Minimum Information Value to accpet
#' @param verbose verbose.
#' @param ...Additional parameters for \code{binning} function.
#' @examples
#'
#' data("german_credit")
#'
#' model <- glm(
#'   good_bad ~ purpose + present_employment_since + credit_history,
#'   data = german_credit, family = binomial
#'   )
#'
#' model_summary(model)
#'
#' @export
raw_feature_selection <- function(data, target = TARGET, max_concentration_limit = 0.9, min_iv = 0.2, verbose = TRUE, ...){

  message("Starting Raw Feature Selection")
  # target <- rlang::expr(TARGET)
  target_var <- rlang::enquo(target)

  # message("fix data")
  # data <- data %>%
  #   mutate_if(is.numeric, replace_infs, replacement = inf_replacement)
  #
  message("\tGetting concentrations")
  data_summary <- summarise_all(data, .funs = max_concentration)

  variables0 <- data_summary %>%
    gather() %>%
    filter(value > max_concentration_limit) %>%
    pull(key)

  variables1 <- data_summary %>%
    gather() %>%
    filter(value <= max_concentration_limit) %>%
    pull(key)

  data <- data %>%
    select(!!target_var, variables1) %>%
    mutate_if(is.numeric, round, 2)

  biv <- describe_bivariate(data, target = !!target_var, verbose = verbose, ...)

  if(verbose){
    biv %>%
      mutate(iv_label = irks::label_iv(iv)) %>%
      count(iv_label) %>%
      print()
  }

  variables2 <- biv %>%
    filter(iv < min_iv) %>%
    pull(variable)

  variables3 <- biv %>%
    filter(iv >= min_iv) %>%
    pull(variable)

  dout <- list(
    data_frame(variable = variables0, decision = "removed by concentration"),
    data_frame(variable = variables2, decision = "removed by iv"),
    data_frame(variable = variables3, decision = "selected")
  ) %>%
    reduce_right(bind_rows)

  dout %>% count(decision)

  dout

}

max_concentration <- function(x){
  x %>%
    table() %>%
    prop.table() %>%
    max()
}
