#' Perform a very raw Feature Selection
#' @param data A data frame to perform the feature selection.
#' @param target The target.
#' @param max_concentration_limit A maximum concentration limit.
#' @param min_iv Minimum Information Value to accpet
#' @param verbose verbose.
#' @param ... Additional parameters for \code{binning} function.
#' @examples
#'
#' data("german_credit")
#'
#' fs <- feature_selection(german_credit, good_bad, max_concentration_limit = 0.4, min_iv = 0.1)
#'
#' table(fs$decision)
#'
#' @export
feature_selection_raw <- function(data, target = TARGET, max_concentration_limit = 0.9, min_iv = 0.2, verbose = TRUE, ...){

  if(verbose) message("Starting Raw Feature Selection")

  # target <- rlang::expr(TARGET)
  target_var <- rlang::enquo(target)

  if(verbose) message("\tCalculating concentrations")
  data_summary <- dplyr::summarise_all(data, .funs = max_concentration)

  variables0 <- data_summary %>%
    tidyr::gather() %>%
    dplyr::filter(value > max_concentration_limit) %>%
    dplyr::pull(key)

  variables1 <- data_summary %>%
    tidyr::gather() %>%
    dplyr::filter(value <= max_concentration_limit) %>%
    dplyr::pull(key)

  data <- data %>%
    dplyr::select(!!target_var, variables1) %>%
    dplyr::mutate_if(is.numeric, round, 2)

  if(verbose) message("\tCalculating Information Values")
  biv <- describe_bivariate(data, target = !!target_var, verbose = verbose, ...)

  if(verbose){
    biv %>%
      dplyr::mutate(iv_label = irks::label_iv(iv)) %>%
      dplyr::count(iv_label) %>%
      print()
  }

  variables2 <- biv %>%
    dplyr::filter(iv < min_iv) %>%
    dplyr::pull(variable)

  variables3 <- biv %>%
    dplyr::filter(iv >= min_iv) %>%
    dplyr::pull(variable)

  dout <- list(
    dplyr::data_frame(variable = variables0, decision = "removed by concentration"),
    dplyr::data_frame(variable = variables2, decision = "removed by iv"),
    dplyr::data_frame(variable = variables3, decision = "selected")
  ) %>%
    dplyr::reduce_right(dplyr::bind_rows)

  dout %>% dplyr::count(decision)

  dout

}
