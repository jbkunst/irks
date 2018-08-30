#' Create variable from behavior data
#' @param data A data frame.
#' @param id_var The ID var.
#' @param time_var The TIME var.
#' @param diff A numeric vector to make sub variables
#' @param verbose verbose.
#' @export
feature_creation <- function(data, id_var = SK_ID_CURR, time_var = MONTHS_BALANCE,
                             diff = c(6, 12,  24,  48,  96), verbose = TRUE) {
  # Setup -------------------------------------------------------------------
  if(verbose) message("Starting Feature Creation")

  # bureau_balance_credit_type$MONTHS_BALANCE %>% quantile(0:10/10)
  # id_var <- rlang::expr(SK_ID_CURR)
  # time_var <- rlang::expr(MONTHS_BALANCE)
  # d <- bureau_balance_credit_type
  # d <- sample_n(d, 1000000)

  id_var <- rlang::enquo(id_var)
  id_var_char <- as.character(id_var)[[2]]
  time_var <- rlang::enquo(time_var)

  vars <- data %>%
    dplyr::select(-!!time_var, -!!id_var) %>%
    names()

  npad <- diff %>% max() %>% stringr::str_length()

  dcross <- tidyr::crossing(var = vars, p1 = diff, p2 = diff) %>%
    dplyr::mutate_at(2:3, ~ .x %>% str_pad(npad, pad = "0"))

  # AVGAVG ------------------------------------------------------------------
  if(verbose) message("\tCalculating avg/avg variables")
  davgavg <- diff %>%
    purrr::map(function(p = 12){

      data %>%
        dplyr::filter(!!time_var <= p) %>%
        dplyr::group_by(!!id_var) %>%
        dplyr::summarise_all(.funs = dplyr::funs(AVG = mean)) %>%
        dplyr::rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    dplyr::reduce(dplyr::full_join, by = id_var_char)

  dcross_trends <- dcross %>%
    dplyr::filter(p1 < p2) %>%
    dplyr::mutate(
      name = stringr::str_c(var, "_AVG_TREND_", p1, "_", p2),
      operation = stringr::str_c(var, "_AVG_", p1, "/", var, "_AVG_", p2)
    ) %>%
    dplyr::select(name, operation) %>%
    tibble::deframe()

  davgavg <- davgavg %>%
    dplyr::mutate_(.dots = dcross_trends) %>%
    dplyr::select_(.dots = c(id_var_char, names(dcross_trends))) %>%
    dplyr::mutate_all(replace_na, 0)

  gc(verbose = FALSE)

  # MINMAX ------------------------------------------------------------------
  if(verbose) message("\tCalculating min/max variables")
  dminmax <- diff %>%
    purrr::map(function(p = 12){

      data %>%
        dplyr::filter(!!time_var <= p) %>%
        dplyr::group_by(!!id_var) %>%
        dplyr::summarise_all(.funs = dplyr::funs(MIN = min, MAX = max)) %>%
        dplyr::rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    dplyr::reduce(dplyr::full_join, by = id_var_char)

  dcross_minmax <- dcross %>%
    dplyr::filter(p1 == p2) %>%
    dplyr::mutate(
      name = stringr::str_c(var, "_MIN_MAX_", p1),
      operation = stringr::str_c(var, "_MIN_", p1, "/", var, "_MAX_", p2)
    ) %>%
    dplyr::select(name, operation) %>%
    tibble::deframe()

  dminmax <- dminmax %>%
    dplyr::mutate_(.dots = dcross_minmax) %>%
    dplyr::select_(.dots = c(id_var_char, names(dcross_minmax))) %>%
    dplyr::mutate_all(tidyr::replace_na, 0)

  gc(verbose = FALSE)

  # AVGMAX ------------------------------------------------------------------
  if(verbose) message("\tCalculating avg/max variables")
  davgmax <- diff %>%
    purrr::map(function(p = 12){

      data %>%
        dplyr::filter(!!time_var <= p) %>%
        dplyr::group_by(!!id_var) %>%
        dplyr::summarise_all(.funs = dplyr::funs(AVG = mean, MAX = max)) %>%
        dplyr::rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    purrr::reduce(full_join, by = id_var_char)

  dcross_avgmax <- dcross %>%
    dplyr::filter(p1 == p2) %>%
    mutate(
      name = stringr::str_c(var, "_AVG_MAX_", p1),
      operation = stringr::str_c(var, "_AVG_", p1, "/", var, "_MAX_", p2)
    ) %>%
    dplyr::select(name, operation) %>%
    tibble::deframe()

  davgmax <- davgmax %>%
    dplyr::mutate_(.dots = dcross_avgmax) %>%
    dplyr::select_(.dots = c(id_var_char, names(dcross_avgmax))) %>%
    dplyr::mutate_all(replace_na, 0)

  gc(verbose = FALSE)

  # REC ANT -----------------------------------------------------------------
  if(verbose) message("\tCalculating rec ant variables")
  drecant <- map(vars, function(x){

    data %>%
      dplyr::select(!!id_var, !!time_var, x) %>%
      dplyr::filter(!!time_var <= max(diff)) %>%
      dplyr::filter(!!sym(x) > 0) %>%
      dplyr::group_by(!!id_var) %>%
      dplyr::summarize(
        REC = min(!!time_var),  ANT = max(!!time_var)
      ) %>%
      dplyr::mutate(
        DIFF_REC_ANT = ANT - REC
      ) %>%
      dplyr::rename_at(-1, ~ paste(x, .x, sep = "_"))

  }) %>%
    dplyr::reduce(dplyr::full_join, by = id_var_char)

  # OUT ---------------------------------------------------------------------
  if(verbose) message("\tCalculating Joining variables")
  dout <- list(
    davgavg,
    dminmax,
    davgmax,
    drecant
  ) %>%
    purrr::reduce(dplyr::full_join, by = id_var_char)

  rm(davgavg, dminmax, davgmax, drecant)
  gc()

  dout

}


