feature_creation <- function(data, id_var = SK_ID_CURR, time_var = MONTHS_BALANCE,
                             # functions = funs(MAX = max, MIN = min, AVG = mean, VAR = var),
                             diff = c(6, 12,  24,  48,  96) - 1) {

  message("Starting")

  # bureau_balance_credit_type$MONTHS_BALANCE %>% quantile(0:10/10)
  # id_var <- rlang::expr(SK_ID_CURR)
  # time_var <- rlang::expr(MONTHS_BALANCE)
  # d <- bureau_balance_credit_type
  # d <- sample_n(d, 1000000)

  id_var <- rlang::enquo(id_var)
  id_var_char <- as.character(id_var)[[2]]
  time_var <- rlang::enquo(time_var)

  vars <- data %>%
    select(-!!time_var, -!!id_var) %>%
    names()

  npad <- diff %>% max() %>% str_length()

  dcross <- crossing(var = vars, p1 = diff, p2 = diff) %>%
    mutate_at(2:3, ~ .x %>% str_pad(npad, pad = "0"))

  # AVGAVG ------------------------------------------------------------------
  message("\tavg/avg variables")
  davgavg <- diff %>%
    map(function(p = 12){

      data %>%
        filter(!!time_var <= p) %>%
        group_by(!!id_var) %>%
        summarise_all(.funs = funs(AVG = mean)) %>%
        rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    reduce(full_join, by = id_var_char)

  dcross_trends <- dcross %>%
    filter(p1 < p2) %>%
    mutate(
      name = str_c(var, "_AVG_TREND_", p1, "_", p2),
      operation = str_c(var, "_AVG_", p1, "/", var, "_AVG_", p2)
    ) %>%
    select(name, operation) %>%
    deframe()

  davgavg <- davgavg %>%
    mutate_(.dots = dcross_trends) %>%
    select_(.dots = c(id_var_char, names(dcross_trends))) %>%
    mutate_all(replace_na, 0)

  gc(verbose = FALSE)

  # MINMAX ------------------------------------------------------------------
  message("\tmin/max variables")
  dminmax <- diff %>%
    map(function(p = 12){

      data %>%
        filter(!!time_var <= p) %>%
        group_by(!!id_var) %>%
        summarise_all(.funs = funs(MIN = min, MAX = max)) %>%
        rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    reduce(full_join, by = id_var_char)

  dcross_minmax <- dcross %>%
    filter(p1 == p2) %>%
    mutate(
      name = str_c(var, "_MIN_MAX_", p1),
      operation = str_c(var, "_MIN_", p1, "/", var, "_MAX_", p2)
    ) %>%
    select(name, operation) %>%
    deframe()

  dminmax <- dminmax %>%
    mutate_(.dots = dcross_minmax) %>%
    select_(.dots = c(id_var_char, names(dcross_minmax))) %>%
    mutate_all(replace_na, 0)

  gc(verbose = FALSE)

  # AVGMAX ------------------------------------------------------------------
  message("\tavg/max variables")
  davgmax <- diff %>%
    map(function(p = 12){

      data %>%
        filter(!!time_var <= p) %>%
        group_by(!!id_var) %>%
        summarise_all(.funs = funs(AVG = mean, MAX = max)) %>%
        rename_at(-1, ~ p %>% str_pad(npad, pad = "0") %>% str_c(.x, ., sep = "_"))

    }) %>%
    reduce(full_join, by = id_var_char)

  dcross_avgmax <- dcross %>%
    filter(p1 == p2) %>%
    mutate(
      name = str_c(var, "_AVG_MAX_", p1),
      operation = str_c(var, "_AVG_", p1, "/", var, "_MAX_", p2)
    ) %>%
    select(name, operation) %>%
    deframe()

  davgmax <- davgmax %>%
    mutate_(.dots = dcross_avgmax) %>%
    select_(.dots = c(id_var_char, names(dcross_avgmax))) %>%
    mutate_all(replace_na, 0)

  gc(verbose = FALSE)


  # REC ANT -----------------------------------------------------------------
  message("\trec ant variables")
  drecant <- map(vars, function(x){

    data %>%
      select(!!id_var, !!time_var, x) %>%
      filter(!!time_var <= max(diff)) %>%
      filter(!!sym(x) > 0) %>%
      group_by(!!id_var) %>%
      summarize(
        REC = min(!!time_var),  ANT = max(!!time_var)
      ) %>%
      mutate(
        DIFF_REC_ANT = ANT - REC
      ) %>%
      rename_at(-1, ~ paste(x, .x, sep = "_"))

  }) %>%
    reduce(full_join, by = id_var_char)

  # OUT ---------------------------------------------------------------------
  message("\tjoining variables")
  dout <- list(
    davgavg,
    dminmax,
    davgmax,
    drecant
  ) %>%
    reduce(full_join, by = id_var_char)

  rm(davgavg, dminmax, davgmax, drecant)
  gc()

  dout

}
