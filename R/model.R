#' Get a summary of the logistic glm model considering reference levels
#' @param model A glm logistic model
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
model_summary <- function(model) {

  # model <- readRDS("D:/Docs/modelo-behavior/data/23/05_modelo.rds")
  # model_summary(model)

  response_variable <- model %>%
    stats::as.formula() %>%
    as.character() %>%
    .[2] %>%
    stringr::str_remove_all("factor\\(|\\)")

  variables <- irks:::model_variables(model)

  dmod <- model$data %>%
    dplyr::select_(.dots = c(response_variable, variables)) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    tidyr::gather("key", "value", variables) %>%
    dplyr::group_by(!!!syms(c("key", "value"))) %>%
    dplyr::summarise(
      p = n(),
      target_rate = mean(!!sym(response_variable))
    ) %>%
    dplyr::group_by(!!sym("key")) %>%
    dplyr::mutate(p = p/sum(p)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      term = paste0(key, value),
      key = factor(key, levels = variables)
    )

  # dmod %>% dplyr::count()
  # dmod %>% dplyr::group_by(key) %>% dplyr::summarise(sum(p))

  dmod1 <- dplyr::tbl_df(broom::tidy(model))

  dmod <- dplyr::full_join(dmod1, dmod, by = "term") %>%
    # arrange(key, target_rate) %>%
    dplyr::mutate(
      key = as.character(key),
      key = ifelse(term == "(Intercept)", "(Intercept)", key),
      key = forcats::fct_inorder(key)
    ) %>%
    dplyr::select(variable = key, term, category = value, estimate, std.error, p.value, marginal_percent = p, target_rate)

  # dmod %>% dplyr::filter(is.na(estimate))
  # dmod %>% dplyr::group_by(variable) %>% dplyr::summarise(sum(marginal_percent))

  lvls <- purrr::map2(
    names(model$xlevels),
    model$xlevels,
    ~ dplyr::data_frame(variable = .x, category = .y)
  ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    tidyr::unite(!!sym("term"), !!sym("variable"), !!sym("category"), sep = "") %>%
    dplyr::bind_rows(dplyr::data_frame(term = "(Intercept)"), .) %>%
    dplyr::pull()

  dmod <- dmod %>%
    dplyr::mutate(t2 = factor(!!sym("term"), levels = lvls)) %>%
    dplyr::arrange(!!sym("t2")) %>%
    dplyr::select(-!!sym("t2"))

  # dmod %>% dplyr::group_by(variable) %>% dplyr::summarise(sum(marginal_percent))
  dmod
}

#' Plot a model
#'
#' @param model A glm logistic model
#' @examples
#'
#' data("german_credit")
#'
#' model <- glm(
#'   good_bad ~ purpose + present_employment_since + credit_history,
#'   data = german_credit, family = binomial
#'   )
#'
#' model_plot(model)
#'
#' @export
model_plot <- function(model){
  model %>%
    irks::model_summary() %>%
    dplyr::filter(variable != "(Intercept)") %>%
    dplyr::mutate(category = forcats::fct_inorder(category)) %>%
    dplyr::mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    dplyr::mutate(variable2 = as.character(variable)) %>%
    dplyr::mutate(variable2 = forcats::fct_inorder(variable2)) %>%
    ggplot2::ggplot() +
    # geom_col(aes(category, marginal_percent), width = 0.5, fill = "gray80") +
    ggplot2::geom_col(ggplot2::aes(category, marginal_percent), group = "estimacion", color = NA, alpha = 0.2, width = 0.3) +
    ggplot2::geom_line(ggplot2::aes(category, target_rate), group = "target_rate_dev", color = "red") +
    ggplot2::geom_line(ggplot2::aes(category, estimate), group = "estimacion", color = "blue") +
    ggplot2::facet_wrap( ~ variable2, scales = "free",  labeller = ggplot2::labeller(label = ggplot2::label_wrap_gen(35))) +
    ggplot2::labs(x = "Categoría", y = "Porcentaje/Tasa de Incumplimiento") +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 8))
}

model_variables <- function(model) {

  # names(model$xlevels)
  model %>%
    as.formula() %>%
    as.character() %>%
    dplyr::last() %>%
    stringr::str_split("\\s\\+\\s") %>%
    unlist()

}

# model <- readRDS("data-raw/modelo.rds")
model_check <- function(model, sig.level = 0.1, missing_category = "(Missing)")  {
  all(
    model_check_significant_coefficients(model, sig.level),
    model_check_monotonous(model, missing_category)
  )
}

model_check_significant_coefficients_detail <- function(model, sig.level = 0.1) {

  irks::model_summary(model) %>%
    dplyr::filter(p.value >= sig.level) %>%
    dplyr::pull(variable) %>%
    unique() %>%
    as.character()

}

model_check_significant_coefficients <- function(model, sig.level = 0.1){

  length(model_check_significant_coefficients_detail(model, sig.level)) == 0

}

model_check_monotonous_detail <- function(model, missing_category = "(Missing)") {

  numeric_variables <- model_variables(model) %>%
    str_remove("_cat") %>%
    select(model$data, .) %>%
    map(class) %>%
    map_chr(first) %>%
    enframe() %>%
    filter(value == "numeric") %>%
    pull(name)

  # model <- modelostep
  # model_plot(model)
  msummary <- irks::model_summary(model) %>%
    filter(variable != "(Intercept)") %>%
    filter(variable %in% str_c(numeric_variables, "_cat", sep = "")) %>%
    mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    # filter(category != missing_category) %>%
    select(variable, category, estimate, target_rate) %>%
    group_by(variable) %>%
    mutate(
      estimate_rank = rank(estimate),
      target_rate_rank = rank(target_rate)
    )

  # variables which does not keep the rank of the target rate/estimate
  v1 <- msummary %>%
    filter(estimate_rank != target_rate_rank) %>%
    distinct(variable) %>%
    pull(1) %>%
    as.character()

  v2 <- msummary %>%
    filter(category != missing_category) %>%
    mutate(
      diff_estimate = c(NA, diff(estimate)),
      diff_target_rate = c(NA, diff(target_rate)),
      sign_estimate = sign(diff_estimate),
      sign_target_rate = sign(diff_target_rate)
    ) %>%
    summarise(
      tbl_sign_estimate = length(table(sign_estimate)),
      tbl_sign_target_rate = length(table(sign_target_rate))
    ) %>%
    filter(tbl_sign_estimate > 1) %>%
    pull(variable) %>%
    as.character()

  as.character(unique(c(v1, v2)))

}

model_check_monotonous <- function(model, missing_category = "(Missing)") {

  length(model_check_monotonous_detail(model, missing_category)) == 0

}


# model <- readRDS("data-raw/modelo2.rds")
# model_summary(model)
fix_model <- function(model, sig.level = 0.1, missing_category = "(Missing)", p.min = 5/100, alpha = 0.1, verbose = TRUE, delta = 5/100,
                      max_iterations = 20) {

  iter <- 1
  p.min_actual <- p.min - delta
  data <- model$data
  dfbreaks_tot <- data_frame()

  while(!model_check(model, sig.level = 0.1, missing_category = "(Missing)")) {

    if(iter > max_iterations) break

    p.min_actual <- p.min_actual + delta

    if(verbose) message("Iteration: ", iter, " p.min_actual: ", p.min_actual)

    vars_to_fix <- c(
      model_check_monotonous_detail(model),
      model_check_significant_coefficients_detail(model)
    ) %>%
      unique()

    message("\tVariables to treat: ", str_c(vars_to_fix, collapse = ", "))

    binnings <- map(vars_to_fix, function(varname){ # varname <- "antiguedad_bf_cat"

      message("\t\tTreating: ", varname)

      nameresponse <- str_c(sample(LETTERS, size = 5), collapse = "")

      bin <- binning(
        data[[str_remove(varname, "_cat")]],
        data %>%
          mutate_(.dots  = set_names(as.character(as.formula(model))[2], nameresponse)) %>%
          pull(nameresponse) %>%
          as.character() %>%
          as.numeric(),
        p.min = p.min_actual,
        alpha = alpha
      )

      if(nrow(bin$summary) == 1) {

        message("Variable ", varname, " don't produce more than 1 bin")
        print(bin)
        return(FALSE)

      }

      data[[varname]] <- apply_binning(bin, data[[str_remove(varname, "_cat")]])


      bin

    })

    dfbreaks_tot <- bind_rows(
      dfbreaks_tot,
      data_frame(
        variable = vars_to_fix,
        binning = binnings
      )
    )

    print(length(unique(data$antiguedad_bf_cat)))

    model <- glm(as.formula(model), data = data, family = binomial(link = logit))

    iter <- iter + 1

  }


}

