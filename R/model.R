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

  response <- model %>%
    stats::as.formula() %>%
    as.character() %>%
    .[2] %>%
    stringr::str_remove_all("factor\\(|\\)")

  variables <- model %>%
    as.formula() %>%
    as.character() %>%
    .[3] %>%
    stringr::str_split(" \\+ ") %>%
    unlist()

  dmod <- model$data %>%
    dplyr::select_(.dots = c(response, variables)) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    tidyr::gather("key", "value", variables) %>%
    dplyr::group_by(!!!syms(c("key", "value"))) %>%
    dplyr::summarise(
      p = n(),
      target_rate = mean(!!sym(response))
      ) %>%
    dplyr::group_by(!!sym("key")) %>%
    dplyr::mutate(p = p/sum(p)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      term = paste0(key, value),
      key = factor(key, levels = variables)
      )

  dmod1 <- dplyr::tbl_df(broom::tidy(model))

  dmod <- dplyr::full_join(dmod1, dmod, by = "term") %>%
    # arrange(key, target_rate) %>%
    dplyr::mutate(
      key = as.character(key),
      key = ifelse(term == "(Intercept)", "(Intercept)", key),
      key = forcats::fct_inorder(key)
    ) %>%
    dplyr::select(term, variable = key, category = value, estimate, std.error, p.value, marginal_percent = p, target_rate)

  lvls <- purrr::map2(
    names(model$xlevels),
    model$xlevels,
    ~ dplyr::data_frame(variable = .x, category = .y)
  ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::bind_rows(dplyr::data_frame(variable = "(Intercept)")) %>%
    tidyr::unite(!!sym("term"), !!sym("variable"), !!sym("category"), sep = "") %>%
    dplyr::pull()

  dmod <- dmod %>%
    dplyr::mutate(t2 = factor(!!sym("term"), levels = lvls)) %>%
    dplyr::arrange(!!sym("t2")) %>%
    dplyr::select(-!!sym("t2"))

  dmod
}

#' Creating a scorecard from logistic model
#'
#' The defaults are given by de Siddiqi example.
#'
#' @param model A glm logistic model
#' @param pdo default 20
#' @param score0 default 600
#' @param pdo0 default to 50/1
#' @param turn.orientation change the orientation of the scorecard points
#'
#' @examples
#'
#' data("german_credit")
#'
#' model <- glm(
#'   good_bad ~ purpose + present_employment_since + credit_history,
#'   data = german_credit, family = binomial
#'   )
#'
#' scorecard(model)
#'
#' @export
scorecard <- function(model, pdo = 20, score0 = 600, pdo0 = 50/1, turn.orientation = FALSE) {

  # model <- readRDS("D:/Docs/modelo-behavior/data/23/05_modelo.rds")
  # pdo <- 20; score0 <- 600; pdo0 <- 50;  turn.orientation = TRUE

  if(turn.orientation) {

    fmla <- as.formula(model)

    response <- as.character(fmla)[2]

    response_name <- stringi::stri_rand_strings(1, length = 10, pattern = "[A-Za-z]")

    response <- model$data %>%
      dplyr::mutate_(response_name = response) %>%
      dplyr::pull(response_name)

    if(is.numeric(response)) {
      response <- 1 - response
    } else {
      response <- forcats::fct_rev(factor(response))
    }

    model$data[[response_name]] <- response

    fmla2 <- as.formula(paste(response_name, " ~ ", as.character(fmla)[3]))

    model <- glm(fmla2, data = model$data, family = binomial(link = logit))

  }

  mod <- model_summary(model)

  b0 <- model$coefficients[1]

  a <- pdo/log(2)
  b <- score0 - (a * log(pdo0))
  k <- length(model$xlevels)

  pb <- (score0 + a * b0) / k

  modscorecard <- mod %>%
    dplyr::select(!!!syms(c("term", "estimate"))) %>%
    dplyr::mutate_(
      "score" = "as.integer(floor(a * ifelse(is.na(estimate), 0, estimate) + pb))"
      )

  modscorecard

}

model_check <- function(model) {
  TRUE
}

model_check_significant_coefficients <- function(model, sig.level = 0.1) {

  model %>%
    broom::tidy() %>%
    dplyr::filter(term != "(Intercept)") %>%
    dplyr::filter(p.value >= sig.level) %>%
    nrow() == 0

}

model_check_monotonous <- function(model, missing_category = "(Missing)") {
  # model <- modelostep
  irks::model_summary(model) %>%
    filter(variable != "(Intercept)") %>%
    filter(category != missing_category) %>%
    mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    select(variable, category, estimate, target_rate) %>%
    group_by(variable) %>%
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
    nrow() > 0
}


