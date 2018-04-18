#' Get a summary of the logistic glm model considering reference levels
#' @param model A glm logistic model
#' @examples
#'
#' data("german_credit")
#' model <- step(glm(good_bad ~ ., data = german_credit, family = binomial))
#'
#' @export
model_summary <- function(model) {

  # model <- modelo

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
    tidyr::gather("key", "value", variables) %>%
    dplyr::group_by(key, value) %>%
    dplyr::summarise_(
      p = "n()",
      target_rate = sprintf("mean(%s)", response)
      ) %>%
    dplyr::group_by(key) %>%
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
    tidyr::unite(term, variable, category, sep = "") %>%
    dplyr::pull()


  dmod <- dmod %>%
    dplyr::mutate(t2 = factor(term, levels = lvls)) %>%
    dplyr::arrange(t2)

  dmod
}

#' Creating a scorecard from logistic model
#'
#' The defaults are given by de Siddiqi example.
#'
#' @param model A glm logistic model
#' @param pdo default 20
#' @param score0 default 600
#' @param pdo0 default to 20
#' @export
scorecard <- function(model, pdo = 50:1, score0 = 600, pdo0 = 20) {

  mod <- model_summary(model)

  b0 <- model$coefficients[1]

  a <- pdo/log(2)
  b <- score0 - (a * log(pdo0))
  k <- length(model$xlevels)

  pb <- (score0 + a * b0) / k

  modscorecard <- mod %>%
    dplyr::select(term, estimate) %>%
    dplyr::mutate(
      score = floor(a * ifelse(is.na(estimate), 0, estimate) + pb),
      score = as.integer(score)
      )

  modscorecard

}

