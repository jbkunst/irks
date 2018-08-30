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
#' model_scorecard(model)
#'
#' @export
model_scorecard <- function(model, pdo = 20, score0 = 600, pdo0 = 50/1, turn.orientation = FALSE) {

  # https://www.worldprogramming.com/us/blog/credit_scoring_pt5

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

  b0 <- model$coefficients[1]

  a <- pdo/log(2)
  b <- score0 - (a * log(pdo0))
  k <- length(model$xlevels)

  # pb <- (score0 + a * b0) / k
  # pb <- floor(pb)
  pb <- (b + a * as.numeric(b0)) / k
  pb <- floor(pb)

  modscorecard <- irks::model_summary(model) %>%
    dplyr::select(!!!syms(c("term", "estimate"))) %>%
    dplyr::mutate(
      score = as.integer(floor(a * ifelse(is.na(!!sym("estimate")), 0, !!sym("estimate")) + pb)),
      score = ifelse(!!sym("term") == "(Intercept)", floor(pb), score)
    )

  attr(modscorecard, "base_points") <- pb

  modscorecard

}

model_scorecard_wrong <- function(model, pdo = 20, score0 = 600, pdo0 = 50/1, turn.orientation = FALSE) {

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

  b0 <- model$coefficients[1]

  a <- pdo/log(2)
  b <- score0 - (a * log(pdo0))
  k <- length(model$xlevels)

  pb <- (score0 + a * b0) / k

  # pb <- floor(pb)
  # pb <- (b + a * b0) / k
  # pb <- floor(pb)

  modscorecard <- irks::model_summary(model) %>%
    dplyr::select(!!!syms(c("term", "estimate"))) %>%
    dplyr::mutate(
      score = as.integer(floor(a * ifelse(is.na(!!sym("estimate")), 0, !!sym("estimate")) + pb)),
      # score = ifelse(!!sym("term") == "(Intercept)", floor(pb), score)
    )

  modscorecard

}

#' Obntaing the weigths and score for every variable in the model
#'
#' @param model A glm logistic model
#' @param newdata newdata
#' @param pdo default 20
#' @param score0 default 600
#' @param pdo0 default to 50/1
#' @param turn.orientation change the orientation of the scorecard points
#' @examples
#'
#' data("german_credit")
#'
#' model <- glm(
#'   good_bad ~ purpose + present_employment_since + credit_history,
#'   data = german_credit, family = binomial
#'   )
#'
#' model_predict(model, newdata = german_credit)
#' model_predict(model, newdata = head(german_credit))
#'
#' @export
model_predict <- function(model, newdata = NULL, pdo = 20, score0 = 600, pdo0 = 50/1, turn.orientation = FALSE) {

  if(is.null(newdata))
    newdata <- model$data

  # model <- readRDS("D:/Docs/modelo-behavior/data/23/05_modelo.rds")
  # newdata <- head(model$data)
  # newdata <- model$data
  # pdo = 20; score0 = 600; pdo0 = 50/1; turn.orientation = TRUE

  beta <- coefficients(model)
  message("WARNING: Using 'model_scorecard_wrong' instead of 'model_scorecard'")
  score <- irks:::model_scorecard_wrong(model, pdo = pdo, score0 = score0, pdo0 = pdo0, turn.orientation = turn.orientation) %>%
    dplyr::filter(!is.na(estimate)) %>%
    dplyr::select(-estimate) %>%
    tibble::deframe()

  variables <- irks:::model_variables(model)

  f <- as.formula(paste("~", paste(variables, collapse = " + "), sep = " "))

  modelmatrixbase <- model.matrix(f, data = model$data) %>%
    tibble::as_data_frame() %>%
    head(0)

  modelmatrix <- model.matrix(f, data = newdata) %>%
    tibble::as_data_frame() %>%
    dplyr::bind_rows(modelmatrixbase, .)

  nms <- attr(model.matrix(f, data = model$data), "dimnames")[[2]]
  # nms <- names(modelmatrixbase)

  # https://stackoverflow.com/questions/3643555/multiply-rows-of-matrix-by-vector
  modelmatrixs <- list(beta, score) %>%
    purrr::map(~as.matrix(modelmatrix) %*% diag(.x)) %>%
    purrr::map(tibble::as_data_frame) %>%
    purrr::map(dplyr::tbl_df) %>%
    purrr::map(set_names, nms)

  modelmatrixs <- modelmatrixs %>%
    purrr::map(function(mmx){

      # pdx = boot::inv.logit(lx),
      # lx2 = predict(model, newdata = newdata, type = "link"),
      # pdx2 = predict(model, newdata = newdata, type = "response")

      variables %>%
        purrr::map(~ dplyr::select(mmx, dplyr::starts_with(.x))) %>%
        purrr::map(rowSums) %>%
        purrr::map(tibble::as_data_frame) %>%
        purrr::reduce(dplyr::bind_cols) %>%
        purrr::set_names(variables) %>%
        dplyr::mutate(intercept = dplyr::pull(mmx, `(Intercept)`)) %>%
        dplyr::select(intercept, dplyr::everything()) %>%
        dplyr::mutate(row_sum = rowSums(.))
    })

  modelmatrixs <- modelmatrixs %>%
    purrr::map2(c("_lx", "_score"), ~ dplyr::rename_all(.x, paste0, .y)) %>%
    purrr::map(~ dplyr::rename_at(.x, ncol(.x), ~ stringr::str_replace(.x, "^row_sum_", "")))


  modelmatrixs[[1]] <- modelmatrixs[[1]] %>%
    dplyr::mutate(pd_lx = boot::inv.logit(!!sym("lx")))

  points_base <- score[[1]]
  modelmatrixs[[2]]  <- modelmatrixs[[2]] %>%
    dplyr::mutate_at(-nrow(.), ~ ifelse(.x == 0, points_base, .x))

  modelmatrixs <- purrr::reduce(modelmatrixs, dplyr::bind_cols)

  modelmatrixs

}



