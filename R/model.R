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


model_variables <- function(model) {

  # names(model$xlevels)
  model %>%
    as.formula() %>%
    as.character() %>%
    last() %>%
    str_split("\\s\\+\\s") %>%
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
    pull(variable) %>%
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
  irks::model_summary(model) %>%
    filter(variable != "(Intercept)") %>%
    filter(variable %in% str_c(numeric_variables, "_cat", sep = "")) %>%
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
    pull(variable) %>%
    as.character()

}

model_check_monotonous <- function(model, missing_category = "(Missing)") {

  length(model_check_monotonous_detail(model, missing_category)) == 0

}

model_plot <- function(model){
  model %>%
    irks::model_summary() %>%
    filter(variable != "(Intercept)") %>%
    mutate(category = fct_inorder(category)) %>%
    mutate(estimate = ifelse(is.na(estimate), 0, estimate)) %>%
    mutate(variable2 = as.character(variable)) %>%
    mutate(variable2 = fct_inorder(variable2)) %>%
    ggplot() +
    # geom_col(aes(category, marginal_percent), width = 0.5, fill = "gray80") +
    geom_col(aes(category, marginal_percent), group = "estimacion", color = NA, alpha = 0.2, width = 0.3) +
    geom_line(aes(category, target_rate), group = "target_rate_dev", color = "red") +
    geom_line(aes(category, estimate), group = "estimacion", color = "blue") +
    facet_wrap( ~ variable2, scales = "free",  labeller = labeller(label = label_wrap_gen(35))) +
    labs(x = "Categoría", y = "Porcentaje/Tasa de Incumplimiento") +
    theme(strip.text.x = element_text(size = 8))
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

