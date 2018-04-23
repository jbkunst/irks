#' Supervised binning
#'
#' @description Automatic supervised binning
#'
#' @param x A variable to binning.
#' @param ... Additional arguments for the binning process.
#' @export
binning <- function(x, ...) {
  UseMethod("binning")
}

#' @export
binning.default <- function(x, ...) {
  stop("Objects of class/type ", paste(class(object), collapse = "/"),
       " are not supported by binning (yet).", call. = FALSE)
}

#' @examples
#'
#' data(german_credit)
#'
#' binning(german_credit$duration_in_month, german_credit$good_bad)
#'
#' binning(german_credit$credit_amount, german_credit$good_bad)
#'
#' @export
binning.numeric <- function(x, y, min.p = 0.1, alpha = 0.05, na_level = "(Missing)", ...){

  # y <- german_credit$good_bad
  # x <- german_credit$duration_in_month
  # min.p = 0.05; alpha = 0.05

  m <- min(x, na.rm = TRUE)
  m2 <- m - 9999

  x <- ifelse(is.na(x), m2, x)

  # ctree controls

  extree_data <- utils::getFromNamespace("extree_data", "partykit")

  mb <- ceiling(round(min.p * length(x)))
  ct_control <- partykit::ctree_control(minbucket = mb, alpha = alpha, ...)

  t <- partykit::ctree(y ~ x, data = dplyr::data_frame(x, y), control = ct_control)
  # plot(t)

  dft <- dplyr::data_frame(
    x, y, xnode = predict(t, type = "node")
  )

  dftg <- dft %>%
    dplyr::group_by(xnode) %>%
    dplyr::summarise(max = max(x)) %>%
    dplyr::arrange(max)

  brks <- c(-Inf, dplyr::pull(dftg)[-nrow(dftg)], Inf)

  dft <- dft %>%
    dplyr::mutate(xcat = cut(x, breaks = brks, include.lowest = TRUE))

  consider_na <- dftg %>%
    dplyr::filter(max == m2) %>%
    { nrow(.) > 0 }

  if(consider_na) {

    lvls <- levels(pull(dft, xcat))
    lvls[1] <- na_level
    lvls[2] <- sprintf("(-Inf,%s]", pull(dftg, max)[2])
    levels(dft$xcat) <- lvls

    brks <- setdiff(brks, m2)

  }

  dfsumm <- bivariate_table(dft[["xcat"]], dft[["y"]])

  monotonous <- dfsumm %>%
    dplyr::filter(x != na_level) %>%
    dplyr::pull(target_rate) %>%
    diff() %>%
    sign() %>%
    table() %>%
    length() %>%
    { . == 1}

  out <- list(
    breaks = brks,
    summary = dfsumm,
    separate_missing = consider_na,
    iv = dfsumm %>% dplyr::summarise(sum(iv)) %>% dplyr::pull(),
    hhi = dfsumm %>% dplyr::summarise(sum(percent^2)) %>% dplyr::pull(),
    monotonous = monotonous,
    na_level = na_level
  )

  class(out) <- "binning"

  out

}

# @param x A variable to binning.
# @param y A binary variable same length as \code{score}.
# @param min.p Minimal proportion in a group (a ctree_control argument).
# @param min.cri Minimal critetion (a ctree_control argument).
#' @export
binning.character <- function(x, y, min.p = 0.2, alpha = 0.05, na_level = "(Missing)", ...) {

  mb <- ceiling(round(min.p * length(x)))
  ct_control <- partykit::ctree_control(minbucket = mb, mincriterion = min.cri)

  t <- partykit::ctree(y ~ x, data = dplyr::data_frame(x, y), control = ct_control)
  # plot(t)
  t

}

#' Apply binning
#' @param bin A \code{binning} object
#' @param x A variable to bin.
#' @export
apply_binning <- function(bin, x) {

  brks <- bin$breaks
  separate_missing <- bin$separate_missing

  if(!separate_missing) {

    missing_val <- brks[2] - 1
    x <- ifelse(is.na(x), missing_val, x)

    xnew <- cut(x, breaks = brks, include.lowest = TRUE)

  } else {

    xnew <- cut(x, breaks = brks, include.lowest = TRUE)
    xnew <- forcats::fct_explicit_na(xnew, na_level = bin$na_level)

  }

  xnew

}

#' Generic method to plot a binnig object
#'
#' @export
plot.binning <- function(x) {

  # x <- readRDS("D:/Docs/modelo-behavior/data/23/06_gghh.rds")

  ggplot2::ggplot(x[["summary"]]) +
    ggplot2::geom_col(ggplot2::aes_string("x", "percent")) +
    ggplot2::geom_line(ggplot2::aes_string("x", "target_rate"), group = 1)

}

