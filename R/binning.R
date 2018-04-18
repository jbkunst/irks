#' Supervised binning
#'
#' @description Automatic supervised binning
#'
#' @param x A variable to binning.
#' @export
binning <- function(x, ...) {
  UseMethod("binning")
}

#' @param x A variable to binning.
#' @param y A binary variable same length as \code{score}.
#' @param min.p Minimal proportion in a group (a ctree_control argument).
#' @param min.cri Minimal critetion (a ctree_control argument).
#' @export
binning.numeric <- function(x, y, min.p = 0.1, min.cri = 0.90){

  # y <- factor(data$marca_inc_12m)
  # x <- data$ratio_dda_mor_max_dda_vig_mean_06
  # min.p = 0.05; min.cri = 0.95; max.depth = 5

  m <- min(x, na.rm = TRUE)
  m2 <- m - 9999

  x <- ifelse(is.na(x), m2, x)

  # ctree controls
  mb <- ceiling(round(min.p * length(x)))
  ct_control <- partykit::ctree_control(minbucket = mb, mincriterion = min.cri, maxdepth = max.depth)

  t <- partykit::ctree(y ~ x, data = data_frame(x, y), control = ct_control)
  # plot(t)

  dft <- data_frame(
    x, y, xnode = predict(t, type = "node")
  )

  dftg <- dft %>%
    dplyr::group_by(xnode) %>%
    dplyr::summarise(max = max(x)) %>%
    dplyr::arrange(max)

  brks <- c(-Inf, pull(dftg)[-nrow(dftg)], Inf)

  dft <- dft %>%
    mutate(xcat = cut(x, breaks = brks, include.lowest = TRUE))

  consider_na <- dftg %>%
    dplyr::filter(max == m2) %>%
    { nrow() > 0 }

  if(consider_na) {

    lvls <- levels(pull(dft, xcat))
    lvls[1] <- "Missing"
    lvls[2] <- sprintf("(-Inf,%s]", pull(dftg, max)[2])
    levels(dft$xcat) <- lvls

    brks <- setdiff(brks, m2)

  }

  dfsumm <- dft %>%
    dplyr::group_by(xcat) %>%
    dplyr::summarise(
      n = dplyr::n(),
      p = dplyr::n(),
      n_target = sum(as.numeric(y) == 1),
      n_non_target = sum(as.numeric(y) == 0),
      target_rate = mean(as.numeric(y))
    ) %>%
    dplyr::mutate(p = p/sum(p))

  monotonous <- dfsumm %>%
    dplyr::filter(xcat != "Missing") %>%
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
    monotonous = monotonous
  )

  class(out) <- "binning"

  out

}

#' @param x A variable to binning.
#' @param y A binary variable same length as \code{score}.
#' @param min.p Minimal proportion in a group (a ctree_control argument).
#' @param min.cri Minimal critetion (a ctree_control argument).
#' @export
binning.numeric <- function(x, y, min.p = 0.2, min.cri = 0.90) {

  mb <- ceiling(round(min.p * length(x)))
  ct_control <- partykit::ctree_control(minbucket = mb, mincriterion = min.cri, maxdepth = max.depth)

  t <- partykit::ctree(y ~ x, data = data_frame(x, y), control = ct_control)
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

    na_lvl <- as.character(sb$summary$xcat[1])

    xnew <- cut(x, breaks = brks, include.lowest = TRUE)
    xnew <- fct_explicit_na(xnew, na_level = na_lvl)

  }

  xnew

}

