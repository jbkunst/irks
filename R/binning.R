#' Grouping or binning
#'
#' @description Automatic supervised binning
#'
#' @param x A variable to binning.
#' @param y A binary variable same length as \code{score}.
#' @param p.min Minimal proportion in a group (a ctree_control argument).
#' @param alpha Minimal critetion (a ctree_control argument).
#' @param na_level String for determinar de NA level.
#' @param keeptree logical to keep tree.
#' @param ... Additional arguments for the binning process.
#' @export
binning <- function(x, y, p.min = 0.05, alpha = 0.05, na_level = "(Missing)", keeptree = FALSE,...) {
  UseMethod("binning")
}

# @param x A variable to binning.
# @param y A binary variable same length as \code{score}.
# @param p.min Minimal proportion in a group (a ctree_control argument).
# @param alpha Minimal critetion (a ctree_control argument).
# @param na_level String for determinar de NA level.
# @param na_level String for determinar de NA level.
# @param ... Additional arguments for the binning process.
#' @rdname binning
#' @export
binning.default <- function(x, y, p.min = 0.05, alpha = 0.05, na_level = "(Missing)", keeptree = FALSE, ...) {
  stop("Objects of class/type ", paste(class(x), collapse = "/"),
       " are not supported by binning (yet).", call. = FALSE)
}

#' Grouping or binning for numeric variables
#' @param x A variable to binning.
#' @param y A binary variable same length as \code{score}.
#' @param p.min Minimal proportion in a group (a ctree_control argument).
#' @param alpha Minimal critetion (a ctree_control argument).
#' @param na_level String for determinar de NA level.
#' @param keeptree logical to keep tree.
#' @param ... Extra arguments to \code{ctree_control}.
#' @examples
#'
#' data(german_credit)
#' binning(german_credit$duration_in_month, german_credit$good_bad)
#' binning(x = german_credit$credit_amount, y = german_credit$good_bad)
#'
#' @export
binning.numeric <- function(x, y, p.min = 0.05, alpha = 0.05, na_level = "(Missing)", keeptree = FALSE, ...){

  # y <- german_credit$good_bad
  # x <- german_credit$duration_in_month
  # p.min = 0.05; alpha = 0.05

  m <- min(x, na.rm = TRUE)
  m2 <- m - 1

  x <- ifelse(is.na(x), m2, x)

  # ctree controls

  extree_data <- utils::getFromNamespace("extree_data", "partykit")

  mb <- ceiling(round(p.min * length(x)))
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

    lvls <- levels(dplyr::pull(dft, xcat))
    lvls[1] <- na_level
    lvls[2] <- sprintf("(-Inf,%s]", dplyr::pull(dftg, max)[2])
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
    dict = dplyr::data_frame(breaks = brks),
    summary = dfsumm,
    separate_missing = consider_na,
    iv = dfsumm %>% dplyr::summarise(sum(iv)) %>% dplyr::pull(),
    hhi = dfsumm %>% dplyr::summarise(sum(percent^2)) %>% dplyr::pull(),
    monotonous = monotonous,
    na_level = na_level,
    class = class(x)
  )

  if(keeptree){
    out[["tree"]] <- t
  }

  class(out) <- "binning"

  out

}

#' Grouping or binning character/factors variables
#' @param x A variable to binning.
#' @param y A binary variable same length as \code{score}.
#' @param p.min Minimal proportion in a group (a ctree_control argument).
#' @param alpha Minimal critetion (a ctree_control argument).
#' @param na_level String for determinar de NA level.
#' @param keeptree logical to keep tree.
#' @param ... Extra arguments to \code{ctree_control}.
#'
#' data(german_credit)
#' binning(german_credit$purpose, german_credit$good_bad)
#'
#' @export
binning.character <- function(x, y, p.min = 0.05, alpha = 0.05, na_level = "(Missing)", keeptree = FALSE, ...) {

  # x <- german_credit$purpose
  # y <- german_credit$good_bad

  extree_data <- utils::getFromNamespace("extree_data", "partykit")

  mb <- ceiling(round(p.min * length(x)))
  ct_control <- partykit::ctree_control(minbucket = mb, alpha = alpha, ...)

  xf <- fix_char_factor(x)

  t <- partykit::ctree(y ~ x, data = dplyr::data_frame(x = xf, y), control = ct_control)
  # plot(t)
  t

  dft <- dplyr::data_frame(
    x, xf, y, xnode = predict(t, type = "node")
  )

  nr <- dft %>%
    dplyr::distinct(xnode) %>%
    nrow() %>%
    as.character() %>%
    nchar()

  dftg <- dft %>%
    dplyr::mutate(
      xnodef = as.factor(xnode),
      xnodef = forcats::fct_reorder(xnodef, y, fun = mean)
      ) %>%
    dplyr::distinct(x, xnode, xnodef) %>%
    dplyr::arrange(xnodef) %>%
    dplyr::mutate(
      idx = dplyr::group_indices(., xnodef),
      group = stringr::str_pad(idx, width = nr, side = "left", pad = "0"),
      group = paste0("g_", group),
      group = forcats::fct_inorder(group)
      ) %>%
    dplyr::select(x, group)

  dft <- dplyr::left_join(dft, dftg, by = "x")

  dfsumm <- bivariate_table(dft[["group"]], dft[["y"]])

  out <- list(
    dict = dftg,
    summary = dfsumm,
    separate_missing = NA,
    iv = dfsumm %>% dplyr::summarise(sum(iv)) %>% dplyr::pull(),
    hhi = dfsumm %>% dplyr::summarise(sum(percent^2)) %>% dplyr::pull(),
    monotonous = NA,
    na_level = na_level,
    class = class(x)
  )

  if(keeptree){
    out[["tree"]] <- t
  }

  class(out) <- "binning"

  out

}

fix_char_factor <- function(x, na_level = "(Missing)", other_level = "Other") {

  xf <- as.factor(x)
  xf <- forcats::fct_explicit_na(x, na_level = na_level)

  if(!is.ordered(xf)) {
    xf <- forcats::fct_lump(xf, n = 30, other_level = other_level)
  }

  xf
}

#' @export
binning.factor <- function(x, ...) {

  binning.character(x, ...)

}

#' Apply binning
#' @param bin A \code{binning} object.
#' @param x A variable to bin.
#' @param woe If the value is the woe instead of the categoric variable.
#' @examples
#'
#' x <- runif(500)
#' y <- rbinom(500, 1, x)
#' bin <- binning(x, y)
#' xnew <- runif(50)
#'
#' apply_binning(bin, xnew)
#' apply_binning(bin, xnew, woe = TRUE)
#'
#'
#' data(german_credit)
#' x <- german_credit$purpose
#' y <- german_credit$good_bad
#' bin <- binning(x, y)
#' xnew <- sample(x, 50)
#'
#' apply_binning(bin, xnew)
#' apply_binning(bin, xnew, woe = TRUE)
#'
#' @export
apply_binning <- function(bin, x, woe = FALSE) {

  stopifnot(class(bin) %in% "binning")
  stopifnot(bin$class == class(x))

  if(class(x) %in% c("integer", "numeric")) {

    brks <- bin$dict$breaks
    separate_missing <- bin$separate_missing

    if(!separate_missing) {

      missing_val <- brks[2] - 1
      x <- ifelse(is.na(x), missing_val, x)

      xnew <- cut(x, breaks = brks, include.lowest = TRUE)

    } else {

      xnew <- cut(x, breaks = brks, include.lowest = TRUE)
      xnew <- forcats::fct_explicit_na(xnew, na_level = bin$na_level)
      xnew <- forcats::fct_relevel(xnew, bin$na_level)

    }

  }

  if(class(x) %in% c("character", "factor")){

    xnew <- data.frame(x, stringsAsFactors = FALSE) %>%
      dplyr::left_join(bin$dict, by = "x") %>%
      dplyr::pull("group")

    if(bin$na_level %in% levels(xnew))
      xnew <- forcats::fct_relevel(xnew, bin$na_level)

  }

  if(woe) {

    xnew <- data.frame(xnew) %>%
      dplyr::left_join(bin$summary, by = c("xnew" = "x")) %>%
      dplyr::pull(woe)

  }

  xnew

}




#' Apply binnings
#' @param binnings A named list of \code{binning} objects.
#' @param data A data frame containing variables to bin. Names need need to
#'   coincide.
#' @param woe If the value is the woe instead of the categoric variable.
#' @param suffix A string to be added to the output to disambiguate the original
#'   variables names.
#' @examples
#'
#' data("german_credit")
#'
#' dbiv <- describe_bivariate(german_credit, target = good_bad)
#' dbiv <- dplyr::filter(dbiv, iv > 0.1)
#'
#' binnings <- dplyr::select(dbiv, variable, binning)
#' binnings <- tibble::deframe(binnings)
#'
#' data <- dplyr::select(german_credit, credit_amount, property,
#'                       purpose, credit_history, good_bad)
#'
#' apply_binnings(binnings, data)
#'
#' @export
apply_binnings <- function(binnings, data, woe = FALSE, suffix = "_cat") {

  variables <- data %>%
    dplyr::select(dplyr::one_of(names(binnings))) %>%
    as.list()

  binnings <- binnings[names(variables)]

  dout <- purrr::map2(binnings, variables, irks::apply_binning, woe = woe) %>%
    purrr::map_dfc(dplyr::as_data_frame) %>%
    purrr::set_names(names(variables)) %>%
    dplyr::rename_all(paste0, suffix)

  dout

}

#' Generic method to plot a binnig object
#' @param x A binning object.
#' @param ... Extra arguments.
#' @examples
#'
#' data(german_credit)
#' bin <- binning(german_credit$duration_in_month, german_credit$good_bad)
#' plot(bin)
#'
#' x <- runif(250)
#' y <- rbinom(250, 1, x)
#' bin <- binning(x, y)
#' plot(bin)
#'
#' @export
plot.binning <- function(x, ...) {

  # x <- readRDS("D:/Docs/modelo-behavior/data/23/06_gghh.rds")
  # x <- bin

  ggplot2::ggplot(x[["summary"]], ggplot2::aes_string("x")) +
    ggplot2::geom_col(ggplot2::aes_string(y = "percent"), width = 0.5) +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymin = "lower", ymax = "upper"), group = 1, width = 0.1) +
    ggplot2::geom_line(ggplot2::aes_string(y = "target_rate"), group = 1) +
    ggplot2::geom_point(ggplot2::aes_string(y = "target_rate"), group = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = NULL, y = NULL)
}

#' Generic method to print a binnig object
#' @param x A binning object.
#' @param ... Extra arguments.
#' @examples
#'
#' data(german_credit)
#' bin <- binning(german_credit$duration_in_month, german_credit$good_bad)
#' bin
#'
#' x <- runif(500)
#' y <- rbinom(500, 1, x)
#' bin <- binning(x, y, alpha = .1)
#' bin
#'
#' @export
print.binning <- function(x, ...) {

  # x <- readRDS("D:/Docs/modelo-behavior/data/23/06_gghh.rds")
  # x <- bin

  cat("binning object:\n\n")

  cat("\tInformation Value: ")
  cat(round(x$iv, 3)," ", as.character(label_iv(x$iv)), "\n")

  cat("\tHerfindahl-Hirschman Index: ")
  cat(round(x$hhi, 3)," ", as.character(label_hhi(x$hhi)), "\n")

  cat("\tNumber of categories:", nrow(x$summary), "\n")

  cat("\n\tSummary table:\n")
  print(x$summary)

  cat("\n")

  cat("\tObject values:", paste(names(x), collapse = ", "))

  invisible(x)

}
