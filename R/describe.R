max_concentration <- function(x){
  x %>%
    table(useNA = "always") %>%
    prop.table() %>%
    max()
}

partial_na_rm_true <- function(f, ...) {
  purrr::partial(f, na.rm = TRUE, ...)
}

n_uniques <- function(x) {
  length(unique(x))
}

na_percent <- function(x) {
  sum(is.na(x))/length(x)
}

gather_separate_spread <- function(x) {

  x %>%
    tidyr::gather(key, value) %>%
    tidyr::separate(key, c("variable", "stat"), sep = "_") %>%
    dplyr::mutate(
      variable = forcats::fct_inorder(variable),
      stat = forcats::fct_inorder(stat)
    ) %>%
    tidyr::spread(stat, value)

}

#' Herfindahl-Hirschman Index
#'
#' @param x A vector to obtain the HHI.
#' @examples
#'
#' x <- sample(LETTERS[1:10], size = 1000, replace = TRUE, prob = sqrt(1:10))
#' hhi(x)
#' plot(table(x))
#'
#' x <- sample(LETTERS[1:5], size = 1000, replace = TRUE, prob = 1:5)
#' hhi(x)
#' plot(table(x))
#'
#' @export
hhi <- function(x) {

  x %>%
    table(useNA = "always") %>%
    prop.table() %>%
    (function(x) { x*x }) %>%
    sum()

}

#' Describe
#'
#' @param data A data frame to describe.
#' @examples
#'
#' data("german_credit")
#'
#' describe(german_credit)
#'
#' describe(german_credit[c("good_bad", "purpose")])
#'
#' describe(german_credit[c("good_bad", "duration_in_month")])
#'
#' describe(german_credit[c("status_of_existing_checking_account", "savings_account_or_bonds")])
#'
#' \dontrun{
#' data(chileancredit, package = "smbinning")
#'
#' d <- subset(chileancredit, select = -c(period))
#'
#' describe(d)
#' }
#'
#' @export
describe <- function(data) {

  # data <- german_credit

  stopifnot(is.data.frame(data))

  classes <- unlist(purrr::map(data, class))

  out <- list(
    numeric = dplyr::data_frame(),
    non_numeric = dplyr::data_frame()
  )

  data <- dplyr::rename_all(data, ~ stringr::str_replace_all(.x, "_", "\\."))

  if(any(classes %in% c("numeric", "integer"))) {

    dfnum <- data %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::summarise_all(
        .funs = list(
          n.unique   = n_uniques,
          na.percent = na_percent,
          min        = partial_na_rm_true(min),
          mean       = partial_na_rm_true(mean),
          median     = partial_na_rm_true(median),
          max        = partial_na_rm_true(max),
          Variance   = partial_na_rm_true(var),
          skewness   = partial_na_rm_true(moments::skewness),
          kurtosis   = partial_na_rm_true(moments::kurtosis),
          max.concentration = max_concentration
          # quantiles = function(x) t(purrr::partial(quantile, probs = c(1, 5, 10, 25, 50, 75, 90, 95, 99)/100)(x))
        )
      )

    if(ncol(dplyr::select_if(data, is.numeric)) == 1) {

      dfnum <- dfnum %>%
        dplyr::rename_all(~paste0(names(dplyr::select_if(data, is.numeric)), "_", .))

    }

    out[["numeric"]] <- gather_separate_spread(dfnum) %>%

      dplyr::mutate(variable = stringr::str_replace_all(as.character(variable), "\\.", "_")) %>%
      dplyr::rename_all(~ stringr::str_replace_all(.x, "\\.", "_"))

  }

  if(any(classes %in% c("character", "factor"))) {

    dfnon <-  data %>%
      dplyr::select_if(function(x) { is.character(x) | is.factor(x) }) %>%
      dplyr::summarise_all(
        .funs = list(
          # class      = class,
          n.unique   = n_uniques,
          na.percent = na_percent,
          hhi        = hhi,
          max.concentration = max_concentration
        )
      )

    if(ncol(dplyr::select_if(data, function(x) { is.character(x) | is.factor(x) })) == 1) {

      dfnon <- dfnon %>%
        dplyr::rename_all(~paste0(names(dplyr::select_if(data, function(x) { is.character(x) | is.factor(x) })), "_", .))

    }

    out[["non_numeric"]] <- gather_separate_spread(dfnon) %>%
      dplyr::mutate(variable = stringr::str_replace_all(as.character(variable), "\\.", "_")) %>%
      dplyr::rename_all(~ stringr::str_replace_all(.x, "\\.", "_"))
  }

  out

}

#' Describe Bivariate
#'
#' @param data A data frame to describe.
#' @param target The target to describe.
#' @param p.min Minimal proportion in a group (a ctree_control argument).
#' @param alpha Minimal critetion (a ctree_control argument).
#' @param verbose verbose.
#  @param ... Additional parameters for \code{binning} function.
#' @examples
#'
#' data("german_credit")
#'
#' dbiv <- describe_bivariate(german_credit, target = good_bad)
#' dplyr::glimpse(dbiv)
#'
#' describe_bivariate(german_credit, target = good_bad, alpha = 0.5)
#'
#'
#' \dontrun{
#'
#' data(chileancredit, package = "smbinning")
#' data <- subset(chileancredit, select = -c(period))
#' describe_bivariate(data, target = fgood)
#'
#' }
#'
#' @export
describe_bivariate <- function(data, target, p.min = 0.05, alpha = 0.05, verbose = TRUE){

  # data <- german_credit %>% dplyr::select(good_bad, credit_amount, purpose, duration_in_month, foreign_worker)
  # target <- rlang::expr(good_bad)
  # target <- rlang::expr(fgood)

  target <- rlang::enquo(target)

  tg <- data %>%
    dplyr::select(!!target) %>%
    dplyr::pull()

  data <- dplyr::select(data, -!!target)

  nmsdata <- names(data)

  if(verbose) {
    W <- getOption("width")
    cw <- max(nchar(nmsdata)) + 1
    w <- W - cw - 5
    pb <- txtProgressBar(min = 0, max = length(nmsdata), initial = 0, style = 3, width = w)
    # pb <- progress::progress_bar$new(total = length(nmsdata), format = " processing :var [:bar] :percent eta: :eta")

  }

  binnings <- purrr::map(nmsdata, function(nm, p = p.min, a = alpha){

    if(verbose) {
      setTxtProgressBar(pb, value = which(nm == nmsdata), label = nm, title = nm)
      cat(paste0(" ", stringr::str_pad(nm, width = cw, side = "right")))
      # pb$tick(tokens = list(var = nm))
    }

    binning(data[[nm]], y = tg, p.min = p, alpha = a)

  })

  dfbinnings <- binnings %>%
    purrr::transpose() %>%
    tibble::as_tibble() %>%
    dplyr::mutate_at(dplyr::vars(iv, hhi, monotonous, na_level, class, separate_missing), unlist)

  dfbinnings <- dfbinnings %>%
    dplyr::mutate(
      variable = nmsdata,
      n_unique = purrr::map_int(summary, nrow)
      ) %>%
    dplyr::select(variable, iv, hhi, n_unique, class, monotonous, na_level, separate_missing, everything()) %>%
    dplyr::mutate(binning = binnings)

  dfbinnings

}



