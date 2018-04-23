#' Get labels for Information Values
#' @param x A numeric vector
#' @export
label_iv <- function(x) {
  cut(
    x,
    include.lowest = TRUE,
    breaks = c(0, 0.02, 0.1, 0.3, 0.5, Inf),
    labels = c("unpredictive", "weak", "medium", "strong", "suspicious")
  )
}

#' Get labels for HH indexes
#' @param x A numeric vector
#' @export
label_hhi <- function(x) {
  cut(
    x,
    breaks = c(0, 0.01, 0.15, 0.25, Inf),
    include.lowest = TRUE,
    # c("Altamente no concentrado", "No concentrado", "muy concentrado", "Altamente concentrado"),
    labels = c("highly diverse","unconcentrated","moderate concentration", "high concentration"))
}

#' Get labels for PSIs
#' @param x A numeric vector
#' @export
label_psi <- function(x) {
  cut(
    x,
    c(0, 0.1, 0.25, Inf),
    include.lowest = TRUE,
    # c("Cambio insignificante", "Algún cambio menor", "Gran cambio en población")
    labels = c("insignificant change", "some minor change", "major shift in population")
  )
}
