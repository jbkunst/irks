% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binning.R
\name{apply_binnings}
\alias{apply_binnings}
\title{Apply binnings}
\usage{
apply_binnings(binnings, data, woe = FALSE, suffix = "_cat")
}
\arguments{
\item{binnings}{A named list of \code{binning} objects.}

\item{data}{A data frame containing variables to bin. Names need need to
coincide.}

\item{woe}{If the value is the woe instead of the categoric variable.}

\item{suffix}{A string to be added to the output to disambiguate the original
variables names.}
}
\description{
Apply binnings
}
\examples{

data("german_credit")

dbiv <- describe_bivariate(german_credit, target = good_bad)
dbiv <- dplyr::filter(dbiv, iv > 0.1)

binnings <- dplyr::select(dbiv, variable, binning)
binnings <- tibble::deframe(binnings)

data <- dplyr::select(german_credit, credit_amount, property,
                      purpose, credit_history, good_bad)

apply_binnings(binnings, data)

}
