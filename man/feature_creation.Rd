% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feature_creation.R
\name{feature_creation}
\alias{feature_creation}
\title{Create variable from behavior data}
\usage{
feature_creation(data, id_var = SK_ID_CURR, time_var = MONTHS_BALANCE,
  diff = c(6, 12, 24, 48, 96), verbose = TRUE)
}
\arguments{
\item{data}{A data frame.}

\item{id_var}{The ID var.}

\item{time_var}{The TIME var.}

\item{diff}{A numeric vector to make sub variables}

\item{verbose}{verbose.}
}
\description{
Create variable from behavior data
}
