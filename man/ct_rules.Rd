% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trees.R
\name{ct_rules}
\alias{ct_rules}
\title{Get rules from partykit object}
\usage{
ct_rules(x)
}
\arguments{
\item{x}{A party object.}
}
\description{
Get rules from partykit object
}
\examples{

library(partykit)
tr <- ctree(Species ~ .,data = iris)
ct_rules(tr)

}
