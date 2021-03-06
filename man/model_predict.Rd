% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_scorecard.R
\name{model_predict}
\alias{model_predict}
\title{Obntaing the weigths and score for every variable in the model}
\usage{
model_predict(model, newdata = NULL, pdo = 20, score0 = 600,
  pdo0 = 50/1, turn.orientation = FALSE)
}
\arguments{
\item{model}{A glm logistic model}

\item{newdata}{newdata}

\item{pdo}{default 20}

\item{score0}{default 600}

\item{pdo0}{default to 50/1}

\item{turn.orientation}{change the orientation of the scorecard points}
}
\description{
Obntaing the weigths and score for every variable in the model
}
\examples{

data("german_credit")

model <- glm(
  good_bad ~ purpose + present_employment_since + credit_history,
  data = german_credit, family = binomial
  )

model_predict(model, newdata = german_credit)
model_predict(model, newdata = head(german_credit))

}
