% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/oneway.R
\name{oneway}
\alias{oneway}
\title{One Way Analysis of Variance}
\usage{
oneway(formula, data)
}
\arguments{
\item{formula}{an object of class formula, relating the
depending variable to the grouping variable}

\item{data}{a data frame containing the variables in the model}
}
\value{
a list with 2 elements:
\item{oneway}{a list with the lm results}
\item{summarystats}{a data frame with the summary statistics}
}
\description{
\code{oneway} computes a oneway analysis of variance
and includes group level summary statistics
}
\details{
This function computes a standard one-way ANOVA, means,
and standard deviations. Missing values are handled via listwise
deletion.
}
\examples{
mileage <- oneway(hwy ~ class, cars)
summary(mileage)
print(mileage)
plot(mileage)
}
\author{
Jiner Zheng <jzheng@wesleyan.edu>
}
