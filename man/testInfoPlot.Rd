% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testInfoPlot.r
\name{testInfoPlot}
\alias{testInfoPlot}
\title{Plotting test information curve}
\usage{
testInfoPlot(
  model,
  theta_range = c(-4, 4),
  adj_factor = 3.5,
  title = "Test Information Curve"
)
}
\arguments{
\item{model}{an object of class \code{SingleGroupClass} returned by the function \code{mirt()}.}

\item{theta_range}{range to be shown on the x-axis}

\item{adj_factor}{adjustment factor for properly overlaying information and standard error.}

\item{title}{title for the plot (defaults to "Item Characteristic Curves")}
}
\value{
a ggplot
}
\description{
This function takes a fitted mirt-model and visualizes test information curve.
}
\examples{
library(mirt)
library(ggmirt)
data <- expand.table(LSAT7)
(mod <- mirt(data, 1))

testInfoPlot(mod)

}
