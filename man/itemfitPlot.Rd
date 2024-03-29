% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemfitPlot.r
\name{itemfitPlot}
\alias{itemfitPlot}
\title{Plotting itemfit estimates}
\usage{
itemfitPlot(
  model,
  fit_stats = "infit",
  color = "red",
  shape = 17,
  title = "Item Infit and Outfit Statistics",
  ...
)
}
\arguments{
\item{model}{an object of class \code{SingleGroupClass} returned by the function \code{mirt()}.}

\item{fit_stats}{a character vector indicating which fit statistics should be computed. See \code{mirt::infit()} for supported inputs.}

\item{color}{color of the item points.}

\item{shape}{shape of the item points}

\item{title}{title for the plot (defaults to "Item Infit and Outfit Statistics")}
}
\value{
a ggplot
}
\description{
This function takes a fitted mirt-model and visualizes item infit and outfit estimates. The function builds on \code{mirt::itemfit()}. Currently only supported \code{fact_stats = "infit"}.
}
\examples{
library(mirt)
library(ggmirt)
data <- expand.table(LSAT7)
(mod <- mirt(data, 1))

itemfitPlot(mod, fit_stats = "infit")

}
\references{
\itemize{
\item Linacre JM. (2002). What do Infit and Outfit, Mean-square and Standardized mean? Rasch Measurement Transactions, 16(2), p.878. https://www.rasch.org/rmt/rmt162f.htm
}
}
