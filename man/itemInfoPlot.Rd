% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/itemInfoPlot.r
\name{itemInfoPlot}
\alias{itemInfoPlot}
\title{Plotting item information curves}
\usage{
itemInfoPlot(
  model,
  items = NULL,
  facet = FALSE,
  title = "Item Information Curves",
  theta_range = c(-4, 4),
  legend = FALSE
)
}
\arguments{
\item{model}{an object of class \code{SingleGroupClass} returned by the function \code{mirt()}.}

\item{items}{numerical vector indicating which items to plot.}

\item{facet}{Should all items be shown in one plot, or each item received its individal facet?}

\item{title}{title for the plot (defaults to "Item Characteristic Curves")}

\item{theta_range}{range to be shown on the x-axis}
}
\value{
a ggplot
}
\description{
This function takes a fitted mirt-model and visualizes items information curves.
}
\examples{
library(mirt)
library(ggmirt)
data <- expand.table(LSAT7)
(mod <- mirt(data, 1))

itemInfoPlot(mod)

}
