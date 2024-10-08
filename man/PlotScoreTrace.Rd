% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score_trace.R
\name{PlotScoreTrace}
\alias{PlotScoreTrace}
\title{Plot the score trace.}
\usage{
PlotScoreTrace(
  chains,
  attribute = "log_score",
  n_burnin = 0,
  same_plot = TRUE,
  col = NULL,
  ...
)
}
\arguments{
\item{chains}{MCMC chains.}

\item{attribute}{Name of attribute to plot. Default is "log_score".}

\item{n_burnin}{Number of steps to remove as burnin.}

\item{same_plot}{Whether to plot on the same figure or on multiple figures.}

\item{col}{A string representing a color for a single chain or a vector of
strings to cycle through for multiple chains.}

\item{...}{Extra parameters to pass to the plot and graphics::line functions.}
}
\description{
Plot the score trace.
}
