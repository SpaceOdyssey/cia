% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_acceptance_rates.R
\name{CalculateAcceptanceRates}
\alias{CalculateAcceptanceRates}
\title{Calculate acceptance rates.}
\usage{
CalculateAcceptanceRates(chains, group_by = NULL)
}
\arguments{
\item{chains}{MCMC chains.}

\item{group_by}{Vector of strings that are in c("chain", "proposal_used").
Default is NULL which will return the acceptance rates marginalised over
chains and the proposal used.}
}
\value{
Summary of acceptance rates per grouping.
}
\description{
This makes the assumption that the proposal has saved a variable "proposal_used"
and mcmc has saved a variable 'accept'.
}
