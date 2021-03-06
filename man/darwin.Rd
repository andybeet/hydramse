% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/darwin.R
\name{darwin}
\alias{darwin}
\title{Darwinian simulation study}
\usage{
darwin(
  seed = NULL,
  nYrs,
  hydraD,
  stockRecruitData,
  simulationRules,
  nSims,
  SRFunctionChoice,
  stochasticity = F,
  inputOptions,
  pathToTPL,
  hydraVersion,
  boolPlot = F
)
}
\arguments{
\item{seed}{Integer. Set seed for testing or reproducability. Default = NULL}

\item{nYrs}{Numeric scalar. Total length of simulation. See \code{Run length} section below for details}

\item{hydraD}{List of current base data (as in \code{hydradata::hydradataList})}

\item{stockRecruitData}{List. Stock recruitment parameter estimates for differnt functional forms (\code{\link{darwinData}})}

\item{simulationRules}{List. Set of rules used in the simulation of parameters (\code{\link{darwinRules}})}

\item{nSims}{Numeric scalar. Number of parameter sets to simulate}

\item{SRFunctionChoice}{Numeric Vector. An argument of \code{\link{simulate_parameters}}}

\item{stochasticity}{Boolean scalar. TRUE if stochasticity is desired (stock recruitment functions in Hydra)}

\item{inputOptions}{List. Set of input values to determine run type}

\item{pathToTPL}{String. Path to location of Hydra executable}

\item{hydraVersion}{String. Name of the Hydra executable}

\item{boolPlot.}{Boolean. True if plotting of biomass and catch are required. Default = FALSE}
}
\value{
List
\item{nSuccesses}{Number of simulated parameter sets that pass the biomass and catch criteria. These successful sets will be saved as rds files in the \code{successfulSims}
folder, located in the project root}
\item{nAttempts}{Total Number of simulated parameter sets}
}
\description{
Simulates parameters, runs hydra, then determines if simulated parameter set is viable. Essentially filtering out poor simulations
Viable sets are saved as rds files in a predetermined folder
}
\section{Run length}{


Hydra is currently set up to run for 53 years (1964-2016). This is the historic period in which fishing data is available. To extend the run length to \code{nYrs}
is adding a period of no fishing of length \code{nYrs}-53 years to the start of the model run. It is assumed that all populations have reached
an equilibrium. This equilibrium is then used as the starting point for the historic portion of the model run.
}

