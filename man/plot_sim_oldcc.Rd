% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_sim_old.R
\name{plot_sim_oldcc}
\alias{plot_sim_oldcc}
\title{plot simulation results for old carrying capacity driven simulations}
\usage{
plot_sim_oldcc(l_time, plot_cc = FALSE)
}
\arguments{
\item{l_time}{list containing simulation results}

\item{plot_cc}{whether to add carryin capacity to the population plot}
}
\value{
maybe nothing, produces a plot
}
\description{
plot insecticide use and resulting changes in population and resistance
}
\examples{
#blank plot
l_time <- init_sim(20)
plot_sim_oldcc(l_time)
#default run
plot_sim_oldcc( run_sim_oldcc())
#modify params
plot_sim_oldcc( run_sim_oldcc( insecticide_kill = 0.3, resist_incr = 0.05 ))
}

