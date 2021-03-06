% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/waitForSlurmQ.R
\name{waitForSlurmQ}
\alias{waitForSlurmQ}
\title{Wait for the SLURM queue to empty}
\usage{
waitForSlurmQ(targetLength = 0, secsToWait = 30, maxWaits = 60)
}
\arguments{
\item{targetLength}{How many runs to leave in the queue.Default is 0.}

\item{secsToWait}{How many seconds to wait between parsing the queue. Default is 30.}

\item{maxWaits}{Maximum number of times the wait should happen before timing out. Default is 20.
waitForSlurmQ()}
}
\description{
A function that waits for the SLURM queue to be smaller than a
target number of runs. Typically used to wait for a run that your
code has kicked off before parsing results. Will print messages
with how many runs it waits for and for how long. The wait will
time out after ( secsToWait * maxWaits ) seconds.
}
\author{
Henrik Bjugård Nyberg - henrik.b.nyberg@farmbio.uu.se
}

