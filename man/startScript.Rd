% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/startScript.R
\name{startScript}
\alias{startScript}
\title{Function to set working directory and load in packages at the start of a script}
\usage{
startScript(wd = "~", packages = c("tidyverse", "devtools", "viridis"))
}
\arguments{
\item{wd}{A character string giving the desired working directory}

\item{packages}{A character vector of packages to be loaded/installed}
}
\value{
Nothing, just a
}
\description{
Function to set working directory and load in packages at the start of a script
}
\examples{
\dontrun{
startScript(wd = '~', packages = c('Bchron', 'simmr'))
}
}
