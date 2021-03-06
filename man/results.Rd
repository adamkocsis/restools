% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility.R
\name{stores}
\alias{stores}
\alias{results}
\title{Result storage}
\usage{
stores(x, v = sessionVersion, within = "export/", folder = "res/")

results(v = sessionVersion, within = "export/", folder = "res/")
}
\arguments{
\item{x}{R object to be saved as a result.}

\item{v}{Character. The version of the results.}

\item{within}{Character. Path leading to the folder for the version.}

\item{folder}{}
}
\description{
Funtions to save results (R-binary objects) in a version-specific folder and retrieve them all.
}
\details{
The purpose of this code is to separate meaningful R objects and merge huge workspaces without making a mess. Merging workspaces is necessary if the combined statistical analysis/plotting/reporting is required.
}
