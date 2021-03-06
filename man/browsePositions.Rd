% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/browsePositions-methods.R
\name{browsePositions}
\alias{browsePositions}
\alias{browsePositions,ANY-method}
\alias{browsePositions,GenomicRangesList-method}
\alias{browsePositions,GenomicRanges-method}
\alias{browsePositions,data.frame-method}
\title{Browse genomic positions}
\usage{
browsePositions(object, ...)

\S4method{browsePositions}{ANY}(object, region = NULL)

\S4method{browsePositions}{GenomicRangesList}(object, region = NULL, plot = TRUE)

\S4method{browsePositions}{GenomicRanges}(object, region = NULL, plot = TRUE)

\S4method{browsePositions}{data.frame}(object, region)
}
\arguments{
\item{object}{GPos, GenomicRangesList or data.frame: Genomic positions}

\item{...}{not currently used.}

\item{region}{GRanges or NULL: Plotting window.}

\item{plot}{logical: Whether to produce a plot or return the plot data.}
}
\value{
ggplot or data.frame.
}
\description{
Genome browser-type plot of single-bp positions, e.g. VCF-like data.
}
\details{
browseFeatures can react to metadata columns: `color`, `facet`, `score`, `shape`.
}
\examples{
# TBA
}
