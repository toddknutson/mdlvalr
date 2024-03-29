% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_stats.R
\name{get_stats}
\alias{get_stats}
\alias{get_stats.hybcap}
\title{Calculate summary statistics of concordance tables}
\usage{
get_stats(mdlvalr_list, pipeline = NULL)

\method{get_stats}{hybcap}(mdlvalr_list, pipeline = "hybcap")
}
\arguments{
\item{mdlvalr_list}{A standard \code{mdlvalr_list} object (as generated by \code{get_stats()}).
The class of this object can be used to automatically determine which
function methods will be used.}

\item{pipeline}{Standard name of the pipeline from which the data were derived.}
}
\value{
Returns a standard \code{mdlvalr_list} object (i.e. a regular R list of lists) that contains tables as named list elements.
The class of the retuned object is updated to include the name of the pipeline. This facilitates use of alternative methods in
downstream functions.
}
\description{
Calculate summary statistics of concordance tables
}
\section{Methods}{

This function is a \strong{generic}, which means that different
implementations (methods) can be executed for different classes. The class
of the first argument will determine which method is used. See the documentation of
individual methods for extra arguments and differences in behaviour.
}

\examples{
\dontrun{
class(mdlvalr_list)
# [1] "hybcap" "list"

mdlvalr_list <- get_stats(mdlvalr_list)
}
}
