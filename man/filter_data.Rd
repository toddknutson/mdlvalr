% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_data.R
\name{filter_data}
\alias{filter_data}
\alias{filter_data.hybcap}
\title{Filter compared data into sub-tables based on vairant labels}
\usage{
filter_data(mdlvalr_list, pipeline = NULL)

\method{filter_data}{hybcap}(mdlvalr_list, pipeline = "hybcap")
}
\arguments{
\item{mdlvalr_list}{A standard \code{mdlvalr_list} object (as generated by \code{add_var_labels()}).
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
Filter compared data into sub-tables based on vairant labels
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
mdlvalr_list <- filter_data(
    mdlvalr_list,
    pipeline = "hybcap")
}
}
