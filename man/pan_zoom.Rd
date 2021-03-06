% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_util.r
\name{pan_zoom}
\alias{pan_zoom}
\title{Pan (offset) and zoom (scale) a 2 column matrix or dataframe.}
\usage{
pan_zoom(pan = c(0L, 0L), zoom = c(1L, 1L), x = NULL)
}
\arguments{
\item{pan}{2 Numeric value to offset/pan the first 2 dimensions of \code{x}.}

\item{zoom}{2 Numeric value to scale/zoom the first 2 dimensions of \code{x}.}

\item{x}{Numeric data object with 2 columns to scale and offset.
Defaults to NULL, passing arguments to scale_axes for use internally.}
}
\value{
Scaled and offset \code{x}.
}
\description{
A manual variant of \code{scale_axes()}. Can be used as the \code{axes} argument
to manually set the size and locations of the axes.
}
\examples{
rb <- tourr::basis_random(6, 2)
pan_zoom(pan = c(-1, 0), zoom = c(2/3, 2/3), x = rb)
}
\seealso{
\code{\link{scale_axes}} for preset choices.
}
