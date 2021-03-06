% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/0_util.r
\name{array2df}
\alias{array2df}
\title{Turns a tour path array into a long data frame.}
\usage{
array2df(array, data = NULL, label = NULL)
}
\arguments{
\item{array}{A (p, d, n_frames) array of a tour, the output of
\code{manual_tour()}.}

\item{data}{Optional, (n, p) dataset to project, consisting of numeric
variables.}

\item{label}{Optional, labels for the reference frame of length 1 or the
number of variables used. Defaults to an abbreviation of the variables.}
}
\value{
A list containing an array of basis frames (p, d, n_frames) and
an array of data frames (n, d, n_frames) if data is present.
}
\description{
Typically called by a wrapper function, \code{play_manual_tour} or
\code{play_tour_path}. Takes the result of \code{tourr::save_history()} or
\code{manual_tour()} and restructures the data from an array to a long data frame
for use in ggplots.
}
\examples{
## Setup
dat_std <- tourr::rescale(wine[, 2:14])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_of(bas)

## Radial tour array to long df, as used in play_manual_tour()
tour_array <- manual_tour(basis = bas, manip_var = mv)
str(
  array2df(array = tour_array, data = dat_std,
           label = paste0("MyLabs", 1:nrow(bas)))
)

## tourr::save_history tour array to long df, as used in play_tour_path()
hist_array <- tourr::save_history(data = dat_std, max_bases = 10)
str(
  array2df(array = hist_array, data = dat_std,
           label = paste0("MyLabs", 1:nrow(bas)))
)
}
