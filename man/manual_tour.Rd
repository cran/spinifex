% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_manual_tour.r
\name{manual_tour}
\alias{manual_tour}
\title{Produce the series of projection bases to rotate a variable into and out
of a projection.}
\usage{
manual_tour(
  basis,
  manip_var,
  theta = NULL,
  phi_min = 0L,
  phi_max = 0.5 * pi,
  angle = 0.05,
  ...
)
}
\arguments{
\item{basis}{A (p, d) orthonormal numeric matrix.
The linear combination the original variables contribute to projection space.
Defaults to NULL, generating a random basis.}

\item{manip_var}{Integer column number or string exact column name of the.
variable to manipulate. Required, no default.}

\item{theta}{Angle in radians of "in-plane" rotation, on the xy plane of the
reference frame. Defaults to theta of the basis for a radial tour.}

\item{phi_min}{Minimum value phi should move to. Phi is angle in radians of
the "out-of-plane" rotation, the z-axis of the reference frame.
Required, defaults to 0.}

\item{phi_max}{Maximum value phi should move to. Phi is angle in radians of
the "out-of-plane" rotation, the z-axis of the reference frame.
Required, defaults to pi/2.}

\item{angle}{Target distance (in radians) between steps. Defaults to .05.}

\item{...}{Handles unused arguments that are being also being passed from
\code{play_manual_tour()} to \code{render_()}.}
}
\value{
A (p, d, 4) history_array of the radial tour. The bases set for
phi_start, \code{phi_min}, \code{phi_max}, and back to phi_start.
}
\description{
Typically called by \code{array2af()}. An array of projections,
the radial tour of the \code{manip_var}, which is rotated from phi's starting
position to \code{phi_max}, to \code{phi_min}, and back to the start position.
}
\examples{
## Setup
dat_std <- scale_sd(wine[, 2:14])
clas <- wine$Type
bas <- basis_pca(dat_std)
mv <- manip_var_of(bas)

## Required arguments
manual_tour(basis = bas, manip_var = mv)

## Full arguments
manual_tour(basis = bas, manip_var = mv,
            theta = pi / 2, phi_min = pi / 16, phi_max = pi, angle = .8)
}
