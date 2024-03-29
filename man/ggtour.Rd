% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/2_ggproto_visualize.r
\name{ggtour}
\alias{ggtour}
\title{Prepare a new grammar of graphics tour}
\usage{
ggtour(
  basis_array,
  data = NULL,
  angle = 0.05,
  basis_label = NULL,
  data_label = NULL,
  do_center_frame = TRUE
)
}
\arguments{
\item{basis_array}{An array of projection bases for the tour, as produced
with \code{manual_tour()} or \code{tour::save_history()}, or a single basis.}

\item{data}{Numeric data to project. If left NULL, will check if it data is
stored as an attribute of the the \code{basis_array}.}

\item{angle}{Target angle (radians) for interpolation frames between
frames of the \code{basis_array}. Defaults to .05.
To opt out of interpolation set to NA or 0.}

\item{basis_label}{Labels for basis display, a character
vector with length equal to the number of variables.
Defaults to NULL; 3 character abbreviation from colnames of data or
rownames of basis.}

\item{data_label}{Labels for \code{plotly} tooltip display.
Defaults to the NULL, rownames and/or numbers of data.}

\item{do_center_frame}{Whether or not to center the mean within each
animation frame. Defaults to TRUE.}
}
\description{
\code{ggtour()} initializes a ggplot object for a tour. \verb{proto_*} functions are
added to the tour, analogous to \verb{ggplot() + geom_*}. The final tour object is
then animated with \code{animate_plotly()} or \code{animate_ggtour()}, or passed to
\code{filmstrip()} for static plot faceting on frames.
}
\examples{
library(spinifex)
dat     <- scale_sd(penguins_na.rm[, 1:4])
clas    <- penguins_na.rm$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

## d = 2 case
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_default(aes_args = list(color = clas, shape = clas),
                identity_args = list(size = 1.5, alpha = .8))
\donttest{
animate_plotly(ggt)
}

## Finer control calling individual proto_* functions
ggt <- ggtour(basis_array = mt_path, data = dat, angle = .3) +
  proto_point(aes_args = list(color = clas, shape = clas),
              identity_args = list(size = 1.5, alpha = .8),
              row_index = which(clas == levels(clas)[1])) +
  proto_basis(position = "right",
              manip_col = "red",
              text_size = 7L) +
  proto_origin()
\donttest{
animate_plotly(ggt)
}

## d = 1 case
bas1d <- basis_pca(dat, d = 1)
mt_path1d <- manual_tour(basis = bas1d, manip_var = mv)

ggt1d <- ggtour(basis_array = mt_path1d, data = dat, angle = .3) +
  proto_default1d(aes_args = list(fill= clas, color = clas))
\donttest{
animate_plotly(ggt1d)
}

## Single basis
ggt <- ggtour(basis_array = bas, data = dat) +
  proto_default(aes_args = list(fill= clas, color = clas))
## ggtour() returns a static ggplot2 plot
\donttest{
ggt
### or as html widget with tooltips
animate_plotly(ggt)
}
}
\seealso{
Other ggtour proto functions: 
\code{\link{append_fixed_y}()},
\code{\link{facet_wrap_tour}()},
\code{\link{proto_basis}()},
\code{\link{proto_default}()},
\code{\link{proto_density}()},
\code{\link{proto_density2d}()},
\code{\link{proto_hex}()},
\code{\link{proto_highlight}()},
\code{\link{proto_hline0}()},
\code{\link{proto_origin}()},
\code{\link{proto_point}()},
\code{\link{proto_text}()}
}
\concept{ggtour proto functions}
