% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zDepricated_2_render.r
\name{render_plotly}
\alias{render_plotly}
\title{Animation the frames as a HTML widget.}
\usage{
render_plotly(fps = 8, html_filename = NULL, save_widget_args = list(), ...)
}
\arguments{
\item{fps}{Frames animated per second. Defaults to 8.}

\item{html_filename}{Optional, saves the plotly object as an HTML widget to
this string (without the directory path).
Defaults to NULL (not saved). For more output control use \code{save_widget_args}
or call \code{htmlwidgets::saveWidget()} on a return object of \code{render_plotly()}.}

\item{save_widget_args}{A list of arguments to be called in
\code{htmlwidgets::saveWidget()} when used with a \code{html_filename}.}

\item{...}{Passes arguments to \code{render_(...)}.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}, see \code{\link{ggtour}}.
Takes the result of \code{array2df()} and animations them via \code{{plotly}}
into a  self-contained HTML widget.
}
\examples{
library(spinifex)
message("It's suggested to switch to the proto api, see `?ggtour` to get started.")

## Setup
dat_std  <- scale_sd(wine[, 2:6])
clas     <- wine$Type
bas      <- basis_pca(dat_std)
mv       <- manip_var_of(bas)
mt_array <- manual_tour(basis = bas, manip_var = mv)
mt_df_ls <- array2df(basis_array = mt_array, data = dat_std)

\donttest{
render_plotly(frames = mt_df_ls)

require("ggplot2")
render_plotly(
  frames = mt_df_ls, axes = "bottomleft", fps = 10,
  aes_args = list(color = clas, shape = clas),
  identity_args = list(size = 1.5, alpha = .7),
  ggproto = list(theme_bw(), scale_color_brewer(palette = "Set2")))

## Saving a .gif, may require additional setup
if(FALSE) ## Don't accidentally save file
  render_plotly(frames = mt_df_ls, axes = "bottomleft", fps = 10,
                html_filename = "myRadialTour.html")}
}
\seealso{
\code{\link{render_}} for \code{...} arguments.

\code{\link[plotly]{ggplotly}} for source documentation of \code{tooltip}.

\code{\link[htmlwidgets]{saveWidget}} for more control of .html output.
}
