#' spinifex
#'
#' spinifex is a package that extends the package `tourr`. 
#' It builds the functionality for manual tours and allows other
#' tours to be rendered by `plotly` or `gganimate`. Tours are a class of
#' dynamic linear (orthogonal) projections of numeric multivariate data from 
#' `p` down to `d` dimensions that are viewed as an animation as `p`-space is 
#' rotated. Manual tours manipulate a selected variable, exploring how they 
#' contribute to the sensitivity of the structure in the projection. This is
#' particularly useful after finding an interesting tour, perhaps via a 
#' guided tour.
#' 
#'
#' Its main functions are:
#' \itemize{
#'   \item [run_app()], running `run_app("intro")` will open an introductory `shiny` app demonstrating radial tours.
#'   \item [play_manual_tour()], performs a manual tour, returning a `plotly` animate by default.
#'   \item [play_tour_path()], turns a tour path into an animation, returning a `plotly` object by default.
#'   \item [view_frame()], plot a basis set on a reference axis.
#'   \item [view_manip_space()], plot a manipulation space highlighting the manip var.
#' }
#'
#' GitHub: \url{https://github.com/nspyrison/spinifex}
#' @seealso tourr (package)
#' @name spinifex
#' @docType package
"_PACKAGE"

# Manual tour globals:
globalVariables(c("phi_min",
                  "phi_max",
                  "manip_col",
                  "n_frames",
                  "theta",
                  "angle")
)

# ggplot aes globals:
globalVariables(c("x",
                  "y",
                  "z",
                  "xend",
                  "yend",
                  "label",
                  "frame")
)
