## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "show", # text/html widgets: "hide", "show"
  eval       = TRUE,   # chunk
  message    = FALSE,
  warning    = FALSE,
  error      = FALSE,
  collapse   = TRUE,
  comment    = "#>",
  fig.height = 4,
  fig.width  = 6,
  fig.align  = "center",
  cache      = FALSE
)

## -----------------------------------------------------------------------------
library("ggplot2")
library("magrittr")

## A ggproto:
gp <- geom_point()
class(gp)

## A list of ggplot elements, a 'head-less' ggplot call
gg_ls <- list(
  gp,
  geom_smooth(method = "loess", formula = y ~ x),
  ggtitle("proto_* functions return lists of geoms_* functions.",
          "These lists can be stored, extended, and added to ggplot(). 
          \n We use this to include the animation of ggplots."),
  facet_grid(cols = vars(Tree))
)
lapply(gg_ls, class)

## ggplot call, without geoms, a 'body-less' ggplot call
gghead <- ggplot(Orange, aes(age, circumference, color = Tree))

## Evaluate together
gghead +
  gg_ls

## ----echo = FALSE-------------------------------------------------------------
data.frame(
  object = c("head", "body", "render"),
  ggplot2 = c("ggplot()", "geom_*()", "NA"),
  `ggproto api` = c("ggtour()", "proto_*()", "animate_*()"),
  `previous api` = c("play_manual_tour()", "play_manual_tour()", "play_manual_tour(render_*())")
) %>%
  knitr::kable()

## ----eval=FALSE---------------------------------------------------------------
#  library(tourr)
#  library(spinifex)
#  
#  ## Scale our numeric data
#  dat  <- scale_sd(penguins_na.rm[, 1:4])
#  ## Use species as a class to set color and shape with
#  clas <- penguins_na.rm$species
#  
#  ## Manual tour, manipulating the contribution of a selected variable
#  bas     <- basis_pca(dat)    ## Start basis
#  mv      <- 1 ## Number of the variable to manipulate
#  mt_path <- manual_tour(bas, manip_var = mv) ## Tour path
#  
#  ## Create a static ggplot2 plot with all frames of the tour
#  ggt <- ggtour(mt_path, dat, angle = .2) +
#    proto_basis() +
#    proto_point(aes_args = list(color = clas, shape = clas),
#                identity_args = list(size = 1.5))
#  
#  ## Animate
#  animate_gganimate(ggt, height = 3, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ----echo=FALSE, out.width="100%"---------------------------------------------
## Cut down sub-directory size, making animations to gif and including those.
if(FALSE){
  library(tourr)
  library(spinifex)
  dat  <- scale_sd(penguins_na.rm[, 1:4])
  clas <- penguins_na.rm$species
  bas     <- basis_pca(dat)    ## Start basis
  mv      <- 1 ## Number of the variable to manipulate
  mt_path <- manual_tour(bas, manip_var = mv) ## Tour path
  ggt <- ggtour(mt_path, dat, angle = .2) +
    proto_basis() +
    proto_point(aes_args = list(color = clas, shape = clas),
                identity_args = list(size = 1.5))
  anim <- animate_gganimate(ggt, height = 3, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("proto_mt_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./proto_mt_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/proto_mt_penguins.gif?raw=true")

## ----eval=FALSE---------------------------------------------------------------
#  ## Save a grand tour basis path, projecting through randomly selected bases
#  gt_path <- save_history(dat, grand_tour(), max_bases = 3)
#  
#  ## Static ggplot of all frames in the tour
#  ggt <- ggtour(gt_path, dat, angle = .2) +
#    ## angle is the distance between (geodesically) interpolated frames.
#    proto_basis(position = "right") +
#    proto_point(list(color = clas, shape = clas))
#  
#  ## Animate
#  animate_gganimate(ggt, height = 2, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ----echo=FALSE, out.width="100%"---------------------------------------------
## Cut down sub-directory size, making animations to gif and including those.
if(FALSE){
  gt_path <- save_history(dat, grand_tour(), max_bases = 3)
  ggt <- ggtour(gt_path, dat, angle = .2) + 
    proto_basis(position = "right") +
    proto_point(list(color = clas, shape = clas))
  anim <- animate_gganimate(ggt, height = 2, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("proto_gt_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./proto_gt_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/proto_guided1d_penguins.gif?raw=true")

## ----eval=FALSE---------------------------------------------------------------
#  ## (Quietly create) a 1d guided tour, optimizing the projection space for the holes() function
#  guided_path <- save_history(dat, guided_tour(holes(), d = 1))
#  
#  ## Static ggplot of all frames in the tour
#  ggt <- ggtour(guided_path, dat, angle = .2) +
#    proto_basis1d() +
#    proto_density(list(fill = clas, color = clas), rug_shape = 3)
#  
#  ## Animate
#  animate_gganimate(ggt, height = 2, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ----echo=FALSE, out.width="100%"---------------------------------------------
## Cut down sub-directory size, making animations to gif and including those.
if(FALSE){
  guided_path <- save_history(dat, guided_tour(holes(), d = 1))
  ggt <- ggtour(guided_path, dat, angle = .2) +
    proto_basis1d() +
    proto_density(list(fill = clas, color = clas), rug_shape = 3)
  anim <- animate_gganimate(ggt, height = 2, width = 4.5, units = "in", res = 150)
  gganimate::anim_save(
    "proto_guided1d_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./proto_guided1d_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/proto_guided1d_penguins.gif?raw=true")

## ----eval=FALSE---------------------------------------------------------------
#  ggt <- ggt +
#    theme_bw() +
#    ggtitle("My Tour Animation") +
#    labs(x = "Y1", y = "Density")
#  
#  animate_gganimate(ggt, height = 2, width = 3, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ----echo=FALSE, out.width="100%"---------------------------------------------
## Cut down sub-directory size, making animations to gif and including those.
if(FALSE){
  ggt <- ggt +
    theme_bw() +
    ggtitle("My Tour animation") +
    labs(x = "Y1", y = "density")
  
  anim <- animate_gganimate(ggt, height = 4, width = 4, units = "in", res = 150)
  gganimate::anim_save(
    "proto_guided1d_interop_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./proto_guided1d_interop_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/proto_guided1d_interop_penguins.gif?raw=true")

## ----eval=FALSE---------------------------------------------------------------
#  gt_path <- save_history(dat, max = 7)
#  ggt <- ggtour(gt_path, dat, angle = .3) +
#    facet_wrap_tour(facet_var = clas, nrow = 1) +
#    proto_point(list(color = clas, shape = clas)) +
#    proto_basis(position = "center") +
#    proto_origin()
#  
#  animate_gganimate(ggt, height = 2, width = 6, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ----echo=FALSE, out.width="100%"---------------------------------------------
## Cut down sub-directory size, making animations to gif and including those.
if(FALSE){
  gt_path <- save_history(dat, max = 7)
  ggt     <- ggtour(gt_path, dat, angle = .3) +
    facet_wrap_tour(facet_var = clas, nrow = 1) +
    proto_point(list(color = clas, shape = clas)) +
    proto_basis(position = "center") +
    proto_origin()
  anim <- animate_gganimate(ggt, height = 2, width = 6, units = "in", res = 150)
  gganimate::anim_save(
    "proto_facet_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./proto_facet_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/proto_facet_penguins.gif?raw=true")

## ----echo = FALSE-------------------------------------------------------------
data.frame(
  `proto functions` =
    c("ggtour", "proto_point", "proto_text", "proto_hex", "proto_origin/1d", "proto_density", "proto_basis/1d", "proto_default/1d", "animate_plotly", "animate_gganimate"),
  `related ggplot2 function` =
    c("ggplot", "geom_point", "geom_text", "geom_hex", "NA", "geom- _density & _rect", "geom- _segment & _text", "several protos", "plotly::ggplotly (with animation)", "gganimate::animate"),
  detail =
    c("Also perfroms setup for the tour.", "-", "-", "Heatmap hexegons, for high observation density", "Line segments for the origin, the space where 0 values project to", "1D density with run hash marks underneath, `position = 'stack'` not working with plotly.", "html widget, row numbers added as tooltip on hover. plotly doesn't presicly map all ggplot2 settings; legends, point size and opacity may vary.", "Direction and magnetude of variables to the projection disp~", "Default protos for 2/1D tours", "gif, mp4 and other video animation. gganimate consumes native ggplots and aestheics should be consistant."), check.names = F) %>%
  knitr::kable()

