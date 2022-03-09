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

## ----pkgs---------------------------------------------------------------------
library("tourr")
library("spinifex")
library("ggplot2")
library("dplyr")

## ----view-basis---------------------------------------------------------------
dat_std <- scale_sd(penguins_na.rm[, 1:4])
colnames(dat_std) <- c("bl", "bd", "fl", "bm")
bas_pca <- basis_pca(dat_std)
clas    <- penguins_na.rm$species

ggtour(basis_array = bas_pca, data = dat_std) +
  proto_basis()

## ----view-manip-space---------------------------------------------------------
view_manip_space(basis = bas_pca, manip_var = 1) 

## ---- eval=FALSE--------------------------------------------------------------
#  ## Save a tour path
#  mt_path <- manual_tour(basis = bas_pca, manip_var = 3)
#  
#  ## Compose the display
#  my_ggtour <- ggtour(basis_array = mt_path, data = dat_std, angle = .2) +
#    ## Angle is the distance between (geodesically) interpolated frames
#    proto_default(aes_args = list(color = clas, shape = clas))
#  
#  ## Animate
#  animate_gganimate(ggtour = my_ggtour, fps = 6,
#                    height = 3, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt, fps = 6)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
## Cut down sub-directory size, Following the approach in cheem making animations to gif and including those.
if(FALSE){
  ## Save a tour path
  mt_path <- manual_tour(basis = bas_pca, manip_var = 3)
  
  ## Compose the display
  my_ggtour <- ggtour(basis_array = mt_path, data = dat_std, angle = .2) +
    ## Angle is the distance between (geodesically) interpolated frames
    proto_default(aes_args = list(color = clas, shape = clas))
  
  ## .gif is about .2 Mb saved, while HTML widget is about 7 Mb.
  anim <- animate_gganimate(my_ggtour, fps = 6,
                            height = 3, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("mt_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./mt_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/mt_penguins.gif?raw=true")

## ---- echo=F------------------------------------------------------------------
nasa <- select(GGally::nasa, lat, long, day, surftemp)

temp.gly <- GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
glyphmap <- ggplot(temp.gly, aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  geom_path() + theme_bw() + labs(x = "", y = "")

glyphmap

## ----Horizontal---------------------------------------------------------------
## Initialize
nasa_std <- cbind(
  GGally::nasa[c("x", "y")],
  scale_sd(GGally::nasa[c("day", "surftemp")])
)
bas <- tourr::basis_init(ncol(nasa_std), 2)

## Horizontal rotation
m_sp_x    <- create_manip_space(basis = bas, manip_var = 3)
rot_mat_x <- rotate_manip_space(manip_space = m_sp_x,
                                theta = 0, phi = pi / 6)
rot_x     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_x))
colnames(rot_x) <- c("x1", "x2", "x_manip_sp")

## ----Vertical-----------------------------------------------------------------
## Vertical rotation
m_sp_y    <- create_manip_space(basis = bas, manip_var = 4)
rot_mat_y <- rotate_manip_space(manip_space = m_sp_y, 
                                theta = pi / 2, phi = pi / 6)
rot_y     <- data.frame(as.matrix(nasa_std) %*% as.matrix(rot_mat_y))
colnames(rot_y) <- c("y1", "y2", "y_manip_sp")

## Combine rotations
rot_xy <- bind_cols(rot_x, rot_y, .name_repair = "unique") %>% 
  select(x = x1, y = y2)

ggplot(rot_xy, aes(x = x, y = y)) + geom_point(size = 0.3) +
  theme_bw() + labs(x = "", y = "")

## ---- eval=FALSE--------------------------------------------------------------
#  ## Transform data
#  dat_std    <- scale_sd(penguins_na.rm[, 1:4])
#  ## Save holes indexed guided tour
#  holes_path <- save_history(dat_std, tourr::guided_tour(tourr::holes()))
#  
#  ## Compose display
#  ggt <- ggtour(holes_path, dat_std, angle = .2) +
#    proto_default(aes_args = list(color = clas, shape = clas))
#  
#  ## Animate
#  animate_gganimate(ggt, height = 3, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
## Cut down sub-directory size, Following the approach in cheem making animations to gif and including those.
if(FALSE){
  ## Transform data
  dat_std    <- scale_sd(penguins_na.rm[, 1:4])
  ## Save holes indexed guided tour
  holes_path <- save_history(dat_std, tourr::guided_tour(tourr::holes()))
  
  ## Compose display
  ggt <- ggtour(holes_path, dat_std, angle = .2) +
    proto_default(aes_args = list(color = clas, shape = clas))
  
  ## .gif is about .2 Mb saved, while HTML widget is about 7 Mb.
  anim <- animate_gganimate(
    ggt, height = 3, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("gt_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./gt_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/gt_penguins.gif?raw=true")

## -----------------------------------------------------------------------------
## Save only the final holes frame
holes_bas <- basis_guided(dat_std, index_f = tourr::holes(), d = 2)

## Print a single frame composition, a ggplot
ggtour(holes_bas, dat_std, angle = .2) +
  proto_default(aes_args = list(color = clas, shape = clas))

## ---- eval=FALSE--------------------------------------------------------------
#  ## Alternatively, ask for the variable by rank of the magnitude contributed:
#  (mv <- manip_var_of(holes_bas, rank = 1))
#  ## A radial, manual tour from the resulting holes basis
#  mt_path <- manual_tour(holes_bas, mv)
#  ## Compose tour
#  ggt <- ggtour(mt_path, dat_std) +
#    proto_point(aes_args = list(color = clas, shape = clas)) +
#    proto_basis() +
#    proto_origin()
#  ## Animate
#  animate_gganimate(ggt, height = 3, width = 4.5, units = "in", res = 150)
#  ## Or as a plotly html widget
#  #animate_plotly(ggt)

## ---- echo=FALSE, out.width="100%"--------------------------------------------
## Cut down sub-directory size, Following the approach in cheem making animations to gif and including those.
if(FALSE){
  ## Alternatively, ask for the variable by rank of the magnitude contributed:
  (mv <- manip_var_of(holes_bas, rank = 1))
  ## A radial, manual tour from the resulting holes basis
  mt_path <- manual_tour(holes_bas, mv)
  ## Compose tour
  ggt <- ggtour(mt_path, dat_std) +
    proto_point(aes_args = list(color = clas, shape = clas)) +
    proto_basis() +
    proto_origin()
  ## Animate
  anim <- animate_gganimate(
    ggtour = my_ggtour, height = 3, width = 4.5, units = "in", res = 150)
  gganimate::anim_save("holes_penguins.gif", animation = anim, path = "./vignettes")
}
#knitr::include_graphics("./holes_penguins.gif")
#knitr::include_url("https://github.com/nspyrison/spinifex/blob/master/vignettes/holes_penguins.gif?raw=true")

