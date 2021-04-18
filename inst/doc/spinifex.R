## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo       = TRUE,   # code
  include    = TRUE,   # plots
  results    = "hide", # text: "hide", "show"
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
library("spinifex")
library("tourr")
library("ggplot2")
library("dplyr")

## ----view-basis---------------------------------------------------------------
dat_std <- scale_sd(tourr::flea[,-7])
bas_pca <- basis_pca(dat_std)
clas <- tourr::flea[, 7]

view_frame(basis = bas_pca)

## ----view-manip-space---------------------------------------------------------
view_manip_space(basis = bas_pca, manip_var = 3, lab = colnames(dat_std)) 

## ---- eval=F------------------------------------------------------------------
#  play_manual_tour(data = dat_std, basis = bas_pca, manip_var = 3, axes = "bottomleft",
#                   aes_args = list(color = clas, shape = clas))

## ---- echo=F------------------------------------------------------------------
nasa <- select(GGally::nasa, lat, long, day, surftemp)

temp.gly <-
  GGally::glyphs(nasa, "long", "day", "lat", "surftemp", height = 2.5)
glyph <-
  ggplot(temp.gly, aes(gx, gy, group = gid)) +
  GGally::add_ref_lines(temp.gly, color = "grey90") +
  GGally::add_ref_boxes(temp.gly, color = "grey90") +
  geom_path() + theme_bw() + labs(x = "", y = "")

glyph

## ----Horizontal---------------------------------------------------------------
## Initialize
nasa_std <- 
  cbind(GGally::nasa[c("x", "y")], tourr::rescale(GGally::nasa[c("day", "surftemp")]))
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

## -----------------------------------------------------------------------------
dat_std <- tourr::rescale(tourr::flea[,1:6])
fpath    <- tourr::save_history(dat_std, tourr::guided_tour(tourr::holes()))

## ---- eval=F------------------------------------------------------------------
#  play_tour_path(tour_path = fpath, data = dat_std, angle = .15,
#    render_type = render_gganimate, col = tourr::flea$species, fps = 4)

## -----------------------------------------------------------------------------
f_holes_bas  <- matrix(as.numeric(fpath[,, dim(fpath)[3]]), ncol = 2)
view_frame(f_holes_bas, lab = colnames(dat_std))

## ---- eval=F------------------------------------------------------------------
#  play_manual_tour(data = dat_std, basis = f_holes_bas,
#                   manip_var = 5, col = tourr::flea$species)

