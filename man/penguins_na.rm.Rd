% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/9_data.r
\docType{data}
\name{penguins_na.rm}
\alias{penguins_na.rm}
\title{Size measurements for adult foraging penguins near Palmer Station, Antarctica}
\format{
A data frame with 333 rows and 4 numeric variables and 3 factor variables
\describe{
\item{bill_length_mm}{a number denoting bill length (millimeters)}
\item{bill_depth_mm}{a number denoting bill depth (millimeters)}
\item{flipper_length_mm}{an integer denoting flipper length (millimeters)}
\item{body_mass_g}{an integer denoting body mass (grams)}
\item{species}{a factor denoting penguin species (Adelie, Chinstrap and Gentoo)}
\item{sex}{a factor denoting penguin sex (female, male)}
\item{island}{a factor denoting island in Palmer Archipelago, Antarctica (Biscoe, Dream or Torgersen)}
}
}
\source{
{palmerpenguins R package. A. Horst, 2020. Palmer Archipelago (Antarctica) Penguin Data.} \url{https://CRAN.R-project.org/package=palmerpenguins}

{Adelie penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Adelie penguins (Pygoscelis adeliae) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \doi{10.6073/pasta/98b16d7d563f265cb52372c8ca99e60f}

{Gentoo penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Gentoo penguin (Pygoscelis papua) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 5. Environmental Data Initiative} \doi{10.6073/pasta/7fca67fb28d56ee2ffa3d9370ebda689}

{Chinstrap penguins: Palmer Station Antarctica LTER and K. Gorman. 2020. Structural size measurements and isotopic signatures of foraging among adult male and female Chinstrap penguin (Pygoscelis antarcticus) nesting along the Palmer Archipelago near Palmer Station, 2007-2009 ver 6. Environmental Data Initiative} \doi{10.6073/pasta/c14dfcfada8ea13a17536e73eb6fbe9e}

{Originally published in: Gorman KB, Williams TD, Fraser WR (2014) Ecological Sexual Dimorphism and Environmental Variability within a Community of Antarctic Penguins (Genus Pygoscelis). PLoS ONE 9(3): e90081. doi:10.1371/journal.pone.0090081}
}
\usage{
penguins_na.rm
}
\description{
Includes measurements for penguin species, island in Palmer Archipelago,
size (flipper length, body mass, bill dimensions), and sex.
}
\details{
This is a cleaned subset of \code{palmerpenguins::penguins}.

Replicating this dataset:

\if{html}{\out{<div class="sourceCode">}}\preformatted{require(palmerpenguins)
d <- palmerpenguins::penguins
d <- d[complete.cases(d), ] ## Remove missing, 2 obs of numeric and several in sex
d <- d[, c(3:6, 1, 7, 2)]   ## Numeric to front, group factors, remove year
penguins_na.rm <- as.data.frame(d) ## Remove \{tibble\} dependency
## save(penguins_na.rm, file = "./data/penguins_na.rm.rda")
}\if{html}{\out{</div>}}
}
\examples{
library(spinifex)
str(penguins_na.rm)
dat   <- scale_sd(penguins_na.rm[, 1:4])
clas1 <- penguins_na.rm$species
clas2 <- penguins_na.rm$sex

bas <- basis_pca(dat)
mv  <- manip_var_of(bas)
mt  <- manual_tour(bas, mv)

ggt <- ggtour(mt, dat, angle = .2) +
  proto_default(aes_args = list(color = clas1, shape = clas2))
\donttest{
animate_plotly(ggt)
}
}
\keyword{datasets}
