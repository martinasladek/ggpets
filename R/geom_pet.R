#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Key Pet
#'
#' @param data,params,size key stuff
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_pet <-  function(data, params, size) {

  filename <- system.file(paste0(data$pet, ".png"), package = "ggpets", mustWork = TRUE)
  # print(filename)
  img <- as.raster(png::readPNG(filename))
  aspect <- dim(img)[1]/dim(img)[2]
  # rasterGrob
  grid::rasterGrob(image         = img)
}

# petGrob
petGrob <- function(x, y, size, pet = "axel", geom_key = list(
  axel =        "axel.png",
  bear =        "bear.png",
  beaux =       "beaux.png",
  bella =       "bella.png",
  benji =       "benji.png",
  chloe =       "chloe.png",
  clive =       "clive.png",
  creasy =      "creasy.png",
  fiona =       "fiona.png",
  jester =      "jester.png",
  kal =         "kal.png",
  kaspian =     "kaspian.png",
  kida =        "kida.png",
  latte =       "latte.png",
  louis =       "louis.png",
  marshmallow = "marshmallow.png",
  milo =        "milo.png",
  pippa =       "pippa.png",
  remy =        "remy.png",
  rosie_swim =  "rosie_swim.png",
  rosie =       "rosie.png",
  rupert =      "rupert.png",
  salem =       "salem.png",
  scrungo =     "scrungo.png",
  smudge =      "smudge.png",
  suki =        "suki.png",
  zuko =        "zuko.png"
)
) {

  filename <- system.file(geom_key[[unique(pet)]], package = "ggpets", mustWork = TRUE)
  img <- as.raster(png::readPNG(filename))

  # rasterGrob
  grid::rasterGrob(x             = x,
                   y             = y,
                   image         = img,
                   # only set height so that the width scales proportionally and so that the icon
                   # stays the same size regardless of the dimensions of the plot
                   height        = size * ggplot2::unit(20, "mm"))
}

# GeomPet
GeomPet <- ggplot2::ggproto(`_class` = "GeomPet",
                                `_inherit` = ggplot2::Geom,
                                required_aes = c("x", "y"),
                                non_missing_aes = c("size", "pet"),
                                default_aes = ggplot2::aes(size = 1, pet = "axel", shape  = 19,
                                                           colour = "black",   fill   = NA,
                                                           alpha  = NA,
                                                           stroke =  0.5,
                                                           scale = 5,
                                                           image_filename = "axel.png"),

                                draw_panel = function(data, panel_scales, coord, na.rm = FALSE) {
                                  coords <- coord$transform(data, panel_scales)
                                  ggplot2:::ggname(prefix = "geom_pet",
                                                   grob = petGrob(x = coords$x,
                                                                      y = coords$y,
                                                                      size = coords$size,
                                                                      pet = coords$pet))
                                },

                                draw_key = draw_key_pet)

#' @title Pet layer
#' @description The geom is used to add Pet to plots. See ?ggplot2::geom_points for more info.
#' @inheritParams ggplot2::geom_point
#' @examples
#'
#' # install.packages("ggplot2")
#'library(ggplot2)
#'
#' ggplot(mtcars) +
#'  geom_pet(aes(mpg, wt), pet = "axel") +
#'  theme_bw()
#'
#' ggplot(mtcars) +
#'  geom_pet(aes(mpg, wt), pet = "axel") +
#'  theme_bw()
#'
#' @importFrom grDevices as.raster
#' @export
geom_pet <- function(mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {

  ggplot2::layer(data = data,
                 mapping = mapping,
                 stat = stat,
                 geom = GeomPet,
                 position = position,
                 show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}



