#extending ggplot2
library(ggplot2)

A <- ggproto("A", NULL, x = 1, inc = function(self) {self$x <- self$x + 1})
A$x

#create a new stat

#the simplest stat
StatChull <- ggproto("StatChull", Stat, 
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     required_aes = c("x", "y"))

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  stat_chull(fill = NA, colour = "black")

ggplot(mpg, aes(displ, hwy, colour = drv)) +
  geom_point() + stat_chull(fill = NA)

ggplot(mpg, aes(displ, hwy)) +
  stat_chull(geom = "point", size = 4, colour = "red")  + geom_point()

#stat parameters
StatLm <- ggproto("StatLm", Stat, required_aes = c("x", "y"),
                  compute_group = function(data, scales) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = rng)
                    
                    mod <- lm(y ~ x, data = data)
                    grid$y <- predict(mod, newdata = grid)
                    
                    grid
                  })
stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + geom_point() + stat_lm()
ggplot(mpg, aes(displ, hwy, color = drv)) + geom_point() + stat_lm()

# inflexible 
StatLm <- ggproto("StatLm", Stat, 
                  required_aes = c("x", "y"),
                  compute_group = function(data, scales, params, n = 100, formula = y ~ x) {
                    rng <- range(data$x, na.rm = TRUE)
                    grid <- data.frame(x = seq(rng[1], rng[2], length = n))
                    
                    mod <- lm(formula, data = data)
                    grid$y <- predict(mod, newdata = grid)
                    
                    grid
                  })
stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, n = 50, formula = y ~ x, ...) {
  layer(
    stat = StatLm, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, formula = formula, na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, hwy)) + geom_point() +
  stat_lm(formula = y ~ poly(x, 10)) +
  stat_lm(formula = y~ poly(x, 10), geom = "point", colour = "red", n = 20)

stat_lm <- function(mapping = NULL, data = NULL, geom = "line",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, n = 50, formula = y ~ x,
                    ...) {
  layer(stat = StatLm, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(n = n, formula = formula, na.rm = na.rm, ...))
}

#picking defaults
StatDensityCommon <- ggproto("StatDensityCommon", Stat, 
                             required_aes = "x",
                             setup_params = function(data, params) {
                               if (!is.null(params$bandwidth))
                                 return(params)
                               
                               xs <- split(data$x, data$group)
                               bws <- vapply(xs, bw.nrd0, numeric(1))
                               bw <- mean(bws)
                               message("Picking bandwidth of ", signif(bw, 3))
                               
                               params$bandwidth <- bw
                               params
                             },
                             
                             compute_group = function(data, scales, bandwidth = 1) {
                               d <- density(data$x, bw = bandwidth)
                               data.frame(x = d$x, y = d$y)
                             }
                             )

stat_density_common <- function(mapping = NULL, data = NULL, geom = "line",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, bandwidth = NULL, ...) {
  layer(
    stat = StatDensityCommon, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(bandwidth = bandwidth, na.rm = na.rm, ...)
  )
}

ggplot(mpg, aes(displ, colour = drv)) +
  stat_density_common()

ggplot(mpg, aes(displ, colour = drv)) + stat_density_common(bandwidth = 0.5)

# Variable names defaults aesthetics

#Collective geoms
GeomSimplePolygon <- ggproto("GeomPolygon", Geom,
                             required_aes = c("x", "y"),
                             default_aes = aes(
                               colour = NA, fill = "grey20", size = 0.5,
                               linetype = 1, alpha = 1
                             ),
                             draw_key = draw_key_polygon,
                             draw_group = function(data, panel_params, coord) {
                               n <- nrow(data)
                               if (n <= 2) return(grid::nullGrob())
                               
                               coords <- coord$transform(data, panel_params)
                               #a polygon can have a single colour, fill, etc, so take from dirst row
                               first_row <- coords[1, , drop = FALSE]
                               
                               grid::polygonGrob(
                                 coords$x, coords$y,
                                 gp = grid::gpar(
                                   col = first_row$colour,
                                   fill = scales::alpha(first_row$fill, first_row$alpha),
                                   lwd = first_row$size * .pt,
                                   lty = first_row$linetype
                                 )
                               )
                             })
geom_simple_polygon <- function(mapping = NULL, data = NULL, stat = "chull", 
                                position = "identity", na.rm = FALSE, show.legend = NA, 
                                inherit.aes = TRUE, ...) {
  layer(
    geom = GeomSimplePolygon, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
ggplot(mpg, aes(displ, hwy)) +
  geom_point() + 
  geom_simple_polygon(aes(colour = class), fill = NA)
