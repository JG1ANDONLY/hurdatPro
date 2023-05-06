#' Brief description of the function
#'
#' Detailed description of the function.
#'
#' @param arg1 Description of argument 1.
#' @param arg2 Description of argument 2.
#' @return Description of the return value.
#' @export
#' @examples
#' my_function(arg1 = 1, arg2 = "abc")
storm_map <- function(storm.ids) {
  library(ggplot2)
  library(maps)

  # load data
  # path <- system.file("data", "hurdat.RData", package = "hurdatPro")

  track <- hurdat[hurdat$id %in% storm.ids, ]
  map_states <- map_data("state")

  p <- ggplot(track, aes(x = numeric.longitude, y = numeric.latitude, group = id, color = max.wind)) + geom_path(size = 1) + scale_color_gradient(low = "blue", high = "red") + theme_minimal() + theme(panel.grid = element_blank())
  p <- p + coord_fixed(1.3)
  p <- p + geom_polygon(data = map_states, aes(x = long, y = lat, group = group), fill = NA, color = "gray50")
  p <- p + labs(title = "Storm Tracks")
  p <- p + labs(x = "Longitude", y = "Latitude")

  return(p)
}
