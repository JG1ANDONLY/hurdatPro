#' Generate a map of storm track(s) given storm ID(s)
#'
#' This function generates a map of storm tracks given a storm ID or a vector of
#' storm ID(s).
#'
#' @param storm.ids A character or a character vector of valid storm ID(s) to
#' include in the map.
#' @param title An optional character vector for the title of the plot. Default
#' is an empty string.
#' @import ggplot2 maps
#' @return A ggplot2 object representing the storm tracks map.
#' @examples
#' # Generate a map of storm tracks for hurricanes with ID "AL052021"
#' storm_map("AL052021")
#' # Generate a map of storm tracks for hurricanes with ids "AL052021" and "AL072021"
#' storm_map(c("AL052021", "AL072021"))
#' @export
storm_map <- function(storm.ids, title = "") {
  data("hurdat")
  # determine if the input is empty
  if (length(storm.ids) == 0) {
    stop("The input storm ID cannot be empty")
  }

  # determine if the input is a valid storm ID
  if (!all(storm.ids %in% hurdat$id)) {
    stop("At least 1 value of the input is a valid storm ID.")
  }

  library(ggplot2)
  library(maps)

  track <- hurdat[hurdat$id %in% storm.ids, ]
  map_states <- map_data("state")

  p <- ggplot(track, aes(x = numeric.longitude, y = numeric.latitude, group = id,
                         color = max.wind)) +
    geom_path(size = 1) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_fixed(1.3) +
    geom_polygon(data = map_states, aes(x = long, y = lat, group = group),
                 fill = NA, color = "gray50") +
    labs(x = "Longitude", y = "Latitude")
  if (title == "") {
    p <- p + labs(title = paste("Storm", storm.ids, "Track"))
  } else {
    p <- p + labs(title = title)
  }
  return(p)
}
