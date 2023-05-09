#' Identify if storm(s) landfell within the border of the United States
#'
#' This function takes in a storm ID or a vector of storm IDs and identifies
#' if each storm landfell within the border of the United States.
#'
#' @param storm.ids A character or a character vector of storm IDs
#' @return A logical vector indicating whether each storm landfell within the
#' border of the United States
#' @keywords hurricane storm landfall
#' @examples
#' # Identify if the storm "AL052005" landfell within the border of the United States
#' storm_ids <- "AL052005"
#' storm_landfall(storm_ids)
#' # Identify which storms landfell within the border of the United States
#' storm_ids <- c("AL052005", "AL062005", "AL072005")
#' storm_landfall(storm_ids)
#' @export
storm_landfall <- function(storm.ids) {
  data("hurdat")
  # determine if the input is empty
  if (length(storm.ids) == 0) {
    stop("The input storm ID cannot be empty")
  }

  # determine if the input is a valid storm ID
  if (!all(storm.ids %in% hurdat$id)) {
    stop("At least 1 value of the input is a valid storm ID.")
  }

  library(sp)
  library(maps)
  # Get the boundary polygon of the continental US
  us_poly <- map("usa", plot = FALSE, fill = TRUE)

  is_in_us <- c()

  for (id in storm.ids) {
    track <- hurdat[which(hurdat$id %in% storm.ids), c(
      "numeric.latitude",
      "numeric.longitude"
    )]
    storm_point <- SpatialPoints(track)
    is_in_us <- append(is_in_us, any(point.in.polygon(
      storm_point$numeric.longitude,
      storm_point$numeric.latitude,
      us_poly$x, us_poly$y
    ) != 0))
  }

  return(is_in_us)
}
