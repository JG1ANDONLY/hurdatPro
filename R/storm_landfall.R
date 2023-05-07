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
storm_landfall <- function(storm.ids) {
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
