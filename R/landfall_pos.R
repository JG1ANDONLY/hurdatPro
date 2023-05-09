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
landfall_pos <- function(storm.id, date, time) {
  library(sp)
  library(maps)
  # Get the boundary polygon of the continental US
  us_poly <- map("usa", plot = FALSE, fill = TRUE)

  storm <- hurdat[which(hurdat$id %in% storm.id), ]
  storm <- storm[which(storm$date == date), ]
  storm <- storm[which(storm$time == time), ]
  pos <- storm[, c("numeric.latitude", "numeric.longitude")]

  is_in_us <- any(point.in.polygon(pos$numeric.longitude, pos$numeric.latitude,
                                   us_poly$x, us_poly$y) != 0)
  return(is_in_us)
}
