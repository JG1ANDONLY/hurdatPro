#' Determine if a storm made landfall in the US
#'
#' This function determines whether a storm with a given ID made landfall within
#' the border of the United States at a given date and time.
#'
#' @param storm.id a character representing the ID of the storm in the
#' HURDAT dataset.
#' @param date a character representing the date of the storm in the
#' format "YYYYMMDD".
#' @param time a character representing the time of the storm in the
#' format "HHMM".
#' @return a logical value indicating whether the storm made landfall within the
#' border of the United States at the given date and time.
#' @examples
#' landfall_pos("AL182012", "20121029", "1200")
#' @export
landfall_pos <- function(storm.id, date, time) {
  data("hurdat")

  # determine if the input is empty
  if (length(storm.id) == 0) {
    stop("The input storm ID cannot be empty")
  }

  # determine if the input is a valid storm ID
  if (!all(storm.id %in% hurdat$id)) {
    stop("At least 1 value of the input is a valid storm ID.")
  }

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
