#' Generate 30-minute interval tracks for the input storm ID(s)
#'
#' This function generates 30-minute interval tracks for the input storm ID(s),
#' containing corresponding information of id, date, time, numeric.latitude,
#' numeric.longitude.
#'
#' @param storm.id A character or a character vector of valid storm ID(s).
#'
#' @return A dataframe containing the 30-minute interval tracks for each input
#' storm ID(s).
#'
#' @examples
#' # Generate 30-minute interval tracks for the storm "AL011851"
#' storm_ids <- "AL011851"
#' storm_30min_incre("storm_ids")
#' # Generate 30-minute interval tracks for the storm "AL011851" and "AL021851"
#' storm_ids <- c("AL011851", "AL021851")
#' storm_30min_incre(storm_ids)
#'
#' @importFrom hurdat hurdat
#' @importFrom base seq.POSIXt
#' @export
#' @keywords hurricane hurdat storm

storm_30min_incre <- function(storm.id) {

  # determine if the input is empty
  if (length(storm.id) == 0) {
    stop("The input storm ID cannot be empty")
  }

  # determine if the input is a valid storm ID
  if (!all(storm.id %in% hurdat$id)) {
    stop("At least 1 value of the input is a valid storm ID.")
  }

  # load data
  # devtools::document()
  # load

  # subset hurdat based on input ID
  track <- hurdat[
    which(hurdat$id %in% storm.id),
    c("id", "date", "time", "numeric.latitude", "numeric.longitude")
  ]

  # create a new empty dataframe for later appending
  newDF <- data.frame(id = c(), date = c(), time = c(), numeric.latitude = c(),
                      numeric.longitude = c())

  # split dataframe based on ID
  track_list <- split(track, track$id)

  # loop through splitted dataframe
  for (eachTrack in track_list) {
    date_list <- split(eachTrack, eachTrack$date)
    for (eachDate in date_list) {
      eachDate <- rbind(eachDate, c(
        eachDate$id[nrow(eachDate)],
        eachDate$date[nrow(eachDate)], "2400",
        eachDate$numeric.latitude[nrow(eachDate)],
        eachDate$numeric.longitude[nrow(eachDate)]
      ))
      rownames(eachDate) <- seq(nrow(eachDate))
      for (index in 1:(nrow(eachDate) - 1)) {
        start <- eachDate[index, "time"]
        startHour <- substr(start, start = 1, stop = 2)
        startMin <- substr(start, start = 3, stop = 4)
        startTime <- paste0(startHour, ":", startMin)
        end <- eachDate[index + 1, "time"]
        endHour <- substr(end, start = 1, stop = 2)
        endMin <- substr(end, start = 3, stop = 4)
        endTime <- paste0(endHour, ":", endMin)
        startTime <- strptime(startTime, format = "%H:%M")
        endTime <- strptime(endTime, format = "%H:%M")
        startEnd30Interval <- format(
          seq.POSIXt(
            from = startTime,
            to = endTime, by = "30 min"
          ),
          format = "%H%M"
        )
        startEnd30Interval <- head(startEnd30Interval, -1)
        temp <- data.frame(
          id = eachDate$id[index],
          date = eachDate$date[index],
          time = startEnd30Interval,
          numeric.latitude = eachDate$numeric.latitude[index],
          numeric.longitude = eachDate$numeric.longitude[index]
        )
        newDF <- rbind(newDF, temp)
      }
    }
    return(newDF)
  }
}
