#' Compute Accumulated Cyclone Energy for given storm(s)
#'
#' This function calculates the Accumulated Cyclone Energy (ACE) of a given set
#' of storm(s) by their storm ID in the HURDAT dataset.
#'
#' @param stormid A character or a character vector of storm IDs in the HURDAT
#' dataset. The ID should be in the format similar to "AL182012" or a vector
#' of multiple IDs.
#' @return A data frame with storm ID(s) and corresponding ACE value(s).
#' @examples
#' cyclone_energy(c("AL182012"))
#' cyclone_energy(c("AL182012", "AL011851"))
#' @export
cyclone_energy <- function(stormid) {
  # Create a vector energy_list to store ace_energy with different stormid
  energy_list <- c()
  for (i in 1:length(stormid)){
    df <- hurdat[which(hurdat$id %in% stormid[i]),]
    df <- df[df$time %in% c("0000", "0600", "1200", "1800"), ]
    df$max.wind <- as.numeric(df$max.wind)
    # Calculate ACE
    max.wind.sqr <- sum((df$max.wind)^2)
    energy <- sum(na.omit(max.wind.sqr))* 10^(-4)
    energy_list[i] <- energy
  }
  ace_energy <- data.frame("stormid" = stormid, "ace_energy" = energy_list)
  return(ace_energy)
}
