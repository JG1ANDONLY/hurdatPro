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
cyclone_energy <- function(stormid) {
  if (length(stormid) == 1){
    # Create df storing storm data matching the input storm id
    df <- hurdat[which(hurdat$id %in% stormid),]
    df <- df[df$time %in% c("0000", "0600", "1200", "1800"), ]
    df$max.wind <- as.numeric(df$max.wind)
    # Calculate ACE
    max.wind.sqr <- (df$max.wind)^2
    ace_energy <- sum(na.omit(max.wind.sqr))* 10^(-4)
  }else{
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
  }
  return(ace_energy)
}
