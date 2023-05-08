accumulated_cyc <- function(stormid){
  if (length(stormid) == 1){
    # Create df storing storm data matching the input storm id
    df <- hurdat[which(hurdat$id == stormid),]
    df <- storm_data[storm_data$time %in% c(" 0000", " 0600", " 1200", " 1800"), ]
    df$max.wind <- as.numeric(df$max.wind)
    # Calculate accumulated cyclone energy
    ace_list <- (df$max.wind)^2 * 10^(-4)
    ace_energy <- sum(na.omit(ace_list))
  }else{
    # Create a vector energy_list to store ace_energy with different storm id 
    energy_list <- c()
    # Get one stormid and calculate its accumulated cyclone energy each time 
    for (i in 1:length(stormid)){
      df <- hurdat[which(hurdat$id %in% stormid[i]),]
      df <- df[df$time %in% c(" 0000", " 0600", " 1200", " 1800"), ]
      df$max.wind <- as.numeric(df$max.wind)
      # Calculate accumulated cyclone energy
      ace_list <- (df$max.wind)^2 * 10^(-4)
      energy <- sum(na.omit(ace_list))
      energy_list[i] <- energy
    }
    ace_energy <- data.frame("stormid" = stormid, "ace_energy" = energy_list)
  }
  return(ace_energy)
}