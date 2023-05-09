#' Map the position and size of a given storm
#'
#' This R function takes the storm ID, date, and time as input and generates a
#' plot of the storm position and its corresponding size of 34 knots, 50 knots,
#' and 64 knots on a map. This can be useful for visualizing the intensity and
#' location of a tropical storm.
#'
#'
#' @param stormid a character representing the hurricane ID in the HURDAT dataset.
#' The ID should be in this format similar to "AL182012".
#' @param date a character representing the date in the "YYYYMMDD" format.
#' @param time a character representing the time in the "HHMM" format
#'
#' @return a ggplot object showing the storm's position and size on a map
#'
#' @import ggplot2
#' @import maps
#'
#' @examples
#' position_size("AL182012", "20190527", "1200")
#'
#' @export

get_coord_stormmap = function(df, knots){
  data("hurdat")
  pos <- c(df$numeric.longitude, df$numeric.latitude)
  nek <- paste("ne", knots, sep = "")
  sek <- paste("se", knots, sep = "")
  swk <- paste("sw", knots, sep = "")
  nwk <- paste("nw", knots, sep = "")
  options(digits = 10)
  coordE <- track_coords(pos, df[nek], 45, df[sek], -45 )
  coordS <- track_coords(pos, df[sek], -45, df[swk], -135 )
  coordW <- track_coords(pos, df[swk], -135, df[nwk], 135 )
  coordN <- track_coords(pos, df[nwk], 135, df[nek], 45 )
  x <- c(coordE$long, coordS$long, coordW$long,coordN$long)
  y <- c(coordE$lat, coordS$lat, coordW$lat,coordN$lat)
  storm_coord <- data.frame(x = numeric(404), y = numeric(404))
  storm_coord$x <- x
  storm_coord$y <- y
  return(storm_coord)
}

position_size <- function(stormid, date, time){
  # determine if the input is empty
  if (length(stormid) == 0) {
    stop("The input storm ID cannot be empty")
  }

  # determine if the input is a valid storm ID
  if (!all(stormid %in% hurdat$id)) {
    stop("At least 1 value of the input is a valid storm ID.")
  }

  #get the line of the dataframe which contains the nominated info
  datnew <- hurdat
  df <- datnew[which(datnew$id == stormid), ]
  df <- df[which(df$time == time), ]
  df <- df[which(df$date == date),]
  pos <- c(df$numeric.longitude, df$numeric.latitude)
  #50knots
  k34 <- get_coord_stormmap(df, "34")
  x34 <- k34$x
  y34 <- k34$y
  #50knots
  k50 <- get_coord_stormmap(df, "50")
  x50 <- k50$x
  y50 <- k50$y
  #64knots
  k64 <- get_coord_stormmap(df, "64")
  x64 <- k64$x
  y64 <- k64$y

  #generate map
  library(ggplot2)
  library(maps)
  map_projection <- "+proj=longlat +datum=WGS84"
  world_map <- map_data("world")
  us_map <- map_data("state")
  map <- ggplot() +
    # Add world map
    geom_map(data = world_map, map = world_map, aes(map_id = region),
             fill = "white", color = "black", linewidth = 0.2) +
    # Add US state map
    geom_map(data = us_map, map = us_map, aes(map_id = region),
             fill = "grey", color = "black", linewidth = 0.2)+
    # Add 34 knot, 50 knot, 64 knot
    geom_polygon(data = k34, aes(x = x34, y = y34, color="34knot"),
                 fill = NA, linewidth = 1)+
    geom_polygon(data = k50, aes(x = x50, y = y50, color="50knot"),
                 fill = NA, linewidth = 1)+
    geom_polygon(data = k64, aes(x = x64, y = y64, color="64knot"),
                 fill = NA, linewidth = 1)+
    geom_point(data = data.frame(pos[1],pos[2]),
               aes(x = pos[1], y = pos[2], color="position"), size = 3)+
    ggtitle("Storm position and size")+
    xlab("Longitude") +
    ylab("Latitude") +
    xlim(-128.5, -63.5) +
    ylim(23, 50)
  return(map)
}

pos_next <- function(pos0, dist, ang){
  ang_rad <- ang * pi/180
  radii = 6371
  pos <- c(0,0)
  pos[2] <- (dist * sin(ang_rad)/ radii) * (360/(2*pi)) + pos0[2]
  pos[1] <- (dist * cos(ang_rad) / (6371*cos((dist * sin(ang_rad)/ radii))))*(360/(2*pi)) + pos0[1]
  return(pos)
}

track_coords <- function(pos, dis0, ang0, dis1, ang1){
  df <- data.frame(long = numeric(101), lat = numeric(101))
  dis <- dis0
  ang <- ang0
  deldis <- (dis1-dis0)/100
  angdel <- 90/100
  for (i in 1:101){
    df[i,] <- pos_next(pos, dis, ang)
    dis <- dis + deldis
    ang <- ang - angdel
  }
  return(df)
}
