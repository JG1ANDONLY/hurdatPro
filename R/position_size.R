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

get_coord_stormmap = function(df, knots){
  df$numeric.longitude <- 180 - df$numeric.longitude
  pos <- c(df$numeric.latitude, df$numeric.latitude)
  ne <- paste("ne", knots, sep = "")
  se <- paste("se", knots, sep = "")
  sw <- paste("sw", knots, sep = "")
  nw <- paste("nw", knots, sep = "")
  coordE <- track_coords(pos, df$ne, 45, df$se, -45 )
  coordS <- track_coords(pos, df$se, -45, df$sw34, -135 )
  coordW <- track_coords(pos, df$sw34, -135, df$nw34, 135 )
  coordN <- track_coords(pos, df$nw34, 135, df$ne34, 45 )
  x <- c(coordE$long, coordS$long, coordW$long,coordN$long)
  y <- c(coordE$lat, coordS$lat, coordW$lat,coordN$lat)
  storm_coord <- data.frame(x = numeric(404), y = numeric(404))
  storm_coord$x <- x
  storm_coord$y <- y
  return(storm_coord)
}

position_size <- function(stormid, date, time){
  #get the line of the dataframe which contains the nominated info
  datnew = hurdat
  df <- subset(hurdat, id == stormid&time == time&date == date)
  df$numeric.longitude <- 180 - df$numeric.longitude
  pos <- c(df$numeric.latitude, df$numeric.latitude)
  #50knots
  x34 <- get_coord_stormmap(df, "34")$x
  y34 <- get_coord_stormmap(df, "34")$y
  #50knots
  x50 <- get_coord_stormmap(df, "50")$x
  y50 <- get_coord_stormmap(df, "50")$y
  #64knots
  x64 <- get_coord_stormmap(df, "64")$x
  y64 <- get_coord_stormmap(df, "64")$y

  #generate map
  library(ggplot2)
  library(maps)
  map_projection <- "+proj=longlat +datum=WGS84"
  world_map <- map_data("world")
  us_map <- map_data("state")
  map <- ggplot() +
    # Add world map
    geom_map(data = world_map, map = world_map, aes(map_id = region),
             fill = "lightgrey", color = "black", size = 0.2) +
    # Add US state map
    geom_map(data = us_map, map = us_map, aes(map_id = region),
             fill = "white", color = "black", size = 0.2)+
    # Add 34 knot, 50 knot, 64 knot
    geom_polygon(data = data.frame(x34,y34),
                 aes(x = x34, y = y34, color="34"),
                 fill = NA, size = 1)+
    geom_polygon(data = data.frame(x50,y50),
                 aes(x = x50, y = y50, color="50"),
                 fill = NA, size = 1)+
    geom_polygon(data = data.frame(x64,y64),
                 aes(x = x64, y = y64, color="64"),
                 fill = NA, size = 1)+
    geom_point(data = data.frame(pos[1],pos[2]),
               aes(x = pos[1], y = pos[2], color="center"), size = 5)+
    xlab("Longitude") +
    ylab("Latitude") +
    xlim(pos[1]-5, pos[1]+5) +
    ylim(pos[2]-5, pos[2]+5)
  return(map)
}
