map_storm_q5 <- function(stormid, date, time){
  #get the line of the dataframe which contains the nominated info
  datnew = hurdat
  df <- datnew[which(datnew$id == stormid), ]
  df <- df[which(df$time == time), ]
  df <- df[which(df$date == date),]
  pos <- c(df$numeric.longitude, df$numeric.latitude)
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
  us_map <- map_data("state")a
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