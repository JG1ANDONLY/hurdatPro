get_coord_stormmap = function(df, knots){
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