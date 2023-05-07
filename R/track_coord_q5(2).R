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