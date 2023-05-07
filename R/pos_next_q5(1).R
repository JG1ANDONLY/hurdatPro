#' Given the point position of a storm, get the coordinate of the position of 
#' the storm based on the moving distance and moving angle.
#'
#' @param pos0, the coordinate of the original point position of a storm
#' @param dist, moving distnce 
#' @param ang, angle of the moving storm
#' 
#' @return pos, a vector contains x-coordinate and y-coordinate of the next 
#' position of the storm
#' 
#' 
#' @examples
#' pos_next <- pos_next(c(-50,30), 10, 45)
#
#' @export pos

pos_next <- function(pos0, dist, ang){
  ang_rad <- ang * pi/180
  radii = 6371
  pos <- c(0,0)
  pos[2] <- (dist * sin(ang_rad)/ radii) * (360/(2*pi)) + pos0[2]
  pos[1] <- (dist * cos(ang_rad) / (6371*cos((dist * sin(ang_rad)/ radii))))*(360/(2*pi)) + pos0[1]
  return(pos)
}