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
storm_map <- function(storm.ids) {
    library(tidyverse)
    library(ggplot2)
    library(maps)
    path <- system.file("data", "hurdat.RData", package = "hurdatPro")
    load(path)

    hurdat_filtered <- hurdat %>% filter(id %in% storm.ids)

    map <- map("world", c("USA", "Canada"), fill = TRUE, col = "gray80", plot = FALSE)


    p <- ggplot() +
      geom_path(data = hurdat_filtered, aes(x = numeric.longitude,
                                            y = numeric.latitude,
                                            group = id,
                                            color = "red"))
      coord_map() +
      theme_void() +
      xlim(range(map$range[1, ])) +
      ylim(range(map$range[2, ])) +
      labs(title="Storm Tracks", x="Longitude", y="Latitude") +
      scale_color_discrete(name="Storm ID")
    p <- p + borders("state", colour = "gray50", size = 0.5) +
      borders("country", colour = "gray50", size = 0.5)
    return(p)

}
