library(rgeos)
library(maptools)
library(data.table)

read_map <- function(filename) {
  data <- readShapeSpatial(filename, proj4string=CRS("+proj=longlat"))
  data@data$id = rownames(data@data)
  data.points = fortify(data, region="id")
  data.table(merge(data.points, data@data, by="id"))
}

plot_europe <- function(map.dt){
  p = ggplot(map.dt, aes(x=long, y=lat)) + theme_void()  +
    theme(legend.position="top")  + guides(alpha="none", fill=guide_legend(title=NULL)) +
    geom_map(map = map.dt, aes(map_id=id), alpha=0.5,
                                            show.legend = FALSE) + xlim(c(-12,30)) + ylim(c(35,60))
  p
}

########## Plotting Economic Data #############
library(ggplot2)
library(data.table)

get_early_austrian_data <- function(){
  Wifo_S44 <- fread("Sonderheft14_S44.csv", sep=";")
  Wifo_S43 <- fread("Sonderheft14_S43.csv", sep=";")
  
  earlyAUT = merge(Wifo_S44, Wifo_S43, by="Jahr")
  earlyAUT[, unemp := 1000*`Arbeitslose (in 1000)`/(`Arbeitslose (in 1000)` + `Erwerbstaetige (in 1000)`)]
  earlyAUT[, NX := `Exports (iwS)` - `Imports (iwS)`]
  earlyAUT[, IM :=  -`Imports (iwS)`]
  earlyAUT[, EX :=  `Exports (iwS)`]
}

