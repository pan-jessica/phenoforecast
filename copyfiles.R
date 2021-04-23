library(R.utils)
library(tidyverse)

path_shiny<-("/srv/shiny-server/phenoforecast_shinyapp/")
today<-read_file(paste0(path_shiny,"today.txt")) %>% as.Date()
path_data<-paste0(path_shiny,"vmdata/archive/",today,"/")

genusoi_list <- c(
  "Quercus", 
  "Betula",
  "Populus",
  "Acer"
)

for (i in 1:length(genusoi_list)){
  genusoi<-genusoi_list[i]
  unlink(paste0(path_shiny,"/data/",genusoi,"/"))
  copyDirectory(from=paste0(path_data,genusoi, "/leaf/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/leaf"))
  copyDirectory(from=paste0(path_data,genusoi, "/flower/output/maps"), to=paste0(path_shiny,"/data/",genusoi, "/flower"))
}

