library(R.utils)
path_data<-paste0("/srv/shiny-server/phenoforecast_shinyapp/vmdata/archive/",today,"/")
path_shiny<-("/srv/shiny-server/phenoforecast_shinyapp")
for (i in 1:length(genusoi_list)){
  copyDirectory(from=paste0(path_data,genusoi, "leaf/output/maps"), to=paste0(path_shiny,"/",genusoi, "/leaf"))
  copyDirectory(from=paste0(path_data,genusoi, "flower/output/maps"), to=paste0(path_shiny,"/",genusoi, "/flower"))
}

