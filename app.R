library(shiny)
library(leaflet)
library(raster)
library(colorRamps)
library(tidyverse)
library(rgdal)

path_app<-"/srv/shiny-server/phenoforecast_shinyapp/"
date_list<-list.files(path_app,pattern =".tif" , recursive=T) %>%
  str_replace(".tif", "") %>%
  str_sub(start= -10) %>%
  unique() %>%
  as.Date() %>%
  sort()
# date_list<-seq(Sys.Date()-years(1), Sys.Date()+44, by=1)

genusoi_list <- c(
  "Quercus", 
  "Betula",
  "Populus",
  "Acer"
  )

leaf_sta_list<-flower_sta_list<-vector(mode="list",length=length(genusoi_list))
names(leaf_sta_list)<-names(flower_sta_list)<-genusoi_list
for (i in 1:length(genusoi_list)) {
  genusoi<-genusoi_list[i]
  leaf_files<-list.files(paste0(path_app,genusoi, "/leaf/"), full.names = T) %>% sort()
  leaf_ras_list<-vector(mode="list")
  for (r in 1:length(date_list)) {
    leaf_ras_list[[r]] <-raster(leaf_files[r])
  }
  leaf_sta<-stack(leaf_ras_list)
  leaf_sta_list[[i]]<-leaf_sta
  
  flower_files<-list.files(paste0(path_app,genusoi, "/flower/"), full.names = T) %>% sort()
  flower_ras_list<-vector(mode="list")
  for (r in 1:length(date_list)) {
    flower_ras_list[[r]] <-raster(flower_files[r])
  }
  flower_sta<-stack(flower_ras_list)
  flower_sta_list[[i]]<-flower_sta
}

sta_list<-list(Leaf=leaf_sta_list,Flower=flower_sta_list)

####
pal_leaf<-colorNumeric(palette = "Greens",  domain = c(0,1), na.color = "transparent")
pal_flower<-colorNumeric(palette = "Reds",  domain = c(0,1), na.color = "transparent")
pal<-list(Leaf=pal_leaf, Flower=pal_flower)

variable_list<-list(Leaf="Enhanced Vegetation Index",
                    Flower="Flowering intensity")

############################
ui<-fillPage(
  tags$style(type = "text/css", 
              "html, body {width:100%; height:100%;}"
             ),
  
  leafletOutput("raster_map", height="100%",width="100%"),
  
  absolutePanel(id = "controls", 
                class = "panel panel-default", 
                fixed = TRUE,draggable = TRUE, 
                top = 50, right = "auto", left = 60, bottom = "auto",
                width = "auto", height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                selectInput("type", "Type",
                            choices = c("Leaf", "Flower"),
                            selected =  "Leaf"),
                
                selectInput("genus", "Genus",
                            choices = genusoi_list,
                            selected = genusoi_list[1]),
                
                # sliderInput("date", "Date", min=mindate, max=maxdate, timeFormat = "%Y-%m-%d", value=1, ticks=T),
                sliderInput("day", "Day", min=44-length(date_list)+1, max=44, value=0, ticks=T)
                # If not using custom CSS, set height of leafletOutput to a number instead of percent     
  ),
  
  absolutePanel(id = "figure",
                class = "panel panel-default",
                fixed = TRUE,draggable = TRUE,
                top = 60, left = "auto", right = 60, bottom = "auto",
                width = 300, height = "auto",
                style = "background-color: rgba(255,255,255,0);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",
                
                # h4("Temporal patterns"),
                plotOutput("lineplot", height = 200),

                tags$a( href="https://twitter.com/intent/tweet?button_hashtag=phenology&ref_src=twsrc%5Etfw",
                        class="twitter-hashtag-button",
                        "data-size"="large",
                        "data-show-count"="false",
                        "Tweet #phenology"),
                tags$script(async=NA,
                            src="https://platform.twitter.com/widgets.js",
                            charset="utf-8")

                # includeScript("http://platform.twitter.com/widgets.js"),
                # https://shiny.rstudio.com/articles/html-tags.html
                # https://community.rstudio.com/t/include-a-button-in-a-shiny-app-to-tweet-the-url-to-the-app/8113/2
  ),
  
  absolutePanel(id = "tweet",
                class = "panel panel-default",
                fixed = TRUE,draggable = TRUE,
                top = 60+280, left = "auto", right = 60, bottom = "auto",
                width = 300, height = "auto",
                style = "background-color: rgba(255,255,255,1);
                border-color: rgba(255,255,255,0);
                box-shadow: 0pt 0pt 0pt 0px",

                tags$script(src="https://apps.elfsight.com/p/platform.js",
                            defer=NA),
                # includeScript("https://apps.elfsight.com/p/platform.js"), # this causes the app to crash
                tags$div(class = "elfsight-app-ab030cd9-764d-413a-9cfa-0e630029053f")
                
  )
  # absolutePanel(id = "figures2", class = "panel panel-default", fixed = TRUE,draggable = TRUE, top = 60+280, left = "auto", right = 60, bottom = "auto",width = 300, height = "auto", 
  #               
  #               # h4("Spatial patterns"),
  #               plotOutput("neighbours",height = 360)
  # )
)


server<-function(input, output){
  output$raster_map = renderLeaflet({leaflet(width = "100%", height="100%") %>%
      addTiles()%>%
      setView(lng = -98, lat = 38, zoom = 4)})
  
  observe({
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-44+length(date_list)]]
    date_label <- tags$div(
      date_list[input$day-44+length(date_list)]
    )  
    
    site_label<-tags$div(id="cite",
             '', tags$em('"PhenoForecast"'), ' by Yiluan Song'
    )
    
    reactiveRaster <- reactive({r_type_genusoi_date})
    leafletProxy("raster_map") %>%
      clearImages() %>%
      clearControls() %>%
      addRasterImage(reactiveRaster(),colors = pal[[input$type]], opacity = 0.8, layerId = "map")%>%
      addLegend(pal =  pal[[input$type]], values = seq(0,1,by=0.1),
                position = "bottomleft",title = "",#variable_list[[input$type]],
                layerId = "map"
      ) %>% 
      addControl(date_label, position = "bottomleft") %>% 
      addControl(site_label, position = "bottomright")
  })
  
  #Show popup on click
  observeEvent(input$raster_map_click, {
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    r_type_genusoi_date<-r_type_genusoi[[input$day-44+length(date_list)]]
    variable<-variable_list[[input$type]]
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    text_lat<-paste0("Latitude: ", round(lat,2))
    text_lng<-paste0("Longtitude: ", round(lng,2))
    text_date<-paste0("Date: ", date_list[[input$day-44+length(date_list)]])
    value<-round(raster::extract(r_type_genusoi_date,data.frame(lng,lat)),2)
    text_value<-paste0(variable,": ", value,"")
    
    content <- as.character(tagList(
      text_lat, tags$br(),
      text_lng, tags$br(),
      text_date, tags$br(),
      text_value, tags$br()
    ))
    
    leafletProxy("raster_map") %>%
      clearPopups() %>%
      addPopups(click$lng, click$lat, content)
  })
  
  # #Neighbours
  # observeEvent(input$raster_map_click, {
  #   r_type<-sta_list[[input$type,drop=F]]
  #   r_type_genusoi<-r_type[[input$genus]]
  #   r_type_genusoi_date<-r_type_genusoi[[input$day-44+length(date_list)]]
  #   variable<-variable_list[[input$type]]
  #   if(input$type=="Leaf") {
  #     col_nei<-"viridis"
  #     col_dir<-1
  #   }
  #   if (input$type=="Flower") {
  #     col_nei<-"magma"
  #     col_dir<--1
  #   }
  #   
  #   click <- input$raster_map_click
  #   lat<-(90+click$lat)%%180-90
  #   lng<-(180+click$lng)%%360-180
  #   # xcoor<-round((lng - 0.05) * 2) / 2 + 0.05
  #   # ycoor<-round((lat - 0.05) * 2) / 2 + 0.05
  #   
  #   # sp<-SpatialPoints(cbind(lng, lat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
  #   e<-extent(lng-0.5, lng+0.5, lat-0.5, lat+0.5)
  #   map_subset<-crop(r_type_genusoi_date, e)
  #   map_subset_df<-as.data.frame(map_subset, xy=T)
  #   colnames(map_subset_df)<-c("lon", "lat", "value")
  #   # map_subset_df<-map_subset_df%>%
  #   # mutate(color=pal_leaf[[input$type]](value)) %>%
  #   # mutate(fct=as.factor(row_number()))
  #   
  #   output$neighbours <- renderPlot({
  #     ggplot(map_subset_df)+
  #       geom_tile(aes(x=lon, y=lat, fill=value))+
  #       # scale_fill_manual(values=map_subset_leaf_df$color)+
  #       scale_fill_viridis_c(option=col_nei,
  #                            direction=col_dir)+
  #       theme_light()+
  #       xlab("")+
  #       ylab("")+
  #       theme(legend.position="bottom")+
  #       guides(fill = guide_colourbar(title=variable,
  #                                     title.position="bottom",
  #                                     title.hjust = 0.5,
  #                                     direction="horizontal",
  #                                     barheight = 2,
  #                                     barwidth =  16))+
  #       coord_equal()+
  #       ggtitle(paste0("Date: ", date_list[input$day-44+length(date_list)]))
  #   })
  # })
  
  #Lineplot
  observeEvent(input$raster_map_click, {
    r_type<-sta_list[[input$type,drop=F]]
    r_type_genusoi<-r_type[[input$genus]]
    # r_type_genusoi_date<-r_type_genusoi[[input$day-44+length(date_list)]]
    variable<-variable_list[[input$type]]
    if(input$type=="Leaf") {
      col_line<-"dark green"
    }
    if (input$type=="Flower") {
      col_line<-"red"
    }
    
    click <- input$raster_map_click
    lat<-(90+click$lat)%%180-90
    lng<-(180+click$lng)%%360-180
    
    sp<-SpatialPoints(cbind(lng, lat),proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
    ts<-raster::extract(r_type_genusoi, sp )
    ts_df<-data.frame(t(ts), date_list)
    colnames(ts_df)<-c("value", "date")
    if (nrow (ts_df)>0) {
      output$lineplot <- renderPlot({
        ggplot(ts_df)+
          geom_line(aes(x=date, y=value),col=col_line)+
          geom_vline(aes(xintercept=date_list[input$day-44+length(date_list)]))+
          geom_vline(aes(xintercept=date_list[0-44+length(date_list)]), alpha=0.5)+
          # geom_smooth(aes(x=date, y=value))+
          theme_light()+
          ylim(-0.1,1.1)+
          xlab("date")+
          ylab(variable)+
          ggtitle(paste0("Longitude: ", round(lng,2), ", Latitude: ", round(lat,2)))
      })
    } else {
      output$lineplot <- renderPlot({
        ggplot()+
          theme_void ()+
          ggtitle("\n No time series available.\n Contribute by submitting your data.")
      })
    }
    
  })
}


shinyApp(ui, server)#, options = list(height=600,width=1200)
