
#devtools::install_github("rstudio/leaflet")
library(leaflet)
library(shiny)
library(shinythemes)
library(shinyjs)
library(dplyr)
library(magrittr)
library(rgdal)
library(DT)
library(tidyr)
library(ggvis)
library(jsonlite, pos=100)
library(RColorBrewer)
library(maps)
library(ggmap)
library(ggplot2)
library(akima)
library(reshape2)
library(grid)
library(gridExtra)
library(raster)
library(htmlwidgets)

#library(rasterVis)
library(sp)

# download and load data
all.data = read.csv("/Users/zach/Dropbox (ZachTeam)/DE_BIRD_ATLAS/DE_bird_analyses/R_Shiny/Data/All_abun_output_points.csv",header=TRUE)
#clean up data for data.table
table.data<-all.data[,c("County","Unit","Manager","Habitat","grts","Latitude","Longitude","Common.name","Scientific.name","AlphaCode","Predicted","SE","lower","upper")]
colnames(table.data)<-c("County","Unit","Manager","Habitat Type","Point ID (GRTS)", "Latitude","Longitude","Common Name", "Scientific Name","Alpha Code",
                        "Mean Abundance Estimate","SE","Lower 95% CI","Upper 95% CI")
row.names(table.data)<-NULL

#bird data for mapping
bird.data<-all.data
bird.data <- bird.data[order(bird.data$Predicted,decreasing=FALSE),]
bird.data$Preidcted<-as.numeric(round(bird.data$Predicted,2))
bird.data$Predicted[(bird.data$Predicted==0)==TRUE]<-NA

#order factor levels for county
bird.data$County<-factor(bird.data$County, levels=c("New Castle","Kent","Sussex"))

#get list of species in data
BN<-data.frame(unique(as.factor(sort(as.character(bird.data$Common.name)))))
colnames(BN)<-"Common.name"
row.names(BN)<-unique(as.factor(sort(as.character(bird.data$Common.name))))


#read in US county shapefile
mapCounties <- map("county", fill = TRUE,
                   plot = FALSE,
                   region = c('delaware'))
mapCounties$names<-as.factor(as.character(mapCounties$names))
summary(mapCounties)

#####################################################################################################################################
#ui function
ui =
  #set up navigation bar
  navbarPage(title=div(img(src="UD_DNREC_logo.png"), "Delaware Breeding Bird Abundance 2015"),
             position = "static-top",inverse=FALSE, collapsible = FALSE, fluid=TRUE, windowTitle = "DE Birds",
             theme= shinytheme("spacelab"), id="MainNavBar",
             
             tabPanel(title="Map", 
                      useShinyjs(),
                      div(class="outer",
                          tags$head(includeCSS("./www/mapstyles.css") ),# brings in  css file that lets map take up whole screen,
             
                      leafletOutput("DEmap", width = "100%", height = "100%"),
                      
                      #Control panel
                      absolutePanel(class="panel panel-default controls", draggable = TRUE, cursor="auto", top="150px", bottom="auto",
                                    height="auto", right="auto", left="10px", width="300px", id="MapControlPanel", fixed=TRUE,
                                    
                        br(),
                                    
                      #Select species controls
                      div(id="SpeciesControls",
                          tags$div(title="Species to display on map",
                                   selectInput(inputId="MapSpecies",label="Species",selected="Acadian Flycatcher", 
                                               choices=c("Species"="",rownames(BN)), multiple=FALSE)
                          )),
                      
                          hr(),

                      tags$div(title="Map Layers",
                          radioButtons(inputId="MapLayers", label="Layers",  selected="Points", choices=c("Points"="Points","Interpolated"="Interpolated", "Both"="Both"),inline=TRUE
                                             
                              )), 
                                    
                            hr(),
                                    
                        
                      tags$div(title="Choose the habitat type",
                            radioButtons(inputId="HabitatType", label="View by habitat type", choices=c("All"="All",
                                                                                                     "Grassland"="Grassland",
                                                                                                     "Forest"="Forest",
                                                                                                     "Forested Wetland"="Forested Wetland",
                                                                                                     "Aquatic Wetland"="Aquatic Wetland",
                                                                                                     "Scrub/Shrub Wetland"="Scrub/Shrub Wetland",
                                                                                                     "Salt marsh/Estuarine"="Salt marsh/Estuarine"),
                                              inline=F
                                 )),
                            
                            tags$div(title="Zoom to county",
                                     radioButtons(inputId="CountySelect", label="Select county", choices=c("All"="All",
                                                                                                         "New Castle"="New Castle",
                                                                                                         "Kent"="Kent",
                                                                                                         "Sussex"="Sussex"),
                                                  inline=TRUE
                                     )), 
                                     
                                
                            hr(),
                            
                                plotOutput("boxPlot", height = 200))
                      )),
                        
                        #actionButton(inputId="AboutMap", class="btn btn-primary", label="About the map..."),
                        

                        #checkboxInput("legend", "Show legend", TRUE),
                        
                       #downloadButton("save", "Export map")
             
                       tabPanel("Data explorer",
                                fluidRow(
                                  column(3,selectInput(inputId="TableSpecies",label="Species",
                                         choices=c("All Species"="",rownames(BN)), multiple=TRUE))

                                ),
                                hr(),
                                DT::dataTableOutput("dataTable")
                       
                         ),
             conditionalPanel("false")
          )
######################################################################################################################################
#server function
  server = function(input, output,session) {
    
    
    #read in shape file to add county borders
    countypal=colorFactor(c("lemonchiffon3", "darkseagreen2", "lightskyblue3") , mapCounties$names) 
    
    #get coordinates of all data
    minLon=min(all.data$Longitude,na.rm=TRUE)-0.2
    maxLon=max(all.data$Longitude,na.rm=TRUE)+0.2
    minLat=min(all.data$Latitude,na.rm=TRUE)-0.2
    maxLat=max(all.data$Latitude,na.rm=TRUE)+0.2
    
    #output the map
    map <- leaflet() %>%
      addTiles(attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')%>%
      fitBounds(minLon, minLat,maxLon, maxLat )%>%
      addScaleBar(position = c("bottomleft"))
    
      output$DEmap = renderLeaflet(map)

      #Choices of basemap views
      observe({
        leafletProxy("DEmap") %>%
          clearTiles()%>%
          addTiles(group="Streets", urlTemplate="https://a.tiles.mapbox.com/v4/mapbox.streets/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpbTgzcHQxMzAxMHp0eWx4bWQ1ZHN2NGcifQ.WVwjmljKYqKciEZIC3NfLA", options=tileOptions(minZoom=5, zIndex=0))%>%
          addTiles(group="Light", urlTemplate="https://b.tiles.mapbox.com/v4/mapbox.light/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpbTgzcHQxMzAxMHp0eWx4bWQ1ZHN2NGcifQ.WVwjmljKYqKciEZIC3NfLA", options=tileOptions(minZoom=5, zIndex=0)) %>%
          addTiles(group="Dark", urlTemplate="https://b.tiles.mapbox.com/v4/mapbox.dark/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpbTgzcHQxMzAxMHp0eWx4bWQ1ZHN2NGcifQ.WVwjmljKYqKciEZIC3NfLA",options=tileOptions(minZoom=5, zIndex=0)) %>%
          addTiles(group="Satellite", urlTemplate="https://a.tiles.mapbox.com/v4/mapbox.satellite/{z}/{x}/{y}.png?access_token=pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpbTgzcHQxMzAxMHp0eWx4bWQ1ZHN2NGcifQ.WVwjmljKYqKciEZIC3NfLA", options=tileOptions(minZoom=5, zIndex=0))%>%
          addLayersControl(baseGroups=c("Streets","Light","Dark","Satellite"), options=layersControlOptions(collapsed=F,autoZIndex=FALSE))
        
      })
      
      
      
      
      #get bounding boxes dynamically by county
      county.data<-reactive({
          return(bird.data[bird.data$County == input$CountySelect,])
        })
      
      
      #Interactive bounding boxes to view counties
      observe({
        if(input$CountySelect=="All"){
          leafletProxy("DEmap")%>%
            setView(lng=-75.427670, lat=39.09,zoom=9)
        } else {
          if(input$CountySelect=="New Castle"){
            #get centroid of county
            newLong<-mean(county.data()$Longitude)
            newLat<-mean(county.data()$Latitude)
            
            minLon=min(county.data()$Longitude,na.rm=TRUE)-0.2
            maxLon=max(county.data()$Longitude,na.rm=TRUE)+0.2
            minLat=min(county.data()$Latitude,na.rm=TRUE)-0.2
            maxLat=max(county.data()$Latitude,na.rm=TRUE)+0.2
            
            
            leafletProxy("DEmap")%>%
              setView(lng=newLong, lat=newLat, zoom=10)
          }else{
          #get centroid of county
          newLong<-mean(county.data()$Longitude)-0.2
          newLat<-mean(county.data()$Latitude)
          
          minLon=min(county.data()$Longitude,na.rm=TRUE)-0.2
          maxLon=max(county.data()$Longitude,na.rm=TRUE)+0.2
          minLat=min(county.data()$Latitude,na.rm=TRUE)-0.2
          maxLat=max(county.data()$Latitude,na.rm=TRUE)+0.2
          
          leafletProxy("DEmap")%>%
            setView(lng=newLong, lat=newLat, zoom=10)
          }
        }
      })
      
      
      
    
    
    # subset data by species 
    speciesData<- reactive({
      return(subset(bird.data, Common.name==input$MapSpecies))
    })
    
    #subset data by habitat
    circleData <- reactive({
      ifelse(input$HabitatType=="All",return(speciesData()),return(subset(speciesData(), Habitat==input$HabitatType)))
    })
    
    
    
   
    
    
 ##########################################################################################################################
#Add maps and circles
    # create reactive raster layer
    
    ras<-reactive({
      new.species<-subset(all.data, Common.name==input$MapSpecies)

      abun<-new.species[,c("Longitude","Latitude","Predicted")]
      
      #use 'akim' package to interpolate among points (Gridded bivariate linear interpolation)
      resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
      abun.new <- interp(x=abun$Longitude, y=abun$Latitude, z=abun$Predicted,
                         xo=seq(min(abun$Longitude),max(abun$Longitude),by=resolution),
                         yo=seq(min(abun$Latitude),max(abun$Latitude),by=resolution), duplicate="mean")
      
      ras<-raster(abun.new)
      #crs(ras) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    })
    
    pal <- colorNumeric(c(alpha("transparent",0.1),alpha("royalblue",1),alpha("springgreen",1),alpha("yellow",1), alpha("red",1)), c(0,14), na.color="transparent",alpha=TRUE)
    
    
    
    ### Add Map Circle
    observe({

        #color palattes ramp with Predicted values
        colorData <- c(0,14)
        #palLegend <- colorBin(c("red","yellow","springgreen","royalblue"), na.color="gray",bins=8,pretty=TRUE,domain=colorData)
        palLegend <- colorNumeric(c("red","yellow","springgreen","royalblue"), na.color="gray",domain=colorData)
        palPoints <- colorNumeric(rev(c("red","yellow","springgreen","royalblue")), na.color=alpha("gray",0.5), domain=colorData)

        ## custom label format function
        myLabelFormat = function(..., reverse_order = FALSE){
          if(reverse_order){
            function(type = "numeric", cuts){
              cuts <- sort(cuts, decreasing = T)
            }
          }else{
            labelFormat(...)
          }
        }


        radiusFunc<-function(x){
          out<-(x-min(x))/(max(x)-min(x))*6000
          return(out)
          }
      
      if(input$MapLayers == "Points"){
        leafletProxy("DEmap") %>%
          clearImages()%>%
          clearShapes() %>%
          addPolygons(data=mapCounties, fill=TRUE, fillColor = ~countypal(names),fillOpacity=0.3, stroke = TRUE, color="darkgray",weight=2, opacity=0.7,options=tileOptions(zIndex=4))%>%
          addCircles(data=circleData(), radius=850, stroke=FALSE,
                     fillColor = ~palPoints(Predicted),  fillOpacity = 0.5, popup = ~paste(Predicted)) %>%
          addLegend("bottomright", pal=palLegend, values=colorData, title="Estimated\nAbundance",
                    layerId="colorLegend",labFormat=myLabelFormat(reverse_order = T))
      }else{
        if(input$MapLayers=="Interpolated"){
        
        leafletProxy("DEmap") %>%
            clearImages()%>%
            clearShapes() %>%
            addPolygons(data=mapCounties, fill=TRUE, fillColor = ~countypal(names),fillOpacity=0.3, stroke = TRUE, color="darkgray",weight=2, opacity=0.7,options=tileOptions(zIndex=4))%>%
            # removeTiles(layerId="rasimg") %>% 
            addRasterImage(ras(), colors=pal, opacity=0.8, layerId="rasimg",project=TRUE,maxBytes = 100000000000000)%>%
          # addCircles(radius=850, stroke=FALSE,
          #            fillColor = ~palPoints(Predicted),  fillOpacity = 0.5, popup = ~paste(Predicted)) %>%
          addLegend("bottomright", pal=palLegend, values=colorData, title="Estimated\nAbundance",
                    layerId="colorLegend",labFormat=myLabelFormat(reverse_order = T))
        }else{
          if(input$MapLayers=="Both"){
            
            leafletProxy("DEmap") %>%
              clearImages()%>%
              clearShapes() %>%
            addPolygons(data=mapCounties, fill=TRUE, fillColor = ~countypal(names),fillOpacity=0.3, stroke = TRUE, color="darkgray",weight=2, opacity=0.7, options=tileOptions(zIndex=4))%>%
            addRasterImage(ras(), colors=pal, opacity=0.8, layerId="rasimg",project=TRUE,maxBytes = 100000000000000)%>%
            addCircles(data=circleData(),radius=850, stroke=FALSE,
                       fillColor = ~palPoints(Predicted),  fillOpacity = 0.5, popup = ~paste(Predicted)) %>%
            addLegend("bottomright", pal=palLegend, values=colorData, title="Estimated\nAbundance",
                      layerId="colorLegend",labFormat=myLabelFormat(reverse_order = T))
        }
        }
      }
        

  })
      

    
    
    
    
    # Add boxolot showing estimates by county

      #get_legend function
      get_legend<-function(myggplot){
        tmp <- ggplot_gtable(ggplot_build(myggplot))
        leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
        legend <- tmp$grobs[[leg]]
        return(legend)
      }
      
      
     
      
     output$boxPlot <- 
          renderPlot({

            #get legend (for all plots)
            boxPlotLegend<-ggplot(data=bird.data, aes(x=County,y=Predicted))+
              geom_boxplot(aes(fill=County), alpha=0.7)+
              scale_fill_manual(values=c("darkseagreen2","lemonchiffon3",  "lightskyblue3"))+
              theme(panel.background=element_rect(fill="white", color="gray30"),
                    panel.border=element_rect(color="gray30",fill="white"))+
              theme(axis.ticks.y=element_line(color="white"),
                    axis.text.y=element_text(color="white"))+
              ggtitle("Estimated abundance\nper point by county")
            
            legend<-get_legend(boxPlotLegend)

      #boxPlots for species with detection data      
      boxPlot1<-ggplot(data=speciesData(), aes(x=County,y=Predicted))+
        geom_boxplot(aes(fill=County), alpha=0.7)+
        scale_fill_manual(values=c("darkseagreen2","lemonchiffon3",  "lightskyblue3"))+
        theme(panel.background=element_rect(fill="white", color="gray30"))+
        theme(panel.border=element_rect(color="gray30",fill=NA))+
        theme(legend.position = "none")+
        ggtitle("Estimated abundance\nper point by county")
        
      
        box.plot.1<-arrangeGrob(boxPlot1, legend,  ncol=2,
                               widths=c(2, 1), heights=c(3, 0.7), padding=unit(0,"line"))

      # Create a text
        grob <- grobTree(textGrob("No detection data.", x=0.2,  y=0.5, hjust=0,
                                  gp=gpar(col="darkgray", fontsize=11)))
        
        
        #get boxPlot for species with no detections
        boxPlotBlank<-ggplot(data=speciesData(), aes(x=County,y=Predicted))+
          geom_boxplot(aes(fill=County), alpha=0.7)+
          scale_fill_manual(values=c("darkseagreen2","lemonchiffon3",  "lightskyblue3"))+
          theme(panel.background=element_rect(fill="white", color="gray30"),
                panel.border=element_rect(color="gray30",fill=NA))+
          theme(axis.ticks.y=element_line(color="white"),
                axis.text.y=element_text(color="white"))+
          theme(legend.position = "none")+
          ggtitle("Estimated abundance\nper point by county")
        
        
        boxPlot2<-boxPlotBlank + annotation_custom(grob)
        
        box.plot.2<-arrangeGrob(boxPlot2, legend,  ncol=2,
                                 widths=c(2, 1), heights=c(3, 0.7), padding=unit(0,"line"))

        # Plot
        if(max(speciesData()$Predicted,na.rm=TRUE)!=-Inf){
          grid.newpage()
          grid.draw(box.plot.1)
        }else{
          grid.newpage()
          grid.draw(box.plot.2)
        }
    
      })


      
        # observe({
      #   if(input$returnpdf == TRUE){
      #     m <- leafletProxy("DEmap")
      #     saveWidget(m, "temp.html", selfcontained = FALSE)
      #     webshot("temp.html", file = "plot.pdf", cliprect = "viewport")
      #   }
      # })
      # 
      # output$pdflink <- downloadHandler(
      #   filename <- "map.pdf",
      #   content <- function(file) {
      #     file.copy("plot.pdf", file)
      #   }
      # )
   
    #trying to define variable to take on current map for export

     #currentMap<- L.mapbox.map('map', 'DEmap').setView([39.09, -75.527670], 9)

      ## 'mapview' objects (image below)\
      
      # output$save <- downloadHandler(
      #   filename = 'plot.pdf',
      #   
      #   content = function(file) {
      #     # temporarily switch to the temp dir, in case you do not have write
      #     # permission to the current working directory
      #     owd <- setwd(tempdir())
      #     on.exit(setwd(owd))
      #     
      #     saveWidget(map(), "temp.html", selfcontained = FALSE)
      #     webshot("temp.html", file = file, cliprect = "viewport")
      #   }
      # )      
      
    # output$save <- downloadHandler(
    #   filename = function() {
    #     paste("DEmap", Sys.Date(), "pdf",sep=".")
    #   },
    #   content = function(file) { 
    #     pdf(file = file, width=10, height=8)
    #     plotInput()
    #     dev.off()      },
    #   contentType="application/pdf"
    # )
    
    # output$save <- downloadHandler(
    #   filename = function() {
    #     paste("DEmap", Sys.Date(), "png",sep=".")
    #   },
    #   content = function(file) { 
    #     png(file)
    #     dev.print()
    #     dev.off()
    #   },
    #   contentType="image/png"
    #)
    
################################################################################################################################
#Data Table
      
    
      #get data to present in table
      table.data.show<-reactive({
        return(table.data[table.data$`Common Name` %in% input$TableSpecies,])
      })
      
      output$dataTable = renderDataTable({
        if(is.null(input$TableSpecies)) {return(table.data)
        }else {
          table.data.show()
          }
        
      })
      
 }     
      # output$dataTable <- DT::renderDataTable({
      #   df <- cleantable %>%
      #     filter(
      #       is.null(input$states) | State %in% input$states,
      #       is.null(input$cities) | City %in% input$cities,
      #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      #     ) %>%
      #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      #   action <- DT::dataTableAjax(session, df)
      #   
      #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
      # })
      
      
      # observe({
      #   species <- if (is.null(input$MapSpecies)) character(0) else {
      #     filter(cleantable, Common.name %in% input$MapSpecies) %>%
      #       `$`('S') %>%
      #       unique() %>%
      #       sort()
      #   }
      #   stillSelected <- isolate(input$county[input$county %in% species])
      #   updateSelectInput(session, "species", choices = species,
      #                     selected = stillSelected)
      # })
      # 
      # observe({
      #   zipcodes <- if (is.null(input$states)) character(0) else {
      #     cleantable %>%
      #       filter(State %in% input$states,
      #              is.null(input$cities) | City %in% input$cities) %>%
      #       `$`('Zipcode') %>%
      #       unique() %>%
      #       sort()
      #   }
      #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
      #   updateSelectInput(session, "zipcodes", choices = zipcodes,
      #                     selected = stillSelected)
      # })
      # 
      # observe({
      #   if (is.null(input$goto))
      #     return()
      #   isolate({
      #     map <- leafletProxy("map")
      #     map %>% clearPopups()
      #     dist <- 0.5
      #     zip <- input$goto$zip
      #     lat <- input$goto$lat
      #     lng <- input$goto$lng
      #     showZipcodePopup(zip, lat, lng)
      #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      #   })
      # })
      # 
      # output$dataTable <- DT::renderDataTable({
      #   df <- cleantable %>%
      #     filter(
      #       is.null(input$states) | State %in% input$states,
      #       is.null(input$cities) | City %in% input$cities,
      #       is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      #     ) %>%
      #     mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      #   action <- DT::dataTableAjax(session, df)
      #   
      #   DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
      # })

shinyApp(ui=ui, server=server)
