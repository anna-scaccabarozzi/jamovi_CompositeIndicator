
# This file is a generated template, your changes will not be overwritten

mapsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mapsClass",
    inherit = mapsBase,
    private = list(
      

        
        .run = function() {
          
          
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to create a map for each indicator you select in <b>Indicators for maps</b>.<br>
                                                    To create maps, please start selecting the <b>statistical unit variable</b> and the <b>indicators</b> you want to plot.
                                                    One map will be shown for each indicator.<br>
                                                    You can customize the longitude and latitude ranges.
                                                  </div")
          
          # - These are input data (jamovi) 
          mydata <- self$data
  
          
          # - Fetch world map data in Simple Feature (sf) format
          world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
          
          # - Set plotData:
          plotData <- merge(x = world,
                                y = mydata,
                                by.x='sovereignt',
                                by.y =self$options$stat_unit,
                                all.x = TRUE)
          

          # - Create array of images:
          images <- self$results$plots
          for (key in images$itemKeys) {
            image <- images$get(key=key)
            image$setState(plotData)
          }
          

        },
        
        # - Plot of the scores
        .plot=function(image, ...) { 
          plotData <- image$state
          

          plot <- ggplot(plotData) +
            geom_sf(aes(fill=.data[[gsub('[\"]', '', image$name)]])) +
            coord_sf(xlim = c(self$options$longitude_min, self$options$longitude_max),
                     ylim = c(self$options$latitude_min, self$options$latitude_max)) + 
            theme_minimal() 

          
          print(plot)
          TRUE 
          
          #self$results$plot_data$setContent(image$name)   # ok is equal to image$title
          
          
          
        }
        
        )
)
