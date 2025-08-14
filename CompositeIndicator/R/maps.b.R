
# This file is a generated template, your changes will not be overwritten

mapsClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "mapsClass",
    inherit = mapsBase,
    private = list(
      

        
        .run = function() {
          
          
          self$results$getting_started$setContent("<h2>Getting started</h2>
                                                    <div>
                                                    This module allows you to create a map for each indicator you select in <b>Indicators for maps</b>.<br>
                                                    To use this analysis you should have a shapefile of the regions you want to map. The shapefile may contain
                                                    more regions than the ones you are interested in analyzing, but it is important that the names of the regions are equal in your dataset
                                                    and in the shapefile.<br>
                                                    <br>
                                                    To create maps, please start selecting the <b>region variable</b> and the <b>indicators</b> you want to plot.
                                                    One map will be shown for each indicator.<br>
                                                    <br>
                                                    You can customize different options:<br>
                                                    <b>Shapefile-related options</b><br>
                                                    First, you should enter the folder where you saved the shapefile and the name of the shapefile.<br>
                                                    <b>Statistical units options</b><br>
                                                    Then, enter the name of the column that identifies the statistical units in the shapefile.
                                                    <br>
                                                    You may exclude some specific regions from your maps if needed.<br>
                                                    <b>Saving options</b><br>
                                                    Specifiy the saving path and the format for your maps. You can customize their size, too.
                                                  
                                                  </div")
          
          # - These are input data (jamovi) 
          mydata <- self$data
          
          # - Rename region/country column as in the shape file:
          mydata[self$options$stat_unit_name_shp] = mydata[self$options$stat_unit]
          
          mydata <- mydata %>% 
            select(-self$options$stat_unit)
          
          

          # - Vec of stat units to remove from shapefile:
          to_remove_vec = unlist(strsplit(self$options$stat_unit_shp_to_remove, ","))
          to_remove_vec = stringr::str_trim(string = to_remove_vec)
          
          # - Import shape file, remove regions/countries contained in to_remove_vec:
          shp_file <- sf::st_read(dsn = self$options$shape_file_path, layer = self$options$shape_file) %>%
                      filter(!.data[[self$options$stat_unit_name_shp]] %in% to_remove_vec)
          #self$results$check$setContent(list(colnames(shp_file), colnames(mydata)))
          
          
          # - Set plotData:
          plotData <- merge(x=shp_file,
                            y=mydata,
                            by=self$options$stat_unit_name_shp,
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
            geom_sf(aes(fill=.data[[gsub('[\"]', '', image$name)]])) 
          
          ggsave(filename = paste0(gsub('[\"]', '', image$name), '.', self$options$map_output_format), #same results as with image$title
                 plot = plot,
                 path = self$options$saving_path) 

          
          print(plot)
          TRUE 
          
          #self$results$plot_data$setContent(image$name)   # ok is equal to image$title
          
          
          
        }
        
        )
)
