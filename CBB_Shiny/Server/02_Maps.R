#------#
# Maps #
#------#

temp_habitatIn <- reactiveValues(habitatIn = NULL) # <- Store aoo output
temp_dat_ne <- reactiveValues(dat_ne = NULL) # <- Store eoo output


observe({
  # Don't run unless a data have been imported
  req(input$text.gbif)
  
  observeEvent(input$gbif.map.button, {
  if(input$habitat.gbif == "mar"){
    
    balearic <- "POLYGON((0.194 37.762, 5.217 37.762,5.217 41.058,0.194 41.058,0.194 37.762))"
    shp <- st_read("./shp/CBB_Balearic_Sea/CBBBalearicSea.shp")
    st_crs(shp) = 4326
    
  } else {

    balearic <- "POLYGON((0.898 38,4.592 38,4.592 40.295,0.898 40.295,0.898 38))"
    shp <- st_read("./shp/Balearic_Islands/Balearic_4326.shp")
    st_crs(shp) = 4326
    
  }
  
  # tax_key <- name_backbone(name = "Podarcis pityusensis", kingdom = "Animalia")
  tax_key <- name_backbone(name = input$text.gbif, kingdom = input$kingdom.gbif)
  key <- ifelse("acceptedUsageKey" %in% colnames(tax_key), tax_key$acceptedUsageKey, tax_key$usageKey)
  
  # List of occurrence in Balearic islands
  dat_ne <- occ_search(taxonKey = key, hasCoordinate = T, 
                       geometry = balearic, limit = 99999) %>% 
    .$data %>% 
    as.data.frame() %>% 
    select(decimalLatitude, decimalLongitude) %>% 
    rename(longitude = decimalLongitude, latitude = decimalLatitude)
  
  # Add ID column
  dat_ne$id <- rownames(dat_ne) 
  
  # Transform the dataframe to spatial dataframe and assign crs
  dat_ne <- st_as_sf(dat_ne, coords = c("longitude", "latitude"), crs = 4326)
  
  # Polygon points intersection
  habitatIn <- st_intersection(dat_ne, shp)
  
  habitatInId <- habitatIn$id
  
  habitatIn <- habitatIn %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(longitude = X, latitude = Y)

  habitatIn <- cbind(habitatIn, habitatInId)
  
  dat_ne$inOut <- ifelse(dat_ne$id %in% habitatIn$habitatInId, "in", "out")
  
  dat_neId <- dat_ne$inOut
  
  dat_ne <- dat_ne %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(longitude = X, latitude = Y)
  
  temp_dat_ne$dat_ne <- cbind(tax_key$scientificName, dat_ne, dat_neId)
  
  # temp_habitatIn$habitatIn <- cbind(habitatIn, habitatInId)
  
  # The map will be plotted after push the button
  # observeEvent(input$gbif.map.button, {
    # Plot map
    output$myMap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = 3.00, lat = 39.71, zoom = 7) %>%
        addMarkers(data = habitatIn, clusterOptions = markerClusterOptions())
    })
    
    
    # Show text whit statistics
    output$mapStatistics <- renderText({
      
      paste("Number of occurrence with coordinates:", nrow(dat_ne), "\n",
            "Number of occurrences inside de habitat:", nrow(habitatIn),
            "-", round(nrow(habitatIn)/nrow(dat_ne) * 100, digits = 2), "%")
      
    })
    
    output$downloadButtonOCC <- renderUI({
      downloadButton("downloadDataOCC", "Download Dataset OCC",
                     style = "padding:6px; font-size:80%")
    }) 
    
    
    })
  
})

# Download temp_df.2 ----
output$downloadDataOCC <- downloadHandler(
  filename = function() {
    paste("CBB_OCC_dataset_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_dat_ne$dat_ne, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)

# AOO calculation
observeEvent(input$aoo.button, {
  
  req(temp_habitatIn$habitatIn)
  
  habitatIn.aoo <- aoo(temp_habitatIn$habitatIn)
  
  # Show text whit statistics
  output$mapAOO <- renderText({
    
    # str1 <- paste("Calculates the Area of Occupancy of a species based on either known 
    #      records or predicted distribution.
    #      AOO is calculated as the area of all known or predicted cells for the 
    #      species. The resolution will be 2x2km as required by IUCN.", sep = "\n")
    
    HTML(paste("AOO:", habitatIn.aoo))
  
  })
  
})

# EOO calculation
observeEvent(input$eoo.button, {
  
  req(temp_habitatIn$habitatIn)
  
  habitatIn.eoo <- eoo(temp_habitatIn$habitatIn)
  
  # Show text whit statistics
  output$mapEOO <- renderText({

  HTML(paste("EOO:", habitatIn.eoo))
    
    #paste("EOO", temp_eoo$eoo)
  })
  
})

