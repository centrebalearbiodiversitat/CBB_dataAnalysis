#############################################################
# Title: Download occurrences from OBIS.                    #
# Aims: 1) Download data from OBIS                          #
# Author: Jorge Palomo, Tommaso Cancellario.                #
# Reviewer: NA                                              #
# Creation: 2023 - 07 - 25                                  #
# Last update: 2023 - 07 - 25                               #
#############################################################

# Load libraries
pacman::p_load(robis, sf, worrms, rvest, tidyverse)

# Set wd
setwd("~/OneDrive - Universitat de les Illes Balears/")

# Read the shapefile
balearicSea <- st_read("CBB objectives/CBB_Balearic_Sea/CBBBalearicSea_NO_islands.shp")
balearicSea <- st_set_crs(balearicSea, st_crs(4326))
plot(balearicSea[1])

# Load sp list.
species.list <- read.csv("/Users/tcanc/Desktop/Chaetognatha_GBIFChecked_2023-07-26.csv", sep = ";")

# Species list
sp <- unique(species.list$acceptedName)

# Genus list
gen <- unique(species.list$genus)


# OBIS occurrence download.
obisInfo <- data.frame()
for(i in 1:length(sp)){
  
  x <- occurrence(scientificname = sp[i]) 
  
  if(length(x) != 0){
    
    x <- select(x, scientificName, decimalLatitude, decimalLongitude)
    
    obisInfo.1 <- cbind(sp[i], x)
    colnames(obisInfo.1)[1] <- "originalName"  
    
    # Convert the dataframe to an sf object
    sf_points <- st_as_sf(obisInfo.1, 
                          coords = c("decimalLongitude", "decimalLatitude"), 
                          crs = 4326)
    # Filter only the points
    intersection_result <- st_intersection(balearicSea, sf_points) %>% 
      as.data.frame() %>% 
      select(originalName, scientificName)
    
    if(nrow(intersection_result) >= 5){
      
      intersection_result$presenceAbsence <- "present"
      obisInfo <- rbind(obisInfo, intersection_result[1,])
      
    } else {
      
      intersection_result <- data.frame(originalName = sp[i],
                                        scientificName = unique(x$scientificName),
                                        presenceAbsence = "absent")
      obisInfo <- rbind(obisInfo, intersection_result)
      
    }
    
  } else {
    
    intersection_result <- data.frame(originalName = sp[i],
                                      scientificName = "not found",
                                      presenceAbsence = "not found")
    
    obisInfo <- rbind(obisInfo, intersection_result)
    
  }
    
  print(paste(i, "---- of ----", length(sp)))
    
}
rm(sf_points, obisInfo.1, intersection_result, i, x)

obisInfo$source <- "Original list"
obisInfo <- obisInfo[!grepl("\\.", obisInfo$scientificName), ]

#################
# GENUS DERIVED #
#################

genObis <- data.frame()
for(i in 1:length(gen)){
  
  key <- jsonlite::fromJSON(paste0("https://www.marinespecies.org/rest/AphiaIDByName/", gen[i], "?marine_only=true&offset=", 1))
  # Start with offset = 1
  offset <- 1
  tryCatch({
    
    repeat {
      # Fetch data for the current offset and genus
      # key <- fetch_worms_data(gen[i], offset)
      data <-  wm_children(id = key, marine_only = FALSE, offset = offset)
      
      # Filter for Species with accepted status
      data <- data[data$rank == "Species" & data$status == "accepted", ] %>%
         select(scientificname)
      
      # Add the data to the taxa_list
      genObis <- rbind(genObis, data)
      
      # Increment the offset for the next iteration
      offset <- offset + 50
    }
    
    
    
  }, error = function(e) {
    # Handle the error without any specific action
    # The loop will automatically continue to the next iteration
  })  
  
  print(paste(i, "---- of ----", length(gen)))
  
}
rm(key, offset, data)

# Convert genus column to vector
genObis <- genObis$scientificname

# Download information from OBIS about genus derived taxa
obisGenusInfo <- data.frame()
for(i in 1:length(genObis)){
  
  x <- occurrence(scientificname = genObis[i]) 
  
  if(length(x) != 0){
    
    x <- select(x, scientificName, decimalLatitude, decimalLongitude)
    
    obisGenusInfo.1 <- cbind(genObis[i], x)
    colnames(obisGenusInfo.1)[1] <- "originalName"  
    
    # Convert the dataframe to an sf object
    sf_points <- st_as_sf(obisGenusInfo.1, 
                          coords = c("decimalLongitude", "decimalLatitude"), 
                          crs = 4326)
    # Filter only the points
    intersection_result <- st_intersection(balearicSea, sf_points) %>% 
      as.data.frame() %>% 
      select(originalName, scientificName)
    
    if(nrow(intersection_result) >= 5){
      
      intersection_result$presenceAbsence <- "present"
      obisGenusInfo <- rbind(obisGenusInfo, intersection_result[1,])
      
    } else {
      
      intersection_result <- data.frame(originalName = genObis[i],
                                        scientificName = unique(x$scientificName),
                                        presenceAbsence = "absent")
      obisGenusInfo <- rbind(obisGenusInfo, intersection_result)
      
    }
    
  } else {
    
    intersection_result <- data.frame(originalName = genObis[i],
                                      scientificName = "not found",
                                      presenceAbsence = "not found")
    
    obisGenusInfo <- rbind(obisGenusInfo, intersection_result)
    
  }
  
  print(paste(i, "---- of ----", length(genObis)))
  
}
rm(sf_points, obisGenusInfo.1, intersection_result, i, x)

obisGenusInfo$source <- "Genus derived"
obisGenusInfo <- obisGenusInfo[!grepl("\\.", obisGenusInfo$scientificName), ]

# Merge the two lists
obisMerged <- merge(obisInfo, obisGenusInfo, by = "originalName", all = TRUE)

obisMerged$scientificName.x <- ifelse(obisMerged$scientificName.x == "not found", obisMerged$originalName, obisMerged$scientificName.x)



write.csv(obisMerged, paste0("./", grName, "_obisBalSea_", Sys.Date(),".csv"), row.names = FALSE)











