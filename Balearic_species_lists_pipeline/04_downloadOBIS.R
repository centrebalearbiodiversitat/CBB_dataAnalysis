#############################################################
# Title: Download occurrences from OBIS.                    #
# Aims: 1) Download data from OBIS                          #
# Author: Jorge Palomo, Tommaso Cancellario.                #
# Reviewer: NA                                              #
# Creation: 2023 - 07 - 25                                  #
# Last update: 2023 - 07 - 25                               #
#############################################################

# Load libraries
pacman::p_load(robis, sf, worrms, rvest)

# Set wd
setwd("~/OneDrive - Universitat de les Illes Balears/")

# Read the shapefile
balearicSea <- st_read("CBB objectives/CBB_Balearic_Sea/CBBBalearicSea_NO_islands.shp")
balearicSea <- st_set_crs(balearicSea, st_crs(4326))
plot(balearicSea[1])


# Load sp list.
sp <- c("Asterina pancerii", "Diplodus cervinus",
        "Carcharodon carcharias", "Dendropoma petraeum", "Tomjorge yves")

gen <- c("Asterina", "Diplodus", "Carcharodon", "Dendropoma")


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
                                        scientificName = NA,
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


#################
# GENUS DERIVED #
#################


genObis <- data.frame()

i=1

# for(i in 1:length(gen)){
# 
#   
#   key <- jsonlite::fromJSON(paste0("https://www.marinespecies.org/rest/AphiaIDByName/", gen[i],"?marine_only=true"))
#   wChildren <- wm_children(id = key, marine_only = FALSE, offset = 1)
#   wChildren <- wChildren[wChildren$rank == "Species" & 
#                            wChildren$status ==  "accepted", ]%>% 
#     select(scientificname) %>% 
#     as.data.frame()
#   
#   genObis <- rbind(genObis, wChildren)
#  
#   print(paste(i, "---- of ----", length(gen)))
#   
# 
#    
# }


# ------------------------------------------------------------------------------

# Function to fetch data for a given offset and genus name
fetch_worms_data <- function(genus, offset) {
  url <- paste0("https://www.marinespecies.org/rest/AphiaIDByName/", genus, "?marine_only=true&offset=", 1)
  data <- jsonlite::fromJSON(url)
  return(data)
}


genus = c("Asterina", "Diplodus")


i=2


taxa_list <- data.frame()


data <- jsonlite::fromJSON(paste0("https://www.marinespecies.org/rest/AphiaIDByName/", genus[i], "?marine_only=true&offset=", 1))
# Start with offset = 1
offset <- 1
tryCatch({
  
  repeat {
    # Fetch data for the current offset and genus
    key <- fetch_worms_data(genus[i], offset)
    data <-  wm_children(id = key, marine_only = FALSE, offset = offset)
    
    # Filter for Species with accepted status
    # data <- data[data$rank == "Species" & data$status == "accepted", ] %>%
    #   select(scientificname)
    
    # Add the data to the taxa_list
    taxa_list <- rbind(taxa_list, data)
    
    # Increment the offset for the next iteration
    offset <- offset + 50
  }
  
}, error = function(e) {
  # Handle the error without any specific action
  # The loop will automatically continue to the next iteration
})



return(taxa_df)



# Example usage for a genus (e.g., "Pseudoceros")
genus <- "Pseudoceros"
result <- download_taxa(genus)



# ------------------------------------------------------------------------------






##### Download genus derived presences
genObis <- genObis$scientificname

genObisInfo <- data.frame()
for(i in 1:length(genObis)){
  
  x <- occurrence(scientificname = genObis[i]) 
  
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
      obisInfo <- rbind(genObisInfo, intersection_result[1,])
      
    } else {
      
      intersection_result <- data.frame(originalName = sp[i],
                                        scientificName = NA,
                                        presenceAbsence = "absent")
      obisInfo <- rbind(genObisInfo, intersection_result)
      
    }
    
  } else {
    
    intersection_result <- data.frame(originalName = sp[i],
                                      scientificName = "not found",
                                      presenceAbsence = "not found")
    
    obisInfo <- rbind(genObisInfo, intersection_result)
    
  }
  
  print(paste(i, "---- of ----", length(genObis)))
  
}

