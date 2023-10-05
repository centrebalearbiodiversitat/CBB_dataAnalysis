################################################
# Title: Taxonomy check COL, NCBI and BOLD     #
# Author: Tommaso Cancellario & Tomas Golomb   #
# Reviewer: Tommaso Cancellario & Tomas Golomb #
# Creation: 2023 - 09 - 06                     #
# Last update: 2023 - 09 - 06                  #
################################################

# Load libraries
pacman::p_load(bold, jsonlite, openxlsx, rentrez, tidyverse, taxize)

# Load dataset 
# Check sheet name
# getSheetNames("~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/data/thTaxa1_2023_09_05.xlsx")

# Load .xlsx
taxa <- read.xlsx(xlsxFile = "~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/data/thTaxa1_2023_09_05.xlsx",
                  sheet = "taxa")
head(taxa)

sp <- unique(taxa$speciesName)


#----#
# COL --------------------------------------------------------------------------
#----#

colNames <- data.frame()

for(i in 1:length(sp)){
  
  sp.1 <- sp[i]
  
  # Json query
  json.sp <- gsub(" ", "%20", sp.1)
  json <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", json.sp, "&type=EXACT&offset=0&limit=1"))
  
  
  if(isFALSE(json$empty)){
    
    # Check name status
    status <- json$result$usage$status
    
    
    if(status == "accepted"){
      
      classification <- as.data.frame(json$result$classification)
      rank <- classification$rank[nrow(classification)]
      
    } else {
      
      id.sp <- json$result$usage$id
      
      # Accepted name from synonym ID
      json.syn <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/synonym/", id.sp))
      json.syn.acc <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", gsub(" ", "%20", json.syn$accepted$name$scientificName), "&type=EXACT&offset=0&limit=1"))
      
      classification <- as.data.frame(json.syn.acc$result$classification)
      rank <- classification$rank[nrow(classification)]
      
      
    }
    
    colNames.1 <- data.frame(originalName = sp.1,
                             colNamesAccepted = classification$name[classification$rank == rank],
                             kingdomAccepted = ifelse(length(classification$name[classification$rank == "kingdom"]) == 0, NA, classification$name[classification$rank == "kingdom"]),
                             phylumAccepted = ifelse(length(classification$name[classification$rank == "phylum"]) == 0, NA, classification$name[classification$rank == "phylum"]),
                             classAccepted = ifelse(length(classification$name[classification$rank == "class"]) == 0, NA, classification$name[classification$rank == "class"]),
                             orderAccepted = ifelse(length(classification$name[classification$rank == "order"]) == 0, NA, classification$name[classification$rank == "order"]),
                             familyAccepted = ifelse(length(classification$name[classification$rank == "family"]) == 0, NA, classification$name[classification$rank == "family"]),
                             genusAccepted = ifelse(length(classification$name[classification$rank == "genus"]) == 0, NA, classification$name[classification$rank == "genus"]),
                             speciesAccepted = ifelse(length(classification$name[classification$rank == "species"]) == 0, NA, classification$name[classification$rank == "species"]),
                             subspeciesAccepted = ifelse(length(classification$name[classification$rank == "subspecies"]) == 0, NA, classification$name[classification$rank == "subspecies"]),
                             originalStatus = json$result$usage$status) %>% 
      unique()
    
          } else {
    
    colNames.1 <- data.frame(originalName = sp.1,
                             colNamesAccepted = NA,
                             kingdomAccepted = NA,
                             phylumAccepted = NA,
                             classAccepted = NA,
                             orderAccepted = NA,
                             familyAccepted = NA,
                             genusAccepted = NA,
                             speciesAccepted = NA,
                             subspeciesAccepted = NA,
                             originalStatus = NA)  
    
  }
  
  colNames <- rbind(colNames, colNames.1)
 
  print(paste(i, "---- of ----", length(sp)))
   
}
rm(i, colNames.1, id.sp, json, json.syn, json.sp, status, rank, classification, 
   json.syn.acc)


#-----#
# NCBI -------------------------------------------------------------------------
#-----#

ncbiNames <- data.frame()

for(i in 410:length(sp)){
  
  ncbiRank <- unlist(tax_rank(sp[i], db = "ncbi"))
  
  if(!is.na(as.vector(ncbiRank[1]))){
    
    ncbiNamesClass <- classification(sp[i], db = "ncbi")
    
    ncbiNames.1 <- data.frame(originalName = sp[i],
                              ncbiNamesClass[[1]]$name[ncbiNamesClass[[1]]$rank == ncbiRank]) 
    colnames(ncbiNames.1)[2] <- "ncbiAcceptedName"
    
    ncbiNames <- rbind(ncbiNames, ncbiNames.1)  
    
  } else {
    
    ncbiNames.1 <- data.frame(originalName = sp[i],
                              ncbiAcceptedName = NA)
    ncbiNames <- rbind(ncbiNames, ncbiNames.1)  
    
  }
    
  print(paste(i, "---- of ----", length(sp)))
  
}
rm(i, ncbiNames.1, ncbiNamesClass, ncbiRank)


#-----#
# BOLD -------------------------------------------------------------------------
#-----#

boldName <- data.frame()

for(i in 1:length(sp)){
  
  boldName.1 <- bold_tax_name(name = sp[[i]])
  
  if(length(colnames(boldName.1)) != 1){
    
    boldName.1 <- boldName.1 %>% 
      select(input, taxon) %>% 
      rename(originalName = input, boldAcceptedName = taxon)
    
  } else {
    
    boldName.1 <- data.frame(originalName = boldName.1$input,
                             boldAcceptedName = NA)
    
  }  
  
  boldName <- rbind(boldName, boldName.1)
  
  print(paste(i, "---- of ----", length(sp)))
  
  
}
rm(i, boldName.1)



# Save bold taxonomy 
# Save .xlsx
wb <- loadWorkbook("~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/data/thTaxa1_2023_09_07.xlsx")

addWorksheet(wb, "colTaxonomy_2023_09_07") # add Worksheet
writeData(wb, "colTaxonomy_2023_09_07", colNames) # Add data in the Worksheet

addWorksheet(wb, "ncbiTaxonomy_2023_09_07") # add Worksheet
writeData(wb, "ncbiTaxonomy_2023_09_07", ncbiNames) # Add data in the Worksheet

addWorksheet(wb, "boldTaxonomy_2023_09_07") # add Worksheet
writeData(wb, "boldTaxonomy_2023_09_07", boldName) # Add data in the Worksheet

saveWorkbook(wb, file = "~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/data/thTaxa1_2023_09_07.xlsx", overwrite = TRUE)


