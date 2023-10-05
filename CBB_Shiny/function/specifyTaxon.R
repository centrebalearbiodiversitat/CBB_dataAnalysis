# X: vector of taxa


specifyTaxon <- function(x){
  
  colList <- list(colNames = data.frame(),
                  colStatus = data.frame()
  )
  
  for(i in 1:length(x)){
    
    sp.1 <- x[i]
    
    # Json query
    json.sp <- gsub(" ", "%20", sp.1)
    json <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", json.sp, "&type=EXACT&offset=0&limit=10"))
    
    if(isTRUE(json$empty)){
      
      colStatus.1 <- data.frame(originalName = sp.1,
                                colStatus = "Not found")
      
      # If the taxon is not present in COL
      colNames.1 <- data.frame(originalName = sp.1,
                               colNamesAccepted = "Not found",
                               Life = "Life",
                               Kingdom = "Not found",
                               Phylum = "Not found",
                               Parvphylum = "Not found",
                               Gigaclass = "Not found",
                               Class = "Not found",
                               Order = "Not found",
                               Family = "Not found",
                               Genus = "Not found",
                               Species = "Not found",
                               Subspecies = "Not found",
                               originalStatus = "Not found") 
    } else {
      
      # Check name status
      status <- json$result$usage$status
      
      colStatus.1 <- data.frame(originalName = sp.1,
                                colStatus = status)
      
      # Named with more taxonomic status
      if(length(status) > 1) {
        
        classification <- as.data.frame(json$result$classification[which(json$result$usage$status == "accepted")])
        rank <- classification$rank[nrow(classification)]
        
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 Life = "Life",
                                 Kingdom = ch0_to_Na(classification$name[classification$rank == "kingdom"]),
                                 Phylum = ch0_to_Na(classification$name[classification$rank == "phylum"]),
                                 Parvphylum = ch0_to_Na(classification$name[classification$rank == "parvphylum"]),
                                 Gigaclass = ch0_to_Na(classification$name[classification$rank == "gigaclass"]),
                                 Class = ch0_to_Na(classification$name[classification$rank == "class"]),
                                 Order = ch0_to_Na(classification$name[classification$rank == "order"]),
                                 Family = ch0_to_Na(classification$name[classification$rank == "family"]),
                                 Genus = ch0_to_Na(classification$name[classification$rank == "genus"]),
                                 Species = ch0_to_Na(word(classification$name[classification$rank == "species"], -1)),
                                 Subspecies = ch0_to_Na(word(classification$name[classification$rank == "subspecies"], -1)),
                                 originalStatus = json$result$usage[which(json$result$usage$status == "accepted"), ]$status) %>%
          unique()
        
      } 
      
      # Accepted names
      if(length(status) == 1 && status == "accepted"){
        
        classification <- as.data.frame(json$result$classification)
        rank <- classification$rank[nrow(classification)]
        
        
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 Life = "Life",
                                 Kingdom = ch0_to_Na(classification$name[classification$rank == "kingdom"]),
                                 Phylum = ch0_to_Na(classification$name[classification$rank == "phylum"]),
                                 Parvphylum = ch0_to_Na(classification$name[classification$rank == "parvphylum"]),
                                 Gigaclass = ch0_to_Na(classification$name[classification$rank == "gigaclass"]),
                                 Class = ch0_to_Na(classification$name[classification$rank == "class"]),
                                 Order = ch0_to_Na(classification$name[classification$rank == "order"]),
                                 Family = ch0_to_Na(classification$name[classification$rank == "family"]),
                                 Genus = ch0_to_Na(classification$name[classification$rank == "genus"]),
                                 Species = ch0_to_Na(word(classification$name[classification$rank == "species"], -1)),
                                 Subspecies = ch0_to_Na(word(classification$name[classification$rank == "subspecies"], -1)),
                                 originalStatus = json$result$usage$status) %>%
          unique()
        
      }
      
      # Synonyms and more
      if(length(status) == 1 && status != "accepted"){
        
        # ID of synonym
        id.sp <- json$result$usage$id
        
        # Accepted name from synonym ID
        json.syn <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/synonym/", id.sp))
        json.syn.acc <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", gsub(" ", "%20", json.syn$accepted$name$scientificName), "&type=EXACT&offset=0&limit=1"))
        
        classification <- as.data.frame(json.syn.acc$result$classification)
        rank <- classification$rank[nrow(classification)]
        
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 Life = "Life",
                                 Kingdom = ch0_to_Na(classification$name[classification$rank == "kingdom"]),
                                 Phylum = ch0_to_Na(classification$name[classification$rank == "phylum"]),
                                 Parvphylum = ch0_to_Na(classification$name[classification$rank == "parvphylum"]),
                                 Gigaclass = ch0_to_Na(classification$name[classification$rank == "gigaclass"]),
                                 Class = ch0_to_Na(classification$name[classification$rank == "class"]),
                                 Order = ch0_to_Na(classification$name[classification$rank == "order"]),
                                 Family = ch0_to_Na(classification$name[classification$rank == "family"]),
                                 Genus = ch0_to_Na(classification$name[classification$rank == "genus"]),
                                 Species = ch0_to_Na(word(classification$name[classification$rank == "species"], -1)),
                                 Subspecies = ch0_to_Na(word(classification$name[classification$rank == "subspecies"], -1)),
                                 originalStatus = json$result$usage$status) %>%
          unique()
        
      }
      
    }
    
    colList$colNames <- rbind(colList$colNames, colNames.1)
    colList$colStatus <- rbind(colList$colStatus, colStatus.1)
    
    print(paste(i, "---- of ----", length(x)))
    
  }
  
  return(colList)
  
}