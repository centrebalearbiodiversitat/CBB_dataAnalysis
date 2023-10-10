# X is the vector containing the species to search.

cbbdbCol <- function(x){
  
  colNames <- data.frame()
  
  withProgress(message = "Downloading taxonomy", value = 0,
  for(i in 1:length(x)){
    
    # Taxonomy source
    taxonSource <- "Catalogue of Life"
    
    # Taxon origin
    taxonOrigin <- "database"
    
    # Species i of the list
    sp.1 <- x[i]
    
    # Json query
    json.sp <- gsub(" ", "%20", sp.1)
    json <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/nameusage/search?content=SCIENTIFIC_NAME&q=", json.sp, "&type=EXACT&offset=0&limit=50"))
    
    
    # Species not found into COL database
    if(isTRUE(json$empty)){
      
      colNames.1 <- data.frame(originalName = sp.1,
                               colNamesAccepted = "Not found",
                               colID = "Not found",
                               Kingdom = "Not found",
                               kingdomAuthor = "Not found",
                               kingdomSource = taxonSource,
                               kingdomOrigin = taxonOrigin,
                               Phylum = "Not found",
                               phylumAuthor = "Not found",
                               phylumSource = taxonSource,
                               phylumOrigin = taxonOrigin,
                               Class = "Not found",
                               classAuthor = "Not found",
                               classSource = taxonSource,
                               classOrigin = taxonOrigin,
                               Order = "Not found",
                               orderAuthor = "Not found",
                               orderSource = taxonSource,
                               orderOrigin = taxonOrigin,
                               Family = "Not found",
                               familyAuthor = "Not found",
                               familySource = taxonSource,
                               familyOrigin = taxonOrigin,
                               Genus = "Not found",
                               genusAuthor = "Not found",
                               genusSource = taxonSource,
                               genusOrigin = taxonOrigin,
                               Species = "Not found",
                               speciesAuthor = "Not found",
                               speciesSource = taxonSource,
                               speciesOrigin = taxonOrigin,
                               Subspecies = "Not found",
                               subspeciesAuthor = "Not found",
                               subspeciesSource = taxonSource,
                               subspeciesOrigin = taxonOrigin,
                               originalStatus = "Not found",
                               taxonRank = "Not Found",
                               freshwater = "Not Found",
                               marine = "Not Found",
                               terrestrial = "Not Found")
    } else {
      
      # Check name status
      status <- json$result$usage$status
      
      # Named with more taxonomic status
      if(length(status) > 1) {
        
        # acc <- colStatus.1$colStatus == "accepted"
        acc <- grepl("accepted", status)
        
        if (all(unique(acc)) | length(which(acc == "TRUE")) > 1) {
          showNotification(paste("The taxon", sp.1, "has more then one accepted name"), 
                           type = "error",
                           duration = NULL)
          # print("Houston, we have a problem...")
          break
        }
        
        classification <- as.data.frame(json$result$classification[which(json$result$usage$status == status[acc])])
        rank <- classification$rank[nrow(classification)]
        
        classificationID <- classification$id[classification$rank == rank]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)
        
        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = json$result$id,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = taxonSource,
                                 kingdomOrigin = taxonOrigin,
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = taxonSource,
                                 phylumOrigin = taxonOrigin,
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = taxonSource,
                                 classOrigin = taxonOrigin,
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = taxonSource,
                                 orderOrigin = taxonOrigin,
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = taxonSource,
                                 familyOrigin = taxonOrigin,
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = taxonSource,
                                 genusOrigin = taxonOrigin,
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies, -1)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = taxonSource,
                                 speciesOrigin = taxonOrigin,
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = taxonSource,
                                 subspeciesOrigin = taxonOrigin,
                                 originalStatus = json$result$usage[which(json$result$usage$status == status[acc]), ]$status, #ifelse(any(status %in% "accepted"), "accepted", "Many status"),
                                 taxonRank = rank,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
          unique()
        
        # any(): check if there are TRUE values in a string
      }
      
      # Accepted names
      if(length(status) == 1 && status == "accepted"){
        
        classification <- as.data.frame(json$result$classification)
        rank <- classification$rank[nrow(classification)]
        
        # Lower classification ID
        classificationID <- classification$id[classification$rank == rank]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)

        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = json$result$id,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = taxonSource,
                                 kingdomOrigin = taxonOrigin,
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = taxonSource,
                                 phylumOrigin = taxonOrigin,
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = taxonSource,
                                 classOrigin = taxonOrigin,
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = taxonSource,
                                 orderOrigin = taxonOrigin,
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = taxonSource,
                                 familyOrigin = taxonOrigin,
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = taxonSource,
                                 genusOrigin = taxonOrigin,
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = taxonSource,
                                 speciesOrigin = taxonOrigin,
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = taxonSource,
                                 subspeciesOrigin = taxonOrigin,
                                 originalStatus = status,
                                 taxonRank = rank,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
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
        
        # Lower classification ID
        classificationID <- classification$id[classification$rank == rank]
        
        # Classification rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37
        classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID))
        
        taxonLower <- ch0_to_Na(classificationLower$name$scientificName) 
        authorLower <- ch0_to_Na(classificationLower$name$authorship)
        
        # Habitat
        # habitat <- ch0_to_Na(classificationLower$environments)
        
        # Higher classification compared to the rank into the list 
        # Api COL: https://api.checklistbank.org/dataset/9923/taxon/8TN37/classification
        classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/9923/taxon/", classificationID, "/classification"))
        
        # Taxon classification
        taxonHigherKingdom <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "kingdom"])
        authorHigherKingdom <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "kingdom"])
        taxonHigherPhylum <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "phylum"])
        authorHigherPhylum <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "phylum"])
        taxonHigherClass <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "class"])
        authorHigherClass <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "class"])
        taxonHigherOrder <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "order"])
        authorHigherOrder <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "order"])
        taxonHigherFamily <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "family"])
        authorHigherFamily <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "family"])
        taxonHigherGenus <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "genus"])
        authorHigherGenus <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "genus"])
        taxonHigherSpecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "species"])
        authorHigherSpecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "species"])
        taxonHigherSubspecies <- ch0_to_Na(classificationHigher$name[classificationHigher$rank == "subspecies"])
        authorHigherSubspecies <- ch0_to_Na(classificationHigher$authorship[classificationHigher$rank == "subspecies"])
        
        
        # Dataframe to add to the main one
        colNames.1 <- data.frame(originalName = sp.1,
                                 colNamesAccepted = classification$name[classification$rank == rank],
                                 colID = json$result$id,
                                 Kingdom = ifelse(rank == "kingdom", taxonLower, taxonHigherKingdom),
                                 kingdomAuthor = ifelse(rank == "kingdom", authorLower, authorHigherKingdom),
                                 kingdomSource = taxonSource,
                                 kingdomOrigin = taxonOrigin,
                                 Phylum = ifelse(rank == "phylum", taxonLower, taxonHigherPhylum),
                                 phylumAuthor = ifelse(rank == "phylum", authorLower, authorHigherPhylum),
                                 phylumSource = taxonSource,
                                 phylumOrigin = taxonOrigin,
                                 Class = ifelse(rank == "class", taxonLower, taxonHigherClass),
                                 classAuthor = ifelse(rank == "class", authorLower, authorHigherClass),
                                 classSource = taxonSource,
                                 classOrigin = taxonOrigin,
                                 Order = ifelse(rank == "order", taxonLower, taxonHigherOrder),
                                 orderAuthor = ifelse(rank == "order", authorLower, authorHigherOrder),
                                 orderSource = taxonSource,
                                 orderOrigin = taxonOrigin,
                                 Family = ifelse(rank == "family", taxonLower, taxonHigherFamily),
                                 familyAuthor = ifelse(rank == "family", authorLower, authorHigherFamily),
                                 familySource = taxonSource,
                                 familyOrigin = taxonOrigin,
                                 Genus = ifelse(rank == "genus", taxonLower, taxonHigherGenus),
                                 genusAuthor = ifelse(rank == "genus", authorLower, authorHigherGenus),
                                 genusSource = taxonSource,
                                 genusOrigin = taxonOrigin,
                                 Species = ifelse(rank == "species", word(taxonLower, -1), word(taxonHigherSpecies, -1)),
                                 speciesAuthor = ifelse(rank == "species", authorLower, authorHigherSpecies),
                                 speciesSource = taxonSource,
                                 speciesOrigin = taxonOrigin,
                                 Subspecies = ifelse(rank == "subspecies", word(taxonLower, -1), word(taxonHigherSubspecies, -1)),
                                 subspeciesAuthor = ifelse(rank == "subspecies", authorLower, authorHigherSubspecies),
                                 subspeciesSource = taxonSource,
                                 subspeciesOrigin = taxonOrigin,
                                 originalStatus = status,
                                 taxonRank = rank,
                                 freshwater = "freshwater" %in% classificationLower$environments,
                                 marine = "marine" %in% classificationLower$environments,
                                 terrestrial = "terrestrial" %in% classificationLower$environments) %>% 
          unique()
        
      }
      
    }
    
    colNames <- rbind(colNames, colNames.1)
    
    # print(paste(i, "---- of ----", length(x)))
    
    # Increment the progress bar, and update the detail text.
    incProgress(1/length(x), detail = paste("Doing:", i))
    
  }
  ) 
  
  return(colNames)
  
}