#############################################################
# Title: NCBI sequences update                              #
# Aims: Update list of sequences                            #
# Author: Tommaso Cancellario                               #
# Reviewer: NA                                              #
# Creation: 2023 - 07 - 28                                  #
# Last update: 2023 - 07 - 28                               #
#############################################################


# Load libraries
pacman::p_load(data.table, tidyverse, openxlsx, rentrez)

# Set wd
setwd("~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/")

# Path where are stored the csv with sequence metadata
csvPathOld <- "./data/originalNamesCSV"

# Path to save the new sequences .csv lists
csvPathNew <- paste0("./data/originalNamesCSV/toAdd_", Sys.Date())
dir.create(csvPathNew)

# Load xlsx with balearic sequences
# Check sheet name
# getSheetNames("./data/thTaxa_2023_07_24.xlsx")

# Load xlsx
thSP <- read.xlsx(xlsxFile = "./data/thTaxa_2023_07_24.xlsx", sheet = "taxonomicCheck")
head(thSP)

# Create taxa vector
sp <- unique(thSP$acceptedName)

# Load .csv path list and file name
spOld.listFiles <- list.files(csvPathOld, full.names = TRUE, pattern = ".csv$") 
spOld.nameFiles <- list.files(csvPathOld, pattern = ".csv$") 

# Set NCBI reference database
dataBase <- "nuccore"


# Create .csv file with lacking new sequences
length(spOld.listFiles)
for(i in 1:50){
  
  print(paste(sp[i], "-", i, "---- of ----", length(spOld.listFiles)))
  
  a <- entrez_search(db = dataBase, term = paste(sp[i], "[ORGANISM]"), use_history = T)
  
  sampleidNCBI <- data.frame()
  
  if(a$count != 0 & a$count != 1) {
    
    # When there are more than 500 ids, we need to create chunk to download the 
    # information.
    for(seq_start in seq(from = 0, to = a$count, 101)){
      cv <- entrez_summary(db=dataBase, web_history = a$web_history,
                           retmax = 101, retstart = seq_start)
      
      for(j in 1:length(cv)){
        sampleidNCBI <- rbind(sampleidNCBI, cv[[j]]$caption)
      }
      colnames(sampleidNCBI) <- "sampleid"
      
      print(paste(round((seq_start/a$count)*100, digits = 2), "%")) # %
      
    }
    
    targetSpecies <- grep(sp[i], spOld.listFiles)
    
    spOld <- read.csv(spOld.listFiles[targetSpecies]) %>% 
      select(sampleid, species_name)
    
    # Check the sequenceid difference between the old dataset and the new one.
    difference.df <- anti_join(sampleidNCBI, spOld, by = "sampleid")
    
    if(nrow(difference.df) != 0){
      
      difference.df$species_name <- sp[i]
      # Remove the date from the file name using regular expressions 
      # the gsub function is used with the regular expression "_\d{4}-\d{2}-\d{2}" 
      # to match the date pattern "yyyy-mm-dd" (e.g., "_2023-07-28") in the file_name variable. 
      # he \\d{4} matches a four-digit year, \\d{2} matches a two-digit month, and \\d{2} 
      # matches a two-digit day.
      fileName <- gsub("_\\d{4}-\\d{2}-\\d{2}", "", spOld.nameFiles[targetSpecies])
      fileName <- gsub("\\.csv$", "", fileName)
      
      write.csv(difference.df, paste0(csvPathNew, "/", fileName, "_updatadTo_", Sys.Date(), ".csv"), row.names = FALSE)
    }
    
  }
  
  # If we have oly one sequences cv can not subset as cv[[j]]., so we change a 
  # bit the approach.
  if(a$count != 0 & a$count == 1) {
    
    cv <- entrez_summary(db=dataBase, web_history = a$web_history)
    
    sampleidNCBI <- rbind(sampleidNCBI, cv$caption)
    colnames(sampleidNCBI) <- "sampleid"
    
    targetSpecies <- grep(sp[i], spOld.listFiles)
    
    spOld <- read.csv(spOld.listFiles[targetSpecies]) %>% 
      select(sampleid, species_name)
    
    # Check the sequenceid difference between the old dataset and the new one.
    difference.df <- anti_join(sampleidNCBI, spOld, by = "sampleid")
    
    if(nrow(difference.df) != 0){
      
      difference.df$species_name <- sp[i]
      # Remove the date from the file name using regular expressions 
      # the gsub function is used with the regular expression "_\d{4}-\d{2}-\d{2}" 
      # to match the date pattern "yyyy-mm-dd" (e.g., "_2023-07-28") in the file_name variable. 
      # he \\d{4} matches a four-digit year, \\d{2} matches a two-digit month, and \\d{2} 
      # matches a two-digit day.
      fileName <- gsub("_\\d{4}-\\d{2}-\\d{2}", "", spOld.nameFiles[targetSpecies])
      fileName <- gsub("\\.csv$", "", fileName)
      
      write.csv(difference.df, paste0(csvPathNew, "/", fileName, "_updatadTo_", Sys.Date(), ".csv"), row.names = FALSE)
    }
    
  }
  
}

