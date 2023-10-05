################################################
# Title: Count number of sequences             #
# Author: Tommaso Cancellario & Tomas Golomb   #
# Reviewer: Tommaso Cancellario & Tomas Golomb #
# Creation: 2023 - 09 - 06                     #
# Last update: 2023 - 09 - 06                  #
################################################


# Load libraries
pacman::p_load(bold, openxlsx, rentrez, tidyverse)


# Set WD
setwd("~/OneDrive - Universitat de les Illes Balears/CBB objectives/GeneticData/Threatened_species_Balearic/")


# Load dataset 
# Check sheet name
getSheetNames("./data/thTaxa1_2023_09_07.xlsx")

# Load xlsx
thSP <- read.xlsx(xlsxFile = "./data/thTaxa1_2023_09_07.xlsx", sheet = "taxa")
head(thSP)

# Create taxa vector from COL taxonomy
sp <- unique(thSP$speciesAccepted) %>% 
  na.omit()

#-----#
# NCBI -------------------------------------------------------------------------
#-----#

# Set NCBI reference database
dataBase <- "nuccore"

ncbiInfoCOL <- data.frame()
for(i in 1:length(sp)) {
  
  # Search in Nucleotide database.
  a <- entrez_search(db = dataBase, term = paste(sp[i], "[ORGANISM]"), use_history = T)
  
  ncbiInfo.1 <- data.frame(acceptedName = sp[i],
                           nOfSequences = a$count)
  
  ncbiInfoCOL <- rbind(ncbiInfoCOL, ncbiInfo.1)  
  
  print(paste(i, "---- of ----", length(sp)))
  
}
rm(i, a, ncbiInfo.1, dataBase)


#-----#
# BOLD -------------------------------------------------------------------------
#-----#

boldInfoCOL <- data.frame()

for (i in 1:length(sp)) {
  
  # Search stats in BOLD
  a <- bold_stats(sp[i])
  
  boldInfoCOL.1 <- data.frame(acceptedName = sp[i],
                              nOfSequences = a$total_records # Number of total records
  ) 
  
  boldInfoCOL <- rbind(boldInfoCOL, boldInfoCOL.1)
  
  print(paste(i, "---- of ----", length(sp)))
  
}
rm(a, boldInfoCOL.1, i)


# Save .csv
write.csv(ncbiInfoCOL, paste0("./data/NCBI/nOfSeq_NCBI_", Sys.Date(), ".csv"), row.names = FALSE)
write.csv(boldInfoCOL, paste0("./data/BOLD/nOfSeq_BOLD_", Sys.Date(), ".csv"), row.names = FALSE)
