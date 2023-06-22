#######################################
# Title: Balearic sequence extraction #
# Author: Tommaso Cancellario         #
# Reviewer: Laura Triginer            #
# Creation: 2023 - 05 - 10            #
# Last update: 2023 - 05 - 10         #
#######################################

# This script allow us to extract genetic information obtained from species 
# sequenced in the Balearic Islands.

# Libraries
library(data.table)
library(dplyr)
library(openxlsx)
library(taxize)

#--------------------------------------------------#
# The firs part is useful if we have one .csv file #
#--------------------------------------------------#

# set working directory
setwd("/Users/tcanc/Library/CloudStorage/OneDrive-UniversitatdelesIllesBalears/CBB objectives/GeneticData")

# Load .csv with all the metadata (this .csv is the result obtained from the script 01_metadataNCBI.R)
seq <- fread("./balearicSequences_2023_06_19.csv")

# Create a new column merging the information of country and isolate
seq$countryMerged <- ifelse(is.na(seq$country), seq$isolate, seq$country) 
head(seq)

# We create a filte-pattern (this is the same string that we used for searching in NCBI)
myPattern <- paste0("Balearic", "|", 
                    "Balears", "|", 
                    "Baleares", "|", 
                    "Minorca", "|", 
                    "Mallorca", "|",
                    "Majorca", "|",
                    "Maiorca", "|",
                    "Mayorca", "|",
                    "Menorca", "|",
                    "Cabrera", "|",
                    "Dragonera", "|",
                    "Ibiza", "|",
                    "Eivissa", "|",
                    "Formentera")

# Filter the data set
balearicSeq <- seq[grep(myPattern, seq$countryMerged), ]
head(balearicSeq)


# Save xlsx
header_style <- createStyle(halign = "center", textDecoration = "bold")

wb <- createWorkbook()
addWorksheet(wb, "balearicNcbi_2023_06_19") # add Worksheet
writeData(wb, "balearicNcbi_2023_06_19", balearicSeq) # Add data in the Worksheet

saveWorkbook(wb, file = "./balearicSequences_2023_06_20.xlsx", overwrite = TRUE)

#write.csv(country, "./countries_2023-06-19.csv", row.names = FALSE)
# ---------------------------------------------------------------------------- #

# Check sheet name
getSheetNames("./balearicSequences_2023_06_20.xlsx")

# Load xlsx
balearicSeq <- read.xlsx("./balearicSequences_2023_06_20.xlsx", sheet = "balearicNcbi_2023_06_19")
head(balearicSeq)

# Create a unique list of species
sp <- unique(balearicSeq$species_name)


taxonomyNCBI <- data.frame()


length(sp)
for(i in 1:40){
  
  # Search name in NCBI database
  x <- classification(sp[i], db = "ncbi")
  x <- as.data.frame(x[1])  
  
  if(!is.na(x[1,1])){
    
    colnames(x) <- c("name", "rank", "id") 
  # Create temporaty data frame
  taxonomyNcbi.1 <- data.frame(kingdom = ifelse("kingdom" %in% x$rank, x$name[x$rank == "kingdom"], NA),
                               phylum = ifelse("phylum" %in% x$rank, x$name[x$rank == "phylum"], NA),
                               class = ifelse("class" %in% x$rank, x$name[x$rank == "class"], NA),
                               order = ifelse("order" %in% x$rank, x$name[x$rank == "order"], NA),
                               family = ifelse("family" %in% x$rank, x$name[x$rank == "family"], NA),
                               genus = ifelse("genus" %in% x$rank, x$name[x$rank == "genus"], NA),
                               species = ifelse("species" %in% x$rank, x$name[x$rank == "species"], NA), 
                               originalName = sp[i]
                               )
  
  # Join temporary df with the main one
  taxonomyNCBI <- rbind(taxonomyNCBI, taxonomyNcbi.1)
    
  } else {
    
    taxonomyNcbi.1 <- data.frame(kingdom = NA,
                                 phylum = NA,
                                 class = NA,
                                 order = NA,
                                 family = NA,
                                 genus = NA,
                                 species = NA, 
                                 originalName = sp[i]
                                 )
    
    # Join temporary df with the main one
    taxonomyNCBI <- rbind(taxonomyNCBI, taxonomyNcbi.1)
    
    
  }
  
  print(paste(i , "---- of ----", length(sp)))
  
}
rm(i, x, taxonomyNcbi.1)
  
  





# ---------------------------------------------------------------------------- #





# Set WD
setwd("~/OneDrive - Universitat de les Illes Balears/Biodiversidad Baleares/threatened_Balearic_species/")

# Load csv
ls <- list.files("./results/originalNamesCSV", full.names = TRUE)
numbers <-  as.numeric(regmatches(ls, regexpr("[0-9]+", ls)))
ls <- ls[order(numbers)]
ls[1:11]
rm(numbers)

# String 
my_pattern <- paste0("Balearic", "|", 
                     "Balears", "|", 
                     "Baleares", "|", 
                     "Minorca", "|", 
                     "Mallorca", "|",
                     "Majorca", "|",
                     "Maiorca", "|",
                     "Mayorca", "|",
                     "Menorca", "|",
                     "Cabrera", "|",
                     "Dragonera", "|",
                     "Ibiza", "|",
                     "Eivissa", "|",
                     "Formentera")

df <-  data.frame()
# i=143
for(i in 1:length(ls)){
  
  sp <- read.csv(ls[i])
  
  sp.1 <- sp[grep(my_pattern, sp$country), ]
  df.1 <- data.frame(taxa = gsub("[0-9_-]+", "", gsub("^\\d+_|\\.csv$", "", basename(ls[i]))),
                     balearicSequenced = ifelse(nrow(sp.1) != 0, "YES", "NO"))
  
  df <- rbind(df, df.1)
  
  print(paste(i, "of", length(ls)))
}

write.csv2(df, "./balearic sequenced.csv")

sort(unique(sp$country))
