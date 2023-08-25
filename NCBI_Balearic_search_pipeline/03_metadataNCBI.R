################################################
# Title: Create .csv from NCBI metadata        #
# Aim: Extract metadata from NCBI              #
# Author: Tommaso Cancellario  & Claudio Padua #
# Reviewer: Laura Triginer                     #
# Creation: 2023 - 06 - 15                     #
# Last update: 2023 - 06 - 15                  #
################################################

# Whit this script you can create a .csv with sequence metadata. 
# You can extract the metadata from the GenBank(full) file downloaded from NCBI.

# More info about rentrez package:
# https://cran.r-project.org/web/packages/rentrez/vignettes/rentrez_tutorial.html

# Load libraries
pacman::p_load(data.table, dplyr, rentrez, seqinr, stringr)

# Set WD
setwd("~/Desktop/NCBI_Download/")

# File list of the .txt without sequences
fileList <- list.files("./", full.names = TRUE, pattern = "removed.txt")
fileName <- list.files("./", pattern = "removed.txt")
fileName <- sub("_removed.txt", "", fileName)


for (i in 1:length(fileList)) {
  
  # Load the downloaded .txt file containing the metadata.
  # Add a empty line at the end of the file. Somtime we have to use this trick.
  write("", file = fileList[i], append = TRUE) 
  lines <- readLines(fileList[i])
  
  # If you want to load only part of your file you can use this line. n is the
  # number of rows to upload.
  # lines <- readLines("./BalearicSequences_2023-06-09_removed.txt", n = 1600)
  
  # Convert to tibble df.
  df <- tibble(text = lines)
  
  # Identify empty rows.
  empty_rows <- which(df$text == "")
  
  # Create chunk for each NCBI record.
  recs.ls <- lapply(seq_along(empty_rows), function(i) {
    if (i == 1) {
      chunk_rows <- 1:(empty_rows[i] - 1)
    } else {
      chunk_rows <- (empty_rows[i - 1] + 1):(empty_rows[i] - 1)
    }
    return(list(text = paste(df$text[chunk_rows], collapse = "\n")))
  })
  
  # Df containing the final information
  ncbiInfo <- data.frame()
  
  # Temporary df to store metadata information
  ncbi.2 <- as.data.frame(matrix(NA, ncol=14))
  colnames(ncbi.2) <- c("sampleid", "species_name","country", "isolate",
                        "lat", "lon", "markercode", "nucleotides_bp",
                        "definition", "voucher", "pubmed", "collection_date",
                        "INV", "authors")
  
  for(j in 1:length(recs.ls)){
    
    print(paste("----", j, "of", length(recs.ls), "----"))
    gbank <- recs.ls[[j]]
    
    # ACCESSION NUMBER
    ncbi.2$sampleid <- word(gsub("\"", "", gsub("^.*ACCESSION\\s*|\\s*\n.*$", "", gbank)), 1)
    
    # TAXONOMY
    taxonomy <- unlist(strsplit(taxonomy <- gsub("\"", "", gsub("^.*ORGANISM\\s*|\\s*.\nREFERENCE.*$", "", gbank)), "\n"))
    ncbi.2$species_name <- taxonomy[1]
    
    # COUNTRY
    # ncbi.2$country <- as.character(ifelse(grepl("country", gbank) == T,
    #                                       gsub("\"", "", gsub("^.*country=\\s*|\\s*\n.*$", "", gbank)), NA))
    
    ncbi.2$country <- as.character(ifelse(grepl("country", gbank),
                                          gsub('\\s+', ' ', gsub('\n', '', gsub('country="|"$', '', regmatches(gbank, regexpr('country="([^"]+)"', gbank))))),
                                          NA))
    
    # ISOLATE
    ncbi.2$isolate <- as.character(ifelse(grepl("isolate", gbank),
                                          gsub('\\s+', ' ', gsub('\n', '', gsub('isolate="|"$', '', regmatches(gbank, regexpr('isolate="([^"]+)"', gbank))))),
                                          NA))
    # LONGITUDE & LATITUDE
    if(isTRUE(grepl("lat_lon", gbank))) {
      
      lat_lon <- gsub("\"","",gsub("^.*lat_lon=\\s*|\\s*\n.*$", "", gbank))
      lon <- as.numeric(word(lat_lon,-2))
      ncbi.2$lon <- ifelse(word(lat_lon,-1) == "W", -abs(lon), lon)
      lat <- as.numeric(gsub("\"", "", as.character(word(lat_lon,1))))
      ncbi.2$lat <- ifelse(word(lat_lon,2) == "S", -abs(lat), lat)
      
    } else {
      
      ncbi.2$lon <- NA
      ncbi.2$lat <- NA
      
    }
    
    # MARKER CODE
    # ncbi.2$markercode <- ifelse(grepl("product=", gbank) == T,
    #                             gsub("\"", "", gsub("^.*product=\\s*|\\s*\n.*$", "", gbank)), NA)
    
    ncbi.2$markercode <- ifelse(grepl("product=", gbank), 
                                paste(unlist(regmatches(gbank, gregexpr('(?<=/product=").*?(?=")', gbank, perl = TRUE))), collapse = ";"), 
                                NA)
    # SEQUENCE
    # seq <- as.character(ifelse(grepl("ORIGIN", gbank) == T,
    #                            gsub("\n", "", gsub("^.*ORIGIN\\s*|\\//.*$", "", gbank)), NA))
    # # ncbi.2$nucleotides <- gsub(" ", "", gsub("[[:digit:]]+", "", seq))
    # nucleotides <- gsub(" ", "", gsub("[[:digit:]]+", "", seq))
    # ncbi.2$nucleotides_bp <- nchar(nucleotides)
    #  
    # nucleotideFasta.1 <- data.frame(seqName =  ncbi.2$sampleid,
    #                                 seqTaxa = taxonomy[1],
    #                                 seqBP = nchar(nucleotides),
    #                                 nucleotides = nucleotides)
    
    ncbi.2$nucleotides_bp <- sub(".*\\s(\\d+)\\s+bp.*", "\\1", gbank)
    
    # VOUCHER
    ncbi.2$voucher <- ifelse(grepl("specimen_voucher=", gbank), 
                             as.character(gsub("\"", "", gsub("^.*specimen_voucher=\\s*|\\s*\n.*$", "", gbank))), NA)
    
    # DEFINITION
    def <- str_replace_all(gsub("^.*DEFINITION\\s*|\\s*.\nACCESSION.*$", "", gbank), "[\r\n]" , "")
    ncbi.2$definition <- gsub("\\s+", " ", str_trim(def))
    
    # PUBMED
    ncbi.2$pubmed <- ifelse(grepl("PUBMED", gbank), 
                            as.character(gsub("\"", "", gsub("^.*PUBMED\\s*|\\s*\n.*$", "", gbank))), NA)
    
    # Collection date
    ncbi.2$collection_date <- ifelse(grepl("collection_date", gbank), 
                                     as.character(gsub("\"", "", gsub("^.*collection_date=\\s*|\\s*\n.*$", "", gbank))), NA)
    
    # INV
    ncbi.2$INV <- ifelse(grepl("INV", gbank), 
                         as.character(gsub("\"", "", gsub("^.*INV\\s*|\\s*\n.*$", "", gbank))), NA)
    
    # AUTHORS
    # ncbi.2$authors <- ifelse(grepl("AUTHORS", gbank) == T, 
    #                          as.character(gsub("\"", "", gsub("^.*AUTHORS\\s*|\\s*\n.*$", "", gbank))), NA)
    
    if(grepl("AUTHORS", gbank)){
      authorExtract <- paste(gsub("\\s+", " ", gsub("\\n", "", str_extract_all(gbank, "(?<=AUTHORS\\s{3})[^\n]+\\n[^\n]+")[[1]])), collapse = ";")
      authorExtract <- ifelse(grepl("TITLE", authorExtract), 
                              paste(gsub("\\s+", " ", gsub("\\s+TITLE.*", "", str_extract_all(gbank, "(?<=AUTHORS\\s{3})[^\n]+\\n[^\n]+")[[1]])), collapse =";"),
                              authorExtract)
      
      
      
      ncbi.2$authors <- authorExtract
      
    } else {
      ncbi.2$authors <- NA
    }
    
    # Create total dataset   
    ncbiInfo <- rbind(ncbiInfo, ncbi.2)
    #nucleotideFasta <- rbind(nucleotideFasta, nucleotideFasta.1)
    
  }
  rm(j, def, authorExtract, taxonomy, ncbi.2, df, gbank)
  
  # Save .csv
  write.csv(ncbiInfo, paste0(fileName[i], "_",Sys.Date(), ".csv"), row.names = FALSE)
  
  
}







