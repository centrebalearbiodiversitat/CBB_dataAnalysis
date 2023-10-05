#--------------------------#
# Shiny App Rotifers v 0.1 #
#--------------------------#

# Reference Database
# refDb <- read.csv("Rotifer_DB_trim.csv", sep = ",")
# refbiomonitoR <- read.csv("Rotifer_DB_biomonitoR.csv", sep = ",")

# Function
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")

server <- function(input, output, session) {
  
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy.R", local = T)
  
  }