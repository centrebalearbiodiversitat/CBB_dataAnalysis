#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Function
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")

server <- function(input, output, session) {
  
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy.R", local = T)
  
  }