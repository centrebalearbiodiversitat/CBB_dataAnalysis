#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Function
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")
source("./function/wormsTaxon.R")

server <- function(input, output, session) {
  
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy.R", local = T)
  
  # Maps -----------------------------------------------------------------------
  source("./Server/02_Maps.R", local = T)
  
  # biomonitoR -----------------------------------------------------------------
  source("./Server/03_RLI.R", local = T)
  
  
  }