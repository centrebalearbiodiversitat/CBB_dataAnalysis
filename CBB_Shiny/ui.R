#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Load libraries
pacman::p_load(DT, jsonlite, leaflet, shiny, shinyWidgets, tidyverse, 
               shinycssloaders, rgbif, shinydashboard, sf, red, biomonitoR)

#----------#
# Function ---------------------------------------------------------------------
#----------#
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")
source("./function/wormsTaxon.R")


#----------#
# Start UI ---------------------------------------------------------------------
#----------#

ui <- navbarPage(title = "CBB_ Taxonomy",
                
                 # Tab panel HOME ----------------------------------------------
                 tabPanel("Home",
                          fluidPage(sidebarLayout(
                            # Side panel About ---- 
                            sidebarPanel(source("./Ui/About_sidebarPanel.R")$value),
                            # Main panel About ----
                            mainPanel(source("./Ui/About_mainPanel.R")$value)
                            )
                            )
                          ), 
                 
                 # Tab panel TAXONOMY ------------------------------------------
                 tabPanel("Taxonomy",
                          fluidPage(sidebarLayout(
                            # Side panel Taxonomy ----
                            sidebarPanel(source("./Ui/Taxonomy_sidebarPanel.R")$value),
                            # Main panel Taxonomy ----
                            mainPanel(source("./Ui/Taxonomy_mainPanel.R")$value)
                            )
                            )
                          ), 
                 
                 # Tab panel MAPS ----------------------------------------------
                 tabPanel("Maps",
                          fluidPage(sidebarLayout(
                            ## sidebarLayout Maps ----
                            sidebarPanel(source("./Ui/Maps_sidebarPanel.R")$value),
                            ## mainPanel Maps ----
                            mainPanel(source("./Ui/Maps_mainPanel.R")$value)
                            )
                            )
                          ),
                 
                 # Tab panel RLI -----------------------------------------------
                 tabPanel("biomonitoR",
                          fluidPage(sidebarLayout(
                            ## sidebarLayout Maps ----
                            sidebarPanel(source("./Ui/RLI_sidebarPanel.R")$value),
                            ## mainPanel Maps ----
                            mainPanel(source("./Ui/RLI_mainPanel.R")$value)
                          )
                          )
                 ),
                 
                 
                 # Tab panel GENETICS ------------------------------------------
                 # tabPanel("Maps",
                 #          fluidPage(sidebarLayout(
                 #            ## sidebarLayout Maps ----
                 #            sidebarPanel(source("Maps_sidebarPannel.R")$value),
                 #            ## mainPanel Maps ----
                 #            mainPanel(source("Maps_mainPanel.R")$value)
                 #          )
                 #          )),
                 # 
                 
                 
                 # Tab panel HELP ----------------------------------------------
                 navbarMenu("Help",
                            tabPanel("Taxonomy"))
)
