#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Load libraries
pacman::p_load(DT, jsonlite, shiny, shinyWidgets, tidyverse, shinycssloaders)

#----------#
# Function ---------------------------------------------------------------------
#----------#
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")


#----------#
# Start UI ---------------------------------------------------------------------
#----------#

ui <- navbarPage("CBB_ Taxonomy",
                 
                 # Tab panel HOME ----------------------------------------------
                 tabPanel("Home",
                          fluidPage(sidebarLayout(
                            # Side panel About ---- 
                            sidebarPanel(source("./Ui/About_sidebarPanel.R")$value),
                            # Main panel About ----
                            mainPanel(source("./Ui/About_mainPanel.R")$value)
                          ))), 
                 
                 # Tab panel TAXONOMY ------------------------------------------
                 tabPanel("Taxonomy",
                          fluidPage(sidebarLayout(
                            # Side panel Taxonomy ----
                            sidebarPanel(source("./Ui/Taxonomy_sidebarPanel.R")$value),
                            # Main panel Taxonomy ----
                            mainPanel(source("./Ui/Taxonomy_mainPanel.R")$value)
                          ))), 
                 
                 
                 # Tab panel MAPS ----------------------------------------------
                 # tabPanel("Maps",
                 #          fluidPage(sidebarLayout(
                 #            ## sidebarLayout Maps ----                           
                 #            sidebarPanel(source("Maps_sidebarPannel.R")$value),
                 #            ## mainPanel Maps ----    
                 #            mainPanel(source("Maps_mainPanel.R")$value)
                 #          )
                 #          )),
                 # 
                 # # Tab panel STATS -------------------------------------------------------------
                 # tabPanel("Stats",
                 #          fluidPage(sidebarLayout(
                 #            ## sidebarLayout Stats ---- 
                 #            sidebarPanel(source("Stats_sidebarPanel.R")$value),
                 #            ## mainPanel Maps ----                     
                 #            mainPanel(source("Stats_mainPanel.R")$value) 
                 #          )
                 #          )),
                 
                 # Tab panel HELP ----------------------------------------------
                 navbarMenu("Help",
                            tabPanel("Taxonomy"))
)
