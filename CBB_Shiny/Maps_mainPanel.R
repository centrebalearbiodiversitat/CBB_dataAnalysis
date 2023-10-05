#----------------#
# Maps mainPanel #
#----------------#

fluidRow(h2("Original dataframe"),
         DT::dataTableOutput("inputDataframe.map"),
  br(), h2("Interactive map"),
   leafletOutput("myMap"),
  br(),uiOutput("modifyMap")
  # br(), h2("Map Plot"),
  # br(), h3("GADM map"),
  # plotOutput("gadmMap"),
  # br(), h3("GADM Counts"),
  # DT::dataTableOutput("contentsGadm")
)