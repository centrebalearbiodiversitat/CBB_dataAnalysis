#--------------------#
# Stats sidebarPanel #
#--------------------#

fluidRow(
  
  # Input: Select a .csv file
  div(style="display: inline-block;vertical-align:top; width: 400px;",   
      fileInput("file2", "Chose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values, text/plain",
                           ".csv"))),
  h3(".csv options"),
  
  # Input: Checkbox if file has header
  div(style="display: inline-block;vertical-align:top; width: 100px;",   
      checkboxInput("header", "Header", TRUE)),
  
  # Input: Select separator
  div(style="display: inline-block;vertical-align:top; width: 200px;",   
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ",")),
  
  # Input: Select quotes
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"')),
  
  # Text input
  div(style="display: inline-block;vertical-align:top; width: 200px;",   
      selectInput("text.stat", "Name of Taxa column", choices = NULL)),
  #textInput("text.stat", h5("Name of taxa column"), 
  #          value = "Taxa")),
  
  br(),
  
  div(style="display: inline-block;vertical-align:top; width: 200px;",   
      selectInput("lon.stat", "Name of LONGITUDE column", choices = NULL)),
  # textInput("lon.stat", h5("Name of longitude column"), 
  #           value = "Lon")),
  
  div(style="display: inline-block;vertical-align:top; width: 200px;",  
      selectInput("lat.stat", "Name of LATITUDE column", choices = NULL)),
  # textInput("lat.stat", h5("Name of latitude column"), 
  #           value = "Lat")),
  
  br(),
  div(style="display: inline-block;vertical-align:top; width: 200px;",   
      selectInput("bioR.aggregation", h5("biomonitoR aggregation"), 
                  choices = list("Subspecies" = "Subspecies",
                                 "Species" = "Species", 
                                 "Genus" = "Genus",
                                 "Tribus" = "Tribus",
                                 "Subfamily" = "Subfamily",
                                 "Family" = "Family",
                                 "Ordere" = "Order",
                                 "Subclass" = "Subclass",
                                 "Class" = "Class",
                                 "Phylum" = "Phylum"), 
                  selected = "Species")),
  
  # biomonitoR Indices
  div(style="display: inline-block;vertical-align:top; width: 200px;", 
      selectInput("selectIndex", h5("Method distance"), 
                  choices = list("All" = "all", 
                                 "Berger-Parker" = "berger.parker",
                                 "Inverse Berger-Parker" = "inverse.berger.parker",
                                 "Brillouin" = "brillouin",
                                 "Simpson's evenness" = "simpson.evenness",
                                 "Simpson's Index of Diversity" = "simpson.diversity",
                                 "Inverse Simpson" = "inverse.simpson",
                                 "Fisher alpha" = "fisher.alpha",
                                 "Margalef diversity" = "margalef.diversity",
                                 "McIntosh dominance" = "mcintosh.dominance",
                                 "Pielou's evenness" = "pielou.evenness",
                                 "Shannon index" = "shannon.index"), 
                  selected = 1)),
  
  br(),
  
  # Button
  div(style="display: inline-block;vertical-align:bottom; width: 200px;",
      actionButton("buttonStat", "Run"))
)