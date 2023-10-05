#-------------------#
# Maps sidebarPanel #
#-------------------#

fluidRow(
  
  div(style="display: inline-block;vertical-align:top; width: 400px;",   
      fileInput("file2", "Chose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values, text/plain",
                           ".csv"))),
  br(),
  
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
  
  div(style="display: inline-block;vertical-align:top; width: 200px;", 
  selectInput("mapView", h5("Select map view"), 
              choices = list("Leaflet" = "leaflet", 
                             "Mapview" = "mapview"))),
  
  br(),
  
  # Input: Checkbox if use taxonomy database
  div(style="display: inline-block;vertical-align:top; width: 200px;",   
      checkboxInput("TaxonomyInMap", "Do you want to use taxonomy dataset?", FALSE)),
  
  br(),
  
  # Text input Longitude
  div(style="display: inline-block;vertical-align:top; width: 200px;", 
      selectInput("Lon.db", h5("Name of LONGITUDE column"), choices = NULL)),
  # textInput("Lon.db", h5("Name of Longitude column"), value = "Lon")),
  # Text input Latitude
  div(style="display: inline-block;vertical-align:top; width: 200px;", 
      selectInput("Lat.db", h5("Name of LATITUDE column"), choices = NULL)),
  # textInput("Lat.db", h5("Name of Latitude column"), value = "Lat")),
  
  br(), 
  
  div(style="display: inline-block;vertical-align:top; width: 200px;",
      h3("Plot map options")),
  
  # Switch for map plot
  div(style="display: inline-block;vertical-align:bottom; width: 100px;",
      switchInput(inputId = "switchMap", value = FALSE, size = "mini")),
  
  br(),
  
  # Select GADM maps
  div(style="display: inline-block;vertical-align:top; width: 200px;", 
      selectInput("gadMap", h5("Select GADM level"), 
                  choices = list("Default" = "./gadm36_levels_shp/Italy.shp",
                                 "gadm 0" = "./gadm36_levels_shp/gadm36_0.shp", 
                                 "gadm 1" = "./gadm36_levels_shp/gadm36_1.shp"),
                  selected = 1)),
  # Extension box map
  div(style="display: inline-block;vertical-align:top; width: 70px;", 
      numericInput("xmin", h5("xmin"), value = 5.0)),
  div(style="display: inline-block;vertical-align:top; width: 70px;", 
      numericInput("ymin", h5("ymin"), value = 36.0)),
  div(style="display: inline-block;vertical-align:top; width: 70px;", 
      numericInput("xmax", h5("xmax"), value = 19.0)),
  div(style="display: inline-block;vertical-align:top; width: 70px;", 
      numericInput("ymax", h5("ymax"), value = 48.0)),
  
  hr(),
  column(4, uiOutput("download.gadmMap")),
  column(4, uiOutput("download.gadmContents"))
  
)
