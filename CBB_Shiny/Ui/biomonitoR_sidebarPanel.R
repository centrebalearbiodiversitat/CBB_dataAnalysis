#-------------------------#
# biomonitoR sidebarPanel #
#-------------------------#
# Add api to download IUCN Information

fluidRow(
  
  HTML("<h3> biomonitoR </h3>
<br> biomonitoR .... <br>"),




hr(),

# Input: Select a .csv file
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    fileInput("fileBr_taxa", h5("Chose TAXA .csv"),
              accept = c(".csv"))
    ),

# Select reference DB for group parameter as_biomonitor
div(style="display: inline-block;vertical-align:top; width: 200px;",   
    selectInput("refdb.group", h5("Taxonomy ref. group"), 
                choices = c("Macroinvertebrate" = "mi", 
                            "Macrophytes" = "mf",
                            "Fish" = "fi",
                            "Diatoms." = "di"),
                selected = "mi")
),
    
# Upload .csv with reference DB
div(style="display: inline-block;vertical-align:top; width: 200px;",   
    fileInput("fileBr_refdb", h5("Chose Reference DB .csv file"),
              accept = c(".csv"))
    ),

# .csv options. It is usefull for both .csv files
h4(".csv options"),

# Input: Select columns separator
div(style = "display: inline-block; vertical-align:top; width: 200px;",
    textInput("sepBioR", value = ",", h5("Column separator:"))
    ),

# Input: Select quotes
div(style = "display: inline-block; vertical-align:top; width: 200px;",
    textInput("quoteBioR", value = '"', h5("Quote:"))
    ),

div(style="display: inline-block;horizontal-align:bottom; width: 400px;",
    actionButton("div.run.button.bior", HTML("<b>Div. index</b>")),
    actionButton("com.run.button.bior", HTML("<b>Com. compo</b>")),
    ),

hr(),

h5("NMDS options"),

# Input: Select a .csv file
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    fileInput("envNMDS", h5("Chose environmental .csv"),
              accept = c(".csv"))
),

# Number of dimension (k)
div(style="display: inline-block;horizontal-align:bottom; width: 130px;",
    numericInput("nmdsk", "Number of dimensions (k)", value = 2),
    ),

# Minimum numbers of random starts in search of stable solution
div(style="display: inline-block;horizontal-align:bottom; width: 130px;",
    numericInput("nmdsTry", "N. random (min)", value = 20)
),

# Maximum numbers of random starts in search of stable solution
div(style="display: inline-block;horizontal-align:bottom; width: 130px;",
    numericInput("nmdsTrymax", "N. random (max)", value = 50)
    ),

# # Column to NMDS group
# div(style="display: inline-block;horizontal-align:bottom; width: 200px;",
#     textInput("nmdsGroup", "Environmental group", value = "")
# ),

# Column to NMDS group
div(style = "display: inline-block;vertical-align:top; width: 150px;",   
    
    # Select column where taxa to analize are stored
    selectInput("nmdsGroup", h5("Environmental group"), 
                choices = NULL)
),



# Select NMDS plot type
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    
    selectInput("nmdsPlotType", h5("NMDS Plot type"), 
                choices = c("General",
                            "Ellipse"),
                selected = "General")
),

# Run button for NMDS
div(style="display: inline-block;horizontal-align:bottom; width: 80px;",
    actionButton("nmds.run.button.bior", HTML("<b>NMDS</b>"))
    ),

# Run button for Accumulation curves
div(style="display: inline-block;horizontal-align:bottom; width: 120px;",
    actionButton("acc.run.button.bior", HTML("<b>Acc. Curves</b>"))
),

# Run button for Rarefaction curves
div(style="display: inline-block;horizontal-align:bottom; width: 120px;",
    actionButton("rar.run.button.bior", HTML("<b>Rar. Curves</b>"))
),

hr(),

# Download button
conditionalPanel(
  condition = "input['div.run.button.bior'] % 2 == 1",
  column(4, uiOutput("dowButtonBiorIndex"))
  ),

conditionalPanel(
  condition = "input['com.run.button.bior'] % 2 == 1",
  column(4, uiOutput("dowButtonBiorComp"))
  ),

conditionalPanel(
  condition = "input['nmds.run.button.bior'] % 2 == 1",
  column(4, uiOutput("dowButtonNMDS"))
  )


)