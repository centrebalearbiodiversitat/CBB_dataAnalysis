#-----------------------#
# Taxonomy sidebarPanel #
#-----------------------#

fluidRow(
HTML("<h3> Taxonomy </h3>
<br> This panel includes .... <br>"),

hr(),

# Input: Select a .csv file
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    fileInput("file1", h5("Chose CSV File"),
              accept = c(".csv"))
    ),

h5(".csv options"),

# Input: Checkbox if file has header
# div(style="display: inline-block;vertical-align:top; width: 100px;",   
#     checkboxInput("header", HTML("First raw <br> as header"), TRUE)),

# Input: Select columns separator
# div(style = "display: inline-block; vertical-align:top; width: 150px;",   
#     textInput("sep", value = ",", h5("Column separator:"))
#     ),

# Input: Select quotes
# div(style = "display: inline-block; vertical-align:top; width: 150px;",
#     textInput("quote", value = '"', h5("Quote:"))
#     ),

# Select taxa column name
div(style = "display: inline-block;vertical-align:top; width: 200px;",   
    
    # Select column where taxa to analize are stored
    selectInput("text.db", h5("Name of taxa column"), 
                choices = NULL)
    ),

div(style = "display: inline-block;vertical-align:top; width: 200px;",
    
    # Select mode to analyze the taxonomy
    selectInput("taxon.an", h5("Choose taxonomy style"), 
                choices = c("Specify_COL", "CBB_DB_COL", 
                            "Specify_WORMS"))
    ),

# Horizontal line
hr(), 

# Run button
div(style="display: inline-block;horizontal-align:bottom; width: 100px;",
    actionButton("taxa.run.button", HTML("<b>Run</b>"))),

# div(style="display: inline-block;horizontal-align:bottom; width: 100px;",
#     actionButton("taxa.reset", HTML("<b>Reset</b>"))),

hr(),

# Download button
column(4, uiOutput("downloadButton"))


)
