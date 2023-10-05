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
              #multiple = TRUE,
              accept = c(#"text/csv",
                         #"text/comma-separated-values, text/plain",
                         ".csv"))),

h5(".csv options"),

# Input: Checkbox if file has header
div(style="display: inline-block;vertical-align:top; width: 100px;",   
    checkboxInput("header", HTML("First raw <br> as header"), TRUE)),

# Input: Select columns separator
div(style = "display: inline-block; vertical-align:top; width: 150px;",   
    radioButtons("sep", "Column separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")),

# Input: Select quotes
div(style = "display: inline-block; vertical-align:top; width: 150px;",
    radioButtons("quote", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"')),

# Text input
div(style = "display: inline-block;vertical-align:top; width: 150px;",   
    
    # Select column where taxa to analize are stored
    selectInput("text.db", h5("Name of taxa column"), 
                choices = NULL)
    ),

div(style = "display: inline-block;vertical-align:top; width: 200px;",
    
    # Select mode to analyze the taxonomy
    selectInput("taxon.an", h5("Choose taxonomy style"), 
                choices = c("Specify_COL", "CBB_DB_COL"))
    ),

    
# textInput("text.db", h5("Name of taxa column"), 
#           value = "Taxa")),

# Per ora lo nascondiamo

# div(style="display: inline-block;horizontal-align:bottom; width: 300px;",
#     HTML("Do you want to merge the nomenclature <br>
#          with your original dataframe?")),
# div(style="display: inline-block;vertical-align:bottom; width: 100px;",
#     switchInput(inputId = "switchTax", value = FALSE, size = "mini")),


# Horizontal line
hr(), 

# Run button
div(style="display: inline-block;horizontal-align:bottom; width: 100px;",
    actionButton("taxa.run.button", HTML("<b>Run</b>"))),
hr(),

# Download button
column(4, uiOutput("downloadButton")),
# column(4, uiOutput("downloadDoubtful")),
# column(4, uiOutput("downloadMergeTaxa")),
br()
)
