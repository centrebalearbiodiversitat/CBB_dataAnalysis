#------------------#
# RLI sidebarPanel #
#------------------#


# Add api to download IUCN Information


fluidRow(
  HTML("<h3> BIOMONITOR </h3>
<br> biomonitoR .... <br>"),

hr(),

# Input: Select a .csv file
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    fileInput("fileBr_taxa", h5("Chose TAXA .csv file"),
              accept = c(".csv")),
    
    fileInput("fileBr_refdb", h5("Chose Reference DB .csv file"),
              accept = c(".csv"))
    ),

h5(".csv options"),

# Input: Checkbox if file has header
div(style="display: inline-block;vertical-align:top; width: 100px;",
    checkboxInput("headerRLI", HTML("First raw <br> as header"), TRUE)),

# Input: Select columns separator
div(style = "display: inline-block; vertical-align:top; width: 150px;",
    radioButtons("sepRLI", "Column separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")
    ),

# Input: Select quotes
div(style = "display: inline-block; vertical-align:top; width: 150px;",
    radioButtons("quoteRLI", "Quote",
                 choices = c(None = "",
                             "Double Quote" = '"',
                             "Single Quote" = "'"),
                 selected = '"')
    ),

div(style="display: inline-block;horizontal-align:bottom; width: 100px;",
    actionButton("taxa.run.button.bior", HTML("<b>Run div. index</b>"))),
hr(),
)