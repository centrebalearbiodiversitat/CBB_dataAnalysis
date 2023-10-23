#---------------#
# RLI mainPanel #
#---------------#

fluidRow(
  h2("CBB biomonitoR"),
  DT::dataTableOutput("inputDataframe_bioR"),
  br(),
  uiOutput("modify_bioR")
)
