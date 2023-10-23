#--------------------#
# Taxonomy mainPanel #
#--------------------#

fluidRow(
  h2("CBB Taxonomy"),
  DT::dataTableOutput("inputDataframe"),
  br(),
  uiOutput("modify")
)