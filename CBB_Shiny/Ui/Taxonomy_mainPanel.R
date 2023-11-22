#--------------------#
# Taxonomy mainPanel #
#--------------------#

fluidRow(
  # h2("CBB Taxonomy"),
  DT::dataTableOutput("inputDataframe"),
  # fileInputWithResetButtonUI("fileModule1"),
  br(),
  dataTableOutput("dataTaxonomy") %>% withSpinner()
)