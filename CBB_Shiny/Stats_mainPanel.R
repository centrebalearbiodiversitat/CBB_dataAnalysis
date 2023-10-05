#-----------------#
# Stats mainPanel #
#-----------------#

fluidRow(
  h2("Stat dataframe"),
  DT::dataTableOutput("inputDataframeStat"),
  br(),
  uiOutput("modifyStat")
)