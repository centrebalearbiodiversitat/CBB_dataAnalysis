#----------------------#
# biomonitoR mainPanel #
#----------------------#

fluidRow(
  
  # Show input table
  DT::dataTableOutput("inputDataframe_bioR"),
  
  # Show general info of table
  DT::dataTableOutput("generalInfo_bioR"),
  
  # Show table with biological indices
  conditionalPanel(
    condition = "input['div.run.button.bior'] % 2 == 1",
    DT::dataTableOutput("modify_index_bioR")),
  
  # Show table with community composition
  conditionalPanel(
    condition = "input['com.run.button.bior'] % 2 == 1",
  DT::dataTableOutput("modify_rich_bioR")),
  
  # Show plot with NMDS results
  conditionalPanel(
    condition = "input['nmds.run.button.bior'] % 2 == 1",
    plotOutput("nmdsPlot")),
  
  # Show plot with accumulation curves 
  conditionalPanel(
    condition = "input['acc.run.button.bior'] % 2 == 1",
    plotOutput("accPlot")),
  
  # Show plot with rarefaction curves
  conditionalPanel(
    condition = "input['rar.run.button.bior'] % 2 == 1",
    plotOutput("rarPlot"))
)
