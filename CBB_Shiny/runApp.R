#---------------------#
# START Shiny CBB App #
#---------------------#

library(shiny)
runGitHub(repo = 'CBB_dataAnalysis', username = 'centrebalearbiodiversitat', 
          ref="main", subdir = "CBB_Shiny")