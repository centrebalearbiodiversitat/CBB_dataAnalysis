#-----#
# RLI # OR biomonitoR
#-----#

temp_br_taxa <- reactiveValues(df_br_taxa = NULL) # <- Store .csv taxa biomonitoR
temp_br_refdb <- reactiveValues(df_br_refdb = NULL) # <- Store .csv reference DB biomonitoR
temp_br_index <- reactiveValues(df_br_index = NULL) # <- Store .csv reference DB biomonitoR

# Show table whit uploaded data (temp_df)
output$inputDataframe_bioR <- DT::renderDataTable({
  req(input$fileBr_taxa)
  temp_br_taxa$df_br_taxa <-  read.csv(input$fileBr_taxa$datapath, 
                                       header = input$headerRLI,
                                       sep = input$sepRLI,
                                       quote = input$quoteRLI)
  temp_br_taxa$df_br_taxa
}, options = (list(scrollX = TRUE, paging = FALSE, scrollY = "300px")),
rownames= FALSE)





observeEvent(input$taxa.run.button.bior,{
  
  req(input$fileBr_taxa)
  
  # As biomoitoR and biomonitoR aggregation
  data_bio <- as_biomonitor(temp_br_taxa$df_br_taxa, group = "mi")
  data_agr <- aggregate_taxa(data_bio)
  
  temp_br_index$df_br_index <- allindices(data_agr, tax_lev = "Taxa", 
                                          base = exp(1))
  
  output$modify_bioR <- renderUI({
    tagList(
      h2("All indices"),
      DT::dataTableOutput("allIndexBior")
    ) %>% withSpinner()
  })
  
  
})


# Show table whit COL information data (temp_df.2)
output$allIndexBior <- DT::renderDataTable({
  round(temp_br_index$df_br_index, digits = 2)
}, options = (list(scrollX = TRUE, paging = FALSE, scrollY = "300px")),
rownames= TRUE)
