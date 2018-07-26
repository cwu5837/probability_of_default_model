# formula 3
output$f3_rep <- renderUI({ withMathJax(model_formula(Model_3f[1,], 3)$formula) })

# df table 3
output$overall_tb3 <- renderDataTable({
  data = overall_df(Z_model_3f)
  data = datatable(data, 
                   options = list(searching = FALSE,
                                  paging = FALSE,
                                  ordering = FALSE,
                                  dom = 't')) %>% 
    formatStyle(0, fontWeight = "bold") %>%
    formatPercentage(colnames(data), digits = 2)
})