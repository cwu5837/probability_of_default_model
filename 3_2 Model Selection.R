# stats test table
output$stats_test <- renderGvis({
  gvisTable(stats.test, options = list(width = "100%"))
})

# sign table
output$signs <- renderGvis({
  gvisTable(sign, options = list(width = "100%"))
})