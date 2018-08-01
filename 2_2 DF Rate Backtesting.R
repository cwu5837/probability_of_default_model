
# dfplot

output$dfplot <- renderPlotly({
  
  plot_ly(df_backtest) %>% add_lines(x=~Year, 
                                 y=~round(Backtesting*100, 2), name="Backtesting",
                                 line = list(shape = "spline",color="#d8b365"))%>%
    add_trace(x=~Year, 
              y=~round(Actual*100, 2),name="Actual",
              type="scatter",
              marker=list(color="#5ab4ac")
              )%>%
    layout(xaxis=list(title="",showgrid=F),
           yaxis=list(title="",ticksuffix = "%"),
           hovermode = "x",
           legend = list(x = 0.35, y = -0.1,orientation = 'h'),
           margin = list(r = 50))%>%
    config(displayModeBar = F)
})


# df rt table
output$dftable <- renderDataTable({

  datatable(df_backtest,
            colnames = c("Year", "Actual", "BackTesting"),
            rownames = FALSE,
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '480px',
                           dom = 't'
                           )) %>% 
    formatPercentage(2:3, digits = 4)

})
