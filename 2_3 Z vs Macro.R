
# macroplot

output$macroplot <- renderPlotly({
  
  
  
  plot_ly(x=~Z$zlist$Period) %>% add_lines(
                                 y=~round(Z$zlist$Zscore, 4), name="Z Score",
                                 line = list(shape = "spline",color="#019882"))%>%
    add_lines( 
              y=~round(macro$x[,input$macro], 4), name="Macro",yaxis="y2",
              line = list(shape = "spline",color="#f4a582"))%>%
    layout(xaxis=list(title="",showgrid=F),
           yaxis=list(title="Z Score",showgrid=F),
           yaxis2=list(overlaying="y",side="right",title="Macro"),
           hovermode = "x",
           legend = list(x = 0.35, y = -0.1,orientation = 'h'),
           margin = list(r = 50))%>%
    config(displayModeBar = F)
})


# macro table
output$macrotable <- renderDataTable({
  data = data.frame(Z$zlist$Period,
                    round(Z$zlist$Zscore,4),
                    round(macro$x[,input$macro],4))

  datatable(data,
            rownames = FALSE,
            colnames = c("Year","ZScore","Macro"),
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '400px',
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))

})
