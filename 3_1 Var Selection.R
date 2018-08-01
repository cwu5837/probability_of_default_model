
# var table
output$macro_var <- renderTable(
  macro_var,hover=T,bordered=T,
  width="900px"
)

# trans table
output$macro_trans <- renderGvis({
  gvisTable(macro_desc, options = list(width = "100%"))
})

# corr.plot
output$corrplot<-renderPlotly({
  data=data.frame(Zscore=Z$zlist$Zscore,macro$x[,-1])
  corr=cor(data,use="complete.obs")
  plot_ly(x=colnames(corr),
          y=rownames(corr),
          z=round(corr,4),
          type="heatmap",
          colorbar = list(title = "Correlation"))%>%
    layout(xaxis = list(side = "bottom"),
           yaxis = list(side = "left"),
           margin = list(r = 0, t = 0, l = 135, b = 135)
           )%>%
    config(displayModeBar = F)
})