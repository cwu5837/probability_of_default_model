
# zplot

output$zplot <- renderPlotly({
  
  p=plot_ly(Z$zlist) %>% add_lines(x=~Period, 
                                 y=~round(Zscore, 4), 
                                 line = list(shape = "spline",color='rgb(11, 118, 123)'), 
                                 hoverinfo = "text",
                                 text=~paste("Year: ",as.character(Period),
                                             "<br />",
                                             "Z Score:", as.character(round(Zscore, 4)))
  ) %>%
    add_annotations(x=~Period[c(4,9,12)],y=~Zscore[c(4,9,12)],
                    text=c("Early 2000s Recession","US Housing Bubble","Global Financial Crisis"),
                    ax = 20,
                    ay = -40)%>%
    layout(xaxis=list(title="",showgrid=F),yaxis=list(title=""))%>%
    config(displayModeBar = F)
})


# rho
output$rho <- renderInfoBox({
  infoBox(
    "Optimal Rho",
    value = tags$p(as.character(round(Z$rho, 4)), style = "font-size: 200%;"),
    icon=icon("check-circle"),
    color="yellow",
    fill=T
  )
})

# ztable
output$ztable <- renderDataTable({
  
  data = data.frame("Year" = Z$zlist$Period,
                    "ZScore" = round(Z$zlist$Zscore, 4))
  
  datatable(data, 
            rownames = FALSE, 
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           scrollY = '405px',
                           dom = 't',
                           columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  
})
