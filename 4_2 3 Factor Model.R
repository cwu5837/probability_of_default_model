
# z3plot

output$z3plot <- renderPlotly({
  
  Pred = Z_model_3f %>% filter(scenario != "Actual") %>% as.data.frame()
  Actual = Z_model_3f %>% filter(scenario == "Actual") %>% as.data.frame()
  
  max = max(Pred$value, Actual$value)
  min = min(Pred$value, Actual$value)
  
  plot_ly() %>% add_lines(data=Pred,
                                                x=~Year, 
                                                y=~round(value, 4), 
                                                line = list(shape = "spline"),
                                                hoverinfo = "y+text+name",
                                                name=~scenario,
                                                text=~Year,
                                                color=~scenario, 
                                                colors = c("black", "black", "red", "orange", "darkgreen"))%>%
                                 add_trace(data=Actual,
                                           x=~Year, 
                                           y=~round(value, 4),
                                           mode = "markers",
                                           hoverinfo = "y+text+name",
                                           name="Actual",
                                           text=~Year,
                                           marker = list(color = "#67a9cf"))%>%
                                    layout(xaxis=list(title="",showgrid=F),yaxis=list(title=""),
                                           legend = list(orientation = 'l'),
                                           margin = list(r = 50),
                                           shapes = list(type = "line", x0 = "2016", x1 = "2016", y0 = min, y1 = max, line = list(color = "gray", dash = "dash", width = 0.5)),
                                           hovermode = "x")%>%
                                    config(displayModeBar = F)
})


# formula
output$f3 <- renderUI({ withMathJax(model_formula(Model_3f[1,], 3)$formula) })

# adjr2
output$adjr3 <- renderUI({ withMathJax(model_formula(Model_3f[1,], 3)$adjr) })

# forecast_z
output$forecast_z3<-renderDataTable({
  data = data.frame(Year = c(2017:2019),
                    Base = filter(Z_model_3f,scenario=="Base")$value[-1],
                    Adverse = filter(Z_model_3f,scenario=="Adverse")$value[-1],
                    Severe = filter(Z_model_3f,scenario=="Severe")$value[-1]
  )
  datatable(data,
            rownames = FALSE, 
            options = list(dom = 't'),
            selection = list(target="cell"))%>%
    formatRound(2:4,4)
})

# df3plot

output$df3plot <- renderPlotly({
  
  Pred = DF_model_3f %>% filter(scenario != "Actual") %>% as.data.frame()
  Actual = DF_model_3f %>% filter(scenario == "Actual") %>% as.data.frame()
  
  max = max(Pred$value, Actual$value)
  min = min(Pred$value, Actual$value)
  
  plot_ly() %>% add_lines(data=Pred,
                                                x=~Year, 
                                                y=~round(value, 4), 
                                                line = list(shape = "spline"),
                                                hoverinfo = "y+text+name",
                                                name=~scenario,
                                                text=~Year,
                                                color=~scenario, 
                                                colors = c("black", "black", "red", "orange", "darkgreen"))%>%
    add_trace(data=Actual,
              x=~Year, 
              y=~round(value, 4),
              mode = "markers",
              hoverinfo = "y+text+name",
              name="Actual",
              text=~Year,
              marker = list(color = "#67a9cf"))%>%
    layout(xaxis=list(title="",showgrid=F),yaxis=list(title="",ticksuffix = "%"),
           legend = list(orientation = 'l'),
           margin = list(r = 50),
           shapes = list(type = "line", x0 = "2016", x1 = "2016", y0 = min, y1 = max, line = list(color = "gray", dash = "dash", width = 0.5)),
           hovermode = "x")%>%
    config(displayModeBar = F)
})


# forecast_df
output$forecast_df3<-renderDataTable({
  data = data.frame(Year = c(2017:2019),
                    Base = filter(DF_model_3f,scenario=="Base")$value[-1]/100,
                    Adverse = filter(DF_model_3f,scenario=="Adverse")$value[-1]/100,
                    Severe = filter(DF_model_3f,scenario=="Severe")$value[-1]/100
  )
  datatable(data,
            rownames = FALSE, 
            options = list(dom = 't'),
            selection = list(target="cell"))%>%
    formatPercentage(2:4, 2)
})

# trans_mtx
output$mtx3<-renderDataTable({
  if(nrow(input$forecast_z3_cells_selected) == 0) return()
  if(input$forecast_z3_cells_selected[1,2] == 0) return()
  data = trans.mtx(Z_model_3f,input$forecast_z3_cells_selected)
  data = datatable(data, 
                   caption = htmltools::tags$caption(
                     style = 'text-align: center;',
                     'Transition Matrix (%)'), 
                   options = list(searching = FALSE,
                                  paging = FALSE,
                                  ordering = FALSE,
                                  dom = 't')) %>% 
    formatStyle(0, fontWeight = "bold") %>%
    formatPercentage(colnames(data), digits = 2)
})