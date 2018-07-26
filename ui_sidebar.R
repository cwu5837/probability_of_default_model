sidebar<-dashboardSidebar(
  width=300,
  
  sidebarMenu(
    
    id = "tabs",
    
    
    menuItem("Methodology",
             icon = icon("book"),
             tabName = "method",
             startExpanded = FALSE),
    
    menuItem("Z Score Calculation", 
             icon = icon("calculator"), 
             tabName = "z_score_calculation",
             startExpanded = FALSE,
             
             menuSubItem("Z Score", tabName = "zscore"),
             menuSubItem("Default Rate Backtesting", tabName = "df_backtest"),
             menuSubItem("Z Score vs. Macro Variables", tabName = "macro")
    ),
    
    menuItem("Model Development", 
             icon = icon("cogs"), 
             tabName = "development",
             startExpanded = FALSE,
             
             menuSubItem("Variable Selection", tabName = "var"),
             menuSubItem("Model Selection", tabName = "model")
    ),
    
    menuItem("Model Implementation", 
             icon = icon("magic"), 
             tabName = "implement",
             startExpanded = FALSE,
             
             menuSubItem("2-Factor Model", tabName = "two"),
             menuSubItem("3-Factor Model", tabName = "three")
    )
  )
)