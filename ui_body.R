body <- dashboardBody(
  includeCSS("styles.css"),
  tabItems(
    # ------------------------------------------------------------------------------------------------------------    
    # methodology
    tabItem(tabName = "method",
            h3("Methodology"),
            fluidRow(
              column(12,
                     tags$b("Overview"),
                     p("Stress testing is a central risk-management 
                       and regulatory tool to assess the potential impact of extreme scenarios on bank holding 
                        companies capital ratios. Stress testing wholesale credit risk, also known as 
                        'C&I' credit risk, is a key component of stress testing 
                        for majority of large financial institutions."),
                     p("A wide range of methods are available for stressing wholesale credit risk, 
                       depending on the type and size of portfolios and data availability. 
                       These methods can be based on either an accounting-based loss approach 
                       (that is, charge-off and recovery) or an economic loss approach (that is, expected losses)."),
                     p("Under the expected loss approach, losses are estimated as a 
                       function of three components: probability of default (PD), loss given default (LGD), 
                       and exposure at default (EAD)."),
                     p("To estimate the PD component, we use a conditional rating transition-based approach to 
                       produce a stressed rating transition matrix for each year, which is then used to estimate
                        the probability of default for loans under various macroeconomic conditions."),
                     tags$b("Modeling Procedures"),
                     tags$ol(
                       tags$li("Loan data is used to generate yearly transition matrices and to construct a long term average matrix."), 
                       tags$li("Differences between yearly historical rating transition matrices and the corresponding long term 
                                average rating transition matrix are translated into a single number, the 'Z Score' for each year; 
                                the Z Score thus reflects the deviations in a year's rating transitions from the long term average transitions."), 
                       tags$li("A time series of these Z Scores is regressed on macro variables to build a model that projects 
                                future Z Scores under various macroeconomic scenarios. This regression constitutes the model developed for stress testing.")
                     ),
                     tags$b("Model Implementation"),
                     tags$ol(
                       tags$li("A forecast macro scenario is then used to calculate the predicted Z Score for 
                               a given observation period using the regression results."), 
                       tags$li("That period's Z Score is used to create a stressed transition matrix based 
                               on the corresponding average transition matrix."), 
                       tags$li("These projected matrices are used to predict transition and default behavior in future stress periods.")
                     ),
                     
                     tags$b("Sample Output"),
                     tags$ul(
                       tags$li("3-Factor Model:"),
                       fluidRow(column(6,uiOutput("f3_rep"))),
                       tags$li("3-Year Average Predicted Default Rate for Loan Ratings"),
                       fluidRow(column(8,dataTableOutput("overall_tb3")))
                      
                     ),
                     br(),
                     tags$b("References"),
                     tags$div(
                       HTML(paste('Lawrence R. Forest, Jr., Barry Belkin, & Stephan J. Suchower','"A one-parameter representation of credit risk and transition matrices."', tags$i("CreditMetrics Monitor,"), "3rd quarter 1998.", sep = " "))
                     )
              )
            )),
    # ------------------------------------------------------------------------------------------------------------    
    # z score
    tabItem(tabName = "zscore",
            # page title
            h3("Z Score"),
            # z score plot
            fluidRow(column(8, 
                            plotlyOutput("zplot",height = 480),br(),
                            helpText("Z Scores' historical movements describe past credit conditions. 
                                              In good years Z Score will be positive, implying for each initial 
                                              credit rating a lower than average default rate and a higher than 
                                              average ratio of upgrades to downgrades. In bad years, the reverse will be true. 
                                              "),
                            helpText("For example, Z remains negative for 2007-2009. This mirrors the general decline 
                                              in credit ratings in the Global Financial Crisis.")
                                     ),
                     column(4,align="center",
                            # rho
                            fluidRow(infoBoxOutput("rho")),
                            # z score table
                            fluidRow(box(dataTableOutput("ztable"), 
                                         title = "Z Score Table",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 10, height = 500)
                            )))
            ),
    # ------------------------------------------------------------------------------------------------------------
    # df rate backtesting
    tabItem(tabName = "df_backtest",
            # page title
            h3("Default Rate Backtesting"),
            fluidRow(
              # df_rt plot
              column(8,
                     plotlyOutput("dfplot",height = 480),
                     helpText("Given the Z Score of a specific year and its corresponding rho, 
                              we can re-create the transition matrix of that year. 
                              From the transition matrix, the default rate can be derived. 
                              The table on the right side shows the actual default rates and 
                              the derived default rates using Z Scores."),
                     helpText("The derived default rates are very close to the actual values, 
                              meaning that the Z Scores are able to capture the features of default events.")),
              column(4,align="center",
                     # df rt table
                     fluidRow(box(dataTableOutput("dftable"),
                                  title = "Default Rate Table",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = 11, height = 580))
              )
            )
    ),
    # ------------------------------------------------------------------------------------------------------------    
    # Z vs Macro
    tabItem(tabName = "macro",
            # page title
            h3("Z Score vs. Macro Variables"),
            fluidRow(
              # macro plot
              column(8,
                     plotlyOutput("macroplot",height=480),
                     helpText("* Data Source: Federal Reserve"),
                     helpText("The market often moves ahead of an upgrade or downgrade. Combinations of a set of 
                              macroeconomic variables can be used to predict dependent variable Z Score.")),
              column(4,
                     align="center",
                     fluidRow(
                       helpText("Select a macroeconomic variable:"),
                       selectInput(inputId = "macro", 
                                   label = NULL,
                                   selected = NULL,
                                   choices = c("Unemployment Rate" = "lag0_Unemp.Rt",
                                               "BBB Corporate Rate" = "lag0_BBB.Rt",
                                               "Mortgage Rate" = "lag0_Mort.Rt",
                                               "Prime Rate" = "lag0_Prime.Rt",
                                               "Dow Jones Stock Market Index" = "lag0_DJIA",
                                               "Market Volatility Index (VIX)" = "lag0_VIX",
                                               "Real GDP Growth Rate" = "lag0_RGDP.Ygr",
                                               "Nominal GDP Growth Rate" = "lag0_NGDP.Ygr",
                                               "Nominal Disposable Income Growth Rate" = "lag0_NDI.Ygr",
                                               "Real Disposable Income Growth Rate" = "lag0_RDI.Ygr",
                                               "Consumer Price Index Rate" = "lag0_CPI.Ygr",
                                               "BBB Spread" = "lag0_BBB.Spd"
                                   ))),
                     # Z vs macro table
                     fluidRow(box(dataTableOutput("macrotable"),
                                  title = "Default Rate Table",
                                  status = "primary",
                                  solidHeader = TRUE,
                                  width = 12, height = 500))
              )
            )
            ),
    # ------------------------------------------------------------------------------------------------------------    
    # variable selection
    tabItem(tabName = "var",
            # page title
            h3("Variable Selection"),
            tabBox(width = "100%",
                   
                   tabPanel("Variable Transformation",
                            fluidRow(column(12,
                                   h5("* Preliminary Variable Pool"),
                                   htmlOutput("macro_var"),
                                   p("A list of macroeconomic variables are selected to the preliminary variable pool of 
                                     the model based on business intuition."),
                                   p("Once the initial variables are identified,
                                     a number of transformations are created for potential inclusion in the model, as detailed
                                     below: "),
                                   column(width = 12, offset = 1,
                                          p("1. Yearly difference (Ydf) for rate variables"),
                                          p("2. Yearly growth rate (Ygr) for index variables"),
                                          p("3. Lag: up to only one year lag for all raw/transformed variables")),
                                   p("Variable transformations are limited to year on year changes and growths in line with 
                                     industry convention."),
                                   p("The table below lists all the variables and their transformations 
                                     considered."),
                                   htmlOutput("macro_trans")
                                   ))),
                   
                   tabPanel("Correlation Analysis",
                            fluidRow(
                              column(12,
                                     p("Correlation analysis is performed to identify 
                                       the level of sensitivity between Z Score and macroeconomic variables."),
                                     p("There are variables that are highly correlation to Z Score,
                                       but not all highly correlated variables are intuitive when it 
                                       comes to interpreting how it impacts Z Score."),
                                     p("With that said, no pre-selection 
                                       is performed due to the limited number of variables and data points."),
                                     plotlyOutput("corrplot",width=1000,height =800))
                                     
                            ))
            )
    ),
    # ------------------------------------------------------------------------------------------------------------    
    # model selection
    tabItem(tabName = "model",
            h3("Model Selection"),
            tabBox(width = "100%",
                   tabPanel("Subset Selection",
                            fluidRow(
                              column(12,
                                     p("Three subset selection procedures have been proposed."),
                                     tags$ol(
                                       tags$b(tags$li("Forward Stepwise Selection")), 
                                       tags$ul(
                                         tags$li("Select models by first introducting a model containing no factors,
                                                  and then adding individual factors, a step at a time.
                                                 At each step, the variable that gives the greatest additional improvement
                                                 to the fit is added to the model."), 
                                         tags$li("Selection process is complete once no additional factors pass 
                                                 the default threshold for entry into the model.")
                                       ),
                                       tags$b(tags$li("Backward Stepwise Elimination")),
                                       tags$ul(
                                         tags$li("Select models by first including all possible factors then 
                                                 removing individual factors, a step at a time, based on 
                                                 the statistical significance of the predictor."), 
                                         tags$li("Variables are deleted from the model one by one (based on the 
                                                 variable exhibiting the least significance) until all variables 
                                                 remaining in the model produce F statistics significant at the 
                                                 default level.")
                                         ),
                                       tags$b(tags$li("Best Subset Selection")),
                                       tags$ul(
                                         tags$li("Given a specific number of predictors (e.g. k), fit all possible models that contain exactly
                                                 k predictors."), 
                                         tags$li("A list of the top 'n' models is generated for further review based on a single metric (usually 
                                                 Adjusted R square); allows for identification of most appropriate models.")
                                         )
                                     ),
                                     p("Of these three approaches, the best subset selection approach was ultimately selected."),
                                     p("The forward and backward selection processes rely on user specified p value thresholds that can be unstable depending on the specific p value chosen. 
                                     In addition, because these are 'greedy' algorithms that look simply to maximize R Square without any filter for intuitiveness of results, 
                                       these two options are eliminated."),
                                     p("By contrast, the best subset selection uses a single metric to rank all possible models, 
                                       allowing for expert review of multiple options. Additional refinement 
                                       can also be performed automatically using a rules based approach.")
                              
                            )
                   )
                   ),
                   tabPanel("Filtering Criteria ",
                            fluidRow(
                              column(5,
                                     p("As the best subsets approach produces a large number of potential models,
                                       it was necessary to define and impose a set of filtering criteria to 
                                       identify a small set of high potential models."),
                                     p("The measurement used to rank the model is Adjusted R Square. This metric is 
                                        chosen because it represents the goodness of fit for the model against the historical data.
                                       For the models ranked by the measurement above, 
                                       additional criteria that filtered out undesirable models are as follows:"),
                                     strong("1. P-value for Coefficents"), 
                                     tags$ul(
                                       tags$li("Models for which at least one coefficient had a p value over 10%. 
                                               This initial threshold was set to avoid eliminating potentially 
                                               attractive models with higher coefficient p values.")
                                       ),
                                     strong("2. Statistical Testing for Model Assumptions"),
                                     tags$ul(
                                       tags$li("Comprehensive testing of the standard linear regression assumptions is conducted to ensure 
                                              validity and correct specification of the regression models.")
                                       ),
                                     strong("3. Intuitive Coefficents' Signs"),
                                     tags$ul(
                                       tags$li("The variables and their respective signs need to be 
                                                business intuitive. The expected coefficients' signs and business intuition 
                                               are described in the table on the right.")
                                       ),
                                     strong("4. Diversity in Conditioning Variables"),
                                     tags$ul(
                                       tags$li("To ensure that the model is responsive to a range of economic indicators 
                                               but at the same time not suffering from multicollinearity issue.")
                                       )
                                     ),
                              column(7,
                                     helpText("* Statistical Testing for Model Assumptions"),
                                     htmlOutput("stats_test"),
                                     br(),
                                     helpText("* Expected Signs for Coefficients"),
                                     htmlOutput("signs")))
                            )
            )
    ),
    # ------------------------------------------------------------------------------------------------------------    
    # 2 factor model
    tabItem(tabName = "two",
            h3("2-Factor Model"),
            tabBox(width = "100%",
                   tabPanel("Z Score",
                            fluidRow(column(10, offset = 1,
                                   plotlyOutput("z2plot",height = 520)
                                   )),
                            fluidRow(column(6,offset=3,
                                            uiOutput("f2"))),
                            fluidRow(
                              column(10, offset = 1,
                                     helpText("Z Score Table",align="center"),
                                     dataTableOutput("forecast_z2"),
                                     helpText("*Click on a specific Z Score to display a corresponding transition matrix.",align="center"),
                                     dataTableOutput("mtx2"))
                            )
                            ),
                   tabPanel("Default Rate",
                            fluidRow(column(10, offset = 1,
                                   plotlyOutput("df2plot",height = 480))),
                            fluidRow(
                              column(10,offset = 1,
                                     helpText("Default Rate Table",align="center"),
                                     dataTableOutput("forecast_df2")
                              )
                            )
                           )
                         )
                   ),
    # ------------------------------------------------------------------------------------------------------------    
    # 3 factor model
    tabItem(tabName = "three",
            h3("3-Factor Model"),
            tabBox(width = "100%",
                   tabPanel("Z Score",
                            fluidRow(column(10, offset = 1,
                                            plotlyOutput("z3plot",height = 520)
                            )),
                            fluidRow(column(6,offset=3,
                                            uiOutput("f3"))),
                            fluidRow(
                              column(10, offset = 1,
                                     helpText("Z Score Table",align="center"),
                                     dataTableOutput("forecast_z3"),
                                     helpText("*Click on a specific Z Score to display a corresponding transition matrix.",align="center"),
                                     dataTableOutput("mtx3"))
                            )
                   ),
                   tabPanel("Default Rate",
                            fluidRow(column(10, offset = 1,
                                            plotlyOutput("df3plot",height = 480))),
                            fluidRow(
                              column(10,offset = 1,
                                     helpText("Default Rate Table",align="center"),
                                     dataTableOutput("forecast_df3")
                              )
                            )
                   )
            )
            )
    ))

