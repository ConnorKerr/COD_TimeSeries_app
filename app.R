library(shiny)
library(fpp3)
library(markdown)
library(shinythemes)

ui <- navbarPage("COD TimeSeries App",
                 theme = shinytheme('darkly'),
                 tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType", "Plot Types:",
                                           c("Seasonality"= 's',
                                             "Autocorrelation"= 'a',
                                             "Decomposition x11"= 'd',
                                             "Decomposition seats" = 'd2',
                                             "Line Plot" = 'l')
                              )
                            ),
                            mainPanel(
                              plotOutput("plot")
                            )
                          )
                 ),
                 
                 tabPanel("Simple Models",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType2", "Simple Models:",
                                           c("Mean" = 'me',
                                             "NAIVE" = 'na',
                                             "Seasonal NAIVE" = 'sn',
                                             "Drift" = 'dr')
                              )
                            ),
                            mainPanel(
                              plotOutput("plot2")
                            )
                          )
                 ),
                 
                 tabPanel("Exponential Smoothing",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType3", "Listed Options:",
                                           c("Holts" = 'Holts',
                                             "Holts/Winters" = 'Holwin',
                                             "More" = 'Ext'
                                           )
                              )
                            ),
                            mainPanel(
                              plotOutput("plot3")
                            )
                          )
                 ),
                 
                 tabPanel("ARIMA",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("plotType4", "Listed Options:",
                                           c("Auto-selected Parameters" = 'arima_auto',
                                             "Manually Selected Parameters" = 'arima_man'
                                           )
                              )
                            ),
                            mainPanel(
                              plotOutput("plot4")
                            )
                          )
                 ),
                 
                 tabPanel("Data Summary",
                          verbatimTextOutput("summary")
                 ),
                 navbarMenu("More",
                            tabPanel("Table",
                                     DT::dataTableOutput("table")
                            ),
                            tabPanel("About Plots",
                                     fluidRow(
                                       column(12,
                                              "This data is the google search for the words 'Call of Duty' from 2004 to the present day.
                                        The data is broken down by months and years."),
                                       column(12,
                                              "Interpretations of the plots:"),
                                       column(12,
                                              '-Seasonality plot helps to show if a seasonal pattern is occuring more clearly. In the graph we can notice there appears to be seasonality during the November month.'),
                                       column(12,
                                              "-Autocorrelation plot shows the correlation between a time series with a lagged version of itself. In this autocorrelation plot we can notice a lag of 6 and 12 being significant."),
                                       column(12,
                                              "-Decomposition plots a time series means separating it into it’s constituent components, which are a trend component, a seasonal component, and a irregular component. From the plots you can see the data is quite seasonal."),
                                       column(12,
                                              "-The line plot shows the data from 2004 to present with a line connecting each point of data.")
                                     )),
                            tabPanel("Instructions",
                                     fluidRow(
                                       column(9,
                                              '-To use this app just click on the top menu to navigate.'
                                       ),
                                       column(9,
                                              '-Under Plot tab you can select which plot you want to see for the data.'
                                       ),
                                       column(9,
                                              '-Under the Data Summary tab you can see the 5 number summary of the data.'
                                       ),
                                       column(9,
                                              '-Under the Exponential Smoothing tab you will see Holts/Winter/More.'
                                       ),
                                       column(9,
                                              '-Under the ARIMA tab you will see both a manual and auto selected ARIMA models.'
                                       )
                                     )
                                     
                                     
                            )
                 )
)

server <- function(input, output, session) {
  
calldata <- read.csv('CallofDutyData.csv')
calldata$Month <- ym(calldata$Month)
Calldata <- calldata
Calldata$Month <- yearmonth(calldata$Month)
Calldata <- tsibble(Calldata)

fit_m <- Calldata %>%
  model(MEAN(Call.of.Duty...United.States.))

fit_n <- Calldata %>%
  model(NAIVE(Call.of.Duty...United.States.))

fit_sn <- Calldata %>%
  model(SNAIVE(Call.of.Duty...United.States.))

fit_dr <- Calldata %>%
  model(NAIVE(Call.of.Duty...United.States. ~ drift()))

fit_Holts <- Calldata %>%
  model(ETS(Call.of.Duty...United.States. ~ error("A") + trend("A") + season("N")))

fit_Holwin <- Calldata %>%
  model(
    additive = ETS(Call.of.Duty...United.States. ~ error("A") + trend("A") +season("A")),
    multiplicative = ETS(Call.of.Duty...United.States. ~ error("M") + trend("A") +season("M"))
  )

fit_extra <- Calldata %>%
  model(
    auto_ets = ETS(Call.of.Duty...United.States.),
    manual_ets = ETS(Call.of.Duty...United.States. ~ error("A") + trend("A") + season("A")),
    tslm = TSLM(Call.of.Duty...United.States. ~ trend() + season())
  )

fit_ar1 <- Calldata %>%
  model(ARIMA(Call.of.Duty...United.States.))

fit_ar2 <- Calldata %>%
  model(ARIMA(Call.of.Duty...United.States. ~ pdq(0,1,2) + PDQ(1,1,0)))

  output$plot <- renderPlot({
    switch(
      input$plotType,
      s = gg_season(Calldata),
      a = autoplot(ACF(Calldata)),
      d = Calldata %>%
        model(X_13ARIMA_SEATS(Call.of.Duty...United.States.~x11())) %>%
        components() %>%
        autoplot(),
      d2 = Calldata %>%
        model(X_13ARIMA_SEATS(Call.of.Duty...United.States.~seats())) %>%
        components() %>%
        autoplot(),
      l = plot(calldata, type=input$plotType)
    )
  })
  
  output$plot2 <- renderPlot({
    switch(
      input$plotType2,
      me = fit_m %>%
        forecast(h = 10) %>%
        autoplot(Calldata),
      na = fit_n %>%
        forecast(h = 10) %>%
        autoplot(Calldata),
      sn = fit_sn %>%
        forecast(h = 10) %>%
        autoplot(Calldata),
      dr = fit_dr %>%
        forecast(h = 10) %>%
        autoplot(Calldata)
    )
  })
  
  output$plot3 <- renderPlot({
    switch(
      input$plotType3,
      Holts = fit_Holts %>%
        forecast(h = '5 years') %>%
        autoplot(Calldata),
      Holwin = fit_Holwin %>%
        forecast(h = '5 years') %>%
        autoplot(Calldata),
      Ext = fit_extra %>%
        forecast(h = '5 years') %>%
        autoplot(Calldata)
    )
  })
  
  output$plot4 <- renderPlot({
    switch(
      input$plotType4,
      arima_auto = fit_ar1 %>%
        forecast(h = '5 years') %>%
        autoplot(Calldata),
      arima_man = fit_ar2 %>%
        forecast(h = '5 years') %>%
        autoplot(Calldata)
    )
  })
  
  output$summary <- renderPrint({
    summary(calldata)
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(calldata)
  })
}

shinyApp(ui, server)
