library(shiny)
library(ggplot2)
library(dplyr)

ecg <- read.csv("ecg_sample.csv", stringsAsFactors = FALSE)

ecg$Time <- ecg$Time - min(ecg$Time)              

ui <- fluidPage(
  titlePanel("Find good subset"),
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("Initial_Seconds", "Initial_Seconds", min = min(ecg$Time/1000), max = max(ecg$Time/1000),
                  value = c(min(ecg$Time/1000))),
      sliderInput("Seconds_wide", "MilliSeconds_wide", 0, 500000,
                  value = c(20000)),
      numericInput("Detrending_Parameter", "Detrending_Parameter", 3, min = 1, max = 20),
      actionButton("do", "Create File")
    ),
    mainPanel(
      "plots",
      # fluidRow(
      #   splitLayout(cellWidths = c("50%", "50%"), plotOutput("Detrended_plot1"),plotOutput("Regular_plot2"))
      # )
      
      tabsetPanel(
        tabPanel("Plots", plotOutput("Detrended_plot1"),plotOutput("Regular_plot2"))
        
      )
      
    )
  )
)

wide_data = function(ini,wide){
  return(ini+wide)
}

server <- function(input, output) {
  
  output$Detrended_plot1 <- renderPlot({
    newdata <- wide_data(input$Initial_Seconds*1000,input$Seconds_wide)
    training_data <- ecg[ecg$Time >= input$Initial_Seconds*1000 & ecg$Time<newdata ,"ec"]
    
    # de-trend the measurements
    
    # define "region of interest" for centered sliding window
    #ROI <- 8:(length(training_data)-7)
    #det_par+1
    ROI <- (input$Detrending_Parameter+1):(length(training_data)-input$Detrending_Parameter)
    
    # take the median value of a 15-width window
    med15 <- sapply(ROI, function(i){
      
      #median(training_data[(i-7):(i+7)]) #Old line
       median(training_data[(i-input$Detrending_Parameter):(i+input$Detrending_Parameter)]) #new line
    })
    
    training_data_detrended <- training_data[ROI] - med15
    
    plot(training_data_detrended, type='l')
    
    output$Regular_plot2 <- renderPlot({
      
      plot(training_data, type='l')
      observeEvent(input$do, {
        write.csv(training_data,"subset.csv")
      })
       
    }) 
    
    
  })
  
}


shinyApp(ui = ui, server = server)

