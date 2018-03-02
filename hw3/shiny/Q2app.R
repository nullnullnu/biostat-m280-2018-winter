#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
setwd(".")
payroll_origin <- 
  read_rds("/home/zhaokezk/Biostat-m280-2018-winter/hw3/payroll_origin.rds")

payroll_origin$totalpay <- as.numeric(gsub("\\$", "", payroll_origin$"Total Payments"))
payroll_origin$basepay <- as.numeric(gsub("\\$", "", payroll_origin$"Base Pay"))
payroll_origin$overtimepay <- as.numeric(gsub("\\$", "", payroll_origin$"Overtime Pay"))
payroll_origin$otherpay <- as.numeric(gsub("\\$", "", payroll_origin$"Other Pay (Payroll Explorer)"))
head(payroll_origin)

payroll <- payroll_origin %>%
  select(year = "Year", total = totalpay, base = basepay , overtime = overtimepay, 
         other = otherpay) 

# Q2 TOTAL PAYROLL
ui <- fluidPage(
   
   # Application title
   titlePanel("Total payroll by LA City"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("Year",
                     "Year:",
                     c(2013:2017))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

