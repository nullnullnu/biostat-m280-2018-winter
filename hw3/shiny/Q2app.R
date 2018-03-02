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
         
        selectInput(inputId = "type",
                    label = "Choose a payroll type:",
                    choices = c("Base Pay", "Overtime Pay", "Other Pay"),
                    selected = "Base Pay")
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(plotOutput("Q2"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$Q2 <- renderPlot({
    payroll$type <- switch(input$type,
                           "Base Pay" = payroll$base,
                           "Overtime Pay" = payroll$overtime,
                           "Other Pay" = payroll$other)
      payroll %>%
        select(year, type) %>%
        group_by(year) %>%
        summarise(total = sum(type)) %>%
        ggplot(mapping = aes(x = year, y = total)) +
        geom_col()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

