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
  read_rds("/home/zhaokezk/Biostat-m280-2018-winter/hw3/Shiny/payroll_origin.rds")

payroll_origin$totalpayments <- 
  as.numeric(gsub("\\$", "", payroll_origin$"Total Payments"))
payroll_origin$basepay <- 
  as.numeric(gsub("\\$", "", payroll_origin$"Base Pay"))
payroll_origin$overtimepay <- 
  as.numeric(gsub("\\$", "", payroll_origin$"Overtime Pay"))
payroll_origin$otherpay <- 
  as.numeric(gsub("\\$", "", payroll_origin$"Other Pay (Payroll Explorer)"))
head(payroll_origin)

payroll <- payroll_origin %>%
  select(year = "Year", base = basepay , totalpayments, overtime = overtimepay, 
         other = otherpay, dept = "Department Title", job = "Job Class Title") 

ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              
              ##################### Q2 
              tabPanel(
                "Total payroll by LA City", 
                titlePanel("Q2 Title"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "type",
                                label = "Select payroll type:",
                                choices = 
                                  c("Base Pay", "Overtime Pay", "Other Pay"),
                                selected = "Base Pay")
                  ),
                  mainPanel = plotOutput("Q2")
                )
                       ),
              
              ##################### Q3
              tabPanel(
                "Who earned the most?",
                titlePanel("Q3 Title"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    sliderInput(inputId = "rank",
                                 label = "Select rank of 
                                the highest paid LA City employee:",
                                 min = 1,
                                 max = 10,
                                 value = 10)
                  ),
                  mainPanel = plotOutput("Q3")
                )
                ),
              
              ##################### Q4
              tabPanel(
                "Which departments earn most?",
                titlePanel("Q4 Title"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year4",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank4",
                                label = "Select rank of 
                                the highest earning department:",
                                choices = c(1:5),
                                selected = 5)
                  ),
                  mainPanel = tableOutput("Q4")
                )
              ),
              
              #################### Q5
              tabPanel(
                "Which departments cost most?",
                titlePanel("Q5 Title")
              ),
              
              #################### Q6
              tabPanel("Visualize other info",
              titlePanel("Q6 Title")
              )
  )
 )
   


  ############### Define server
  server <- function(input, output) {
       
    #############Q2
    output$Q2 <- renderPlot({
      payroll$type <- switch(input$type, 
                             "Base Pay" = payroll$base,
                             "Overtime Pay" = payroll$overtime,
                             "Other Pay" = payroll$other)
      payroll %>%
        select(year, type) %>%
        group_by(year) %>%
        summarise(total = sum(type, na.rm = TRUE)) %>%
        ggplot(aes(x = year, y = total)) +
        geom_col() +
        scale_y_continuous(labels = scales::dollar_format("$")) +
        labs(x = "Year", y = "Total Pay by LA City")
       })
    
    
    ################Q3
    output$Q3 <- renderTable({
      payroll %>%
        select(job, dept, totalpayments, base, overtime, other, year) %>%
        filter(year == input$year) %>%
        arrange(desc(totalpayments)) %>%
        head(input$rank) 
      
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

