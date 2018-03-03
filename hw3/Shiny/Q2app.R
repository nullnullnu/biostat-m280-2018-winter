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
payroll_origin$cost <- 
  as.numeric(gsub("\\$", "", payroll_origin$"Average Benefit Cost"))
head(payroll_origin)


payroll <- payroll_origin %>%
  select(year = "Year", base = basepay , totalpayments, overtime = overtimepay, 
         other = otherpay, dept = "Department Title", 
         job = "Job Class Title", cost)

# meanq4 <- payroll %>%
#   group_by(dept, year) %>%
#   summarise(meantot = mean(totalpayments, na.rm = TRUE),
#             meanbase = mean(base, na.rm = TRUE),
#             meanover = mean(overtime, na.rm = TRUE),
#             meanother = mean(other, na.rm = TRUE)) %>%
#   select(dept, year, meantot, meanbase, meanover, meanother)

# medq4 <- payroll %>%
#   group_by(dept, year) %>%
#   summarise(medtot = median(totalpayments, na.rm = TRUE),
#             medbase = median(base, na.rm = TRUE),
#             medover = median(overtime, na.rm = TRUE),
#             medother = median(other, na.rm = TRUE)) %>%
#   select(dept, year, medtot, medbase, medover, medother)


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
                    selectInput(inputId = "year3",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank3",
                                label = "Select rank of 
                                the highest paid LA City employee:",
                                choices = c(1:10),
                                selected = 10),
                    
                    submitButton("Submit")
                  ),
                  mainPanel(tableOutput("Q3"))
                )
              ),
              
              ##################### Q4
              tabPanel(
                "Which departments earn the most?",
                titlePanel("Q4 Title"),
                sidebarLayout(
                  sidebarPanel(
                    
                    radioButtons(inputId = "button", 
                                 label =  "Choose mean or median:",
                                 c("mean", "median")),
                    
                    selectInput(inputId = "year4",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank4",
                                label = "Select rank of 
                                the highest earning department:",
                                choices = c(1:5),
                                selected = 5),
                    
                    submitButton("Submit")
                  ),
                  mainPanel = tableOutput("Q4")
                )
                ),
              
              #################### Q5
              tabPanel(
                "Which departments cost most?",
                titlePanel("Q5 Title"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year5",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank5",
                                label = "Select rank of 
                                the departments that cost the most:",
                                choices = c(1:5),
                                selected = 5),
                    
                    submitButton("Submit")
                  ),
                  mainPanel = tableOutput("Q5")
                )
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
      group_by(job) %>%
      filter(year == input$year3) %>%
      arrange(desc(totalpayments)) %>%
      head(input$rank3) %>%
      select(job, dept, totalpayments, base, overtime, other) 
  })
  
  ################04
  output$Q4 <-renderTable({
    if(input$button == "mean") {
      meanq4 <- payroll %>%
        filter(year == input$year4) %>%
        group_by(dept) %>%
        summarise(meantot = mean(totalpayments, na.rm = TRUE),
                  meanbase = mean(base, na.rm = TRUE),
                  meanover = mean(overtime, na.rm = TRUE),
                  meanother = mean(other, na.rm = TRUE)) %>%
        arrange(desc(meantot)) %>%
        head(input$rank4) %>%
        select(dept, meantot, meanbase, meanover, meanother)
    } 
    else {
      medq4 <- payroll %>%
        filter(year == input$year4) %>%
        group_by(dept) %>%
        summarise(medtot = median(totalpayments, na.rm = TRUE),
                  medbase = median(base, na.rm = TRUE),
                  medover = median(overtime, na.rm = TRUE),
                  medother = median(other, na.rm = TRUE)) %>%
        head(input$rank4) %>%
        select(dept, medtot, medbase, medover, medother)
    }
  })
  
  ################Q5
  output$Q5 <- renderTable({
    payroll %>%
      filter(year == input$year5) %>%
      group_by(dept) %>%
      arrange(desc(cost)) %>%
      head(input$rank5) %>%
      select(dept, cost, totalpayments, base, overtime, other) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

