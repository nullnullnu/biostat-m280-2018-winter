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
payroll_origin$bonus <-
  as.numeric(gsub("\\$", "", payroll_origin$"Permanent Bonus Pay"))
head(payroll_origin)


payroll <- payroll_origin %>%
  select(year = "Year", base = basepay , totalpayments, overtime = overtimepay, 
         other = otherpay, dept = "Department Title", 
         job = "Job Class Title", cost, bonus)

ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              
              ##################### Q2 
              tabPanel(
                "Total payroll by LA City", 
                titlePanel("Total payroll by LA City"),
                mainPanel(plotOutput("Q2"))
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
              tabPanel("Which departments offer the highest permanent bonus pay?",
                       titlePanel("Q6 Title"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "year6",
                             label = "Select year:",
                             choices = c(2013:2017),
                             selected = 2017),
                           
                           selectInput(inputId = "rank6",
                                       label = "Select the rank:",
                                       choices = c(1:5),
                                       selected = 5),
                           
                           submitButton("Submit")
                         ),
                         mainPanel = tableOutput("Q6")
                       )
              )
              )
  )


############### Define server
server <- function(input, output) {
  
  #############Q2
  output$Q2 <- renderPlot({
    payroll %>%
      select(year, base, overtime, other) %>%
      group_by(year) %>%
      summarise(
        totbase = sum(base, na.rm = TRUE),
        totover = sum(overtime, na.rm = TRUE),
        totother = sum(other, na.rm = TRUE)) %>%
      gather(totbase, totover, totother, key = "paytype", value = "payamount") %>%
      ggplot(mapping = aes(x = year, y = payamount, fill = paytype)) +
      geom_col() +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay by LA City") +
      scale_fill_discrete(name = "Type of Pay",
                          labels = c("Base Pay", "Other Pay", "Overtime Pay"))
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
        arrange(desc(medtot)) %>%
        head(input$rank4) %>%
        select(dept, medtot, medbase, medover, medother)
    }
  })
  
  ################Q5
  output$Q5 <- renderTable({
    payroll %>%
      filter(year == input$year5) %>%
      group_by(dept) %>%
      summarise(totcost = sum(cost, na.rm = TRUE),
                totpay = sum(totalpayments, na.rm =TRUE),
                totbase = sum(base, na.rm = TRUE),
                totover = sum(overtime, na.rm = TRUE),
                totother = sum(other, na.rm = TRUE)) %>%
      arrange(desc(totcost)) %>%
      select(dept, totcost, totpay, totbase, totover, totother) %>%
      head(input$rank5)
  })
  
  ###############Q6
  output$Q6 <- renderTable({
    payroll %>%
      filter(year == input$year6) %>%
      group_by(dept) %>%
      summarise(totbonus = sum(bonus, na.rm = TRUE)) %>%
      arrange(desc(totbonus)) %>%
      head(input$rank6) %>%
      select("Department" = dept, "Bonus Pay" = totbonus)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

