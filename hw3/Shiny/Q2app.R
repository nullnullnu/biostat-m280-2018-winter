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

ui <- fluidPage(
  
  tabsetPanel(type = "tabs",
              
              ##################### Q2 
              tabPanel(
                "Total payroll by LA City", 
                titlePanel("Different types of pay in each year"),
                mainPanel(plotOutput("Q2"))
              ),
              
              ##################### Q3
              tabPanel(
                "Who earned the most?",
                titlePanel("Top paid LA City employees by year"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year3",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank3",
                                label = "Select rank:",
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
                titlePanel("Top earning departments"),
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
                                label = "Select rank:",
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
                titlePanel("Top expensive departments"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "year5",
                                label = "Select year:",
                                choices = c(2013:2017),
                                selected = 2017),
                    
                    selectInput(inputId = "rank5",
                                label = "Select rank:",
                                choices = c(1:5),
                                selected = 5),
                    
                    submitButton("Submit")
                  ),
                  mainPanel = tableOutput("Q5")
                )
              ),
              
              #################### Q6
              tabPanel("Which departments offer the highest bonus pay?",
                       titlePanel("Departments with the top bonus pay"),
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(
                             inputId = "year6",
                             label = "Select year:",
                             choices = c(2013:2017),
                             selected = 2017),
                           
                           selectInput(inputId = "rank6",
                                       label = "Select rank:",
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
  
  ##########read in data and tidy
  convert("/home/m280-data/la_payroll/LA_City_Employee_Payroll.csv",
          "payroll_origin.rds")
  payroll_origin <- read_rds("payroll_origin.rds")
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
    select(year = "Year", base = basepay , totalpayments, 
           overtime = overtimepay, other = otherpay, dept = "Department Title", 
           job = "Job Class Title", cost, bonus)
  
  #############Q2
  output$Q2 <- renderPlot({
    payroll %>%
      select(year, base, overtime, other) %>%
      group_by(year) %>%
      summarise(
        totbase = sum(base, na.rm = TRUE),
        totover = sum(overtime, na.rm = TRUE),
        totother = sum(other, na.rm = TRUE)) %>%
      gather(totbase, totover, totother, 
             key = "paytype", value = "payamount") %>%
      ggplot(mapping = aes(x = year, y = payamount, fill = paytype)) +
      geom_col() +
      scale_y_continuous(labels = scales::dollar_format("$")) +
      labs(x = "Year", y = "Total Pay by LA City") +
      scale_fill_discrete(name = "Type of Pay",
                          labels = c("Base Pay ($)", "Other Pay ($)", "Overtime Pay ($)"))
  })
  
  
  ################Q3
  output$Q3 <- renderTable({
    payroll %>%
      group_by(job) %>%
      filter(year == input$year3) %>%
      arrange(desc(totalpayments)) %>%
      head(input$rank3) %>%
      select("Job Type" = job, "Department" = dept, 
             "Total Payments ($)" = totalpayments, "Base Pay ($)" = base, 
             "Overtime Pay ($)" = overtime, "Other Pay ($)" = other) 
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
        select("Department" = dept, "Mean Total Payments ($)" = meantot, 
               "Mean Base Pay ($)" = meanbase, "Mean Overtime Pay ($)" = meanover, 
               "Mean Other Pay ($)" = meanother)
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
        select("Department" = dept, "Median Total Payments ($)" = medtot, 
               "Median Base Pay ($)" = medbase, "Median Overtime Pay ($)" = medover, 
               "Median Other Pay ($)" = medother)
    }
  })
  
  ################Q5
  output$Q5 <- renderTable({
    payroll %>%
      filter(year == input$year5) %>%
      group_by(dept) %>%
      summarise(totcost = sum(cost, na.rm = TRUE),
                totpay = sum(totalpayments, na.rm = TRUE),
                totbase = sum(base, na.rm = TRUE),
                totover = sum(overtime, na.rm = TRUE),
                totother = sum(other, na.rm = TRUE)) %>%
      arrange(desc(totcost)) %>%
      select("Department" = dept, "Total Cost ($)" = totcost, 
             "Total Payments ($)" = totpay, "Total Base Pay ($)" = totbase, 
             "Total Overtime Pay ($)" = totover, "Total Other Pay ($)" = totother) %>%
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
      select("Department" = dept, "Bonus Pay ($)" = totbonus)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

