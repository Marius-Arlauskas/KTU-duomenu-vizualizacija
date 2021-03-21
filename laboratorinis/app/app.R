library(shiny)
library(tidyverse)
library(shinythemes)
ui <- fluidPage( theme = shinytheme("darkly"),
                 navbarPage(
                   ("Medienos, statybiniu medziagu ir sanitariniu irenginiu didmenine prekyba"),
                   
                   
                   
                   tabPanel("Imoniu duomenys",
                            
                            sidebarPanel(
                              selectizeInput("imones_pav1", choices = NULL, label = "Imones pavadinimas"),
                              checkboxGroupInput("stulp", "Duomenys")
                            ),
                            
                            mainPanel(
                              tableOutput("table")
                            )
                            
                   ),
                   
                   tabPanel("Atlyginimo ir apdraustuju kaita",
                            
                            sidebarPanel(
                              selectizeInput("imones_pav", choices = NULL, label = "Imones pavadinimas")
                            ),
                            
                            
                            mainPanel(
                              plotOutput("wage"),
                              plotOutput("workers")
                            )
                   )
                 )
)

server <- function(input, output, session) {
  data <- read_csv("https://raw.githubusercontent.com/Marius-Arlauskas/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")
  data <- data %>% mutate("men" = as.integer(substr(month, 5, 6)))
  data <- data %>% filter(ecoActCode==467300)
  
  
  updateSelectizeInput(session, "imones_pav", choices = data$name, server = TRUE)
  updateSelectizeInput(session, "imones_pav1", choices = data$name, server = TRUE)
  
  updateCheckboxGroupInput(session, "stulp",
                           choices = colnames(data),
                           selected = colnames(data)
  )
  
  
  output$table <- renderTable(
    data %>% filter(name == input$imones_pav1) %>%
      select(input$stulp),
    digits = 0
  )
  
  output$wage <- renderPlot(
    data %>%filter(name == input$imones_pav) %>%
      ggplot(aes(x = men, y = avgWage)) +
      geom_line(color = "darkslategrey", size = 1.2) +
      geom_point()+
      scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
      labs(x = 'Menuo', y = 'Vidutinis atlyginimas') +
      theme_linedraw()
  )
  output$workers <- renderPlot(
    data %>%filter(name == input$imones_pav) %>%
      ggplot(aes(x = men, y = numInsured)) +
      geom_line(color = "darkslategrey", size = 1.2) +
      geom_point()+
      scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
      labs(x = 'Menuo', y = 'Apdrausti darbuotojai') +
      theme_linedraw()
  )
  
  
}


shinyApp(ui = ui, server = server)