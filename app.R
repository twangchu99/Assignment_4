library(shiny)
library(devtools)

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") # Goals:

library(shiny)
library(tidyverse) 
library(DT)        
library(bslib)    

states <- covid19 %>% 
          distinct(state) %>%
          arrange(state) %>%
          pull(state)

# Define UI for application 
ui <- fluidPage(
  theme = bs_theme(primary = "#123B60", 
                   secondary = "#D44420", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "sandstone"),
  # Application title
  titlePanel("Covid-19 Cases Across the U.S."),
  
  # Sidebar with inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "state", # to use in code
                  label = "States:", # how it looks in UI
                  choices = states, 
                  selected = ""
      )
    ),
    
    # Show a plot of cumulative weight for chosen vegetable
    # Show a table beneath
    mainPanel(
      plotOutput(outputId = "cum_cases"),
      dataTableOutput(outputId = "cum_cases_tbl")
    )
  )
)

# Define server logic 
server <- function(input, output) {

# Enclose in reactive() - makes a function
  covid_smry <- reactive(covid19 %>% 
                         filter(state == input$state) %>%
                         group_by(date) %>% 
                         summarize(daily_cases = sum(cases)) %>%
                         mutate(cum_cases = cumsum(daily_cases)))
  
  # Now use that function, with no arguments.
  output$cum_cases <- renderPlot({
    covid_smry() %>% 
      ggplot(aes(x = date, y = cum_cases)) +
      geom_line() +
      labs(title = paste("Cumulative cases of COVID-19", input$cases),
           x = "",
           y = "") +
      theme_minimal()
  })
  
  output$cum_cases_tbl <- renderDataTable(covid_smry())
  
}

# Run the application 
shinyApp(ui = ui, server = server)


