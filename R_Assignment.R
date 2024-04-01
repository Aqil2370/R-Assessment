# Load the required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define the user interface
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims"),
  sidebarLayout(
    sidebarPanel(
      textInput("y2017_1", "2017 - Development Year 1: ", ""),
      textInput("y2017_2", "2017 - Development Year 2: ", ""),
      textInput("y2017_3", "2017 - Development Year 3: ", ""),
      textInput("y2018_1", "2018 - Development Year 1: ", ""),
      textInput("y2018_2", "2018 - Development Year 2: ", ""),
      textInput("y2019_1", "2019 - Development Year 1: ", ""),
      sliderInput("Tail_factor", "Tail factor", min = 0, max = 10, step = 0.1, value = 1.1)
    ),
    mainPanel(
      tableOutput("claims_table"),
      tableOutput("cumulative_table"),
      plotOutput("cumulative_plot")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  cumulative_table <- reactive({
    claims_data <- data.frame(
      Year = c(2017, 2018, 2019),
      Development_Year_1 = c(as.numeric(input$y2017_1), 
                             as.numeric(input$y2018_1), 
                             as.numeric(input$y2019_1)
      ),
      Development_Year_2 = c(
        as.numeric(input$y2017_1) + as.numeric(input$y2017_2),
        as.numeric(input$y2018_1) + as.numeric(input$y2018_2),
        NA
      ),
      Development_Year_3 = c(
        as.numeric(input$y2017_1) + as.numeric(input$y2017_2) + as.numeric(input$y2017_3),
        as.numeric(input$y2018_1) + as.numeric(input$y2018_2),
        NA
      ),
      Development_Year_4 = c(
        as.numeric(input$y2017_1) + as.numeric(input$y2017_2) + as.numeric(input$y2017_3),
        as.numeric(input$y2018_1) + as.numeric(input$y2018_2),
        NA
      )
    )
    
    # Applying Tail factor
    claims_data$Development_Year_4 <- claims_data$Development_Year_3 * input$Tail_factor
    
    # Applying Tail factor for 2018 Development Year 3
    claims_data$Development_Year_3[2] <- (claims_data$Development_Year_2[2] * claims_data$Development_Year_3[1]) / claims_data$Development_Year_2[1]
    
    # Applying Tail factor 2018
    claims_data$Development_Year_4[2] <- claims_data$Development_Year_3[2] * input$Tail_factor
    
    # Calculating 2019 Development Year 1
    claims_data$Development_Year_1[3] <- as.numeric(input$y2019_1)
    
    # Calculating 2019 Development Year 2
    claims_data$Development_Year_2[3] <- ((claims_data$Development_Year_2[1] + claims_data$Development_Year_2[2]) / 
                                            (claims_data$Development_Year_1[1] + claims_data$Development_Year_1[2])) * claims_data$Development_Year_1[3]
    
    # Calculating 2019 Development Year 3
    claims_data$Development_Year_3[3] <- (claims_data$Development_Year_2[3] * claims_data$Development_Year_3[1]) / claims_data$Development_Year_2[1]
    
    # Calculating 2019 Development Year 4
    claims_data$Development_Year_4[3] <- claims_data$Development_Year_3[3] * input$Tail_factor
    
    claims_data
  })
  
  output$claims_table <- renderTable({
    claims_data <- data.frame(
      Year = c(rep(2017, 3), rep(2018, 2), 2019),
      Development_Year = c(1, 2, 3, 1, 2, 1),
      Amount_of_claim = c(
        as.numeric(input$y2017_1),
        as.numeric(input$y2017_2),
        as.numeric(input$y2017_3),
        as.numeric(input$y2018_1),
        as.numeric(input$y2018_2),
        as.numeric(input$y2019_1)
      )
    )
    claims_data
  })
  
  output$cumulative_table <- renderTable({
    cumulative_table()
  })
  
  output$cumulative_plot <- renderPlot({
    cumulative_data <- cumulative_table()
    melt_data <- reshape2::melt(cumulative_data, id.vars = "Year", variable.name = "Development_Year", value.name = "Cumulative_Paid_Claims")
    
    ggplot(data = melt_data, aes(x = Development_Year, y = Cumulative_Paid_Claims, group = Year, color = as.factor(Year))) +
      geom_line() +
      labs(title = "Cumulative Paid Claims", x = "Development Year", y = "Cumulative Paid Claims") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui, server)
