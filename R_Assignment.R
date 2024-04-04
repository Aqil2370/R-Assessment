<<<<<<< HEAD
library(shiny)
library(readxl)
library(ggplot2)

# Define the UI
=======
# Load the required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define the user interface
>>>>>>> 53b8d5f6eac1804d61b50582fe76e23423160745
ui <- fluidPage(
  titlePanel("Cumulative Paid Claims"),
  sidebarLayout(
    sidebarPanel(
<<<<<<< HEAD
      fileInput("file", "Input claims data file (.xlxs)", accept = c(".xlsx")),
      sliderInput("tailFactor", "Tail Factor", 
                  min = 0, 
                  max = 2, 
                  value = 1.1, 
                  step = 0.1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Imported Data", tableOutput("importedData")),
        tabPanel("Cumulative Paid Claims ", tableOutput("claims")),
        tabPanel("Plot", plotOutput("plot"))
      )
=======
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
>>>>>>> 53b8d5f6eac1804d61b50582fe76e23423160745
    )
  )
)

# Define the server logic
<<<<<<< HEAD
server <- function(input, output, session){
  # Read the Excel file
  imported_data <- reactive({
    # To ensure the file input exist
    req(input$file)
    filepath_1 <- read_excel(input$file$datapath, range = "Sheet1!A1:C7")
    filepath_1
  })
  
  claims_paid <- reactive({
    filepath_2 <- imported_data()
    num_loss_year <- length(unique(filepath_2$'Loss Year'))
    num_develop_year <- max(filepath_2$'Development Year')
    loss_year <- sort(unique(filepath_2$'Loss Year'))
    develop_year <- c(1:num_develop_year)
    
    cum_claims_1 <- matrix(0, nrow = length(loss_year), ncol = length(develop_year),
                           byrow = T, dimnames = list(loss_year, develop_year))
    
    for(i in 1:(nrow(filepath_2))){
      loss_yr <- as.character(filepath_2[i, "Loss Year"])
      develop_yr <- as.character(filepath_2[i, "Development Year"])
      cum_claims_1[loss_yr, develop_yr] <- as.double(filepath_2[i, "Amount of Claims Paid"])
    }
    
    cumulative <- apply(cum_claims_1, 1, cumsum)
    cum_claims_2 <- t(cumulative)
    
    j <- num_develop_year
    dev_factor <- numeric(j)
    for(k in 1:(j-1)){
      dev_1 <- sum(cum_claims_2[1:(j-k), k+1])
      dev_2 <- sum(cum_claims_2[1:(j-k), k])
      dev_factor[k] <- dev_1/dev_2
    }
    dev_factor[j] <- input$tailFactor
    
    cum_claims_final <- cbind(cum_claims_2, "4" = rep(0, num_loss_year))
    
    for(m in 1:j){
      cum_claims_final[(j-m+1):j, m+1] <- round(cum_claims_final[(j-m+1):j, m]*dev_factor[m] )
    }
    # Remove print() from colnames() and rownames()
    colnames(cum_claims_final) <- colnames(cum_claims_final)
    rownames(cum_claims_final) <- rownames(cum_claims_final)
    cum_claims_final
  })
  
  
  line_graph <- reactive({
    cum_claims_final <- claims_paid()
    filepath_3 <- imported_data()
    num_loss_year <- length(unique(filepath_3$'Loss Year'))
    num_develop_year <- max(filepath_3$'Development Year')
    loss_year <- sort(unique(filepath_3$'Loss Year'))
    develop_year <- c(1:num_develop_year)
    
    LossYear <- rep(rownames(cum_claims_final), ncol(cum_claims_final))
    DevYear <- rep(1:(ncol(cum_claims_final)), each = nrow(cum_claims_final))
    PaidAmount <- round(as.vector(cum_claims_final))
    
    df_4 <- data.frame(LossYear, DevYear, PaidAmount)
    ggplot(df_4, aes(x = DevYear, y = PaidAmount, 
                     group = LossYear, color = LossYear)) +
      geom_line(size = 1) +
      geom_point(size = 2.2) +
      labs(title = "Plot of Cumulative Paid Claims", 
           x = "Development Year",
           y = "Paid Amount ($)") +
      geom_text(aes(label = paste("$", PaidAmount)), size = 4.5, color = "black",
                vjust = -0.6, check_overlap = TRUE) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 13))
  })
  
  output$importedData <- renderTable({
    imported_data()
  }, align = "c", digits = 0)
  
  output$claims <- renderTable({
    claims_paid()
  }, rownames = TRUE, align = "c", digits = 0)
  
  output$plot <- renderPlot({
    line_graph()
  }, width = 800, height = 460)
  
}

# Run the app
shinyApp(ui, server)
=======
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
>>>>>>> 53b8d5f6eac1804d61b50582fe76e23423160745
