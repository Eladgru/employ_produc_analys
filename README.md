#design by Ela Doğru
# employ_produc_analys
install.packages("readx1")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("shiny")
install.packages("lubridate")
install.packages("DT")


library(readxl)
library(ggplot2)
library(dplyr)
library(shiny)
library(lubridate)

# Read the Excel file
excel_data <- read_excel("C:/Users/eladg/Desktop/employee-production-analysis-data.xlsx", sheet = 1)

# Convert WorkingDate column to Date format
excel_data$WorkingDate <- as.Date(excel_data$WorkingDate, format = "%d.%m.%Y")
#bir dönüşüm satır 21-52 eski version
# Find the nearest Monday before the minimum date
start_date <- min(excel_data$WorkingDate) - (wday(min(excel_data$WorkingDate)) - 2) %% 7

# Define UI
ui <- fluidPage(
  div(
    h2(HTML("<strong>Employee Production Analysis</strong>")),
    HTML("<p style='font-size: 14px; margin-bottom: 0;'>The employees are working from April 16th to October 22nd in 2018</p>")
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("employee", "Select Employee", choices = unique(excel_data$Employee)),
      dateRangeInput("dateRange", "Select Date Range", 
                     start = min(excel_data$WorkingDate), end = max(excel_data$WorkingDate)),
      actionButton("showActivity", "Show Employee Activity", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Employee Production", dataTableOutput("employeeTable")),
        tabPanel("Working Days", dataTableOutput("workingDaysTable")),
        #tabPanel("New Hires and Quits", dataTableOutput("newHiresQuitsTable")),
        tabPanel("Average Products per Day", dataTableOutput("averageProductsTable")),
        tabPanel("Employee Activity", plotOutput("activityPlot"))
      )
    )
  )
)
# Define server
server <- function(input, output) {
  
  filtered_data <- reactive({
    excel_data %>%
      filter(Employee == input$employee, 
             WorkingDate >= input$dateRange[1] & WorkingDate <= input$dateRange[2])
  })
  output$employeeTable <- renderDataTable({
    # Employee production table
    aggregate(Product ~ Employee, filtered_data(), sum)
  })
  
  output$workingDaysTable <- renderDataTable({
    # Working days table
    filtered_data() %>%
      group_by(WorkingDate) %>%
      summarise(Worked = n_distinct(Employee))
  })
  
  #output$newHiresQuitsTable <- renderDataTable({
    # New hires and quits table
   # data <- filtered_data() %>%
    #  group_by(WorkingDate) %>%
     # summarise(NewHires = sum(Employee == first(Employee)), 
      #          Quits = sum(Employee == last(Employee)))
    #data$WorkingDate[data$NewHires == 0] <- "Quit"
    #data
  #})
  
  output$averageProductsTable <- renderDataTable({
    # Average products per day table
    filtered_data() %>%
      group_by(WorkingDate) %>%
      summarise(AverageProducts = mean(Product))
  })
  
  output$activityPlot <- renderPlot({
    # Employee activity plot
    employee_data <- filtered_data()
    employee_data$WorkingDate <- as.Date(employee_data$WorkingDate, format = "%d.%m.%Y")  # Convert to Date class
    plot_data <- employee_data %>%
      group_by(WorkingDate) %>%
      summarise(Worked = n_distinct(Employee))
    
    # Identify the days of absence
    absence_days <- setdiff(seq.Date(input$dateRange[1], input$dateRange[2], by = "day"), plot_data$WorkingDate)
    
    # Create a separate data frame for absence days if there are any
    if (length(absence_days) > 0) {
      absence_data <- data.frame(WorkingDate = absence_days, Worked = 0)
      plot_data <- rbind(plot_data, absence_data)
    }
    
    ggplot(plot_data, aes(x = WorkingDate, y = Worked)) +
      geom_point(size = 5, shape = 21, fill = ifelse(plot_data$Worked == 0, "red", "green"), color = "black") +
      labs(x = "Working Date", y = "Employee Activity", title = "Employee Activity") +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
  })
}


output$activityPlot <- renderPlot({
  # Employee activity plot
  employee_data <- filtered_data()
  employee_data$WorkingDate <- as.Date(employee_data$WorkingDate, format = "%d.%m.%Y")  # Convert to Date class
  plot_data <- employee_data %>%
    group_by(WorkingDate) %>%
    summarise(Worked = n_distinct(Employee))
  
  # Identify the days of absence
  absence_days <- setdiff(seq.Date(input$dateRange[1], input$dateRange[2], by = "day"), plot_data$WorkingDate)
  
  # Create a separate data frame for absence days if there are any
  if (length(absence_days) > 0) {
    absence_data <- data.frame(WorkingDate = absence_days, Worked = 0)
    plot_data <- rbind(plot_data, absence_data)
  }
  
  # Calculate the excess number of products for each working day
  excess_products <- employee_data %>%
    group_by(WorkingDate) %>%
    summarise(ExcessProducts = sum(Product) - mean(Product))
  
  # Merge excess_products with plot_data
  plot_data <- left_join(plot_data, excess_products, by = "WorkingDate")
  
  # Plot the activity and boxplot
  ggplot(plot_data, aes(x = WorkingDate, y = Worked)) +
    geom_boxplot(size = 5, shape = 21, fill = ifelse(plot_data$Worked == 0, "red", "green"), color = "black") +
    labs(x = "Working Date", y = "Employee Activity", title = "Employee Activity") +
    theme_minimal() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))
})

# Run the Shiny app
shinyApp(ui, server)

