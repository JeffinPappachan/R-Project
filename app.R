library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(forecast)

ui <- dashboardPage(
  dashboardHeader(title = "Shop Monitoring Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-area")),
      menuItem("Operations", tabName = "operations", icon = icon("tools"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales"),
                valueBoxOutput("total_footfall"),
                valueBoxOutput("total_energy")
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Monthly Sales Trend", plotOutput("monthly_sales"), width = 6),
                box(title = "Footfall Trend", plotOutput("monthly_footfall"), width = 6)
              )
      ),
      tabItem(tabName = "table",
              DTOutput("data_table")
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "6-Month Sales Forecast", plotOutput("sales_prediction_plot"), width = 12),
                box(title = "6-Month Footfall Forecast", plotOutput("footfall_prediction_plot"), width = 12),
                box(title = "6-Month Energy Consumption Forecast", plotOutput("energy_prediction_plot"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Read data
  shop_data <- read.csv("shop_data_large.csv")
  
  # Value boxes
  output$total_sales <- renderValueBox({
    valueBox(sum(shop_data$Total_Amount), "Total Sales", icon = icon("dollar-sign"), color = "green")
  })
  output$total_footfall <- renderValueBox({
    valueBox(sum(shop_data$Footfall_Count), "Total Footfall", icon = icon("users"), color = "blue")
  })
  output$total_energy <- renderValueBox({
    valueBox(sum(shop_data$Energy_Consumption), "Total Energy (kWh)", icon = icon("bolt"), color = "yellow")
  })
  
  # Plots for trends
  output$monthly_sales <- renderPlot({
    shop_data %>%
      group_by(Month, Year) %>%
      summarise(Sales = sum(Total_Amount)) %>%
      ggplot(aes(x = factor(Month), y = Sales, fill = factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Sales", title = "Monthly Sales Trend")
  })
  output$monthly_footfall <- renderPlot({
    shop_data %>%
      group_by(Month, Year) %>%
      summarise(Footfall = sum(Footfall_Count)) %>%
      ggplot(aes(x = factor(Month), y = Footfall, fill = factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Footfall", title = "Monthly Footfall Trend")
  })
  
  # Data Table
  output$data_table <- renderDT({
    datatable(shop_data)
  })
  
  # Prediction - sales forecast for next 6 months
  monthly_sales <- shop_data %>%
    group_by(Year, Month) %>%
    summarize(Total_Sales = sum(Total_Amount)) %>%
    arrange(Year, Month)
  sales_ts <- ts(monthly_sales$Total_Sales, start = c(min(monthly_sales$Year), min(monthly_sales$Month)), frequency = 12)
  sales_fit <- auto.arima(sales_ts)
  sales_forecasted <- forecast(sales_fit, h = 6)
  output$sales_prediction_plot <- renderPlot({
    plot(sales_forecasted, main = "6-Month Sales Forecast")
  })
  
  # Prediction - footfall forecast for next 6 months
  monthly_footfall <- shop_data %>%
    group_by(Year, Month) %>%
    summarize(Total_Footfall = sum(Footfall_Count)) %>%
    arrange(Year, Month)
  footfall_ts <- ts(monthly_footfall$Total_Footfall, start = c(min(monthly_footfall$Year), min(monthly_footfall$Month)), frequency = 12)
  footfall_fit <- auto.arima(footfall_ts)
  footfall_forecasted <- forecast(footfall_fit, h = 6)
  output$footfall_prediction_plot <- renderPlot({
    plot(footfall_forecasted, main = "6-Month Footfall Forecast")
  })
  
  # Prediction - energy forecast for next 6 months
  monthly_energy <- shop_data %>%
    group_by(Year, Month) %>%
    summarize(Total_Energy = sum(Energy_Consumption)) %>%
    arrange(Year, Month)
  energy_ts <- ts(monthly_energy$Total_Energy, start = c(min(monthly_energy$Year), min(monthly_energy$Month)), frequency = 12)
  energy_fit <- auto.arima(energy_ts)
  energy_forecasted <- forecast(energy_fit, h = 6)
  output$energy_prediction_plot <- renderPlot({
    plot(energy_forecasted, main = "6-Month Energy Consumption Forecast")
  })
}

shinyApp(ui, server)
