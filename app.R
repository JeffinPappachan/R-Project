library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Shop Monitoring Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Data Table", tabName = "table", icon = icon("table")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-area"))
    ),
    # Filtering widgets
    hr(),
    sliderInput("year_filter", "Select Year:", min = 2023, max = 2025, value = c(2023, 2025), sep = ""),
    sliderInput("month_filter", "Select Month:", min = 1, max = 12, value = c(1, 12)),
    checkboxGroupInput("category_filter", "Select Category (optional):", choices = c("All"), selected = "All")
    # Add more filters as needed for demographics etc.
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_sales"),
                valueBoxOutput("total_footfall"),
                valueBoxOutput("total_energy")
              ),
              fluidRow(
                box(title = "Summary Statistics (Filtered)", width = 12, htmlOutput("summary_stats"))
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                box(title = "Monthly Sales Trend", plotlyOutput("monthly_sales"), width = 6),
                box(title = "Footfall Trend", plotlyOutput("monthly_footfall"), width = 6)
              )
      ),
      tabItem(tabName = "table",
              DTOutput("data_table")
      ),
      tabItem(tabName = "prediction",
              fluidRow(
                box(title = "6-Month Sales Forecast", plotlyOutput("sales_prediction_plot"), width = 12),
                box(title = "6-Month Footfall Forecast", plotlyOutput("footfall_prediction_plot"), width = 12),
                box(title = "6-Month Energy Consumption Forecast", plotlyOutput("energy_prediction_plot"), width = 12)
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # Read data
  shop_data <- read.csv("shop_data_large.csv")
  
  # Apply reactive filtering
  filtered_data <- reactive({
    df <- shop_data %>%
      filter(Year >= input$year_filter[1], Year <= input$year_filter[2],
             Month >= input$month_filter[1], Month <= input$month_filter[2])
    df
  })
  
  # Value boxes (on filtered data)
  output$total_sales <- renderValueBox({
    valueBox(sum(filtered_data()$Total_Amount), "Total Sales (Filtered)", icon = icon("dollar-sign"), color = "green")
  })
  output$total_footfall <- renderValueBox({
    valueBox(sum(filtered_data()$Footfall_Count), "Total Footfall (Filtered)", icon = icon("users"), color = "blue")
  })
  output$total_energy <- renderValueBox({
    valueBox(sum(filtered_data()$Energy_Consumption), "Total Energy (kWh, Filtered)", icon = icon("bolt"), color = "yellow")
  })
  
  # Summary statistics
  output$summary_stats <- renderUI({
    df <- filtered_data()
    total <- nrow(shop_data)
    filtered <- nrow(df)
    HTML(paste0("Showing <b>", filtered, "</b> of <b>", total, "</b> records."))
  })
  
  # Interactive Plotly/ggplotly plots
  output$monthly_sales <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Month, Year) %>%
      summarise(Sales = sum(Total_Amount)) %>%
      ggplot(aes(x = factor(Month), y = Sales, fill = factor(Year), text = paste("Month:", Month, "<br>Year:", Year, "<br>Sales:", Sales))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Sales", title = "Monthly Sales Trend") +
      theme_minimal(base_size = 15)
    ggplotly(p, tooltip = "text")
  })
  
  output$monthly_footfall <- renderPlotly({
    p <- filtered_data() %>%
      group_by(Month, Year) %>%
      summarise(Footfall = sum(Footfall_Count)) %>%
      ggplot(aes(x = factor(Month), y = Footfall, fill = factor(Year), text = paste("Month:", Month, "<br>Year:", Year, "<br>Footfall:", Footfall))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Month", y = "Footfall", title = "Monthly Footfall Trend") +
      theme_minimal(base_size = 15)
    ggplotly(p, tooltip = "text")
  })
  
  output$data_table <- renderDT({
    datatable(filtered_data())
  })
  
  # Prediction with static ARIMA for demo (can be replaced with LSTM/torch as discussed earlier)
  monthly_sales <- reactive({
    filtered_data() %>%
      group_by(Year, Month) %>%
      summarise(Total_Sales = sum(Total_Amount)) %>%
      arrange(Year, Month)
  })
  output$sales_prediction_plot <- renderPlotly({
    ms <- monthly_sales()
    if (nrow(ms) < 24) return(NULL)
    sales_ts <- ts(ms$Total_Sales, start = c(min(ms$Year), min(ms$Month)), frequency = 12)
    sales_fit <- forecast::auto.arima(sales_ts)
    sales_forecasted <- forecast::forecast(sales_fit, h = 6)
    p <- autoplot(sales_forecasted) + labs(title = "6-Month Sales Forecast") + theme_minimal(base_size = 15)
    ggplotly(p)
  })
  
  monthly_footfall <- reactive({
    filtered_data() %>%
      group_by(Year, Month) %>%
      summarise(Total_Footfall = sum(Footfall_Count)) %>%
      arrange(Year, Month)
  })
  output$footfall_prediction_plot <- renderPlotly({
    mf <- monthly_footfall()
    if (nrow(mf) < 24) return(NULL)
    footfall_ts <- ts(mf$Total_Footfall, start = c(min(mf$Year), min(mf$Month)), frequency = 12)
    footfall_fit <- forecast::auto.arima(footfall_ts)
    footfall_forecasted <- forecast::forecast(footfall_fit, h = 6)
    p <- autoplot(footfall_forecasted) + labs(title = "6-Month Footfall Forecast") + theme_minimal(base_size = 15)
    ggplotly(p)
  })
  
  monthly_energy <- reactive({
    filtered_data() %>%
      group_by(Year, Month) %>%
      summarise(Total_Energy = sum(Energy_Consumption)) %>%
      arrange(Year, Month)
  })
  output$energy_prediction_plot <- renderPlotly({
    me <- monthly_energy()
    if (nrow(me) < 24) return(NULL)
    energy_ts <- ts(me$Total_Energy, start = c(min(me$Year), min(me$Month)), frequency = 12)
    energy_fit <- forecast::auto.arima(energy_ts)
    energy_forecasted <- forecast::forecast(energy_fit, h = 6)
    p <- autoplot(energy_forecasted) + labs(title = "6-Month Energy Consumption Forecast") + theme_minimal(base_size = 15)
    ggplotly(p)
  })
}

shinyApp(ui, server)
