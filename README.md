# 🛍️ Shop Monitoring Dashboard

This Shiny dashboard provides an interactive interface to monitor and forecast key metrics for a retail shop, including sales, footfall, and energy consumption. It visualizes trends, displays raw data, and generates 6-month forecasts using time series models.

## 📊 Features

- **Overview Tab**: Displays total sales, footfall, and energy consumption using value boxes.
- **Trends Tab**: Visualizes monthly trends for sales and footfall.
- **Data Table Tab**: Shows the raw dataset in an interactive table.
- **Prediction Tab**: Forecasts sales, footfall, and energy consumption for the next 6 months using ARIMA models.

## 📁 Dataset

The app uses a CSV file named `shop_data_large.csv` with the following columns:

- `Year`: Year of the record
- `Month`: Month of the record
- `Total_Amount`: Total sales amount
- `Footfall_Count`: Number of visitors
- `Energy_Consumption`: Energy usage in kWh

Make sure this file is placed in the root directory of the project.

## 🚀 Getting Started

### Prerequisites

Install the required R packages with a single command:

```r
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "forecast"))
