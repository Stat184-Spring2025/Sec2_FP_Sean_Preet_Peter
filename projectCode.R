#NY, SF, Houston, Phiily, Chicago, LA

# Load required libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)

# Load the dataset
# Update the file path below to where you saved the CSV after downloading from Kaggle
GlobalLandTemperaturesByCity <- majorCity_globalTemp

# Define the cities of interest
target_cities <- c("New York", "San Francisco", "Los Angeles", 
                   "Houston", "Philadelphia", "Chicago")

# Filter and preprocess the data
majorCity_globalTemp <- GlobalLandTemperaturesByCity %>%
  filter(City %in% target_cities,
         Country == "United States",
         !is.na(AverageTemperature),
         dt >= as.Date("1913-01-01")) %>%
  mutate(
    Year = year(dt),
    AverageTemperatureF = (AverageTemperature * 9/5) + 32
  )

# Compute yearly average temperature
yearly_avg_temp <- majorCity_globalTemp %>%
  group_by(City, Year) %>%
  summarize(AvgTemperatureF = mean(AverageTemperatureF, na.rm = TRUE), .groups = 'drop')

# Fancy plotting function
plot_city_temp <- function(city_name) {
  city_data <- filter(yearly_avg_temp, City == city_name) %>%
    arrange(Year) %>%
    mutate(
      RollingAvg = (AvgTemperatureF +
                      lag(AvgTemperatureF, 1) +
                      lag(AvgTemperatureF, 2) +
                      lag(AvgTemperatureF, 3) +
                      lag(AvgTemperatureF, 4)) / 5
    )
  
  ggplot(city_data, aes(x = Year)) +
    geom_line(aes(y = AvgTemperatureF, color = "Range of Temperatures"), size = 1.1, alpha = 0.8) +
    geom_smooth(aes(y = AvgTemperatureF, color = "Average Temperature"), method = "loess", se = FALSE, size = 1.2) +
    scale_color_manual(
      name = "Legend",
      values = c("Range of Temperatures" = "#1f77b4", "Average Temperature" = "#ff7f0e")
    ) +
    labs(
      title = paste(city_name),
      subtitle = "Average Yearly Temperature (°F) from 1913 to Present",
      x = "Year",
      y = "Avg Temperature (°F)"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(color = "gray30"),
      legend.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
      plot.background = element_rect(fill = "#f9f9f9", color = NA),
      panel.background = element_rect(fill = "#f9f9f9", color = NA)
    )
}

# Display plots one after another
for (city in target_cities) {
  print(plot_city_temp(city))
}





