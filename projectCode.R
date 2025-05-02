#NY, SF, Houston, Phiily, Chicago, LA

# Load required libraries
library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)

# Load the dataset
file_path <- "~/Downloads/archive (1)/GlobalLandTemperaturesByCity.csv"
GlobalLandTemperaturesByCity <- read_csv(file_path)

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
  city_data <- filter(yearly_avg_temp, City == city_name)
  
  ggplot(city_data, aes(x = Year, y = AvgTemperatureF)) +
    geom_line(color = "#1f77b4", size = 1.1, alpha = 0.8) +
    geom_smooth(method = "loess", color = "#ff7f0e", se = FALSE, size = 1.2) +
    labs(
      title = paste(city_name),
      subtitle = "Average Yearly Temperature (°F) from 1913 to Present",
      x = "Year",
      y = "Avg Temperature (°F)",
      caption = "Source: GlobalLandTemperaturesByCity.csv"
    ) +
    theme_minimal(base_family = "Arial") +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.title = element_text(size = 11, face = "bold"),
      axis.text = element_text(color = "gray30"),
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




