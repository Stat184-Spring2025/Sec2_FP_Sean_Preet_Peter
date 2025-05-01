#NY, SF, Houston, Phiily, Chicago, LA

library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)
majorCity_globalTemp <- GlobalLandTemperaturesByCity %>%
  filter(City == c("New York", "San Fransisco", "Los Angeles", "Chicago",
                   "Houston","Philadelphia")) %>%
  filter(Country == "United States") %>%
  filter(dt >= as.Date("1913-01-01"))


yearly_avg_temp <- majorCity_globalTemp %>%
  mutate(Year = year(dt)) %>%  # Extract the year from the date
  group_by(City, Year) %>%
  summarize(AvgTemperature = mean(AverageTemperature, na.rm = TRUE)) # Calculate the yearly average


# Create a plot for each city
city_plots <- map(unique(yearly_avg_temp$City), function(city_name) {
  ggplot(yearly_avg_temp %>% filter(City == city_name), aes(x = Year, y = AvgTemperature)) +
    geom_line() +
    geom_point(size = 0.5, alpha = 0.7) +
    labs(
      title = paste("Temperature Trend for", city_name),
      x = "Year",
      y = "Average Temperature (°C)"
    ) +
    scale_x_continuous(breaks = seq(1910, 2010, 10)) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  })


