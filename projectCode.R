#NY, SF, Houston, Phiily, Chicago, LA

library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)

majorCity_globalTemp <- GlobalLandTemperaturesByCity %>%
  filter(City == c("New York", "San Francisco", "Los Angeles", "Chicago",
                   "Houston","Philadelphia")) %>%
  filter(Country == "United States") %>%
  filter(dt >= as.Date("1913-01-01"))


yearly_avg_temp <- majorCity_globalTemp %>%
  mutate(Year = year(dt), 
         AverageTemperatureF = (AverageTemperature * 9/5) + 32) %>%# Extract the year from the date
  group_by(City, Year) %>%
  summarize(AvgTemperatureF = mean(AverageTemperatureF, na.rm = TRUE)) # Calculate the yearly average


avg_NY <- yearly_avg_temp %>%
  filter(City == "New York")

avg_SF <- yearly_avg_temp %>%
  filter(City == "San Francisco")

avg_LA <- yearly_avg_temp %>%
  filter(City == "Los Angeles")

avg_Hous <- yearly_avg_temp %>%
  filter(City == "Houston")

avg_Phil <- yearly_avg_temp %>%
  filter(City == "Philadelphia")

avg_Chi <- yearly_avg_temp %>%
  filter(City == "Chicago")

avg_Cali <- yearly_avg_temp %>%
  filter(City %in% c("Los Angeles", "San Francisco"))

# Create a plot for each city
ggplot(avg_Cali, 
       aes(
         x = Year, 
         y = AvgTemperatureF, 
         color = City,
         linetype = City)) +
  geom_smooth(se = FALSE, size = 1) +
  theme_minimal() +
  labs(title = "Average Temperature of 2 California Cities over 100 Years",
       x = "Year",
       y = "Average Temperature (F)") +
  theme(legend.title = element_blank())

ggplot(avg_Phil, 
       aes(
         x = Year, 
         y = AvgTemperatureF, 
         color = City,
         linetype = City)) +
  geom_smooth(se = FALSE, size = 1) +
  theme_minimal() +
  labs(title = "Average Temperature of Philadelphia over 100 Years",
       x = "Year",
       y = "Average Temperature (F)") +
  theme(legend.title = element_blank())








