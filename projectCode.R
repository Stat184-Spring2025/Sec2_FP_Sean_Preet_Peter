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

  
