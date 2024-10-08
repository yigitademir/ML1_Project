library(dplyr)

setwd("/Users/kasperr14/Desktop/Wszystko/1. HSLU Data Science/Semester 2/Applied Machine Learning and Predictive Modelling/Project")

data <- read.csv("SeoulBikeData.csv", fileEncoding = "CP949")

data$Seasons <- as.factor(data$Seasons)
data$Holiday <- as.factor(data$Holiday)

# Changing the column names
colnames(data)[colnames(data) == "Temperature.캜."] <- "Temperature"
colnames(data)[colnames(data) == "Humidity..."] <- "Humidity"
colnames(data)[colnames(data) == "Dew.point.temperature.캜."] <- "Dew.Point.Temperature"
colnames(data)[colnames(data) == "Wind.speed..m.s."] <- "Wind.Speed"
colnames(data)[colnames(data) == "Solar.Radiation..MJ.m2."] <- "Solar.Radiation"
colnames(data)[colnames(data) == "Visibility..10m."] <- "Visibility"
colnames(data)[colnames(data) == "Rainfall.mm."] <- "Rainfall"
colnames(data)[colnames(data) == "Snowfall..cm."] <- "Snowfall"

# Removing non-functioning days
data <- data[data$Functioning.Day != "No", ]

data <- data %>% 
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"),
    Weekday = weekdays(Date),
    Is.Weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Yes", "No")
  )

View(data)
