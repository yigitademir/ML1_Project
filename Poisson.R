library(dplyr)
library(ggplot2)

# setwd("/Users/kasperr14/Desktop/Wszystko/1. HSLU Data Science/Semester 2/Applied Machine Learning and Predictive Modelling/Project")

# -----------------------------------------------------------------------------
# Common Part
# Loading data set
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

# Adding Weekdays and weekend label
data <- data %>% 
  mutate(
    Date = as.Date(Date, format = "%d/%m/%Y"), # Changing Date format from character to date
    Weekday = weekdays(Date),  # Adding column with weekday
    Is.Weekend = ifelse(Weekday %in% c("Saturday", "Sunday"), "Yes", "No") # Determining whether a weekday is weekend
  )

View(data)

# -----------------------------------------------------------------------------
# Playing with the data
# Boxplot of number of bikes rented and Season
ggplot(mapping = aes(y = data$Rented.Bike.Count,
                     x = data$Seasons)) + 
  geom_boxplot()

# Boxplot of number of bikes rented and weekday
ggplot(mapping = aes(y = data$Rented.Bike.Count,
                     x = data$Weekday)) + 
  geom_boxplot()

# Scatterplot of number of bikes rented and hour
ggplot(mapping = aes(y = data$Rented.Bike.Count,
                     x = data$Hour)) +
  geom_point()

# Scatterplot of number of bikes rented and temperature + weekend/no weekend
ggplot(mapping = aes(y = data$Rented.Bike.Count,
                     x = data$Temperature,
                     colour = data$Is.Weekend)) +
  geom_point()


# Generalised Linear Model with family set to Poisson
# Creating a model
glm.bikes <- glm(Rented.Bike.Count ~ Temperature + Humidity + Wind.Speed + Rainfall,
                 family = "poisson",
                 data = data)

summary(glm.bikes)

