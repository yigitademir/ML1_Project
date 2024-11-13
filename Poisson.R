library(dplyr)
library(ggplot2)

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

#Adding Month Column
data$Month <- month(data$Date, label = TRUE, abbr = TRUE)

#Adding TimeofDay Column
data <- data %>%
  mutate(
    TimeOfDay = case_when(
      Hour >= 23 | Hour < 2 ~ "Early Night",
      Hour >= 2 & Hour < 7 ~ "Late Night",
      Hour >= 7 & Hour < 10 ~ "Morning Rush Hour",
      Hour >= 10 & Hour < 14 ~ "Mid-day",
      Hour >= 14 & Hour < 18 ~ "Afternoon",
      Hour >= 18 & Hour < 20 ~ "Evening Rush Hour",
      Hour >= 20 & Hour <= 22 ~ "Evening"
    )
  )

#Change datatypes for factor variables
data$Seasons <- as.factor(data$Seasons)
data$Holiday <- as.factor(data$Holiday)
data$Is.Weekend <- as.factor(data$Is.Weekend)
data$Weekday <- as.factor(data$Weekday)
data$TimeOfDay <- as.factor(data$TimeOfDay)
data$Hour <- as.factor(data$Hour)

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

