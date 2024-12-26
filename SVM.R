library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(tidyverse)
library(e1071)
library(caret)


data <- read.csv("SeoulBikeData.csv", fileEncoding = "CP949")

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
data$Functioning.Day <- as.factor(data$Functioning.Day)

str(data)


# --------------------------------------------------------
summary(data$Rented.Bike.Count)
med_rent <- median(data$Rented.Bike.Count)
med_rent

data$High.Rentals <- ifelse(data$Rented.Bike.Count > med_rent, 1, 0)
data$High.Rentals <- as.factor(data$High.Rentals)

# SVM

ggplot(data,
       aes(x = Temperature,
           y = Rented.Bike.Count,
           color = TimeOfDay)) + 
  geom_point()

# Split data
set.seed(123)
indices <- createDataPartition(data$High.Rentals, p = .85, list = F)

train <- data %>% 
  slice(indices)

test_in <- data %>% 
  slice(-indices) %>% 
  select(-High.Rentals)

test_truth <- data %>% 
  slice(-indices) %>% 
  pull(High.Rentals)

train <- train %>% 
  select_if(function(x) !(is.factor(x) && length(levels(x)) < 2))
table(train$High.Rentals)

# SVM Model 
train_features <- train %>% select(-High.Rentals, -Date)
svm_model <- svm(High.Rentals ~ ., train, kernel = "radial")
summary(svm_model)

# Predictors
predictions <- predict(svm_model, newdata = test_in)

# Confusion Matrix 
confusionMatrix(predictions, test_truth)
