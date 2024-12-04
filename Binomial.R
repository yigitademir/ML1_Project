library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)


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

str(data)

#--------------------------------------------------------------------------------
# EDA

summary(data$Rented.Bike.Count)
med_rent <- median(data$Rented.Bike.Count)
med_rent

data$High.Rentals <- ifelse(data$Rented.Bike.Count > med_rent, 1, 0)
data$High.Rentals <- as.factor(data$High.Rentals)

# FACTOR VARIABLES
# Calculate proportions for each combination of Seasons and High.Rentals
data_prop <- data %>%
  group_by(Seasons, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = Seasons, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Season", x = "Season", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calculate proportions for each combination of Is.Weekend and High.Rentals
data_prop <- data %>%
  group_by(Month, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = Month, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Months", x = "Months", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calculate proportions for each combination of Is.Weekend and High.Rentals
data_prop <- data %>%
  group_by(Is.Weekend, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = Is.Weekend, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Weekend", x = "Weekend", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calculate proportions for each combination of Is.Weekend and High.Rentals
data_prop <- data %>%
  group_by(Weekday, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = Weekday, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Weekday", x = "Weekday", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calculate proportions for each combination of Is.Weekend and High.Rentals
data_prop <- data %>%
  group_by(TimeOfDay, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = TimeOfDay, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Time of Day", x = "TimeOfDay", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Calculate proportions for each combination of Is.Weekend and High.Rentals
data_prop <- data %>%
  group_by(Hour, High.Rentals) %>%
  summarise(count = n()) %>%
  mutate(prop = count / sum(count)) %>%
  ungroup()

# Plot with percentages displayed on the bars
ggplot(data_prop, aes(x = Hour, y = prop, fill = High.Rentals)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)), 
            position = position_fill(vjust = 0.5), size = 3) +
  labs(title = "Proportion of High Rentals by Hour", x = "TimeOfDay2", y = "Proportion", fill = "High Rentals") +
  scale_fill_discrete(labels = c("No", "Yes")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


# CONTINOUS VARIABLES
# Summary statistics for continuous variables by High Rentals
data %>%
  group_by(High.Rentals) %>%
  summarise(
    avg_temperature = mean(Temperature, na.rm = TRUE),
    avg_humidity = mean(Humidity, na.rm = TRUE),
    avg_wind_speed = mean(Wind.Speed, na.rm = TRUE),
    avg_visibility = mean(Visibility, na.rm = TRUE)
  )

# Box plot for Temperature by High Rentals
ggplot(data, aes(x = High.Rentals, y = Temperature)) +
  geom_boxplot() +
  labs(title = "Temperature vs. High Rentals", x = "High Rentals", y = "Temperature (°C)") +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  theme_minimal()

# Density plot for Temperature
ggplot(data, aes(x = Temperature, fill = High.Rentals)) +
  geom_density(alpha = 0.5) +
  labs(title = "Temparature for High and Low Rentals", x = "Temperature (°C)", y = "Density") +
  scale_fill_manual(name = "High Rentals", values = c("0" = "red", "1" = "blue")) +
  theme_minimal()

# Box plot for Humiditiy by High Rentals
ggplot(data, aes(x = High.Rentals, y = Humidity)) +
  geom_boxplot() +
  labs(title = "Humidity vs. High Rentals", x = "High Rentals", y = "Humidity (%)") +
  scale_x_discrete(labels = c("0" = "Low", "1" = "High")) +
  theme_minimal()

# Density plot for Humidity
ggplot(data, aes(x = Humidity, fill = High.Rentals)) +
  geom_density(alpha = 0.5) +
  labs(title = "Humidity Distribution for High and Low Rentals", x = "Humidity (%)", y = "Density") +
  scale_fill_manual(name = "High Rentals", values = c("0" = "red", "1" = "blue")) +
  theme_minimal()


# Binomial GLM
glm_model <- glm(High.Rentals ~ Hour + Temperature + Humidity + Seasons,
                 family = binomial, data = data)

# Summary of the model
summary(glm_model)

# Predict probabilities
data$Predicted_Prob <- predict(glm_model, type = "response")

# Convert probabilities to binary classes (threshold = 0.5)
data$Predicted_Class <- ifelse(data$Predicted_Prob > 0.5, 1, 0)

# Create confusion matrix
confusion_matrix <- table(Predicted = data$Predicted_Class, Actual = data$High.Rentals)

# Print confusion matrix
print(confusion_matrix)

# Evaluate metrics
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
f1_score <- 2 * ((precision * recall) / (precision + recall))

# Print performance metrics
cat("Accuracy: ", accuracy, "\n")
cat("Precision: ", precision, "\n")
cat("Recall: ", recall, "\n")
cat("F1 Score: ", f1_score, "\n")
