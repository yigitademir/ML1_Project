library(dplyr)
library(ggplot2)
library(lubridate)

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
data <- data[ , !(names(data) %in% "Functioning.Day")]

# Adding Weekdays and weekend label
Sys.setlocale("LC_TIME", "English")
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

#--------------------------------------------------------------------------------

data$Seasons <- relevel(data$Seasons, ref = "Summer")
data$Is.Weekend <- relevel(data$Is.Weekend, ref = "Yes")
data$Holiday <- relevel(data$Holiday, ref = "No Holiday")


# linear model
lm.data <- lm(Rented.Bike.Count ~ Temperature + Humidity + Wind.Speed + Visibility + Dew.Point.Temperature + Solar.Radiation + Rainfall + Snowfall + Holiday + Seasons + Is.Weekend + Weekday + TimeOfDay, data=data)
summary(lm.data)

lm.data.2 <- update(lm.data, .~. - Wind.Speed - Visibility - Solar.Radiation)
summary(lm.data.2)
#adjusted R-squared values are basically identical, simpler model should be better

#check effects of categorical variables
drop1(lm.data, test = 'F')
drop1(lm.data.2, test = 'F')

#checking the model performance
AIC(lm.data, lm.data.2)
BIC(lm.data, lm.data.2)
#basically the same values, simpler model should be better

#comparing both models
anova(lm.data, lm.data.2)
#no significant difference, simpler model should be better


"""
We remove variables Wind.Speed, Visibility and Solar.Radiation, 
which do not significantly contribute to the model.
TO make the decision we compared the models using 
analysis of variance which showed no evidence that 
the models are different, indicating that the more 
complex model is not better in any way. We also 
compared the AIC and BIC values along with the
adjusted R squared. Those values were almost identical between the models
supporting the decision to choose the simpler model.

The F-test revealed that all the categorical variables
have an effect on the rented bike count, justifying their
inclusion

"""

lm.data.0 <- lm(Rented.Bike.Count ~ 1, data = data)
coef(lm.data.0)
lm.data.seasons <- lm(Rented.Bike.Count ~ Seasons, data = data)
anova(lm.data.0, lm.data.seasons)
summary(lm.data.seasons)


ggplot(data, aes(x = Seasons, y = Rented.Bike.Count, fill = Seasons)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rented Bike Count by Seasons",
       x = "Season",
       y = "Rented Bike Count") +
  scale_fill_brewer(palette = "Set3")

#Boxplot Bike Count by Weekday
ggplot(data, aes(x = Weekday, y = Rented.Bike.Count, fill = Weekday)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rented Bike Count by weekdays",
       x = "Weekday",
       y = "Rented Bike Count") +
  scale_fill_brewer(palette = "Set3")

#Boxplot Bike Count by weekend status
ggplot(data, aes(x = Is.Weekend, y = Rented.Bike.Count, fill = Is.Weekend)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rented Bike Count by weekend status",
       x = "Weekend yes/no",
       y = "Rented Bike Count") +
  scale_fill_brewer(palette = "Set3")

#Boxplot Bike Count by holiday status
ggplot(data, aes(x = Holiday, y = Rented.Bike.Count, fill = Holiday)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Rented Bike Count by holiday status",
       x = "holiday status",
       y = "Rented Bike Count") +
  scale_fill_brewer(palette = "Set3")


#Scatterplots Bike count by different weather conditions 
library(tidyr)

data_long <- data %>%
  pivot_longer(cols = c(Temperature, Wind.Speed, Rainfall, Snowfall, Solar.Radiation),
               names_to = "Variable",
               values_to = "Value")

ggplot(data_long, aes(x = Value, y = Rented.Bike.Count)) +
  geom_point(alpha = 0.5, color = "blue") +   # Scatter points
  geom_smooth(method = "lm", se = FALSE, color = "red") +   # Add linear trend line
  facet_wrap(~ Variable, scales = "free_x") +  # Create a separate plot for each variable
  theme_minimal() +
  labs(title = "Rented Bike Count vs Various Weather Conditions",
       x = "Weather Conditions",
       y = "Rented Bike Count")
