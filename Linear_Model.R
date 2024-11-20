library(dplyr)
library(ggplot2)
library(lubridate)
library(car)

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

cor(data$Temperature, data$Dew.Point.Temperature)
cor(data$Temp_Dew_Spread, data$Humidity)
cor(data$Temperature, data$Snowfall)

data$Holiday <- relevel(data$Holiday, ref = "No Holiday")
data$TimeOfDay <- relevel(data$TimeOfDay, ref = "Morning Rush Hour")



data$Temp_Dew_Spread <- data$Temperature - data$Dew.Point.Temperature

# linear model

#We want to predict the number of bikes rented in Seoul
#so our response variable will be Rented.Bike.Count

#Since season and month are highly correlated 
#(multicollinear), month is excluded in
#the model. Similarly, because Is.Weekend and
#Weekday are strongly correlated, Weekday
#is excluded in the model."

#Model version 1

#We include all the variables apart from those which
#were mentioned before and dropped

lm.data <- lm(Rented.Bike.Count ~ Temperature + 
                Humidity + Wind.Speed + Visibility + 
                Solar.Radiation + Rainfall + Snowfall + 
                Seasons + Holiday + Is.Weekend + TimeOfDay, 
                data=data)
summary(lm.data)

#We check effects of categorical variables
drop1(lm.data, test = 'F')

#Since there are some variables who don't seem
#to have any effect on the rental bike count,
#we will check if we can fit a better model


#We now fit another model, removing the variables
#which do not seem to have an effect on the rental
#bike count to see how the model performance changes.

lm.data.2 <- update(lm.data, .~. - Wind.Speed - Visibility - Solar.Radiation)
summary(lm.data.2)

#We check effects of categorical variables
drop1(lm.data.2, test = 'F')

#Check for multicollinearity
vif(lm.data.2)

#In the second model, the GVIF values of Temperature
#and Seasons are just below the threshold of 5 so
#they should be fine to keep in the model.

#Comparing the two models

#We want to see which model performs better in
#predicting the rental bike count in Seoul.
#We look at the adjusted R-squared of both and see that
#they are almost identical at around 0.63, suggesting that
#the simpler model is justified.



#Next we check the fit of the models
AIC(lm.data, lm.data.2)
BIC(lm.data, lm.data.2)

#The AIC values are very similar but the second
#model has a lower BIC value, indicating that the
#simpler model is a better fit

#Finally, we compare the models with an F-test
anova(lm.data, lm.data.2)

#There is no significant difference between the two
#models, which means we can opt for the simpler one


#With the second model chosen for the analysis, let's
#interpret the coefficients
summary(lm.data.2)
drop1(lm.data.2, test= 'F')

#Intercept:
#There is strong evidence that the mean number of rental
#bikes on a normal working day in autumn during the
#morning rush hour is not zero. Around 1233 bikes are
#expected to be rented in this situation.

#Temperature:
#There is strong evidence that for each increase in
#temperature by one degree, while all other variables 
#are kept constant, an extra 25.62 bikes will be rented.

#Humidity:
#There is strong evidence that for each increase in
#humidity by one percentage point, while all other
#variables are kept constant, 7.7 fewer bikes will
#be rented.

#Rainfall:
#There is strong evidence that for each increase in 
#rainfall by 1 mm, while all other variables are kept
#constant, 64.62 fewer bikes will be rented.

#Snowfall:
#There is some evidence that for each increase in
#snowfall by 1 cm, while all other variables are kept
#constant, 28.54 more bikes will be rented.
#This is an unexpected result...

#We now interpret the coefficients of categorical
#variables, it is important to note that the observed
#values are all in comparison to the reference
#level of each variable but not between each other.
#In the next section we will compare the categorical
#variables Seasons and TimeOfDay between each level.

#Seasons:
#There is strong evidence that for a spring day where
#other variables are kept constant, 154.74 fewer bikes
#will be rented than in autumn. For a summer
#day, a decrease of 164.2 bikes is expected and for a
#winter day, a decrease of 356.07 bikes is expected
#compared to autumn.

#Holiday:
#There is strong evidence that on a public holiday,
#while all other variables are kept constant, 126.56
#fewer bikes will be rented than on a normal day.

#Is.Weekend:
#There is strong evidence that on a weekend, while all
#other variables are kept constant, 84.62 fewer bikes
#will be rented.

#TimeOfDay:
#There is strong evidence that during mid-day, if all
#other variables are kept constant, 351.16 fewer bikes
#will be rented compared to the morning rush hour.
#Similarly, in the afternoon, a decrease of 159.57
#rented bikes is expected. In the evening rush hour,
#an increase of 406 bikes is expected compared to the
#morning rush hour and in the evening, there is an
#increase of 159.48 bikes. In the early and late night,
#the decrease compared to the morning rush hour is 248.54
#and 530.01 bikes respectively,





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
