
data <- read.csv("SeoulBikeData.csv", fileEncoding = "CP949")
library(ggplot2)
head(data)
str(data)
View(data)
data$Seasons <- as.factor(data$Seasons)
data$Holiday <- as.factor(data$Holiday)
colnames(data)[colnames(data) == "Temperature.캜."] <- "Temperature"
colnames(data)[colnames(data) == "Humidity..."] <- "Humidity"
colnames(data)[colnames(data) == "Dew.point.temperature.캜."] <- "Dew.point.temperature"
data <- data[data$Functioning.Day != "No", ]
View(data)
str(data)
#thanks for the tutorial Yigit but does this actually work
