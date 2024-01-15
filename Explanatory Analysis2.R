library(tidyverse)
library(ggplot2)

#Importing the dataset
Brexit_trade_1_ <- read_excel("Brexit trade (1).xlsx")
#REmoving the month variable since is not necessary in our analysis
Brexit <- Brexit_trade_1_[,-7]
##cleaning of the data
#making the data a adatframe
Brexit <- as.data.frame(Brexit)
#Checking structure of the data and descriptive statistics
str(Brexit)
summary(Brexit)
# Create a variable 'before_after' to indicate before or after 2016
Brexit$before_after <- ifelse(Brexit$Year < 2016, "Before 2016", "After 2016")
head(Brexit)

##Univariate plots (bar chart/histogram )to check normality  
# Bar chart for overall trade
ggplot(Brexit, aes(x = before_after)) +
  geom_bar() +
  labs(title = "Overall Trade Before and After 2016",
       x = "Year Group",
       y = "Count")
ggplot(Brexit, aes(x = `Flow Type`)) +
  geom_bar() +
  labs(title = "Overall Trade for Exports and Imports for EU and NON EU",
       x = "Flow Type",
       y = "Count")


# Histogram to check for normality
ggplot(Brexit, aes(x = `Value (£)`)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of value",
       x = "value",
       y = "Frequency")

# Multivariate Plot - Before and After 2016 Group Comparison
# Multivariate plot for continent-wise comparison

ggplot(Brexit, aes(x = before_after, y = `Value (£)`, fill = Continent)) +
  geom_boxplot() +
  labs(title = "Outflow Value Comparison Before and After 2016 by Continent",
       x = "Year Group",
       y = "Outflow Value",
       fill = "Continent")

ggplot(Brexit, aes(x = before_after, y = `Value (£)`, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Outflow Value Comparison Before and After 2016 by Continent",
       x = "Year Group",
       y = "Outflow Value",
       fill = "Continent")

#4.)Independent T-test.
# Independent t-test for outflow value
#First subset the data into before 2016 and after 2016
Before_2016 <- subset(Brexit, before_after== "Before 2016")
After_2016  <- subset(Brexit, before_after== "After 2016")

t_test_result <- t.test(Before_2016$`Value (£)`, After_2016$`Value (£)`)
print(t_test_result)

########Northern Ireland
#import the data
Trade <- read_csv("Trade in goods between Northern Ireland and European Union since 2013.csv")
#For this we shall also create the variable before and after 2016
Trade$before_after <- ifelse(Trade$year < 2016, "Before 2016", "After 2016")
head(Trade)
##Descriptive statistcs
summary(Trade)

##Univariate plots (bar chart/histogram )to check normality  
# Bar chart for overall trade in North
ggplot(Trade, aes(x = before_after)) +
  geom_bar() +
  labs(title = "Overall Trade in the North Before and After 2016",
       x = "Year Group",
       y = "Count")
# Histogram to check for normality
ggplot(Trade, aes(x = importVal)) +
  geom_histogram(bins = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of importvalue",
       x = "value",
       y = "Frequency")

ggplot(Trade, aes(x = exportVal)) +
  geom_histogram(bins = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Exportvalue",
       x = "value",
       y = "Frequency")

# Multivariate Plot - Before and After 2016 Group Comparison
# Multivariate plot for import and export  comparison

ggplot(Trade, aes(x = before_after, y = importVal)) +
  geom_bar(stat = "identity") +
  labs(title = "Import Value Comparison Before and After 2016",
       x = "Year Group",
       y = "import Value")
ggplot(Trade, aes(x = before_after, y = exportVal)) +
  geom_bar(stat = "identity") +
  labs(title = "Export Value Comparison Before and After 2016",
       x = "Year Group",
       y = "Export Value")

#4.)Independent T-test.
# Independent t-test for import value and export value
#First subset the data into before 2016 and after 2016
Before_2016 <- subset(Trade, before_after== "Before 2016")
After_2016  <- subset(Trade, before_after== "After 2016")

t_test_result_for_import <- t.test(Before_2016$importVal, After_2016$importVal)
print(t_test_result_for_import)
t_test_result_for_export <- t.test(Before_2016$exportVal, After_2016$exportVal)
print(t_test_result_for_export)
