Brexit Trade Analysis
Overview:
This analysis focuses on examining trade patterns before and after the pivotal year 2016, marked by Brexit. The dataset used contains information on overall trade and specific trade details, such as imports and exports, providing insights into the economic changes associated with Brexit. The analysis is conducted using R programming language, specifically leveraging the tidyverse and ggplot2 libraries for efficient data manipulation and visualization.

Part 1: Overall Trade Analysis
1. Load Required Libraries:
library(tidyverse)
library(ggplot2)
2. Importing and Cleaning Data:

Brexit_trade_1_ <- read_excel("Brexit trade (1).xlsx")
Brexit <- Brexit_trade_1_[,-7]
Brexit <- as.data.frame(Brexit)

3. Descriptive Statistics and Data Exploration:

str(Brexit)
summary(Brexit)

4. Univariate and Multivariate Plots:
# Bar charts for overall trade
ggplot(Brexit, aes(x = before_after)) +
  geom_bar() +
  labs(title = "Overall Trade Before and After 2016",
       x = "Year Group",
       y = "Count")

# Histogram for value distribution
ggplot(Brexit, aes(x = `Value (£)`)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Value",
       x = "Value",
       y = "Frequency")

# Multivariate plots for continent-wise comparison
ggplot(Brexit, aes(x = before_after, y = `Value (£)`, fill = Continent)) +
  geom_boxplot() +
  labs(title = "Outflow Value Comparison Before and After 2016 by Continent",
       x = "Year Group",
       y = "Outflow Value",
       fill = "Continent")
       
5. Independent T-test:
# Independent t-test for outflow value
Before_2016 <- subset(Brexit, before_after == "Before 2016")
After_2016 <- subset(Brexit, before_after == "After 2016")
t_test_result <- t.test(Before_2016$`Value (£)`, After_2016$`Value (£)`)
print(t_test_result)


Part 2: Northern Ireland Trade Analysis
1. Importing and Preprocessing:
Trade <- read_csv("Trade in goods between Northern Ireland and European Union since 2013.csv")
Trade$before_after <- ifelse(Trade$year < 2016, "Before 2016", "After 2016")
2. Descriptive Statistics and Data Exploration:
summary(Trade)
3. Univariate and Multivariate Plots:

# Bar chart for overall trade in North
ggplot(Trade, aes(x = before_after)) +
  geom_bar() +
  labs(title = "Overall Trade in the North Before and After 2016",
       x = "Year Group",
       y = "Count")

# Histograms for import and export values
ggplot(Trade, aes(x = importVal)) +
  geom_histogram(bins = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Import Value",
       x = "Value",
       y = "Frequency")

ggplot(Trade, aes(x = exportVal)) +
  geom_histogram(bins = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Export Value",
       x = "Value",
       y = "Frequency")

# Multivariate plots for import and export comparison
ggplot(Trade, aes(x = before_after, y = importVal)) +
  geom_bar(stat = "identity") +
  labs(title = "Import Value Comparison Before and After 2016",
       x = "Year Group",
       y = "Import Value")

ggplot(Trade, aes(x = before_after, y = exportVal)) +
  geom_bar(stat = "identity") +
  labs(title = "Export Value Comparison Before and After 2016",
       x = "Year Group",
       y = "Export Value")
4. Independent T-test:

# Independent t-test for import and export values
Before_2016 <- subset(Trade, before_after == "Before 2016")
After_2016 <- subset(Trade, before_after == "After 2016")
t_test_result_for_import <- t.test(Before_2016$importVal, After_2016$importVal)
print(t_test_result_for_import)
t_test_result_for_export <- t.test(Before_2016$exportVal, After_2016$exportVal)
print(t_test_result_for_export)

Results and Conclusions:
Comprehensive analysis of overall trade and specific import/export values before and after 2016.
Utilization of visualizations to depict trends and distributions.
Independent T-tests provide statistical insights into the significance of observed changes.
Feel free to explore, replicate, and contribute to the analysis. For any inquiries, contact [njorogeofrey73@gmail.com].





