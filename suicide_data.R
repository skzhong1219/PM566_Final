library(data.table)
library(ggplot2)
library(dplyr)
suicide_data <- read.csv("C:/Users/sabzh/Downloads/data.csv")
#dim(suicide_data)
#head(suicide_data)
#tail(suicide_data)
#str(suicide_data)

#removing columns with NA values
suicide_data <- suicide_data[-c(15:23, 25, 26, 28, 31, 32)]

#print(unique(suicide_data$ParentLocation))
#print(unique(suicide_data$Location))

#changing names of columns
colnames(suicide_data)[colnames(suicide_data) == "Dim1"] <- "Sex"
colnames(suicide_data)[colnames(suicide_data) == "FactValueNumeric"] <- "Rate"
colnames(suicide_data)[colnames(suicide_data) == "FactValueNumericLow"] <- "Rate_CI_low"
colnames(suicide_data)[colnames(suicide_data) == "FactValueNumericHigh"] <- "Rate_CI_high"

#sum(is.na(suicide_data$Rate))
#sum(is.na(suicide_data$Rate_CI_low))
#sum(is.na(suicide_data$Rate_CI_high))

#histogram 
data_female <- subset(suicide_data, suicide_data$Sex == "Male" | suicide_data$Sex == "Female")

data_female %>%
  ggplot(aes(x=Rate, fill=Sex)) +
  geom_histogram(binwidth = 10) +
  xlim(-10,200) +
  xlab("Suicide Rate (per 100,000 people)") +
  ylab("Count") +
  ggtitle("Distribution of Suicide Rates by Sex") +
  facet_wrap(~Sex, ncol = 1)

data_female %>%
  ggplot() +
  geom_bar(aes(x=ParentLocation, fill=Sex)) +
  xlab("Parent Location") +
  ylab("Count") +
  ggtitle("Distribution of Sex for Each Parent Location")

#summary statistics table
summary_stats_sex <- suicide_data %>%
  group_by(Sex) %>%
  summarise(
    Frequency = table(Sex),
    Mean = mean(Rate),
    Mean_Lower_CI = mean(Rate_CI_low),
    Mean_Higher_CI = mean(Rate_CI_high),
    Median = median(Rate),
    SD = sd(Rate),
    Min = min(Rate),
    Max = max(Rate)
  )

summary_stats_sex

summary_stats_location <- suicide_data %>%
  group_by(ParentLocation, Sex) %>%
  summarise(
    Frequency = table(ParentLocation),
    Mean = mean(Rate),
    Mean_Lower_CI = mean(Rate_CI_low),
    Mean_Higher_CI = mean(Rate_CI_high),
    Median = median(Rate),
    SD = sd(Rate),
    Min = min(Rate),
    Max = max(Rate)
  )

summary_stats_location

#boxplots
boxplot1 <- suicide_data %>%
  ggplot(aes(x=Sex, y=Rate)) +
  geom_boxplot() + 
  ylab("Suicide Rate (per 100,000 people)") +
  ggtitle("Boxplot of Suicide Rate vs. Sex Overall")

suicide_data %>%
  ggplot(aes(x=Sex, y=Rate)) +
  geom_boxplot() +
  facet_wrap(~ParentLocation, nrow = 1) +
  ggtitle("Boxplot of Suicide Rate vs. Sex by Parent Location") +
  ylab("Suicide Rate (per 100,000 people)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Caluclating mean suicide rate by year, location, and sex
mean_rate <- suicide_data %>% group_by(Period, ParentLocation, Sex) %>%
  summarize(mean_rate = mean(Rate, na.rm = T))

mean_rate_location <- suicide_data %>% group_by(ParentLocation, Location, Sex) %>%
  summarize(mean_rate = mean(Rate, na.rm = T))

africa <- mean_rate_location[mean_rate_location$ParentLocation == "Africa",]
americas <- mean_rate_location[mean_rate_location$ParentLocation == "Americas",]
east_med <- mean_rate_location[mean_rate_location$ParentLocation == "Eastern Mediterranean",]
west_pac <- mean_rate_location[mean_rate_location$ParentLocation == "Western Pacific",]
se_asia <- mean_rate_location[mean_rate_location$ParentLocation == "South-East Asia",]
europe <- mean_rate_location[mean_rate_location$ParentLocation == "Europe",]

#Scatterplot of suicide rate trend by location
mean_rate %>%
  ggplot(aes(x = Period, y = mean_rate, color = Sex)) +
  geom_point() +
  facet_wrap(~ParentLocation, nrow = 2) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Suicide Rates from 2000-2019 by Parent Location")

africa %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of Africa")

americas %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of the Americas")

east_med %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of Eastern Mediterranean")

west_pac %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of Western Pacific")

se_asia %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of Southeast Asia")

europe %>%
  ggplot(aes(x = Location, y = mean_rate, color = Sex)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Mean Suicide Rate per 100,000 People") +
  ggtitle("Mean Suicide Rate in Locations of Europe")

