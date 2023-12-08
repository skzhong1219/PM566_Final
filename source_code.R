library(data.table)
library(ggplot2)
library(dplyr)
library(plotly)
suicide_data <- read.csv("C:/Users/sabzh/Downloads/data/suicide_rates.csv")
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

#Population dataset
americas <- read.csv("C:/Users/sabzh/Downloads/data/americas_population.csv")

#dim(americas)
#str(americas)
americas_pop <- americas %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
americas_pop$ParentLocation <- "Americas"

se_asia <- read.csv("C:/Users/sabzh/Downloads/data/southeast_asia_population.csv")

#dim(se_asia)
se_asia_pop <- se_asia %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
se_asia_pop$ParentLocation <- "South-East Asia"

europe <- read.csv("C:/Users/sabzh/Downloads/data/europe_population.csv")

europe_pop <- europe %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
europe_pop$ParentLocation <- "Europe"

western_pacific <- read.csv("C:/Users/sabzh/Downloads/data/western_pacific_population.csv")

west_pac_pop <- western_pacific %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
west_pac_pop$ParentLocation <- "Western Pacific"

eastern_mediterranean <- read.csv("C:/Users/sabzh/Downloads/data/eastern_mediterranean_population.csv")

east_med_pop <- eastern_mediterranean %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
east_med_pop$ParentLocation <- "Eastern Mediterranean"

africa <- read.csv("C:/Users/sabzh/Downloads/data/africa_population.csv")
africa_pop <- africa %>% group_by(Time) %>%
  summarize(population = sum(Value, na.rm = T))
africa_pop$ParentLocation <- "Africa"

all_pop <- rbind(americas_pop, se_asia_pop, europe_pop, west_pac_pop, east_med_pop, africa_pop)

suicide_data <- merge(
  x     = suicide_data,      
  y     = all_pop, 
  by.x  = c("ParentLocation", "Period"),
  by.y  = c("ParentLocation", "Time"), 
  all.x = TRUE,      
  all.y = FALSE
)

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
  summarize(mean_rate = mean(Rate, na.rm = T), 
            mean_rate_lower = mean(Rate_CI_low, na.rm = T),
            mean_rate_upper = mean(Rate_CI_high, na.rm = T))

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


#Interactive Maps for Website
overall_mean_rate <- suicide_data %>% group_by(ParentLocation, Sex) %>%
  summarize(mean_rate = mean(Rate, na.rm = T), 
            mean_rate_lower = mean(Rate_CI_low, na.rm = T),
            mean_rate_upper = mean(Rate_CI_high, na.rm = T),
            population = mean(population, na.rm = T))

p1_scatter <- overall_mean_rate %>% 
  plot_ly(x = ~Sex, y = ~mean_rate,
          type = 'scatter', mode = 'markers', color = ~ParentLocation,
          size = ~population, sizes = c(5, 30), marker = list(sizemode='diameter', opacity=0.5),
          hoverinfo = 'text',
          text = ~paste( paste(ParentLocation, ":", sep = ""), 
                         paste(" Suicide Rate per 100k: ", round(mean_rate, 2), " (", round(mean_rate_lower, 2), ", ", 
                               round(mean_rate_upper, 2), ") ", sep = ""), sep = "<br>")) %>%
  layout(title = "Suicide Rate per 100k vs. Sex by Parent Location",
         yaxis = list(title = "Suicide Rate per 100k"), xaxis = list(title = "Sex"),
         hovermode = "compare")

p1_scatter

p2_male_scatter <- mean_rate %>% filter(Sex == "Male") %>%
  plot_ly(x = ~Period, y = ~mean_rate, color = ~ParentLocation, type = "scatter", mode = "lines+markers",
          hoverinfo = 'text',
          text = ~paste(paste(ParentLocation, ":", sep=""), 
                        paste("Year: ", Period, sep=""),
                        paste(" Suicide Rate per 100k: ", round(mean_rate, 2), " (", round(mean_rate_lower, 2), ", ", 
                              round(mean_rate_upper, 2), ") ", sep = ""), sep = "<br>")) %>%
  layout(title = "Male Suicide Rate per 100k in Each Parent Location by Year", xaxis = list(title = "Year"),
        yaxis = list(title = "Suicide Rate per 100k"), hovermode = 'compare')
p2_male_scatter

p2_female_scatter <- mean_rate %>% filter(Sex == "Female") %>%
  plot_ly(x = ~Period, y = ~mean_rate, color = ~ParentLocation, type = "scatter", mode = "lines+markers",
          hoverinfo = 'text',
          text = ~paste(paste(ParentLocation, ":", sep=""), 
                        paste("Year: ", Period, sep=""),
                        paste(" Suicide Rate per 100k: ", round(mean_rate, 2), " (", round(mean_rate_lower, 2), ", ", 
                              round(mean_rate_upper, 2), ") ", sep = ""), sep = "<br>")) %>%
  layout(title = "Female Suicide Rate per 100k in Each Parent Location by Year", xaxis = list(title = "Year"),
         yaxis = list(title = "Suicide Rate per 100k"), hovermode = 'compare')
p2_female_scatter
