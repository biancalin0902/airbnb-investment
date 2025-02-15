
cat("\014") # clears console 
rm(list = ls()) # clears global environment 
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots 
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages 
options(scipen = 100) # disables scientific notation for entire R session

install.packages("pacman")
install.packages("viridis")
library(pacman)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(viridis)
library(dplyr)
p_load(tidyverse)
library(corrplot)

data_list <- c("Montreal", "NewBrunswick","Quebec", "Toronto", "Vancouver", "Victoria", "Winnipeg")
Montreal <- read.csv("Montreal.csv")
NewBrunswick <- read.csv("NewBrunswick.csv")
Quebec <- read.csv("Quebec.csv")
Toronto <- read.csv("Toronto.csv")
Vancouver <- read.csv("Vancouver.csv")
Victoria <- read.csv("Victoria.csv")
Winnipeg <- read.csv("Winnipeg.csv")

# Create function to clean data
cleaning_data <- function(data) {
  # Select specific columns
  selected_columns <- c(
    "id", "room_type", "accommodates", "bathrooms_text", "bedrooms", "beds",
    "price", "reviews_per_month", "review_scores_rating",
    "review_scores_accuracy", "review_scores_cleanliness",
    "review_scores_checkin", "review_scores_communication",
    "review_scores_location", "review_scores_value",
    "neighbourhood_cleansed", "latitude", "longitude")
  data <- data[selected_columns]
  data$bathrooms <- as.numeric(gsub(" .*$", "", data$bathrooms_text))
  # Remove the original bathrooms_text column
  data <- subset(data, select = -c(bathrooms_text))
  # Change price from dollar to numeric data
  library(readr)
  data$price <- parse_number(data$price)
  # Choose Entire house data
  data <- data[data$room_type=="Entire home/apt",]
  # Remove outliers
  quartiles <- quantile(data$price, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(data$price)
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  data <- subset(data, data$price > Lower & data$price < Upper)
  # Return the processed data frame
  return(data)
}

Montreal <- cleaning_data(Montreal)
NewBrunswick <- cleaning_data(NewBrunswick)
Quebec <- cleaning_data(Quebec)
Toronto <- cleaning_data(Toronto)
Vancouver <- cleaning_data(Vancouver)
Victoria <- cleaning_data(Victoria)
Winnipeg <- cleaning_data(Winnipeg)
listings <- bind_rows(
  mutate(Montreal, city = "Montreal"),
  mutate(NewBrunswick, city = "NewBrunswick"),
  mutate(Quebec, city = "Quebec"),
  mutate(Toronto, city = "Toronto"),
  mutate(Vancouver, city = "Vancouver"),
  mutate(Victoria, city = "Victoria"),
  mutate(Winnipeg, city = "Winnipeg"))

# Part 1 EDA
# Price distribution
# Figure 1 - Price and log-transformed price distributions by different cities. The circle shows the mean price value
library(gridExtra)
g1 <- ggplot(listings, aes(x = city, y = price, fill = city)) +
  geom_violin(alpha = 0.6) +
  geom_point(stat = "summary", fun = "mean", shape = 1, size = 3, fill = "white") +
  labs(x = "City", y = "Price") +
  ggtitle("Price Distribution") +
  theme_clean() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g2 <- ggplot(listings, aes(x = city, y = log(price), fill = city)) +
  geom_violin(alpha = 0.6) +
  geom_point(stat = "summary", fun = "mean", shape = 1, size = 3, fill = "white") +
  labs(x = "City", y = "Log Transformed Price") +
  ggtitle("Log Transformed Price distribution") +
  theme_clean() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(g1, g2, ncol=2)


# Add log transformed price distribution and remove outliers, so we reset data and clean again
# Create function to clean data
cleaning_data2 <- function(data) {
  # Select specific columns
  selected_columns <- c(
    "id", "room_type", "accommodates", "bathrooms_text", "bedrooms", "beds",
    "price", "reviews_per_month", "review_scores_rating",
    "review_scores_accuracy", "review_scores_cleanliness",
    "review_scores_checkin", "review_scores_communication",
    "review_scores_location", "review_scores_value",
    "neighbourhood_cleansed", "latitude", "longitude")
  data <- data[selected_columns]
  data$bathrooms <- as.numeric(gsub(" .*$", "", data$bathrooms_text))
  # Remove the original bathrooms_text column
  data <- subset(data, select = -c(bathrooms_text))
  # Change price from dollar to numeric data
  library(readr)
  data$price <- parse_number(data$price)
  data$log_price <- log(data$price)
  # Choose Entire house data
  data <- data[data$room_type=="Entire home/apt",]
  # Remove outliers
  quartiles <- quantile(data$price, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(data$price)
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  data <- subset(data, data$price > Lower & data$price < Upper)
  
  quartiles2 <- quantile(data$log_price, probs=c(.25, .75), na.rm = FALSE)
  IQR2 <- IQR(data$log_price)
  Lower2 <- quartiles2[1] - 1.5*IQR2
  Upper2 <- quartiles2[2] + 1.5*IQR2
  data <- subset(data, data$log_price > Lower2 & data$log_price < Upper2)
  return(data)
}
Montreal <- read.csv("Montreal.csv")
NewBrunswick <- read.csv("NewBrunswick.csv")
Quebec <- read.csv("Quebec.csv")
Toronto <- read.csv("Toronto.csv")
Vancouver <- read.csv("Vancouver.csv")
Victoria <- read.csv("Victoria.csv")
Winnipeg <- read.csv("Winnipeg.csv")

Montreal <- cleaning_data2(Montreal)
NewBrunswick <- cleaning_data2(NewBrunswick)
Quebec <- cleaning_data2(Quebec)
Toronto <- cleaning_data2(Toronto)
Vancouver <- cleaning_data2(Vancouver)
Victoria <- cleaning_data2(Victoria)
Winnipeg <- cleaning_data2(Winnipeg)

listings_new <- bind_rows(
  mutate(Montreal, city = "Montreal"),
  mutate(NewBrunswick, city = "NewBrunswick"),
  mutate(Quebec, city = "Quebec"),
  mutate(Toronto, city = "Toronto"),
  mutate(Vancouver, city = "Vancouver"),
  mutate(Victoria, city = "Victoria"),
  mutate(Winnipeg, city = "Winnipeg"))

# Figure 2 - Log-transformed price distribution after removing outliers
ggplot(listings_new, aes(x = city, y = log_price, fill = city)) +
  geom_violin(alpha = 0.6) +
  geom_point(stat = "summary", fun = "mean", shape = 1, size = 3, fill = "white") +
  labs(x = "City", y = "Log Transformed Price") +
  ggtitle("Log Transformed Price distribution after removing outliers") +
  theme_clean() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Create function to run two-samples t test for each city with remaining cities.
t_test_process <- function(city_name, data) {
  city <- data[data$city == city_name, ]
  not_city <- data[data$city != city_name, ]
  result <- t.test(city$log_price, not_city$log_price, alternative = "greater", var.equal = FALSE)
  return(result)
}
t_test_process("Vancouver",listings_new)
t_test_process("Toronto",listings_new)
t_test_process("Victoria",listings_new)
t_test_process("Montreal",listings_new)
t_test_process("NewBrunswick",listings_new)
t_test_process("Quebec",listings_new)
t_test_process("Winnipeg",listings_new)

# Question 1: Question 1: Does distance from the World Heritage Site impact the price of Airbnb? 
# Latitude, Longitude: using formula of Professor Yvonne
lat2 <- 46.8111111
lon2 <- -71.2091667
lat2 <- lat2 * (pi/180)
lon2 <- lon2 * (pi/180)
distance <- function(lat1, lon1) {
  # Earth radius in kilometers
  R <- 6371
  # Convert degrees to radians
  lat1 <- lat1 * (pi/180)
  lon1 <- lon1 * (pi/180)
  # Haversine formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- R * c
  return(distance)
}
## Quebec
quebec <- listings_new[listings_new$city=="Quebec",]
quebec$distance <- distance(quebec$latitude, quebec$longitude)
distance_threshold_que <- quantile(quebec$distance, 0.5)
# Filter data for the top 50% lowest values based on distance
top_50_percentile <- subset(quebec, distance <= distance_threshold_que)
below_50_percentile <- subset(quebec, distance > distance_threshold_que)
t.test(top_50_percentile$log_price,below_50_percentile$log_price)
t.test(top_50_percentile$price,below_50_percentile$price)

data_list_qe <- list(
  top_50_percentile = top_50_percentile$price,
  below_50_percentile = below_50_percentile$price
)
# Figure 3 - Price near and far from World Heritage Site
boxplot(data_list_qe,
        main = "Price by near and far from World Heritage site",
        xlab = "Distance from World Heritage Site",
        ylab = "Price",
        col = c("lightblue", "coral"),  # Specify colors for the boxes
        names = c("Top 50 percentile nearest distance", "Below 50 percentile nearest distance"))

# Figure 4 - Scatterplot and linear regression between price and distance from World Heritage site
lm_qe_price <- lm(price ~ distance, data = quebec)
summary(lm_qe_price)
ggplot(quebec, aes(x=distance, y=price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Distance from World Heritage site", y = "Price") +
  ggtitle("Relationship of log price and distance from World Heritage site")+
  theme_clean()

quartiles_distance <- quantile(quebec$distance, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(quebec$distance)
Lower <- quartiles_distance[1] - 1.5*IQR
Upper <- quartiles_distance[2] + 1.5*IQR 
quebec_no_outlier <- subset(quebec, quebec$distance > Lower & quebec$distance < Upper)
lm_qe_no_outliers <- lm(price ~ distance, data = quebec_no_outlier)
summary(lm_qe_no_outliers)

ggplot(lm_qe_no_outliers, aes(x=distance, y=price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Distance from World Heritage site", y = "Price") +
  ggtitle("Relationship of price and distance from World Heritage site")+
  theme_clean()

threshold_que_distance2 <- quantile(quebec_no_outlier$distance, 0.2)
# Filter data for the top 20% lowest values based on distance
quebec_top20_distance <- subset(quebec_no_outlier, distance <= threshold_que_distance2)
lm_qe_20_distance <- lm(price ~ distance, data = quebec_top20_distance)
summary(lm_qe_20_distance)
ggplot(quebec_top20_distance, aes(x=distance, y=price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Distance from World Heritage site", y = "Log price") +
  ggtitle("Relationship of log price and distance from World Heritage site")+
  theme_clean()

quebec_by_neighbourhood <- quebec_no_outlier %>% group_by(quebec_no_outlier$neighbourhood_cleansed) %>%
  summarise(listing_count = n(),
            mean_price = mean(price),
            mean_distance = mean(distance))
lm_qe_neighbourhood <- lm(mean_price ~ mean_distance, data = quebec_by_neighbourhood)
summary(lm_qe_neighbourhood)

ggplot(quebec_by_neighbourhood, aes(x=mean_distance, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Distance from World Heritage site", y = "Price") +
  ggtitle("Relationship of log price and distance from World Heritage site")+
  theme_clean()
#------------------------------------------------------------------
# Question 2:Does the crime rate impact the price of Airbnb? 
str(Toronto)
#Add neighborhood crime rate open dataset
crime<-read.csv("Neighbourhood_Crime_Rates_Open_Data.csv")
# Group data by neighbourhood
by_neighbourhood <- Toronto %>%
  group_by(neighbourhood_cleansed) %>%
  summarize(mean_price = mean(price), 
            listing_count = n())
by_neighbourhood
# Select only the desired columns
selected_columns <- c(
  "OBJECTID", "AREA_NAME", "HOOD_ID",
  "POPN_PROJ_2022", "ASSAULT_2022", "AUTOTHEFT_2022",
  "BIKETHEFT_2022", "BREAKENTER_2022", "HOMICIDE_2022",
  "ROBBERY_2022", "SHOOTING_2022", "THEFTFROMMV_2022",
  "THEFTOVER_2022"
)

crime <- crime %>%
  select(all_of(selected_columns))
# Add a column called crime_rate and calculate via formula
crime <- crime %>%
  mutate(
    crime_rate = round((ASSAULT_2022 + AUTOTHEFT_2022 + BIKETHEFT_2022 + 
                          BREAKENTER_2022 + HOMICIDE_2022 + ROBBERY_2022 + 
                          SHOOTING_2022 + THEFTFROMMV_2022 + THEFTOVER_2022) / POPN_PROJ_2022, 4)
  )

# Merge 2 datasets with a common column, which is neighborhood
merged_data <- merge(crime, by_neighbourhood, 
                     by.x = "AREA_NAME", by.y = "neighbourhood_cleansed", all.x = TRUE)

# Delete the NA item
merged_data <- subset(merged_data, !is.na(mean_price))

#Create new column of crime rate percentage
merged_data$crime_rate_percentage <- merged_data$crime_rate*100

par(mfrow=c(1,2))
# Figure 5 - Boxplot of crime rate distribution (before and after removing outliers)
boxplot(merged_data$crime_rate_percentage,
        main = "Crime Rate Distribution", 
        ylab= "Crime Rate (%)",
        col = c("lightblue"))
#Remove outliers
quartiles <- quantile(merged_data$crime_rate, probs=c(.25, .75), na.rm = FALSE)
IQR <- IQR(merged_data$crime_rate)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
merged_data <- subset(merged_data, merged_data$crime_rate > Lower & merged_data$crime_rate < Upper)
#Boxplot of crime rate distribution after removing outliers
boxplot(merged_data$crime_rate_percentage, 
        main = "Crime Rate Distribution (After removing outliers)", 
        ylab= "Crime Rate (%)",
        col = c("lightblue"))

#Set the threshold of 0.5
rating_threshold_l <- quantile(merged_data$crime_rate, 0.5)
# Subset the data with crime rate in the top 50%
top_50_quantile_data <- merged_data %>% filter(crime_rate >= rating_threshold_l)
top_50_quantile_data
# Subset the data with crime rate in the lowest 50%
low_50_quantile_data <- merged_data %>% filter(crime_rate < rating_threshold_l)
low_50_quantile_data
# Perform Welch two sample t-test
t_test_result <- t.test(top_50_quantile_data$mean_price,low_50_quantile_data$mean_price, alternative = "less",var.equal = FALSE)
t_test_result
# Calculate the critical value for one-tailed test
df <- 105.24
critical_value <- qt(0.95, df)
critical_value
data_list <- list(
  top_50_quantile_data = top_50_quantile_data$mean_price,
  low_50_quantile_data = low_50_quantile_data$mean_price
)

par(mfrow=c(1,1))

# Figure 6 - Boxplot of mean price by crime rate (left) and linear regression (right)
boxplot(data_list,
        main = "Boxplot of Mean Price By Crime Rate",
        xlab = "Crime Rate",
        ylab = "Mean Price ($)",
        col = c("lightblue", "coral"),
        names = c("Top 50 percentile crime rate ", "Below 50 percentile crime rate "))

# Inferential Statistics
correlation_coefficient <- cor(merged_data$crime_rate_percentage, merged_data$mean_price)
round(correlation_coefficient,4)
# Linear Regression model summary
linear_model<- lm(mean_price ~ crime_rate_percentage, data = merged_data)
summary(linear_model)
# Scatterplot between crime rate(%) and mean price ($)
g1<- ggplot(merged_data, aes(x=crime_rate_percentage, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Crime Rate (%)", y = "Mean Price ($)") +
  ggtitle("Relationship of Crime Rate and Mean Price")+
  theme_clean()
g1

#------------------------------------------------------------------
# Question 3: Do the residential rental costs impact Airbnb prices?

by_neighbourhood <- listings_new %>% group_by(listings_new$city,listings_new$neighbourhood_cleansed,listings_new$bedrooms) %>%
  summarise(listing_count = n(),
            mean_log_price = mean(log_price),
            mean_price = mean(price))
by_bedroom <- listings_new %>% group_by(listings_new$bedrooms) %>%
  summarise(listing_count = n(),
            mean_log_price = mean(log_price),
            mean_price = mean(price))
by_bedroom
colnames(by_neighbourhood) <- c("city", "neighbourhood_cleansed", "bedroom","listing_count","mean_log_price","mean_price")

#by_neighbourhood
by_neighbourhood_one_bedroom <- by_neighbourhood[by_neighbourhood$bedroom==1,]
by_neighbourhood_two_bedroom <- by_neighbourhood[by_neighbourhood$bedroom==2,]
by_neighbourhood_three_bedroom <- by_neighbourhood[by_neighbourhood$bedroom==3,]
rental <- read.csv("rental.csv")
glimpse(rental)
p_load(janitor) 
rental <- clean_names(rental) 
rental$bachelor_price <- parse_number(rental$bachelor)
rental$one_bedroom_price <- parse_number(rental$x1_bedroom)
rental$two_bedroom_price <- parse_number(rental$x2_bedroom)
rental$three_bedroom_price <- parse_number(rental$x3_bedroom)
rental$price <- parse_number(rental$total)
rental <- subset(rental, select = -c(zone, neighbourhood, bachelor,
                                     x1_bedroom,x2_bedroom,x3_bedroom, total))

glimpse(rental)

# 0ne-bedroom type
one_bedroom <- merge(by_neighbourhood_one_bedroom, rental, by = "neighbourhood_cleansed")
glimpse(one_bedroom)
one_bedroom <- subset(one_bedroom, select = -c(bachelor_price,
                                               two_bedroom_price, 
                                               three_bedroom_price,
                                               price,
                                               city.y))
glimpse(one_bedroom)

complete_rows_one <- complete.cases(one_bedroom[, c("mean_log_price", "one_bedroom_price")])
one_bedroom_clean <- one_bedroom[complete_rows_one,]
one_bedroom_clean
price_threshold_one <- quantile(one_bedroom_clean$one_bedroom_price, 0.5)
top_50_percentile_one <- subset(one_bedroom_clean, one_bedroom_price >= price_threshold_one)
below_50_percentile_one <- subset(one_bedroom_clean, one_bedroom_price < price_threshold_one)
t.test(top_50_percentile_one$mean_price,top_50_percentile_one$mean_price, alternative = "greater", var.equal = FALSE)

lm_one_bedroom_clean1 <- lm(mean_log_price ~ one_bedroom_price, data = one_bedroom_clean)
summary(lm_one_bedroom_clean1)
lm_one_bedroom_clean2 <- lm(mean_price ~ one_bedroom_price, data = one_bedroom_clean)
summary(lm_one_bedroom_clean2)

# Figure 7 – Linear Regression lines between Airbnb mean price and residential rental cost (three type house)
g_one <- ggplot(one_bedroom_clean, aes(x=one_bedroom_price, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Residential rental cost", y = "Airbnb mean price") +
  ggtitle("One-bedroom type")+
  theme_clean()+
  ylim(0,400)
g_one

#Two-bedroom type
two_bedroom <- merge(by_neighbourhood_two_bedroom, rental, by = "neighbourhood_cleansed")
glimpse(two_bedroom)
two_bedroom <- subset(two_bedroom, select = -c(bachelor_price,
                                               one_bedroom_price, 
                                               three_bedroom_price,
                                               price,
                                               city.y))

complete_rows_two <- complete.cases(two_bedroom[, c("mean_log_price", "two_bedroom_price")])
two_bedroom_clean <- two_bedroom[complete_rows_two,]
two_bedroom_clean
price_threshold_two <- quantile(two_bedroom_clean$two_bedroom_price, 0.5)
top_50_percentile_two <- subset(two_bedroom_clean, two_bedroom_price >= price_threshold_two)
below_50_percentile_two <- subset(two_bedroom_clean, two_bedroom_price < price_threshold_two)
t.test(top_50_percentile_two$mean_price,top_50_percentile_two$mean_price, alternative = "greater", var.equal = FALSE)

lm_two_bedroom_clean1 <- lm(mean_log_price ~ two_bedroom_price, data = two_bedroom_clean)
summary(lm_two_bedroom_clean1)
lm_two_bedroom_clean2 <- lm(mean_price ~ two_bedroom_price, data = two_bedroom_clean)
summary(lm_two_bedroom_clean2)

ggplot(two_bedroom_clean, aes(x=two_bedroom_price, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Residential rental cost", y = "Airbnb Price") +
  ggtitle("Airbnb mean price vs. Residential rental cost
          Two-bedroom type")+
  theme_clean()

#Remove outliers in residential cost in two bedroom type
quartiles_r2 <- quantile(two_bedroom_clean$two_bedroom_price, probs=c(.25, .75), na.rm = FALSE)
IQR_r2 <- IQR(two_bedroom_clean$two_bedroom_price)
Lower_r2 <- quartiles_r2[1] - 1.5*IQR_r2
Upper_r2 <- quartiles_r2[2] + 1.5*IQR_r2
two_bedroom_clean_2 <- subset(two_bedroom_clean, two_bedroom_clean$two_bedroom_price > Lower_r2 & two_bedroom_clean$two_bedroom_price < Upper_r2)
lm_two_bedroom_clean3 <- lm(mean_price ~ two_bedroom_price, data = two_bedroom_clean_2)
summary(lm_two_bedroom_clean3)

g_two <- ggplot(two_bedroom_clean_2, aes(x=two_bedroom_price, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Residential rental cost", y = "Airbnb mean price") +
  ggtitle("Two-bedroom type")+
  theme_clean()
g_two
# Three-bedroom type
three_bedroom <- merge(by_neighbourhood_three_bedroom, rental, by = "neighbourhood_cleansed")
glimpse(three_bedroom)
three_bedroom <- subset(three_bedroom, select = -c(bachelor_price,
                                                   one_bedroom_price, 
                                                   two_bedroom_price,
                                                   price,
                                                   city.y))

complete_rows_three <- complete.cases(three_bedroom[, c("mean_log_price", "three_bedroom_price")])
three_bedroom_clean <- three_bedroom[complete_rows_three,]
three_bedroom_clean
price_threshold_three <- quantile(three_bedroom_clean$three_bedroom_price, 0.5)
top_50_percentile_three <- subset(three_bedroom_clean, three_bedroom_price >= price_threshold_three)
below_50_percentile_three <- subset(three_bedroom_clean, three_bedroom_price < price_threshold_three)
t.test(top_50_percentile_three$mean_price,top_50_percentile_three$mean_price, alternative = "greater", var.equal = FALSE)

lm_three_bedroom_clean1 <- lm(mean_log_price ~ three_bedroom_price, data = three_bedroom_clean)
summary(lm_three_bedroom_clean1)
lm_three_bedroom_clean2 <- lm(mean_price ~ three_bedroom_price, data = three_bedroom_clean)
summary(lm_three_bedroom_clean2)

g_three <- ggplot(three_bedroom_clean, aes(x=three_bedroom_price, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Residential rental cost", y = "Airbnb mean price") +
  ggtitle("Three-bedroom type")+
  theme_clean()
g_three
grid.arrange(g_one, g_two, g_three, ncol=3)

# Combine data
colnames(one_bedroom_clean)[7] <- "rental_cost"
colnames(two_bedroom_clean)[7] <- "rental_cost"
colnames(three_bedroom_clean)[7] <- "rental_cost"
rental_combine <- bind_rows(
  mutate(one_bedroom_clean, Type = "One bedroom"),
  mutate(two_bedroom_clean, Type = "Two bedroom"),
  mutate(three_bedroom_clean, Type = "Three bedroom"))
rental_combine

rental_combine$two_bedroom<-ifelse (rental_combine$Type == "Two bedroom", 1, 0) 
rental_combine$three_bedroom<-ifelse (rental_combine$Type == "Three bedroom", 1, 0) 
glimpse(rental_combine)

# Figure 8 - Regression model between Airbnb mean price with residential rental cost and bedroom types
lm_total <- lm(mean_price ~ rental_cost + two_bedroom + three_bedroom, data = rental_combine)
summary(lm_total)

lm_total <- lm(mean_price ~ rental_cost + Type, data = rental_combine)
summary(lm_total)

lm_total.coef <- coef(lm_total)
lm_total.coef
# Figure 9 – Linear regression between Airbnb mean price and residential cost (black line) and how each bedroom type line impacts the regression line.
# Total line
gt1 <- ggplot(rental_combine, aes(x=rental_cost, y=mean_price)) + 
  geom_point(color='#2980B9', size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  labs(x = "Residential rental cost", y = "Airbnb Price") +
  ggtitle("Airbnb price vs. Residential rental cost")+
  theme_clean()
# Each dummy variable line
gt2 <- ggplot(rental_combine, aes(x=rental_cost, y=mean_price)) + 
  geom_point(aes(color = rental_combine$Type), size = 1) + 
  geom_smooth(method=lm, color='#2C3E50')+
  geom_abline(aes(intercept=lm_total.coef[1],slope=lm_total.coef[2],col="One bedroom"))+
  geom_abline(aes(intercept=lm_total.coef[1]+lm_total.coef[3],slope=lm_total.coef[2],col="Two bedroom"))+
  geom_abline(aes(intercept=lm_total.coef[1]+lm_total.coef[4],slope=lm_total.coef[2],col="Three bedroom"))+
  labs(x = "Residential rental cost", y = "Airbnb Price") +
  ggtitle("Airbnb price vs. Residential rental cost")+
  theme_clean()
grid.arrange(gt1, gt2, ncol=2)

# Combine lines together
ggplot(rental_combine, aes(x = rental_cost, y = mean_price)) + 
  geom_point(aes(color = Type), size = 1) + 
  geom_smooth(method = lm, color = '#2C3E50') +
  geom_abline(aes(intercept = lm_total.coef[1], slope = lm_total.coef[2], color = "One bedroom")) +
  geom_abline(aes(intercept = lm_total.coef[1] + lm_total.coef[3], slope = lm_total.coef[2], color = "Two bedroom")) +
  geom_abline(aes(intercept = lm_total.coef[1] + lm_total.coef[4], slope = lm_total.coef[2], color = "Three bedroom")) +
  labs(x = "Residential rental cost", y = "Airbnb Price") +
  ggtitle("Airbnb price vs. Residential rental cost") +
  theme_clean() +
  scale_color_manual(values = c("One bedroom" = "red", "Two bedroom" = "blue", "Three bedroom" = "green"),
                     name = "Type")

# Figure 10 - Top locations with high rental costs
top5 <- rental_combine %>%
  group_by(Type) %>%
  arrange(desc(rental_cost)) %>%
  slice_head(n = 5) %>%
  ungroup()
top5 <- arrange(top5, bedroom, desc(rental_cost))
top5
# Top locations with high rental costs
ggplot(top5, aes(x = paste(neighbourhood_cleansed, " - ", city.x), y = rental_cost, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge2(width = 2, preserve = "single"), width = 0.7) +
  labs(x = "Neighborhood", y = "Rental Cost") +
  ggtitle("Top rental costs for each bedroom type") +
  theme_clean() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







