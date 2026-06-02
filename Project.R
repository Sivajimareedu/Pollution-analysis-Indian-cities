dataset<-read.csv("C:/Users/sivaj/Downloads/city_day.csv/city_day.csv", header=TRUE)
print(dataset)
library("ggplot2")
#Preprocessing
#head and tail
head(dataset$AQI)
tail(dataset$AQI)
head(dataset$AQI_Bucket)
tail(dataset$AQI_Bucket)
summary(dataset)
#NA
n1<-which(is.na(dataset$PM2.5))
n2<-which(is.na(dataset$PM10))
n3<-which(is.na(dataset$NO))
n4<-which(is.na(dataset$NO2))
n5<-which(is.na(dataset$NOx))
n6<-which(is.na(dataset$NH3))
n7<-which(is.na(dataset$CO))
n8<-which(is.na(dataset$SO2))
n9<-which(is.na(dataset$O3))
n10<-which(is.na(dataset$Benzene))
n11<-which(is.na(dataset$Toluene))
n12<-which(is.na(dataset$Xylene))
n13<-which(is.na(dataset$AQI))

library(ggplot2)
# Check for missing values and impute with mean
dataset <- data.frame(lapply(dataset, function(x) if (is.numeric(x)) ifelse(is.na(x), mean(x, na.rm=TRUE), x) else x))

# Visualizing PM2.5 vs AQI using ggplot2
ggplot(dataset, aes(x=PM2.5, y=AQI)) +
  geom_point() +
  labs(title="PM2.5 vs AQI", x="PM2.5", y="AQI")

# Example probability calculation (for AQI values > 200)
probability <- sum(dataset$AQI > 200) / nrow(dataset)
print(paste("Probability of AQI > 200:", probability))

# Load necessary libraries
library(ggplot2)

dataset$Date <- as.Date(dataset$Date, format="%Y-%m-%d")

ggplot(dataset, aes(x=Date, y=PM2.5, group=City, color=City)) +
  geom_line() +
  labs(title="PM2.5 Levels Over Time", x="Date", y="PM2.5") +
  theme_minimal()

# Optional: Line graph of AQI over time
ggplot(dataset, aes(x=Date, y=AQI, group=City, color=City)) +geom_line() +
  labs(title="AQI Over Time", x="Date", y="AQI") +
  theme_minimal()
        




# Load necessary libraries
library(ggplot2)

# Ensure the Date column is in the correct format
dataset$Date <- as.Date(dataset$Date, format="01-07-2016")

# Filter the dataset for Visakhapatnam
vizag_data <- subset(dataset,City == "Visakhapatnam")

# Plot a line graph of PM2.5 over time for Visakhapatnam
ggplot(vizag_data, aes(x=Date, y=PM2.5)) +
  geom_line(color="blue") +
  labs(title="PM2.5 Levels Over Time in Visakhapatnam", x="Date", y="PM2.5") +theme_minimal()

# Optional: Line graph of AQI over time for Visakhapatnam
ggplot(vizag_data, aes(x=Date, y=AQI)) +
  geom_line(color="red") +
  labs(title="AQI Over Time in Visakhapatnam", x="01", y="AQI") +
  theme_minimal()





#probability to increase in next 2 years
library(ggplot2)

# Ensure the Date column is in the correct format
vizag_data$Date <- as.Date(vizag_data$Date, format="%Y-%m-%d")

# Check column names
colnames(vizag_data)

# Filter the dataset for Visakhapatnam
Vizag_data <- subset(dataset, City =="Visakhapatnam")

# Remove rows with missing CO values (use !is.na to keep non-NA values)
vizag_data <- vizag_data[!is.na(vizag_data$CO), ]

# Fit a linear model to CO levels over time
model <- lm(CO ~ Date, data=vizag_data)

# Predict future CO levels 2 years (730 days) from the last date in the dataset
last_date <- max(vizag_data$Date)
future_date <- last_date + 730  # 2 years from last recorded date
predicted_CO <- predict(model, newdata=data.frame(Date=future_date))

# Calculate the current average CO level
current_CO <- mean(vizag_data$CO, na.rm=TRUE)

# Calculate the required 50% increase
required_CO_increase <- current_CO * 1.50

# Calculate the probability that CO will increase by at least 50%
if (predicted_CO > required_CO_increase) {
  prob_increase <- 1
} else {
  prob_increase <- 0
}

# Print results
print(paste("Predicted CO level in 2 years:", predicted_CO))
print(paste("Current average CO level:", current_CO))
print(paste("Required CO level for 50% increase:", required_CO_increase))
print(paste("Probability of a 50% increase in CO levels:", prob_increase))







#highest aqi and lowest aqi
# Load necessary libraries
library(dplyr)
str(dataset)
# Ensure AQI column is numeric
dataset$AQI <- as.numeric(dataset$AQI)
# Find the city with the highest AQI
highest_aqi <- dataset %>%
  filter(AQI == max(AQI, na.rm = TRUE))

# Print the result
print(highest_aqi)
# Find the city with the lowest AQI
lowest_aqi <- dataset %>%
  filter(AQI == min(AQI, na.rm = TRUE))

# Print the result
print(lowest_aqi)







#charts

# Bar Chart
ggplot(dataset, aes (x = AQI, y = City, fill = City)) +
  geom_bar(stat = "identity") +
  labs(title = "Highest and Lowest AQI Cities", x = "City", y = "AQI") + theme_minimal()





#probability
# Example: Average number of high AQI days per year
lambda <- 50  # Average number of high AQI days per year

# Probability of exactly 100 high AQI days
prob_exact <- dpois(100, lambda)
print(paste("Probability of exactly 100 high AQI days:", prob_exact))

# Probability of at most 100 high AQI days
prob_at_most <- ppois(100, lambda)
print(paste("Probability of at most 100 high AQI days:", prob_at_most))




#for regression

# Filter for Visakhapatnam
vizag_data <- subset(dataset,City == "Visakhapatnam")
# Remove rows with missing values in key columns using na.omit
cleaned_data <- vizag_data[complete.cases(vizag_data[c("AQI", "PM2.5", "PM10", "NO", "NO2", "NOx", "SO2", "CO", "O3")]), ]

# Check the cleaned data
head(cleaned_data)

# Check if cleaned_data has rows
if (nrow(cleaned_data) == 0) {
  stop("No data available after cleaning. Check for missing values in the key columns.")
}

# Perform linear regression to predict AQI using key pollutants
model <- lm(AQI ~ PM2.5 + PM10 + NO + NO2 + NOx + SO2 + CO + O3, data = cleaned_data)

# Summarize the regression model
summary(model)

# Install and load ggplot2 if not already done
install.packages("ggplot2")
library(ggplot2)
# Scatterplot for AQI vs PM2.5
ggplot(cleaned_data, aes(x = PM2.5, y = AQI)) +
  geom_point(color = "blue", alpha = 0.6) +   # Add scatter points
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Add linear regression line
  labs(
    title = "Relationship between PM2.5 and AQI",
    x = "PM2.5 Concentration ",
    y = "Air Quality Index (AQI)"
  ) +
  theme_minimal() 


#correlation
# Ensure Date column is in Date format
cleaned_data$Date <- as.Date(cleaned_data$Date)

# Extract Year from the Date column
cleaned_data$Year <- format(cleaned_data$Date, "%Y")

# Boxplot of AQI by Year
ggplot(cleaned_data, aes(x = Year, y = AQI)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Distribution of AQI Across Years in Visakhapatnam",
    x = "Year",
    y = "Air Quality Index (AQI)"
  ) +
  theme_minimal() 

# Extract year from Date and calculate average AQI per year
ggplot(cleaned_data, aes(x = factor(Year), y = AQI)) +
  geom_boxplot() +
  labs(title = "Box Plot of AQI by Year for Visakhapatnam",
       x = "Year",
       y = "AQI") +
  theme_minimal()

#Anova
# Perform ANOVA
anova_result <- aov(AQI ~ factor(NH3), data = cleaned_data)
summary(anova_result)
#### K means clustering


# Perform K-means clustering
kmeans_result <- kmeans( cleaned_data[, c("PM2.5", "PM10")], centers = 3)

# Add cluster results to the data frame
cleaned_data$Cluster <- as.factor(kmeans_result$cluster)
# Create a scatter plot of PM2.5 vs PM10 colored by cluster
ggplot(cleaned_data, aes(x = PM2.5, y = PM10, color = Cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = " Clustering of PM2.5 and PM10 in Visakhapatnam",
       x = "PM2.5",
       y = "PM10") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green")) 


# Calculate correlation matrix
# Load necessary libraries
install.packages("corrplot")
library(ggplot2)
library(dplyr)
library(corrplot)
correlation_matrix <- cor(cleaned_data[, c("NO", "NO2")])
# Visualize correlation matrix using corrplot
corrplot(correlation_matrix, method = "circle", type = "upper", 
         tl.col = "black", tl.srt = 45,
         title = "Correlation between NO and NO2 in Visakhapatnam",
         mar = c(0,0,1,0))


