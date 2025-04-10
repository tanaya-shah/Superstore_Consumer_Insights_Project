# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(corrplot)
library(ggplot2)
library(dplyr)

# Import the dataset
df <- read.csv(file.choose())

# Preview the data
head(df)
str(df)
summary(df)

#####################
### Data Cleaning ###
#####################

# remove columns - `Row ID`, `customer name`, `postal code`, `country`, `Sub.Category`
drops <- c("Row.ID","Customer.Name","Postal.Code", "Country", "Product.Name", "Sub.Category")
DATA <- df[,!(names(df) %in% drops)]

# Convert date columns
DATA$`Order.Date` <- mdy(DATA$`Order.Date`)
DATA$`Ship.Date` <- mdy(DATA$`Ship.Date`)

# Convert categorical variables to factors
categorical_vars <- c("Ship.Mode", "Segment", "City", "State", "Region", "Category")
DATA[categorical_vars] <- lapply(DATA[categorical_vars], as.factor)

# Ensure numeric variables are numeric
numeric_vars <- c("Sales", "Quantity", "Discount", "Profit")
DATA[numeric_vars] <- lapply(DATA[numeric_vars], as.numeric)

# Check for missing and duplicated values
colSums(is.na(DATA))
duplicated(DATA)

# Create dummy variables, excluding the intercept column (which would cause multicollinearity)
dummies <- model.matrix(~ State + Region + Ship.Mode + Segment + Category - 1, data = DATA)

# Combine the dummy variables with the original dataset
DATA <- cbind(DATA, dummies)

# Preview the data
head(DATA)
str(DATA)
summary(DATA)

# Export excel file
# install.packages("openxlsx")
# library(openxlsx)
# 
# write.xlsx(DATA, "cleaned_data.xlsx")


#####################
### Visualization ###
#####################

# 1. Set up Data
profit_data_by_segment_year <- DATA %>%
  mutate(Year = year(as.Date(Order.Date, "%m/%d/%Y"))) %>%
  group_by(Year, Segment) %>%
  summarize(Total_Profit = sum(Profit, na.rm = TRUE),
            Total_Sales=sum(Profit,na.rm = TRUE))

sales_profit_by_category_year <- DATA %>%
  mutate(Year = year(as.Date(Order.Date, "%m/%d/%Y"))) %>%
  group_by(Year, Category) %>%
  summarize(
    Total_Sales = sum(Sales, na.rm = TRUE),
    Total_Profit = sum(Profit, na.rm = TRUE)
  )

customer_count_data <- DATA %>%
  mutate(Order.Date = as.Date(Order.Date, "%m/%d/%Y")) %>%
  mutate(Year = year(Order.Date)) %>%
  group_by(Year) %>%
  summarize(Customer_Count = length(unique(Customer.ID)))

increaseinsales_by_segment <- profit_data_by_segment_year %>%
  arrange(Year) %>%
  group_by(Segment) %>%
  mutate(IncreaseInSales = (Total_Sales / lag(Total_Sales) - 1) * 100)

increaseinsales_by_category <- sales_profit_by_category_year %>%
  arrange(Year) %>%
  group_by(Category) %>%
  mutate(IncreaseInSales = (Total_Sales / lag(Total_Sales) - 1) * 100)

# 2. Draw up graphs

# Ship Mode vs. Sales
ggplot(data = DATA, aes(x = `Ship.Mode`, y = Sales)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  labs(title = "Ship Mode vs. Sales", x = "Ship Mode", y = "Sales") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 300))

# Segment vs. Sales
ggplot(data = DATA, aes(x = `Segment`, y = Sales)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  labs(title = "Segment vs. Sales", x = "Segment", y = "Sales") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 300))

# State vs. Sales
ggplot(data = DATA, aes(x = `State`, y = Sales)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  labs(title = "State vs. Sales", x = "State", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  coord_cartesian(ylim = c(0, 2000))

# Category vs. Sales
ggplot(data = DATA, aes(x = `Category`, y = Sales)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  labs(title = "Category vs. Sales", x = "Category", y = "Sales") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1000))

# Discount vs. Sales
ggplot(data = DATA, aes(x = as.factor(Discount), y = Sales)) +
  geom_boxplot(outlier.shape = NA, width = 0.7) +
  labs(title = "Discount vs. Sales", x = "Discount", y = "Sales") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 2000))


# Increase in Customers Vs Time
ggplot(customer_count_data, aes(x = Year, y = Customer_Count)) +
  geom_line() +
  labs(title = "Increase in Customers Over Time",
       x = "Year",
       y = "Number of Customers")

# Increase in Sales by Segment Vs Year
ggplot(increaseinsales_by_segment, aes(x = Year, y = IncreaseInSales, color = Segment)) +
  geom_line() +
  labs(title = "Increase in Sales by Segment Per Year",
       x = "Year",
       y = "Increase in Sales (%)") 

# Increase in Sales by Category Vs Year
ggplot(increaseinsales_by_category, aes(x = Year, y = IncreaseInSales, color = Category)) +
  geom_line() +
  labs(title = "Increase in Sales by Category Per Year",
       x = "Year",
       y = "Increase in Sales (%)") 

##############
### End ###
##############

# Create new table for customer lifetime value prediction and customer segmentation 
# Step 1: Calculate the most frequent value
get_mode <- function(x) {
  x[which.max(table(x))]
}

NEW_DATA <- DATA %>%
  group_by(Customer.ID) %>%
  summarise(
    total_sales = sum(Sales),
    total_profit = sum(Profit),
    total_orders = n_distinct(Order.ID),
    avg_order_value = mean(Sales),
    first_purchase = min(Order.Date),
    last_purchase = max(Order.Date),
    region = get_mode(Region),        # Get the most frequent region
    ship_mode = get_mode(Ship.Mode),  # Get the most frequent ship mode
    segment = get_mode(Segment),      # Get the most frequent segment
    category = get_mode(Category),    # Get the most frequent category
  ) %>%
  mutate(
    tenure_days = as.numeric(difftime(last_purchase, first_purchase, units = "days")),
    frequency = total_orders / tenure_days
  )

# Replace NA values in tenure_days or frequency with 0 (if tenure is 0, the frequency might be NA)
NEW_DATA$tenure_days[is.na(NEW_DATA$tenure_days)] <- 0
NEW_DATA$tenure_days[is.infinite(NEW_DATA$tenure_days)] <- 0
NEW_DATA$frequency[is.na(NEW_DATA$frequency)] <- 0
NEW_DATA$frequency[is.infinite(NEW_DATA$frequency)] <- 0

NEW_DATA <- NEW_DATA %>% filter(!is.na(region))
NEW_DATA <- NEW_DATA %>% filter(!is.na(ship_mode))
NEW_DATA <- NEW_DATA %>% filter(!is.na(segment))
NEW_DATA <- NEW_DATA %>% filter(!is.na(category))

# Convert categorical variables to dummy variables
NEW_DATA <- dummy_cols(NEW_DATA, select_columns = c("region", "ship_mode", "segment", "category"), 
                       remove_first_dummy = TRUE)

install.packages("fastDummies")
library(fastDummies)

# Install required packages
install.packages("glmnet")
install.packages("randomForest")
install.packages("caret")

# Load libraries
library(glmnet) 
library(randomForest)
library(caret)

# Split the data into training and testing sets (80/20 split)
set.seed(123)
trainIndex <- createDataPartition(NEW_DATA$total_sales, p = .8, list = FALSE)
cltv_train <- NEW_DATA[trainIndex, ]
cltv_test <- NEW_DATA[-trainIndex, ]

cltv_train <- cltv_train %>%
  mutate(frequency = ifelse(tenure_days == 0, 0, total_orders / tenure_days))

cltv_test <- cltv_test %>%
  mutate(frequency = ifelse(tenure_days == 0, 0, total_orders / tenure_days))

# Check for Inf values in cltv_train and cltv_test
sum(is.infinite(cltv_train$total_sales))
sum(is.infinite(cltv_test$total_sales))

sum(is.infinite(cltv_train$total_orders))
sum(is.infinite(cltv_test$total_orders))

# Check for NaN values in the dataset
sum(is.nan(cltv_train$total_sales))
sum(is.nan(cltv_test$total_sales))

##########################################
### Customer Lifetime Value Prediction ###
##########################################

### A. Linear Regression Model

head(cltv_train)

# 1. Fit a linear regression model
lm_model <- lm(total_sales ~ total_orders + avg_order_value + region_East + region_South + region_West + 
                 segment_Corporate + `segment_Home Office` + `category_Office Supplies` +
                 category_Technology
               ,data = cltv_train)

summary(lm_model)

# 2. Make predictions for both in-sample (training) and out-of-sample (test)
predicted_train <- predict(lm_model, newdata = cltv_train)
predicted_test <- predict(lm_model, newdata = cltv_test)

# 3. Calculate Out-of-sample R-squared
# Calculate total sum of squares (SST) for the test data
ss_total <- sum((cltv_test$total_sales - mean(cltv_test$total_sales))^2)

# Calculate residual sum of squares (SSR) for the test data
ss_residual <- sum((cltv_test$total_sales - predicted_test)^2)

r_squared_test <- 1 - (ss_residual / ss_total)
print(paste("Out-of-sample R-squared:", r_squared_test))

# 4. Calculate In-sample MAPE
mape_train <- mean(abs((cltv_train$total_sales - predicted_train) / cltv_train$total_sales)) * 100
print(paste("In-sample MAPE:", mape_train))

# 5. Calculate In-sample MAPE
mape_test <- mean(abs((cltv_test$total_sales - predicted_test) / cltv_test$total_sales)) * 100
print(paste("Out-of-sample MAPE:", mape_test))

### B. Lasso Regression Model

# 1. Removing the intercept column
x_train <- model.matrix(total_sales ~ total_orders + avg_order_value + region_East + region_South + region_West + 
                          segment_Corporate + `segment_Home Office` + `category_Office Supplies` +
                          category_Technology, cltv_train)[,-1]
y_train <- cltv_train$total_sales

x_test <- model.matrix(total_sales ~ total_orders + avg_order_value + region_East + region_South + region_West + 
                         segment_Corporate + `segment_Home Office` + `category_Office Supplies` +
                         category_Technology, cltv_test)[,-1]
y_test <- cltv_test$total_sales

# 2. Fit a Lasso regression model
set.seed(123)
lasso_model <- cv.glmnet(x_train, y_train, alpha = 1)  # alpha=1 for Lasso

# 3. Make predictions for both in-sample (training) and out-of-sample (test)
lasso_predicted_train <- predict(lasso_model, s = "lambda.min", newx = x_train)
lasso_predicted_test <- predict(lasso_model, s = "lambda.min", newx = x_test)

# 4. Calculate In-sample R-squared
ss_total_train <- sum((y_train - mean(y_train))^2)  # Total sum of squares
ss_residual_train <- sum((y_train - lasso_predicted_train)^2)  # Residual sum of squares
r_squared_train <- 1 - (ss_residual_train / ss_total_train)
print(paste("In-sample R-squared: ", r_squared_train))

# 5. Calculate Out-of-sample R-squared
ss_total_test <- sum((y_test - mean(y_test))^2)  # Total sum of squares
ss_residual_test <- sum((y_test - lasso_predicted_test)^2)  # Residual sum of squares
r_squared_test <- 1 - (ss_residual_test / ss_total_test)
print(paste("Out-of-sample R-squared: ", r_squared_test))

# 6. Calculate In-sample MAPE
mape_train <- mean(abs((y_train - lasso_predicted_train) / y_train)) * 100
print(paste("In-sample MAPE:", mape_train))

# 7. Calculate In-sample MAPE
mape_test <- mean(abs((y_test - lasso_predicted_test) / y_test)) * 100
print(paste("Out-of-sample MAPE:", mape_test))

### C. Random Forest model
set.seed(123)
rf_model <- randomForest(total_sales ~ total_orders + avg_order_value
                         + region + category + segment,
                         data = cltv_train)


# 1. In-sample (training data) and Out-of-sample (test data) predictions
rf_predicted_train <- predict(rf_model, newdata = cltv_train)
rf_predicted_test <- predict(rf_model, newdata = cltv_test)

# 2. Calculate In-sample R-squared
ss_total_train <- sum((cltv_train$total_sales - mean(cltv_train$total_sales))^2)
ss_residual_train <- sum((cltv_train$total_sales - rf_predicted_train)^2)
r_squared_train <- 1 - (ss_residual_train / ss_total_train)
print(paste("Random Forest In-sample R-squared: ", r_squared_train))

# 3. Calculate Out-of-sample R-squared
ss_total_test <- sum((cltv_test$total_sales - mean(cltv_test$total_sales))^2)
ss_residual_test <- sum((cltv_test$total_sales - rf_predicted_test)^2)
r_squared_test <- 1 - (ss_residual_test / ss_total_test)
print(paste("Random Forest Out-of-sample R-squared: ", r_squared_test))

# 4. Calculate In-sample MAPE
mape_train <- mean(abs((rf_predicted_train - cltv_train$total_sales) / rf_predicted_train)) * 100
print(paste("In-sample MAPE:", mape_train))

# 5. Calculate In-sample MAPE
mape_test <- mean(abs((rf_predicted_test - cltv_test$total_sales) / rf_predicted_test)) * 100
print(paste("Out-of-sample MAPE:", mape_test))

# 6. Check feature importance
importance(rf_model)
varImpPlot(rf_model)


##########################
### K-means clustering ###
##########################

library(tidyverse)
library(dplyr)

install.packages("factoextra")
library(factoextra)

# 1. Prepare data for clustering

# Scale numerical features
clustering_data <- NEW_DATA %>%
  mutate_at(vars(total_sales, total_profit, total_orders, avg_order_value, tenure_days, frequency), scale)

clustering_data <- dummy_cols(clustering_data, select_columns = c("region", "ship_mode", "segment", "category"), 
                              remove_first_dummy = TRUE)

# Check structure
str(clustering_data)

# Convert all non numeric columns to numeric or remove
clustering_data <- clustering_data %>%
  select_if(is.numeric)  # Keep only numeric columns

# Check for NaN and Inf values
sum(is.nan(as.matrix(clustering_data)))
sum(is.infinite(as.matrix(clustering_data)))

# Remove rows with NaN or Inf values
clustering_data_clean <- clustering_data %>%
  filter_all(all_vars(!is.nan(.))) %>%
  filter_all(all_vars(!is.infinite(.)))

# 2. Use the Elbow Method to determine the optimal number of clusters
fviz_nbclust(clustering_data_clean, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)

# Set seed for reproducibility
set.seed(123)

# 3. Apply k-means clustering with k = 4
FourCenters <- kmeans(clustering_data_clean, centers = 4, nstart = 30)

# Print clustering results
print(FourCenters)

# Examine cluster centers
FourCenters$centers[1,]
FourCenters$centers[2,]
FourCenters$centers[3,]
FourCenters$centers[4,]

# Check cluster sizes
FourCenters$size

# Calculate variation explained by the 4 clusters
1 - FourCenters$tot.withinss/ FourCenters$totss
# around 40%

# Add cluster assignment to the original data set
clustering_data_clean$Cluster <- FourCenters$cluster

# 4. Visualization
library(dplyr)
fviz_cluster(FourCenters, data = clustering_data_clean) +
  labs(title = "K-means Clustering")

# 5. Reverse Scale using the means and standard deviation
means <- colMeans(NEW_DATA[,c("total_sales", "total_profit", "total_orders", "avg_order_value", "tenure_days", "frequency")], na.rm = TRUE)
standard_deviations <- apply(NEW_DATA[ , c("total_sales", "total_profit", "total_orders", "avg_order_value", "tenure_days", "frequency")], 2, sd, na.rm = TRUE)

clustering_data_clean <- clustering_data_clean %>%
  mutate(
    total_sales = total_sales * standard_deviations["total_sales"] + means["total_sales"],
    total_profit = total_profit * standard_deviations["total_profit"] + means["total_profit"],
    total_orders = total_orders * standard_deviations["total_orders"] + means["total_orders"],
    avg_order_value = avg_order_value * standard_deviations["avg_order_value"] + means["avg_order_value"],
    tenure_days = tenure_days * standard_deviations["tenure_days"] + means["tenure_days"],
    frequency = frequency * standard_deviations["frequency"] + means["frequency"]
  )

# 6. Analyze Cluster Characteristics
aggregate(clustering_data_clean$total_sales ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$total_orders ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$avg_order_value ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$tenure_days ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$frequency ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$region_East ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$region_South ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$region_West ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`ship_mode_Same Day` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`ship_mode_Second Class` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`ship_mode_Standard Class` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`segment_Corporate` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`segment_Home Office` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`category_Office Supplies` ~ clustering_data_clean$Cluster, FUN = mean)
aggregate(clustering_data_clean$`category_Technology` ~ clustering_data_clean$Cluster, FUN = mean)

library(dplyr)
library(tidyr)


# Clustering Region Summary
region_summary <- clustering_data_clean %>%
  group_by(Cluster) %>%
  mutate(region_Central = 1 - (region_East + region_South + region_West))  %>%
  summarise(
    region_East = mean(region_East),
    region_South = mean(region_South),
    region_West = mean(region_West),
    region_Central = mean(region_Central)
  ) 

region_summary <- region_summary%>%
  pivot_longer(cols = starts_with("region_"), names_to = "Region", values_to = "Proportion")

cluster_1_data <- region_summary %>%
  filter(Cluster == 1) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_1_data, aes(x = "", y = Proportion, fill = Region)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 1") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_2_data <- region_summary %>%
  filter(Cluster == 1) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_1_data, aes(x = "", y = Proportion, fill = Region)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 1") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_2_data <- region_summary %>%
  filter(Cluster == 2) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_2_data, aes(x = "", y = Proportion, fill = Region)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() +
  labs(title = "Cluster 2") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_3_data <- region_summary %>%
  filter(Cluster == 3) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_3_data, aes(x = "", y = Proportion, fill = Region)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 3") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_4_data <- region_summary %>%
  filter(Cluster == 4) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_4_data, aes(x = "", y = Proportion, fill = Region)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 4") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

# Clustering  Segment Summary
segment_summary <- clustering_data_clean %>%
  group_by(Cluster) %>%
  mutate(segment_Consumer = 1 - (segment_Corporate + `segment_Home Office`))  %>%
  summarise(
    segment_Consumer = mean(segment_Consumer),
    segment_Corporate = mean(segment_Corporate),
    `segment_Home Office`= mean(`segment_Home Office`)
  ) 

segment_summary <- segment_summary %>%
  pivot_longer(cols = starts_with("segment_"), names_to = "Segment", values_to = "Proportion")

cluster_1_data <- segment_summary %>%
  filter(Cluster == 1) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_1_data, aes(x = "", y = Proportion, fill = Segment)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 1") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_2_data <- segment_summary %>%
  filter(Cluster == 2) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_2_data, aes(x = "", y = Proportion, fill = Segment)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 2") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_3_data <- segment_summary %>%
  filter(Cluster == 3) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_3_data, aes(x = "", y = Proportion, fill = Segment)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 3") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_4_data <- segment_summary %>%
  filter(Cluster == 4) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_4_data, aes(x = "", y = Proportion, fill = Segment)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 4") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

# Clustering Category Summary
category_summary <- clustering_data_clean %>%
  group_by(Cluster) %>%
  mutate(category_Furniture = 1 - (`category_Office Supplies` + category_Technology ))  %>%
  summarise(
    `category_Office Supplies` = mean(`category_Office Supplies`),
    category_Technology = mean(category_Technology),
    category_Furniture = mean(category_Furniture)
  ) 

category_summary <- category_summary %>%
  pivot_longer(cols = starts_with("category_"), names_to = "Category", values_to = "Proportion")

cluster_1_data <- category_summary %>%
  filter(Cluster == 1) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_1_data, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 1") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_2_data <- category_summary %>%
  filter(Cluster == 2) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_2_data, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 2") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_3_data <- category_summary %>%
  filter(Cluster == 3) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_3_data, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 3") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

cluster_4_data <- category_summary %>%
  filter(Cluster == 4) %>%
  mutate(Percentage = Proportion * 100)

ggplot(cluster_4_data, aes(x = "", y = Proportion, fill = Category)) +
  geom_bar(stat = "identity",width = 1) +
  coord_polar("y", start = 0) + 
  theme_void() + 
  labs(title = "Cluster 4") +
  geom_text(aes(label = round(Percentage,2)),
            position = position_stack(vjust = 0.5))

