# Superstore_Consumer_Insights_Project
## Project Overview

This project analyzes the Superstore dataset to extract insights on customer behavior and sales performance. The objective was to clean the data, engineer features for customer-level analysis, predict Customer Lifetime Value (CLV) using multiple models, and segment customers using clustering.

## Tasks Completed

### Task 1. Data Cleaning and Feature Engineering
Removed unnecessary columns and converted dates and categorical variables.
Handled missing and duplicated values.
Created customer-level variables such as:
Average Order Value
Tenure (days)
Purchase Frequency
### Task 2. Predictive Modeling – Customer Lifetime Value
Built and compared the following models to predict total sales per customer:
Linear Regression
Lasso Regression
Random Forest
Evaluated models using R-squared and MAPE on both training and test data.
### Task 3. Customer Segmentation – K-Means Clustering
Scaled customer metrics and generated dummy variables.
Used the elbow method to identify optimal clusters (k=4).
Visualized and interpreted clusters by region, segment, and category.

## Technologies Used

R – primary language used for the entire pipeline  
ggplot2 – for visualizations  
dplyr, tidyr – for data manipulation  
caret, glmnet, randomForest – for modeling  
factoextra – for clustering visualization

### Results and Analysis

Identified key drivers of customer value using regression and feature importance.
Found that Random Forest offered better predictive accuracy.
Segmented customers into four distinct clusters with varying behaviors and value.
Offered insights into regional and segment-level marketing strategies.
