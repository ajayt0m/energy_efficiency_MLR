'''
1. Detailed Problem Statement
Bengaluru experiences extreme temperatures during the summer, leading to increased reliance on air conditioning and cooling systems in commercial and residential buildings. This surge in cooling loads can lead to overloading of the electricity distribution system, causing potential blackouts, 
reduced service reliability, and increased operating costs. Therefore, understanding the factors that influence cooling load demands is crucial for enhancing energy efficiency and sustainability.
The specific problem is to investigate the influence of different building characteristics on cooling load demands experienced by various buildings throughout Bengaluru during the hot summer months. 
This investigation aims to determine the key building attributes that significantly contribute to higher cooling loads and how these attributes vary across different areas and building types.

2. Industry
The Energy Management industry focuses on optimizing energy use, reducing wastage, and promoting sustainable practices for businesses and individuals. It involves monitoring, analyzing, and implementing strategies to improve energy efficiency and reduce environmental impact.

3. Objectives 
Identify significant building characteristics impacting cooling load demands during Bengalurus hot summer season.
Investigate individual features effects on cooling load, understanding their positive or negative influences.
Develop a predictive model for forecasting future summer cooling load demands, aiding effective energy distribution planning.
'''


#1. Import
# Load the required package
library(readxl)

# Set the file path
file_path <- "C:/Users/imaja/OneDrive/Desktop/MBA 2022 -2024/T4/MLA1/CIA1/energy+efficiency/bescom_load_data.xlsx"

# Read the data from the Excel file
bescom_df <- read_excel(file_path)

# View the first few rows of the data
library(DT)
head(bescom_df)


#2. Data Transformation
#The headers doesn't seem readable. Let's rename it!
# Set meaningful column names
colnames(bescom_df) <- c("relative_compactness", "surface_area", "wall_area", "roof_area",
                         "overall_height", "orientation", "glazing_area", "glazing_area_distribution",
                         "heating_load", "cooling_load")

# View the updated dataframe
head(bescom_df)





#2. EDA
# Load required libraries
library(ggplot2)

# Summary statistics
summary(bescom_df)

# Correlation matrix
cor_matrix <- cor(bescom_df)  
print(cor_matrix)
# Plot correlation matrix as a heatmap
library(DataExplorer)
plot_correlation(bescom_df)


# Check for missing values
library(DataExplorer)
plot_missing(bescom_df)




#LM modelling
#FULL MODEL
#Shuffle Data

set.seed(123) 
bescom_mixed <-bescom_df[order(runif(1:nrow(bescom_df))),]

#75% training data, 25% test data
bescom_train <- bescom_mixed[1:(0.75*nrow(bescom_df)),]
bescom_test <- bescom_mixed[((0.75*nrow(bescom_df))+1):nrow(bescom_df),]

lm_bescom_full <- lm(cooling_load ~ . - heating_load, data = bescom_train)
summary(lm_bescom_full)



#reduced - 1st
lm_bescom_reduced1 <- lm(cooling_load ~ . - heating_load - roof_area, data = bescom_train)
summary(lm_bescom_reduced1)

#reduced - 2nd
lm_bescom_reduced2 <- lm(cooling_load ~ . - heating_load - roof_area - orientation, data = bescom_train)
summary(lm_bescom_reduced2)

#reduced - 3rd
lm_bescom_reduced3 <- lm(cooling_load ~ . - heating_load - roof_area - orientation - glazing_area_distribution, data = bescom_train)
summary(lm_bescom_reduced3)






View(bescom_test[c(-4,-6,-8,-9,-10)])

#Model Evaluation
bescom_predict <- predict(lm_bescom_reduced3, newdata = bescom_test[c(-10)])

library(ggplot2)
# Combine actual and predicted values into a data frame
results <- data.frame(Actual = bescom_test$cooling_load, Predicted = bescom_predict)


# Create the best predicted vs. actual graph
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual", y = "Predicted", title = "Best Predicted vs. Actual") +
  theme_minimal()


#Calculating Residuals
bescom_residuals <- bescom_test$cooling_load - bescom_predict

#1. Normality test
library(DataExplorer)
plot_density(bescom_residuals)

library(car)
qqPlot(bescom_residuals)

library(nortest)
ad.test(bescom_residuals)




#2. Independence
library(lmtest)
durbinWatsonTest(bescom_residuals)
durbinWatsonTest(lm_bescom_reduced3)


#3. Linearity
crPlots(lm_bescom_reduced3)


#4. Homoskedasticity
ncvTest(lm_bescom_reduced3)
