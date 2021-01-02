#	Rubric
  #	Well commented
  #	Code runs easily
  #	Consistent w/ report
  #	File Paths are relative
  #	Missing packages installed automatically w/ if(!require) statements
  # The dataset you use should either be automatically downloaded by your code or provided in your GitHub repo along with the rest of your files (Rmd, PDF, R).
         # If your dataset is provided as a zip file in GitHub, your code should automatically unzip and load it.

##########################################################
# Create Helper Functions
##########################################################

#Function to calculate the Root Mean Squared Error
calcRMSE <- function(predictedResult, actualResult){
  # calculate difference between the predicted and actual results
  error <- predictedResult - actualResult
  # square the error
  squaredError <- error^2
  #find the mean of all the squared errors
  meanSquaredError <- mean(squaredError)
  # take the square root of the mean
  sqrt(meanSquaredError)
}


# Install Required Packages
options(install.packages.compile.from.source = "always")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages('forecast', dependencies = TRUE, repos = "http://cran.us.r-project.org")

###########################
# Import Data 
###########################

# TODO: FROM FILE
read_csv("./indian_liver_patient.csv")
###########################
# Data Exploration
###########################


###########################
# Data Visualization
###########################


###########################
# Dimension Reduction and PCA
###########################


####################################
#Create Train & Validation sets
####################################



####################################
# Create Model w/ Train Set 
####################################

# TODO:  At least two different models or algorithms must be used, with at least one being more advanced than linear or logistic regression for prediction problems.



####################################
#	Test Model with Validation Set
####################################


####################################
# Accuracy and Performance
####################################

# Calculate RMSE

# Time Took by Final Modeling against Validation Set
