#	Rubric
  #	Well commented
  #	Code runs easily
  #	Consistent w/ report
  #	File Paths are relative
  #	Missing packages installed automatically w/ if(!require) statements
  # The dataset you use should either be automatically downloaded by your code or provided in your GitHub repo along with the rest of your files (Rmd, PDF, R).
         # If your dataset is provided as a zip file in GitHub, your code should automatically unzip and load it.

##########################################################
# Create Helper Functions if needed
##########################################################

# Install Required Packages
options(install.packages.compile.from.source = "always")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages('forecast', dependencies = TRUE, repos = "http://cran.us.r-project.org")

###########################
# Import Data 
###########################

# TODO: FROM FILE
liverData <- read_csv("https://raw.githubusercontent.com/willcpo/liver-disease-analysis/main/indian_liver_patient.csv")
pca <- liverData %>% mutate(Gender = as.numeric(as.factor(Gender))) %>% prcomp()

###########################
# Data Exploration
###########################

# Check for NAN values
sum(is.na(liverData))

# remove NA values 
liverData <- na.omit(liverData)

# confirm NAs removed
sum(is.na(liverData))

# check for features with little variation
nearZeroVar(liverData)



###########################
# Data Visualization
###########################


###########################
# Data Preparation
###########################

#Make predictions factors for training model
liverData <- liverData %>% mutate(Dataset=as.factor(Dataset))

####################################
#Create Train & Validation sets
####################################

test_index <- createDataPartition(liverData$Age, times = 1, p = 0.5, list = FALSE)

validationSet <- liverData[test_index, ]
train_set <- liverData[-test_index, ]

####################################
# Create Model w/ Train Set 
####################################

# TODO:  At least two different models or algorithms must be used, with at least one being more advanced than linear or logistic regression for prediction problems.


model <- train(Dataset~., data=train_set, method="knn")

confusionMatrix(model)
# accuracy() ??
####################################
#	Test Model with Validation Set
####################################

####################################
# Accuracy and Performance
####################################

# Confusion Matrix? 
#confusionMatrix()
# Accuracy ?
# RMSE? or MSE?

# Time Took by Final Modeling against Validation Set



####################################
# Further Analysis
####################################

# check PCA
pca <- liverData %>% mutate(Gender = as.numeric(as.factor(Gender))) %>% prcomp(scale=TRUE, center=TRUE)
summary(pca)
