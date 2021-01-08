###############################################################
# Install Required Packages
###############################################################
set.seed(1, sample.kind="Rounding")
options(install.packages.compile.from.source = "always")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", dependencies = TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages('Rborist', dependencies = TRUE, repos = "http://cran.us.r-project.org")

###############################################################
# Import Data 
###############################################################
fallData <- read_csv("https://raw.githubusercontent.com/willcpo/fall-detection-analysis/main/falldeteciton.csv")

###############################################################
# Data Exploration
###############################################################

# Check for NAN values
amtNANValues <- sum(is.na(fallData))
# None Detected
amtNANValues

# check for features with little variation
columnsNearZero <- nearZeroVar(fallData)
# None Detected
columnsNearZero

fallDataRowIndexes <- 1:length(fallData$ACTIVITY)
rowCorrelation <- cor(fallDataRowIndexes, fallData$ACTIVITY)
rowCorrelation

###############################################################
# Data Preparation
###############################################################

#Alter Activity Column as Falling or not falling
fallData <- fallData %>% mutate(ACTIVITY=as.factor(ACTIVITY==3))
#Reverses factor order to make TRUE the positive class
fallData <- fallData %>% mutate(ACTIVITY=factor(ACTIVITY, levels=rev(levels(ACTIVITY)),labels = make.names(rev(levels(ACTIVITY)))))


###############################################################
#Create Train & Validation sets
###############################################################

test_index <- createDataPartition(fallData$ACTIVITY, times = 1, p = 0.5, list = FALSE)

validationSet <- fallData[test_index, ]
remainderSet <- fallData[-test_index, ]

test_index <- createDataPartition(remainderSet$ACTIVITY, times = 1, p = 0.25, list = FALSE)
crossValidation1 <- remainderSet[test_index, ]
remainderSet <- remainderSet[-test_index, ]

test_index <- createDataPartition(remainderSet$ACTIVITY, times = 1, p = 0.33, list = FALSE)
crossValidation2 <- remainderSet[test_index, ]
remainderSet <- remainderSet[-test_index, ]

test_index <- createDataPartition(remainderSet$ACTIVITY, times = 1, p = 0.5, list = FALSE)
crossValidation3 <- remainderSet[test_index, ]
crossValidation4 <- remainderSet[-test_index, ]

########################################################################
# Create KNN Model w/ 1st Cross Validation Set 
########################################################################

train_knn1 <- train(ACTIVITY ~ ., method = "knn", 
                    data = crossValidation1,
                    metric="Accuracy",
                    tuneGrid = data.frame(k = seq(9, 71, 2)))
# Plot best value of k
plot_knn1 <-ggplot(train_knn1, highlight = TRUE)
plot_knn1
# Predict training set for confusion Matrix
predictions_knn1 <- predict(train_knn1, crossValidation1)
# Get Accuracy information with confusion matrix
confusionMatrix_knn1 <- confusionMatrix(predictions_knn1, crossValidation1$ACTIVITY)
confusionMatrix_knn1
#
# Redo optimizing for sensitivity
#

# Train using KNN
train_knn2 <- train(ACTIVITY ~ ., method = "knn", 
                    data = crossValidation1,
                    tuneGrid = data.frame(k = seq(9, 71, 2)),
                    metric = "Sens",
                    trControl=trainControl(method="cv", 
                                           number=5,
                                           summaryFunction = twoClassSummary,
                                           classProbs = TRUE,
                                           savePredictions='all'))
# Plot best value of k
plot_knn2 <- ggplot(train_knn2, highlight = TRUE)
plot_knn2
# Predict training set for confusion Matrix
predictions_knn2 <- predict(train_knn2, crossValidation1)
# Get Accuracy information with confusion matrix
confusionMatrix_knn2 <- confusionMatrix(predictions_knn2, crossValidation1$ACTIVITY)
confusionMatrix_knn2
########################################################################
# Create QDA Model w/ 2nd Cross Validation Set 
########################################################################

train_qda <- train(ACTIVITY ~ ., method = "qda", 
                   data = crossValidation2,
                   metric = "Sens",
                   trControl=trainControl(method="cv", 
                                          number=5,
                                          summaryFunction = twoClassSummary,
                                          classProbs = TRUE,
                                          savePredictions='all'))
# Predict training set for confusion Matrix
predictions_qda <- predict(train_qda, crossValidation2)
# Get Accuracy information with confusion matrix
confusionMatrix_qda <- confusionMatrix(predictions_qda, crossValidation2$ACTIVITY)
confusionMatrix_qda

########################################################################
# Create Classification Tree Model w/ 3rd Cross Validation Set 
########################################################################

train_rpart <- train(ACTIVITY ~ ., method = "rpart", 
                     data = crossValidation3,
                     metric = "Sens",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     trControl=trainControl(method="cv", 
                                            number=5,
                                            summaryFunction = twoClassSummary,
                                            classProbs = TRUE,
                                            savePredictions='all'))

# Plot best value of k
plot_rpart <- ggplot(train_rpart, highlight = TRUE)
plot_rpart
# Predict training set for confusion Matrix
predictions_rpart <- predict(train_rpart, crossValidation3)
# Get Accuracy information with confusion matrix
confusionMatrix_rpart <- confusionMatrix(predictions_rpart, crossValidation3$ACTIVITY)
confusionMatrix_rpart

########################################################################
# Create Random Forest Model w/ 4th Cross Validation Set 
########################################################################

train_rf <- train(ACTIVITY ~ ., method = "Rborist", 
                  data = crossValidation4,
                  metric = "Sens",
                  tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)),
                  trControl=trainControl(method="cv", 
                                         number=5,
                                         summaryFunction = twoClassSummary,
                                         classProbs = TRUE,
                                         savePredictions='all'))

# Plot best value of k
plot_rf <- ggplot(train_rf, highlight = TRUE)
plot_rf

# Predict training set for confusion Matrix
predictions_rf <- predict(train_rf, crossValidation4)
# Get Accuracy information with confusion matrix
confusionMatrix_rf <- confusionMatrix(predictions_rf, crossValidation4$ACTIVITY)
confusionMatrix_rf

####################################
#	Test Random Forest Model with Validation Set
####################################

#Start Timer
startTime <- proc.time()

#Train
train_validation <- train(ACTIVITY ~ ., method = "Rborist", 
                          data = validationSet,
                          metric = "Sens",
                          tuneGrid = data.frame(predFixed = 2, minNode = 9),
                          trControl=trainControl(method="cv", 
                                                 number=5,
                                                 summaryFunction = twoClassSummary,
                                                 classProbs = TRUE,
                                                 savePredictions='all'))

#Stop Timer and Record time
totalTime <- proc.time() - startTime
####################################
# Accuracy and Performance
####################################

# Predict training set for confusion Matrix
predictions_validation <- predict(train_validation, validationSet)
# Get Accuracy information with confusion matrix
confusionMatrix_validation <- confusionMatrix(predictions_validation, validationSet$ACTIVITY)
confusionMatrix_validation
# Time Taken in Seconds by Final Modeling against Validation Set
totalTime["elapsed"]


####################################
# Further Analysis
####################################

# check PCA
pca <- fallData %>% mutate(ACTIVITY=as.numeric(ACTIVITY)) %>% prcomp(scale=TRUE, center=TRUE)
pcaSumary <- summary(pca)
pcaSumary

