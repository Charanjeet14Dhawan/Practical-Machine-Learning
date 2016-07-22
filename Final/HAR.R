setwd("E:/Projects/R/Practical Machine Learning/")

library(lubridate)
library(forecast)

# Read data from csv files
training <- read.csv("./Final/pml-training.csv", na.strings=c("NA","#DIV/0!",""))
testing <- read.csv("./Final/pml-testing.csv", na.strings=c("NA","#DIV/0!",""))

library(caret)

# Remove variables containing missing values
mvIdx <- colSums(is.na(training)) != 0
training <- training[, !mvIdx]
testing <- testing[, !mvIdx]

# Remove some trivial information like names and timestamps
trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]

# Create training data-partition and valid one
set.seed(1026) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
trainSet <- trainData[inTrain, ]
validSet <- trainData[-inTrain, ]

control <- trainControl(method = "cv", number = 5)



# Training a decision tree
control <- trainControl(method = "cv", number = 5)
rpartMod <- train(classe ~ ., data = trainSet, method = "rpart", preProcess=c("center", "scale"),
                   trControl = control)
print(rpartMod, digits = 4)

library(rattle)
fancyRpartPlot(rpartMod$finalModel)

# predict outcomes using validation set
predict_rpart <- predict(rpartMod, validSet)
# Show prediction result
(conf_rpart <- confusionMatrix(validSet$classe, predict_rpart))

(predict(rpartMod, testData))




# Training a gbm model
control <- trainControl(method = "cv", number = 5)
gbmMod <- train(classe ~ ., data = trainSet, method = "gbm", preProcess=c("center", "scale"),
                   trControl = control)
print(gbmMod, digits = 4)

# predict outcomes using validation set
predict_gbm <- predict(gbmMod, validSet)
# Show prediction result
(conf_gbm <- confusionMatrix(validSet$classe, predict_gbm))

(predict(gbmMod, testData))





# Training a random forest
control <- trainControl(method = "cv", number = 5)
trainSetP <- subset(trainSet, select=-c(classe))
trainSetC <- trainSet$classe

rfMod <- train(trainSetC ~ ., data=trainSetP[, c(1:7)], method = "rf", preProcess=c("center", "scale"),
                   trControl = control, prox = TRUE)
print(rfMod, digits = 4)

# predict outcomes using validation set
predict_rf <- predict(rfMod, validSet)
# Show prediction result
(conf_rf <- confusionMatrix(validSet$classe, predict_rf))

(predict(rfMod, testData))

