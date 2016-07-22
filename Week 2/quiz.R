# Quiz 4
# Basic
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
selectDomain <- grep("^IL", names(training), value=TRUE)
predictors <- predictors[, selectDomain]
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# 2
library(AppliedPredictiveModeling);library(Hmisc)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
indexTrain <- 1:nrow(training)
qplot(indexTrain, CompressiveStrength, data=training, color=cut2(training$Age, g=3))

# 3
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
hist(training$Superplasticizer)
hist(log10(training$Superplasticizer + 1))


# 4
preProc <- preProcess(training[, -1], method="pca", thresh=0.9)
preProc


# 5
# training without PCA
modFitNoPCA <- train(diagnosis ~ ., data=training, method="glm")
finModNoPCA <- modFitNoPCA$finalModel
print(modFitNoPCA)
predNoPCA <- predict(modFitNoPCA, testing)
confusionMatrix(testing$diagnosis, predNoPCA)

# training with PCA
modFitPCA <- train(diagnosis ~ ., data=training, preProcess="pca", method="glm")
finModPCA <- modFitPCA$finalModel
print(modFitPCA)
predPCA <- predict(modFitPCA, testing)
confusionMatrix(testing$diagnosis, predPCA)
