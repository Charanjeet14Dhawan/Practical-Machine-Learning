# Week 2: Basic preprocessing

library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type,
								p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="", xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)

# Standardizing
trainCapAves <- training$capitalAve
preObj <- preProcess(training[, -58], method=c("center", "scale"))
trainCapAves <- predict(preObj, training[, -58])$capitalAve
mean(trainCapAves)
sd(trainCapAves)

# With train function
set.seed(32343)
modelFit <- train(type ~., data=training, 
				preProcess=c("center", "scale"), method="glm")
modelFit

# Box-Cox transforms
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAves <- predict(preObj, training[, -58])$capitalAve
par(mfrow=c(1,2));
hist(trainCapAves);
qqnorm(trainCapAves)


# Imputing data
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] <- NA
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[, -58])$capAve
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])

