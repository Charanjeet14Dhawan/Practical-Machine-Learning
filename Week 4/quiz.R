# Quiz 1
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test)

set.seed(33833)
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
mod1 <- train(y ~., data=vowel.train, method="rf", prox=TRUE)
mod2 <- train(y ~., method="gbm", data=vowel.train, verbose=FALSE)

pred1 <- predict(mod1, vowel.test)
pred2 <- predict(mod2, vowel.test)

confusionMatrix(pred_rf, vowel.test$y)$overall[1]
a1 <- mean(pred1 == vowel.test$y)
a2 <- mean(pred2 == vowel.test$y)

a <- mean((pred1 == vowel.test$y) & (pred2 == vowel.test$y))/mean(pred1 == pred2)


# Quiz 2
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)
mod1 <- train(diagnosis ~., data=training, method="rf", prox=TRUE)
mod2 <- train(diagnosis ~., data=training, method="gbm", verbose=FALSE)
mod3 <- train(diagnosis ~., data=training, method="lda")

pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
pred3 <- predict(mod3, testing)

a1 <- mean(pred1 == testing$diagnosis)
a2 <- mean(pred2 == testing$diagnosis)
a3 <- mean(pred3 == testing$diagnosis)

combD <- data.frame(pred1, pred2, pred3, diagnosis=testing$diagnosis)
combMod <- train(diagnosis ~., data=combD, method="rf", prox=TRUE)

pred <- predict(combMod, data=testing)
a <- mean(pred == testing$diagnosis)

# Quiz 3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

set.seed(233)

modFit <- train(CompressiveStrength ~., method="lasso", data=training)
plot.enet(modFit$finalModel, xvar="penalty", use.color=TRUE)

# Quiz 4
library(lubridate)
library(forecast)
dat = read.csv("./Week 4/gaData.csv")
training = dat[year(dat$date)<2012,]
testing = dat[year(dat$date)>2011, ]
tstrain = ts(training$visitsTumblr)
tstest = ts(testing$visitsTumblr)

etsMod <- bats(tstrain)
fcast <- forecast(etsMod, level = 95, h = dim(testing)[1])
plot(fcast)
lines(tstest, col="red")

sum(fcast$lower < testing$visitsTumblr & testing$visitsTumblr < fcast$upper) / dim(testing)[1]

# Quiz 5
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p=3/4)[[1]]
training = concrete[inTrain, ]
testing = concrete[-inTrain, ]

set.seed(325)
modSVM <- svm(CompressiveStrength ~., data=training)
pred <- predict(modSVM, testing)
sqrt(mean((pred - testing$CompressiveStrength)^2))

