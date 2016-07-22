# Week 2: predicting with Regression Multiple Covariates
library(ISLR);library(ggplot2);library(caret);library(Hmisc);library(gridExtra)
data(Wage); Wage <- subset(Wage, select=-c(logwage))
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
								p=0.7, list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing);

featurePlot(x=training[, c("age", "education", "jobclass")],
			y=training$wage,
			plot="pairs")

qplot(age, wage, data=training)
qplot(age, wage, colour=jobclass, data=training)
qplot(age, wage, colour=education, data=training)

# Fit a linear model
modFit <- train(wage ~ age + jobclass + education, 
				method = "lm", data=training)
finMod <- modFit$finalModel
print(modFit)

# Diagnostics
plot(finMod, 1, pch=19, cex=0.5, col="#00000010")

# color by variates not used in the model
qplot(finMod$fitted, finMod$residuals, colour=race, data=training)

# plot by index
plot(finMod$residuals, pch=19)

# predicted versus truth in test set 
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)

# use all Covariates
modFitAll <- train(wage ~ ., data=training, method="lm")
pred <- predict(modFitAll, testing)
qplot(wage, pred, data=testing)
