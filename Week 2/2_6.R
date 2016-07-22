# Week 2: Covariate creation

library(ISLR);library(ggplot2);library(caret);library(Hmisc);library(gridExtra)
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage,
								p=0.7, list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

table(training$jobclass)


# dummy variates
dummies <- dummyVars(wage ~ jobclass, data=training)
head(predict(dummies, newdata=training))

# remove zero Covariates
nsv <- nearZeroVar(training, saveMetrics=TRUE)
nsv

# Spline basis
library(splines)
bsBasis <- bs(training$age, df=3)
bsBasis

# fitting curves with splines
lm1 <- lm(wage ~ bsBasis, data=training)
plot(training$age, training$wage, pch=19, cex=0.5)
points(training$age, predict(lm1, newdata=training), col="red", pch=19, cex=0.5)
