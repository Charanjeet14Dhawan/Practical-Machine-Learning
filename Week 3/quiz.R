# Quiz 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

training <- segmentationOriginal[segmentationOriginal$Case=='Train', ]
testing <- segmentationOriginal[segmentationOriginal$Case=='Test', ]

set.seed(125)
modFit <- train(Class ~. , method="rpart", data=training)
print(modFit$finalModel)

# plot the tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)

# Quiz 3
library(pgmm)
data(olive)
olive = olive[,-1]

modFit <- train(Area ~. , method="rpart", data=olive)
print(modFit$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(modFit$finalModel, newdata)

# Quiz 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
modFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
				method = "glm", family="binomial", data=trainSA)

missClass <- function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
trainP <- predict(modFit$finalModel, trainSA)
testP <- predict(modFit$finalModel, testSA)
trainM <- missClass(trainSA$chd, trainP)
testM <- missClass(testSA$chd, testP)

trainM
testM

# Quiz 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)

set.seed(33833)
modFit <- train(y ~., data=vowel.train, method="rf", importance = TRUE, prox=TRUE)
fm <- modFit$finalModel
varImp(fm, scale = FALSE)



