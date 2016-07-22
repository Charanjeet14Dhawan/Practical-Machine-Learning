# Week 3: Predicting with trees
data(iris); library(ggplot2); library(caret)
names(iris)
table(iris$Species)

inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain, ]
testing <- iris[-inTrain, ]
dim(training); dim(testing)

# plot the metrics
qplot(Petal.Width, Sepal.Width, colour=Species, data=training)

# Train a tree
library(caret)
modFit <- train(Species ~. , method="rpart", data=training)
print(modFit$finalModel)

# plot the tree
plot(modFit$finalModel, uniform=TRUE, main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

# plot with rattle library
library(rattle)
fancyRpartPlot(modFit$finalModel)

# predict the Species of testing set
predict(modFit, newdata=testing)
