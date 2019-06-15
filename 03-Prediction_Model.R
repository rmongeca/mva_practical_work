##
##
## 03 - Decision Trees
##
##

set.seed (42)

library(tree)
library(randomForest)

####################  Load dataset #########################
train <- read.csv("training.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)
test <- read.csv("test.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)
# set variables as factors
as.factor(train$Region)
as.factor(test$Region)

####################  Model: Decision Tree #########################

model.tree <- tree(Happiness.score ~ ., data=train) # removing sd and cv happiness
summary(model.tree)
?tree
plot (model.tree)
text (model.tree,pretty=0)

# in the contect of a regression tree, the deviance is the sum of squared errors for the tree

# use cv.tree() to check whether pruning the tree will improve performance
cv.res <- cv.tree(model.tree)
plot(cv.res$size, cv.res$dev, type='b')

# predict with the unpruned tre model
pred.tree <- predict(model.tree, newdata=test)
happy.test <- test[,"Happiness.score"]

plot(pred.tree, happy.test)
abline(0,1)
mean((pred.tree-happy.test)^2) #0.28


####################  Model: Random Forest #########################
# model
model.rf <- randomForest(Happiness.score ~ ., data=train[,-c(12,13)], ntree=100, proximity=FALSE)

# and predict
pred.rf <- predict (model.rf, test)

(ct <- table(Truth=spam3[-learn,]$type, Pred=pred.rf1))