##
##
## 03 - Decision Trees
##
##

set.seed (42)

library(tree)
library(randomForest)
library(rattle)
library(rpart)

# Color palette
colors <- sample(c('#000000', '#FF0000', '#808000', '#00FF00', '#008000', 
                   '#00FFFF', '#0000FF', '#FF00FF', '#800080', '#ffa500'))

####################  Load dataset #########################
train <- read.csv("training.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)
test <- read.csv("test.csv", header = TRUE, dec=".", check.names = TRUE, row.names = 1)
# set variables as factors
as.factor(train$Region)
as.factor(test$Region)

####################  Model: Decision Tree #########################

model.tree <- rpart(Happiness.score ~ ., data=train) # removing sd and cv happiness
summary(model.tree)

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

# plot
fancyRpartPlot(model.tree, palettes = "Purples", sub = "  ")


####################  Model: Random Forest #########################
# random forest model for regression
model.rf <- randomForest(Happiness.score ~ ., data=train, mtry=5, importance=TRUE )
model.rf
# and predict
pred.rf <- predict (model.rf, newdata=test)
plot(pred.rf, happy.test)
abline(0,1)
mean((pred.rf - happy.test)^2) # 0.1425542


# try bagging model
# DOES NOT improve the result
bag.rf <- randomForest(Happiness.score ~ ., data=train, mtry=14, importance=TRUE )
bag.rf
# and predict
pred.bag <- predict (bag.rf, newdata=test)
plot(pred.bag, happy.test)
abline(0,1)
mean((pred.bag - happy.test)^2) # 0.1371605


## Optimize the number of trees, guided by the MSE:

(ntrees <- round(seq(0,1000,by=100)))
ntrees[1]<-1

rf.results <- matrix (rep(0,2*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "MMSE")
rf.results[,"ntrees"] <- ntrees
rf.results[,"MMSE"] <- 0

ii <- 1

for (nt in ntrees)
{ 
    print(nt)
    
    model.rf <- randomForest(Happiness.score ~ ., data=train, ntree=nt,  importance = TRUE)
    
    # get the OOB
    rf.results[ii,"MMSE"] <- mean(model.rf$mse)
    
    ii <- ii+1
}

# print results
rf.results
plot(rf.results, type='b')

# get lowest MSE error
lowest.MSE.error <- as.integer(which.min(rf.results[,"MMSE"]))
(ntrees.best <- rf.results[lowest.MSE.error,"ntrees"])

## Now refit the RF with the best value of 'ntrees'
model.rf.fin <- randomForest(Happiness.score ~ ., data=train, ntree=ntrees.best, importance=TRUE)

# final test error
pred.rf.fin <- predict (model.rf.fin, newdata=test)
plot(pred.rf.fin, happy.test)
abline(0,1)
mean((pred.rf - happy.test)^2)


# importance of variables
importance(model.rf.fin)
varImpPlot(model.rf.fin)

