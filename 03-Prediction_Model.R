##
##
## 03 - Decision Trees
##
##

set.seed (42)
rm(list=ls())
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

model.tree <- rpart(Happiness.score ~ ., data=train,
                    control=rpart.control(cp=0.001, xval=10)) # removing sd and cv happiness
summary(model.tree)

plot (model.tree)
text (model.tree,pretty=0)

# in the contect of a regression tree, the deviance is the sum of squared errors for the tree
## Function to prune tree
pruneTree <- function(tree.rpart) {
  rpart.cptable <- as.data.frame(tree.rpart$cptable)
  ind <- which.min(rpart.cptable$xerror)
  xerr <- rpart.cptable$xerror[ind]
  xstd <- rpart.cptable$xstd[ind]
  i = 1
  while (rpart.cptable$xerror[i] > xerr+xstd) i = i+1
  # prune the tree with the choosen CP
  tree.prune <- prune(tree.rpart, cp = rpart.cptable[i,1])
  fancyRpartPlot(tree.prune, palettes = "Purples", sub = "  ")
  return(tree.prune)
}

plot(model.tree$cptable[,4] ~ rownames(model.tree$cptable), type='b', col="purple",
     main="Subtree validation error against tree size",
     xlab="Tree size (num of leaves)", ylab="Tree validation error")
# Point out selected point
ind <- which.min(model.tree$cptable[,4])
xerr <- model.tree$cptable[ind,4]
xstd <- model.tree$cptable[ind,5]
i = 1
while (model.tree$cptable[i,4] > xerr+xstd) i = i+1
points(model.tree$cptable[i,4] ~ i, pch=20, cex=2, col="purple")
tree.prune <- pruneTree(model.tree)
# plot
fancyRpartPlot(model.tree, palettes = "Purples", sub = "  ")

# predict with the unpruned tre model
pred.tree <- predict(model.tree, newdata=test)
pred.tree.prun <- predict(tree.prune, newdata = test)
happy.test <- test[,"Happiness.score"]

plot(pred.tree, happy.test, col="red", pch=1,
     main="Test set prediction vs real value",
     xlab="Prediction", ylab="Real value")
points(pred.tree.prun, happy.test, col="blue", pch=2)
abline(0,1)
legend("bottomleft", pch=1:2, inset=c(0,0.7), horiz=F, bty="n",
       legend = c("Maximal Tree predictions", "Pruned tree predictions"), col=c("red", "blue"))

## Error of predictions
mean((pred.tree-happy.test)^2) #0.27
mean((pred.tree.prun-happy.test)^2) #0.34


####################  Model: Random Forest #########################
# random forest model for regression
model.rf <- randomForest(Happiness.score ~ ., data=train, mtry=ncol(train)/3, importance=TRUE )
model.rf
# and predict
pred.rf <- predict (model.rf, newdata=test)
plot(pred.rf, happy.test, col="#900c3f", pch=1,
     main="Test set prediction vs real value",
     xlab="Prediction", ylab="Real value", cex=1.2)
abline(0,1)
mean((pred.rf - happy.test)^2) # 0.1425542


# try bagging model
# DOES NOT improve the result
bag.rf <- randomForest(Happiness.score ~ ., data=train, mtry=14, importance=TRUE )
bag.rf
# and predict
pred.bag <- predict (bag.rf, newdata=test)
#plot(pred.bag, happy.test, col="#900c3f", pch=1,
#     main="Test set prediction vs real value",
#     xlab="Prediction", ylab="Real value")
#abline(0,1)
points(pred.bag, happy.test, col="#4422ee", pch=19, cex=1)
legend("bottomleft", pch=c(1,19), inset=c(0,0.7), horiz=F, bty="n", pt.cex=1.2,
       legend = c("Random forest predictions", "Bagged decision trees"), col=c("#900c3f", "#4422ee"))
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
plot(rf.results, type='b', col="#900c3f",
     main="Random forest error against forest size",
     xlab="Forest size (num of decision trees)", ylab="MMSE")
text(rf.results, label=round(rf.results[,2], digits=2), col="#900c3f", pos=3)

# get lowest MSE error
lowest.MSE.error <- as.integer(which.min(rf.results[,"MMSE"]))
(ntrees.best <- rf.results[lowest.MSE.error,"ntrees"])

## Now refit the RF with the best value of 'ntrees'
model.rf.fin <- randomForest(Happiness.score ~ ., data=train, ntree=ntrees.best, importance=TRUE)

# final test error
pred.rf.fin <- predict (model.rf.fin, newdata=test)
plot(pred.rf.fin, happy.test, col="#900c3f", pch=1,
     main="Test set prediction vs real value",
     xlab="Prediction", ylab="Real value")
abline(0,1)
mean((pred.rf.fin - happy.test)^2)


# importance of variables
randomForest::importance(model.rf.fin)
varImpPlot(model.rf.fin, main="Importance of variables in the random forest")

