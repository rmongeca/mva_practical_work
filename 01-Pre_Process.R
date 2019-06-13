par(mfrow=c(1,1))
rm(list=ls())
set.seed(42)


##########  READ DATA AND SPLIT IT ##########
table <- read.delim("whr2019.csv", header = TRUE, sep = ";", dec = ".")
## Get 2015-2016 ladder data
data.2015 <- table[which(table$Year == 2015),c(1,4)]
data.2016 <- table[which(table$Year == 2016),c(1,4)]
data.2015 <- data.frame(data.2015[,2], row.names = data.2015$Country.name)
data.2016 <- data.frame(data.2016[,2], row.names = data.2016$Country.name)
## Drop unused years
table <- table[which(table$Year %in% 2017:2018),]
rownames(table) <- NULL


##########  CHECK VARIABLES AND INDIVIDUALS ##########
## Check variables
colnames(table)
summary(table)
## Check variable missings
(var.na <- apply(table, 2, function(x) 100*(sum(is.na(x))/nrow(table))))
## Drop variables lots of missing
dropped.columns <- colnames(table)[c(14:15, 18, 21:27)]
table <- table[,- c(14:15, 18, 21:27)]
## Check individuals missings
(ind.na <- apply(table, 1, function(x) 100*(sum(is.na(x)))/ncol(table)))
## Not many missings per individual
## we remove the individuals that have more than 25% missing values
dropped.rows <- table[which(ind.na >= 25),]
table <- table[which(ind.na < 25),]

##########  SPLIT YEARS AND ADD COUNTRY NAMES AS ROW NAMES  ##########
## Split yearly data
data.2017 <- table[which(table$Year == 2017),]
data.2018 <- table[which(table$Year == 2018),]
rownames(data.2017) <- data.2017$Country.name
rownames(data.2018) <- data.2018$Country.name
data.2017 <- data.2017[,-c(1,3)]
data.2018 <- data.2018[,-c(1,3)]
(ind.na.2018 <- apply(data.2018, 1, function(x) 100*(sum(is.na(x)))/ncol(table)))
data.2018 <- data.2018[which(ind.na.2018 < 0.1),]

##########  ADD GROWTH VARIABLES FOR PREVIOUS 2016, 2015 YEARS  ##########
## See country differences between years
setdiff(unique(rownames(data.2015)), unique(rownames(data.2017)))
setdiff(unique(rownames(data.2016)), unique(rownames(data.2017)))
setdiff(unique(rownames(data.2017)), unique(rownames(data.2018)))
## Add growth variables 2016:
data.2017$Growth.2016 <- NA
growth16 <- as.data.frame(data.2017$Life.Ladder - data.2016[rownames(data.2017),], row.names = rownames(data.2017))
data.2017[,"Growth.2016"] <- merge(data.2017, growth16, by="row.names", all.x=TRUE)[,18]

## Add growth variables 2015:
data.2017$Growth.2015 <- NA
growth15 <- as.data.frame(data.2016 - data.2015[rownames(data.2016),], row.names = rownames(data.2016))
data.2017[,"Growth.2015"] <- merge(data.2017, growth15, by="row.names", all.x=TRUE)[,19]

# remove redundant dataframes
rm(growth15, growth16)

########## OUTLIER DETECTION AND HANDLING ##########


########## IMPUTATION OF MISSING VAUES ##########
# Imputation types:
#   * 1 - mice
#   * 2 - knn ** not checked
#   * 3 - random forest
#################################################  
typeImp = 1 
  
#if (typeImp ==1) { #mice
  library(mice) 
  m = mice(data.2017, m = 1, print = FALSE, seed = 1)
  #complete(m)[!complete.cases(data.2017),]
  #data.2017 = complete(m) ;
  mImp = complete(m) ;
  rownames(mImp) = rownames(data.2017);
  #}

#if (typeImp ==2) { #knn
  library(DMwR) # knn imputation
  #data.2017 = knnImputation(data.2017, k = 1, scale = T)
  knnImp = knnImputation(data.2017, k = 1, scale = T)
  #}

#if (typeImp ==3) { # forest imputation
  library(missForest) 
  rf = missForest(data.2017)
  # data.2017 = rf$ximp;
  rfImp= rf$ximp;
#}


mImp$type = "mice";
rfImp$type = "random forest";
knnImp$type = "knn";
mImp$country = rownames(data.2017);
rfImp$country = rownames(data.2017);
knnImp$country = rownames(data.2017);
newdf <- rbind(mImp[!complete.cases(data.2017),], rfImp[!complete.cases(data.2017),])
newdf <- rbind(newdf, knnImp[!complete.cases(data.2017),])

