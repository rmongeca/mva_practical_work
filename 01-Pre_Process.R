par(mfrow=c(1,1))
rm(list=ls())
set.seed(42)

library(DMwR)
library(chemometrics)
library(corrplot) 
library(mice) 
library(missForest) 

##########  READ, RENAME DATA AND SPLIT IT ##########
table <- read.delim("whr2019.csv", header = TRUE, sep = ";", dec = ".")
## Rename variables
colnames(table)
colnames(table) <- c(
  "Country.name",
  "Region",
  "Year",
  "Happiness.score",
  "Log.GDPpc",
  "Social.support",
  "HALE",
  "Freedom.choice",
  "Generosity",
  "Corruption",
  "Positive.affect",
  "Negative.affect",
  "Govern.confidence",
  "Democratic.Quality",
  "Delivery.Quality",
  "Sd.happiness.score",
  "Cv.happiness.score",
  "GINI.World.Bank",
  "GINI.World.Bank.est",
  "GINI.household.income",
  "Most.people.can.be.trusted.Gallup",
  "Most.people.can.be.trusted.WVS.1981.1984",
  "Most.people.can.be.trusted.WVS.1989.1993",
  "Most.people.can.be.trusted.WVS.1994.1998",
  "Most.people.can.be.trusted.WVS.1999.2004",
  "Most.people.can.be.trusted.WVS.2005.2009",
  "Most.people.can.be.trusted.WVS.2010.2014"
)
## Get 2015-2016 ladder data
data.2015 <- table[which(table$Year == 2015),c(1,4)]
data.2016 <- table[which(table$Year == 2016),c(1,4)]
data.2015 <- data.frame(data.2015[,2], row.names = data.2015$Country.name)
data.2016 <- data.frame(data.2016[,2], row.names = data.2016$Country.name)
## Drop unused years
table <- table[which(table$Year %in% 2017:2018),]
## Reset rownames index
rownames(table) <- NULL


##########  CHECK VARIABLES AND INDIVIDUALS ##########
## Check variables
colnames(table)
summary(table)
## Check variable missings
(var.na <- apply(table, 2, function(x) 100*(sum(is.na(x))/nrow(table))))
## Drop variables lots of missing
dropped.columns <- colnames(table)[c(14:17, 18, 21:27)]
table <- table[,- c(14:17, 18, 21:27)]
## Check individuals missings
(ind.na <- apply(table, 1, function(x) 100*(sum(is.na(x)))/ncol(table)))
## Not many missings per individual
## we remove the individuals that have more than 40% missing values
dropped.rows <- table[which(ind.na >= 40),]
table <- table[which(ind.na < 40),]

##########  SPLIT YEARS AND ADD COUNTRY NAMES AS ROW NAMES  ##########
## Split yearly data
data.2017 <- table[which(table$Year == 2017),]
data.2018 <- table[which(table$Year == 2018),]
rownames(data.2017) <- data.2017$Country.name
rownames(data.2018) <- data.2018$Country.name
data.2017 <- data.2017[,-c(1,3)]
data.2018 <- data.2018[,-c(1,3)]
## Remove data points with missing values in 2018 data
ind.na.2018 <- apply(data.2018, 1, function(x) 100*(sum(is.na(x)))/ncol(table))
data.2018 <- data.2018[which(ind.na.2018 < 0.1),]

##########  ADD GROWTH VARIABLES FOR PREVIOUS 2016, 2015 YEARS  ##########
## See country differences between years
setdiff(unique(rownames(data.2015)), unique(rownames(data.2016)))
setdiff(unique(rownames(data.2016)), unique(rownames(data.2017)))
setdiff(unique(rownames(data.2017)), unique(rownames(data.2018)))

## Add growth variables 2017:
data.2018$Growth1 <- NA
growth17 <- as.data.frame(data.2018$Happiness.score - data.2017[rownames(data.2018),"Happiness.score"], row.names = rownames(data.2018))
data.2018[,"Growth1"] <- merge(data.2018, growth17, by="row.names", all.x=TRUE)[,16]

## Add growth variables 2016:
data.2017$Growth1 <- NA
data.2018$Growth2 <- NA
growth16 <- as.data.frame(data.2017$Happiness.score - data.2016[rownames(data.2017),], row.names = rownames(data.2017))
growth16.2 <- as.data.frame(data.2017[rownames(data.2018),"Happiness.score"] - data.2016[rownames(data.2018),], row.names = rownames(data.2018))
data.2017[,"Growth1"] <- merge(data.2017, growth16, by="row.names", all.x=TRUE)[,16]
data.2018[,"Growth2"] <- merge(data.2018, growth16.2, by="row.names", all.x=TRUE)[,17]

## Add growth variables 2015:
data.2017$Growth2 <- NA
growth15 <- as.data.frame(data.2016 - data.2015[rownames(data.2016),], row.names = rownames(data.2016))
data.2017[,"Growth2"] <- merge(data.2017, growth15, by="row.names", all.x=TRUE)[,17]

rm(growth15, growth16, growth16.2, growth17)

########## CORRELATION BEFORE IMPUTATION ########## 

# we use 2:15 to avoid region, growth 2015 and growth 2016 in cor.
corrplot(cor(data.2017[,2:15] ,use="pairwise.complete.obs"),tl.cex=0.5)

########## IMPUTATION OF MISSING VAUES ########## 
  
m = mice(data.2017, m = 5, print = FALSE, seed = 1)
names <- rownames(data.2017)
data.2017 = complete(m)
rownames(data.2017) <- names


########## OUTLIER DETECTION AND HANDLING ##########
numeric <- data.2017[,-1]

### Univariate outliers discussion
for( i in 1:ncol(numeric)) {
  title <- paste("Histogram of", colnames(numeric)[i])
  #hist(numeric[,i], main=title)
  b <- boxplot(numeric[,i], main=title, horizontal = T)
  for(j in b$out) {
    print(rownames(numeric[which(numeric[,i] == j),]))
  }
  #readline(prompt="Press [enter] to continue")
}

### Multivariate outliers discussion

## Mahalanobis distance outlier detection
mout.dist <- Moutlier(numeric, plot = F)
plot(rd ~ md, mout.dist, main="Multivariate outlier detection with Mahalanobis distance",
     ylab="Robustified Mahalanobis distace", xlab="Mahalnobis distance")
abline(h=mout.dist$cutoff, v=mout.dist$cutoff, col="red")
outliers.mout <- numeric[which(mout.dist$md > mout.dist$cutoff & mout.dist$rd  > mout.dist$cutoff),]
text(mout.dist$rd[rownames(outliers.mout)] ~ mout.dist$md[rownames(outliers.mout)],
     labels=rownames(outliers.mout), cex=0.9, pos=3)

## Density factor outlier detection
out.scores <- data.frame(lof=lofactor(numeric, k=5))
out.scores$index <- rownames(out.scores)
rownames(out.scores) <- rownames(numeric)
plot(lof ~ index, out.scores, main="Multivariate outlier detection with Local Outlier Factor",
     ylab="Local Outlier Factor", xlab="Index")
abline(h=1.4, col="red")
outliers.lof <- subset(out.scores, lof > 1.4, row.names=rownames(out.scores))
text(lof~index, outliers.lof, labels=rownames(outliers.lof), pos=1)


########## WRITE OUTPUT DATAFRAMES TO CSV ##########
write.csv(data.2017, file = "training.csv")
write.csv(data.2018, file = "test.csv")




