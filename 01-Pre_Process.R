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
table <- table[,- c(14:15, 18, 21:27)]
## Check individuals missings
(ind.na <- apply(table, 1, function(x) 100*(sum(is.na(x)))/ncol(table)))
## Not many missings per individual

##########  SPLIT YEARS AND ADD VARIABLES ##########
## Split yearly data
data.2017 <- table[which(table$Year == 2017),]
data.2018 <- table[which(table$Year == 2018),]
rownames(data.2017) <- data.2017$Country.name
rownames(data.2018) <- data.2018$Country.name
data.2017 <- data.2017[,-c(1,3)]
data.2018 <- data.2018[,-c(1,3)]
## See country differences between years
setdiff(unique(rownames(data.2015)), unique(rownames(data.2017)))
setdiff(unique(rownames(data.2016)), unique(rownames(data.2017)))
setdiff(unique(rownames(data.2017)), unique(rownames(data.2018)))
## Add growth variables TODO:
data.2017$Growth.2016 <- NA
data.2017$Growth.2015 <- NA
#data.2017[rownames(data.2016)[1], 28] <-  data.2017[rownames(data.2016)[1], 3] - data.2016[rownames(data.2016)[1], 3] 
## Add region variable TODO:


########## OUTLIER DETECTION AND HANDLING ##########


########## IMPUTATION OF MISSING VAUES ##########


