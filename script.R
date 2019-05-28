par(mfrow=c(1,1))
rm(list=ls())
set.seed(42)
table <- read.delim("whr2019.csv", header = TRUE, sep = ",", dec = ".")

## 
colnames(table)
data.2015 <- table[which(table$Year == 2015),]
data.2016 <- table[which(table$Year == 2016),]
data.2017 <- table[which(table$Year == 2017),]
data.2018 <- table[which(table$Year == 2018),]

## See country differences between years
setdiff(unique(data.2015$Country.name), unique(data.2017$Country.name))
setdiff(unique(data.2016$Country.name), unique(data.2017$Country.name))
setdiff(unique(data.2017$Country.name), unique(data.2018$Country.name))

a <- unique(table$Country.name)
write.csv(a, file="countries.csv")
