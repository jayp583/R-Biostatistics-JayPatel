library(data.table)
library(DescTools)

LungCapData <- fread(choose.files(),header = T)
head(LungCapData)
class(LungCapData[,Smoke])
LungCapData[,Smoke:=as.factor(Smoke)]
LungCapData[,Gender:=as.factor(Gender)]
LungCapData[,Caesarean:=as.factor(Caesarean)]

Desc(LungCapData)

#Freq Table (Counts)
table(LungCapData[,Smoke])

#Freq Table (Proportion)
table(LungCapData[,Smoke])/nrow(LungCapData)

#barpolot
barplot(table(LungCapData[,Smoke])/nrow(LungCapData), main = "Smoker Proportion", xlab = "Smoker or not",names.arg = c("no","yes"),
        ylim = c(0,1), ylab = "Proportion",las=1)

#pie chart is similar using

#pie(...)


# Frequency distribution
hist(LungCapData[,LungCap])

#Probablity Density
hist(LungCapData[,LungCap],freq = F)

#Change number of bars
hist(LungCapData[,LungCap],freq = F,breaks = 12, xlim=c(0,20))

#number of bars by using a sequence of breaks at different points
hist(LungCapData[,LungCap],freq = F,breaks = seq(from=0,to=16,by=.5))

#boxplot
boxplot(LungCapData[,LungCap])

#boxplot of lung capacity by Gender on the same plot
boxplot(LungCapData[,LungCap]~LungCapData[,Gender])

#figuring out various quantiles
quantile(LungCapData[,LungCap],probs=c(0,.25,.5,.75,1))
quantile(LungCapData[,LungCap],probs=c(.33))
