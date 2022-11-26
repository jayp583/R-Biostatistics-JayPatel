healthexp <- read.csv(file = "C:\\Users\\jaypa\\Documents\\R Workspace\\Princples of Biostats - Homework\\Ch 2\\prob 8.csv", header = T)

#must use dataframe$variablename the $ must be used or else this next line will not work with just putting the variablename
healthexp[order(healthexp$Per.Capita.Expenditure),]
healthexp[order(-healthexp$Per.Capita.Expenditure),]
healthexp[4:7,"Nation"]
healthexp[which(healthexp$Per.Capita.Expenditure>1000),2]

exectiondata <- read.csv(choose.files(),header=T)
exectiondata
barplot(exectiondata$Exections,names.arg = exectiondata$Year)


#problem 12
hipfracture <- read.csv(choose.files(),header=T,)

hip.matrix <-as.matrix(x = hipfracture)
hip.matrix <- as.numeric(hip.matrix)
hip.matrix <- matrix(hip.matrix,nrow =4)
hip.matrix
tab <-as.table(rbind(hip.matrix[1,2:5],hip.matrix[2,2:5],hip.matrix[3,2:5],hip.matrix[4,2:5]))
dimnames(tab) <- list(AgeGroup = c("65-74","75-84","85-94","95+"), 
                      RaceSex= c("White Men", "Black Men", "White Women", "Black Women"))
library(DescTools)
work<-Untable(tab)
table(work)




#alternative, less elegant solution
names(hipfracture)
hipfracture

names(hipfracture)
age.group.vector_orig <- c(hipfracture$Age.Group)
age.group.vector <- vector()
for (x in c(1:4)) {
  age.group.vector <- append(age.group.vector,rep(age.group.vector_orig[x],times=4))
}

gender.race <- rep(names(hipfracture)[2:5],times=4)


frac.vector <- vector()
for (x in c(1:4)) {
  for(y in c(2:5)) {
    frac.vector <- append(frac.vector,hipfracture[x,y])
  }
}



df <- data.frame(age.group.vector,gender.race,frac.vector)
is.numeric(df$frac.vector)

install.packages("esquisse")
library(esquisse)


#problem 14

#add data
Blood_data <- data.frame(Blood.Lead = c("<20", " 20-29", "30-39", "40-49", "50-59", "60-69", "70-79", ">=80"),
           data1979 = c(11.5,12.1,13.9,15.4,16.5,12.8,8.4,9.4),
           data1987 = c(37.8,14.7,13.1,15.3,10.5,6.8,1.4,.4)
)

