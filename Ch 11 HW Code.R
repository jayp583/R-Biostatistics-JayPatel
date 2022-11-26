
library(DescTools)
library(data.table)


#Problem 5

LDL <- data.table(CF = c(4.61,6.42,5.40,4.54,3.98,3.82,5.01,4.34,3.80,4.56,5.35,3.89,2.25,4.24),
                  OB = c(3.84,5.57,5.85,4.8,3.68,2.96,4.41,3.72,3.49,3.84,5.26,3.73,1.84,4.14)
                  )

mean(LDL[,CF])

t.test(LDL[,CF],LDL[,OB],paired = T,conf.level = .95)


#Problem 6

Saliva <- data.table(Twelve = c(73,58,67,93,33,18,147),
                     Twentyfour = c(24,27,49,59,0,11,43)
                     )

t.test(Saliva[,Twelve],Saliva[,Twentyfour],paired = T, conf.level = .95,alternative = "greater", mu = 0)

#Problem 10
source("~/R Workspace/Princples of Biostats - Homework/Summary Data Utilities.R", echo=TRUE)
t.test.from.summary.data(54.8,28.1,156,69.5,34.7,148,conf.level = .95,alternative = "two.sided",paired = F)



#Problem 13
bed_data <- fread(choose.files())

Desc(bed_data)

t.test(bed_data[,bed80],bed_data[,bed86],paired = F, alternative = "two.sided", conf.level = .95,)

t.test(bed_data[,bed80],bed_data[,bed86],paired = T, alternative = "two.sided", conf.level = .95,)



