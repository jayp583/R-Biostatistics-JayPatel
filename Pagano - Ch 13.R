
library(DescTools)
library(data.table)

#from example in the book
d <- data.table(
  placebo = c(224,80,75,541,74,85,293,-23,525,-38,508,255,525,1023),
  drug = c(213,95,33,440,-32,-28,445,-178,367,140,323,10,65,343))

d[,difference:=placebo-drug]
Desc(d)
wilcox.test(d[,placebo],d[,drug], paired = T)


#Q1
# parametric tests are relevant when a population distribution can be described 
# with the mean and SD parameters (normal-like).  If the population distribution
# is not normal yes the sample means do converge into a normal distribution, but 
# since the population being sampled is not normal, knowing the mean is not too 
# useful

#Q3
# It provides some test statistics that gives some sense of the magnitude of power (or lack thereof)
# of the sample in relation to the test hypothesis

#Q7

Time1 <- c(62,35,38,80,48,48,68,26,48,27,43,67,52,88)
Time2 <- c(46,42,40,42,36,46,45,40,42,40,46,31,44,48)

RespRateDT <- data.table(Time1 = Time1,
                         Time2 = Time2)

wilcox.test(RespRateDT[,Time2],RespRateDT[,Time1], paired = T,exact = F)
wilcox.test(RespRateDT[,Time1],RespRateDT[,Time2], paired = T,exact = F)

#Q10
Bulimic <- c(15.9,16.0,16.5,17.0,17.6,18.1,18.4,18.9,18.9,19.6,21.5,21.6,22.9,23.6,24.1,24.5,25.1,25.2,25.6,28.0,28.7,29.2,30.9)
Healthy <- c(20.7,22.4,23.1,23.8,24.5,25.3,25.7,30.6,30.6,33.2,33.7,36.6,37.1,37.4,40.8)

wilcox.test(Bulimic,Healthy, paired=F, exact=F)

Desc(Bulimic)
Desc(Healthy)



