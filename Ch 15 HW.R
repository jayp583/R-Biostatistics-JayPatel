#Q6
pchisq(9.21,df=2,lower.tail = F)
pchisq(7.38,df=2,lower.tail = F)
qchisq(.1,df=2,lower.tail = F)

#Q7
pchisq(33.41,df=17,lower.tail = F)
pchisq(27.59,df=17,lower.tail = F)
qchisq(.1,df=17,lower.tail = F)


#Q9
CalcTab <- as.table(rbind(c(27,287),c(20,288)))
dimnames(CalcTab) <- list(Treatment = c("Calcitriol", "Calcium"),
                          Withdrawal = c("Yes", "No"))

chisq.test(CalcTab)

#Fail to reject Null

#Q10
#just wanted to create fake data to work with table function
library(data.table)
dataTsurgery = data.table(Specialty = sample(c("Internal", "Surgery", "Radio", "Oncology", "Gyn"),size = 440,replace = T),
                  Surgery = sample(c("R", "CR", "C"),size = 440,replace = T))
Tab<- table(dataTsurgery)

chisq.test(Tab)

#here is the actual solution
SurgeryTab <- as.table(rbind(c(6,22,42),c(23,61,127),c(2,3,1),c(1,12,43),c(1,12,31)))
dimnames(SurgeryTab)<- list(Specialty = c("Internal","Surgery", "Radiotherapy","Oncology","Gynecology"),
                            Surgery = c("R","CR","C"))
SurgeryTab
chisq.test(SurgeryTab)


#Q17
library(epitools)
PregTab <- as.table(rbind(c(28,279-28),c(6,279-6)))
dimnames(PregTab)<- list(EctoPreg = c("Yes","No"),
                         PID = c("Yes","No"))
PregTab
#wald method is the same as the common ad/bc
oddsratio(PregTab,conf.level = .95,correction = T,method = "wald")


#Q15
HeadTab <- as.table(rbind(c(2,2),c(8,33)))
mcnemar.test(HeadTab,correct = T)



