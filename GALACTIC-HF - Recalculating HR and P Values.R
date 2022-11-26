

#primary composite endpoint
RRPrimary <- (1523/4120)/(1607/4112)
RRPrimary
omdata2<-as.table(rbind(c(1523,1607),c(2597,2505)))
dimnames(omdata)<-list(Death = c("Yes","No"),
                       Treatment = c("Drug","Placebo"))


#secondary cardiac deaths
RRCardiac <- (808/4120)/(798/4112)
RRCardiac
omdata<-as.table(rbind(c(808,798),c(3312,3314)))
dimnames(omdata)<-list(Death = c("Yes","No"),
                        Treatment = c("Drug","Placebo"))
omdata                                     
chisq.test(omdata)                                      


omdata2                                     
chisq.test(omdata2,correct=T)                                      


prop.test(c(1523,1607),n=c(4120,4112))
prop.test(c(808,798),n=c(4120,4112))


library(gsDesign)
library(pwr)

#nSurv()
#nEvents()
#nSurvival()

# survival(t)=e^(-haz)*(t)
# haz = -ln(survival(t))/t
# Assume 25% incidence rate over 36 months -> .75 survival at t=36 for control
# https://www.youtube.com/watch?v=6lWSnk6F__k&list=PLNII03n862n1wGaVnp11pMwVsMHvQSokr&index=5
nEvents(hr=.8,alpha=.05,sided=2,n=850)

haz_monthly_control<- -log(.8)/36  #Assuming a cardiac death rate of 20% in 36 months
haz_monthly_treatment<- haz_monthly_control*.8  #yields our intended 0.8 Hazard Ratio
nSurvival(lambda1 = haz_monthly_control,lambda2 = haz_monthly_treatment,Ts=36,Tr=6,alpha = .05,beta=.1,sided = 2,ratio =1) 



pwr.2p.test(h=ES.h(.2*.8,.2),sig.level = .05,power=.9)
power.prop.test(p1=.8*.2,p2=.2,sig.level = .05,power=.9,alternative = "two.sided")          