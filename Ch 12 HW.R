
library(DescTools)
library(data.table)



#Ch 12

#Problem 5
pf(2.09,8,16,lower.tail = F)
qf(.01,8,16,lower.tail = F)
pf(4.52,8,16,lower.tail = T)


#Problem 8
#Data_from_summary_stats is a function which takes summary statistics and makes a set of raw data with the same
#characteristics of the data as described (n elements, mean and standard deviation).  It utilizes the scale function.


source("~/R Workspace/Princples of Biostats - Homework/Summary Data Utilities.R", echo=TRUE)
Int_claud <- Data_from_summary_Stats(6.22,1.62,73)
Maj_Asymp_Dis <- Data_from_summary_Stats(5.81,1.43,105)
Min_Asymp_Dis <- Data_from_summary_Stats(5.77,1.24,240)
No_Disease <- Data_from_summary_Stats(5.47,1.31,1080)

Label_col <- c(rep("Int_Claud",length(Int_claud)),rep("Maj_Asymp_Dis",length(Maj_Asymp_Dis)),rep("Min_Asympt_Dis",length(Min_Asymp_Dis)),
               rep("No_disease",length(No_Disease)))

LDL_DataT <- data.table(Risk_Factor = Label_col,
                  LDL_Level = c(Int_claud,Maj_Asymp_Dis,Min_Asymp_Dis,No_Disease))

LDL_ANOVA <- aov(LDL_Level ~ Risk_Factor, data = LDL_DataT)
summary(LDL_ANOVA)


#Problem 9
FP_IT <- Data_from_summary_Stats(49.46,15.47,37)
NFP_IT <- Data_from_summary_Stats(54.76,11.41,312)
Pub_IT <- Data_from_summary_Stats(53.25,11.08,169)

FP_GT <- Data_from_summary_Stats(105.83,42.91,30)
NFP_GT <- Data_from_summary_Stats(98.68,31.27,296)
Pub_GT <- Data_from_summary_Stats(94.17,27.12,165)

minutes_data <- c(FP_IT,NFP_IT,Pub_IT,FP_GT,NFP_GT,Pub_GT)
Treatment_Center_Lab <- c(rep("FP",length(FP_IT)),rep("NFP",length(NFP_IT)),rep("Pub",length(Pub_IT)),
                          rep("FP",length(FP_GT)),rep("NFP",length(NFP_GT)),rep("Pub",length(Pub_GT)))
IT_GP_Lab <- c(rep("IT",length(FP_IT)+length(NFP_IT)+length((Pub_IT))),rep("GT",length(FP_GT)+length(NFP_GT)+length((Pub_GT))))

Therapy_DT <- data.table(Therapy_Min = minutes_data,
                          Treatment_Center = Treatment_Center_Lab,
                          IT_or_GT = IT_GP_Lab)

# IT_Therapy_DT <- Therapy_DT[IT_or_GT == "IT",.(Treatment_Center,Therapy_Min)]
# GT_Therapy_DT <- Therapy_DT[IT_or_GT == "GT",.(Treatment_Center,Therapy_Min)]
#t.test(Therapy_DT[Treatment_Center == "FP" & IT_or_GT == "IT",Therapy_Min],y=NULL, conf.level = .95)


#part B compute 95% confidence intervals using t-test function
for (p in c("IT","GT")) {
  for (j in c("FP","NFP","Pub")) {
    v <- c(p,j)
    print(v)
    out <- t.test(Therapy_DT[Treatment_Center == j & IT_or_GT == p,Therapy_Min],y=NULL, conf.level = .95)
    print(out)
    
  }
}

#make subdata set for part c
# FP_IT <- c(FP_IT,rep("FP",length(FP_IT)),rep("IT",length(FP_IT)))
# NFP_IT <- c(NFP_IT,rep("NFP",length(NFP_IT)),rep("IT",length(NFP_IT)))
# Pub_IT <- c(Pub_IT,rep("Pub",length(Pub_IT)),rep("IT",length(Pub_IT)))
# 

# 
# FP_GT <- c(FP_GT,rep("FP",length(FP_GT)),rep("GT",length(FP_GT)))
# NFP_GT <- c(NFP_GT,rep("NFP",length(NFP_GT)),rep("GT",length(NFP_GT)))
# Pub_GT <- c(Pub_GT,rep("Pub",length(Pub_GT)),rep("GT",length(Pub_GT)))
# 



#part C run ANOVA on Individual Therapy (IT) data

IT_Anova <- aov(Therapy_Min ~ Treatment_Center, data = Therapy_DT[IT_or_GT == "IT", .SD])
GT_Anova <- aov(Therapy_Min ~ Treatment_Center, data = Therapy_DT[IT_or_GT == "GT", .SD])
summary.aov(IT_Anova)
summary.aov(GT_Anova)

#Use lsmeans function to do a pairwise comparison -> refer to https://benwhalley.github.io/just-enough-r/multiple-comparisons.html
# for reference for the main function https://cran.r-project.org/web/packages/emmeans/emmeans.pdf


lm_IT <- lm(Therapy_Min ~ Treatment_Center, data = Therapy_DT[IT_or_GT == "IT", .SD])
lsm_IT <- lsmeans(lm_IT,pairwise~Treatment_Center,adjust = "bonferroni")
lsm_IT$contrasts
contrast(lsm_IT)



#Problem 11
lowbwt <- fread(choose.files(),skip = 1,header = T)
t.test(lowbwt[sex==1,sbp],lowbwt[sex==0,sbp],paired = F,conf.level = .95,mu=0)
lowbwt_anov <- aov(sbp~sex,data = lowbwt[,sex,sbp])
summary.aov(lowbwt_anov)

