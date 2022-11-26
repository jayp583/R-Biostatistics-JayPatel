
library(DescTools)
library(data.table)
library(psych)

#Q5


Chol_level <- c(5.12,6.18,6.77,6.665,6.36,5.90,5.48,6.02,10.34,8.51)
Tri_level <- c(2.3,2.54,2.95,3.77,4.18,5.31,5.53,8.83,9.48,14.20)

Patient_table <- data.table(Chol_level = Chol_level,
                            Tri_level = Tri_level)

head(Patient_table)

#used esquisse for part A and B

#C
cor(Patient_table[,Chol_level],Patient_table[,Tri_level],method = "pearson")
cor(Patient_table[,Chol_level],Patient_table[,Tri_level],method = "spearman")

#D
c<-corr.test(Patient_table,method = "spearman",alpha=.05,adjust="holm")
c$ci.adj


#Q7

lowbwt_DT <- fread(file=choose.files(),header = T,skip=1)
cor(lowbwt_DT[,sbp],lowbwt_DT[,apgar5],method="spearman")
d<-corr.test(lowbwt_DT[,.(sbp,apgar5)],method="spearman",alpha = .05,adjust = "holm")
d$ci.adj
print(d,short=F)
