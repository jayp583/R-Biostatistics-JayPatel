library(DescTools)
library(data.table)
library(psych)
library(ggpmisc)



# Q9
lowbwt_DT <- fread(file=choose.files(),header = T,skip=1)
head(lowbwt_DT)
lowbwt_model<- lm(sbp~sex+gestage,data = lowbwt_DT)
summary(lowbwt_model)

predict(lowbwt_model)

library(ggplot2)

ggplot(lowbwt_DT) +
 aes(x = sbp, y = gestage) +
 geom_point(shape = "circle", size = 1.5, colour = "#112446") +
 theme_minimal() + stat_poly_line() +
  stat_poly_eq() +
  geom_point()



#logistic regression example from https://stats.oarc.ucla.edu/r/dae/logit-regression/

Admit_DT <- fread("https://stats.idre.ucla.edu/stat/data/binary.csv",header = T)
head(Admit_DT)
Desc(Admit_DT)

#raw data has just integer value for rank.  as.factor converts it into a categorical
Admit_DT[,rank:=as.factor(rank)]
Desc(Admit_DT)

#family = binomial makes it into a logistic regression
Admit_model <- glm(formula = admit ~ gre+gpa+rank,data = Admit_DT,family = "binomial")
summary(Admit_model)
head(Admit_DT)

#use predict function to apply the model to new data
# type=response gives the % return.  It basically takes the raw response from the model
# and applies the logistic function = 1/(1+e^(-x)) where x is the raw response from the model.
# the output of the logistic function is a % prediction
# how to interpret logistic regression coefficients 
# https://stats.oarc.ucla.edu/other/mult-pkg/faq/general/faq-how-do-i-interpret-odds-ratios-in-logistic-regression/

predict(Admit_model,newdata = data.frame(gre = c(600),
                                         gpa = c(3.4),
                                         rank = as.factor(3)),type="response")
