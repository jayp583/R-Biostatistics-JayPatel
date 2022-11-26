# Ch 14 HW


#Q5
#a
pbinom(4,40,.15,lower.tail = T)
#b - make sure to apply the continuity correction of .5 to phat
z = ((4.5/40)-.15)/sqrt((.15*(1-.15))/40)
pnorm(z)
#c - Yes they are approx consistent

#Q7
#a
phat <- 15/27
phat
tcrit <- qt(.025,26,lower.tail = F)
lowerb <- phat-tcrit*sqrt((phat*(1-phat))/27)
upperb <- phat+tcrit*sqrt((phat*(1-phat))/27)
conf <- c(lowerb,upperb)
conf

#b
#proportion of cleft is the same for mothers who smoke as other abnormalities

#c
#proportion of cleft is not the same for mothers who smoke compared to other abnormalities

#d
pnull <- .328
zvalue <- (phat-pnull)/sqrt((pnull*(1-pnull))/27)
pt(zvalue,26,lower.tail = F)

#e
#reject the null, p value is <.01.

#f
#for proportions, you can use pwr.p.test and use ES.h(p1, p2) to find the Effect Size for two proportions.
library(pwr)
pwr.t.test(d=ES.h(.328,.25),sig.level = .01, power = .9,type = "one.sample") #just to compare with pwr.p.test
pwr.p.test(h=ES.h(.328,.25),sig.level = .01,power=.9)
((2.58*sqrt(.328*(1-.328))+1.28*sqrt(.25*.75))/(.328-.25))^2




