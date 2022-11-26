
#https://stats.stackexchange.com/questions/499308/analysis-of-danish-mask-study-data-by-nassim-nicholas-taleb-binomial-glm-with-c


# ha ha -> Just a note for others - the study also tested for several other respiratory viruses via PCR finding 9 in the mask group and 11 in the non-mask group. So even if this study suggests mask protect against covid, it also suggests they do not protect against other respiratory viruses. – 



p1=rbinom(1E8, 2470, (5+1)/(2470+1))/(2470+1)
p2=rbinom(1E8, 2392, (0+1)/(2392+1))/(2392+1)
mean(p1<=p2) 
2*mean(p1<=p2) 


d1=c(0,2,3,0,0)
d2=c(1,0,0,2,7)
mean(d1<=d2)
