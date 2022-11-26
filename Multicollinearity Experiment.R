
x=c(1:1000)+runif(1000,min=-.5,max=.5)
y=c(1:1000)+runif(1000,min=-.5,max=.5)
z=c(1:1000)+runif(1000,min=-.5,max=.5)

fit <-lm(y~x+z)
summary(fit)

cor(x,y)
cor(y,z)