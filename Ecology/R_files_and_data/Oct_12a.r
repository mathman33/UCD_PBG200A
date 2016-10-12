library(IPMpack)
data("dataIPMpackCryptantha")
stuff<-dataIPMpackCryptantha

# We could plot some stuff
plot(stuff$size,stuff$sizeNext)
plot(stuff$size,stuff$surv)

# No normally distributed errors, so we need a Generalized Linear Model
sm = glm(surv~size,family=binomial,data=stuff)
s = function(x){
  predict(sm,newdata=data.frame(size=x),type="response")
}

xs = seq(0, 120, length=300)
lines(xs, s(xs),col="red")

# Without using the family argument, this is a simple linear regression
gm=glm(sizeNext~size,data=stuff)

g = function(x,y){
  dnorm(y,mean=predict(gm,newdata=data.frame(size=x),type="response"),sd=sd(gm$residuals))
}