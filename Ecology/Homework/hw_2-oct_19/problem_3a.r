a = 0.001
b = 2
mu = -2.5
Ns = seq(from=0,to=70,by=0.01)
Next = (exp(mu)*Ns^b)/(1 + a*Ns^b)

plot(Ns,Next,type="l",xlab="N(t)",ylab="N(t+1)",axes=FALSE)
lines(Ns,Ns)
ticks = c(14.879801634313651,0,67.20519698958515)
axis(side = 1, at = ticks)
axis(side = 2, at = c(0,20,40,60,80))
text(47,35,"N(t+1)=N(t)")
text(5,50,"N(t+1) = ")
text(21,55,"exp(-2.5)N(t)^2")
text(21,46,"1 + 0.001N(t)^2")
segments(11,50.5,31,50.5)
segments(14.879801634313651,0,14.879801634313651,14.879801634313651)
segments(67.20519698958515,0,67.20519698958515,67.20519698958515)
