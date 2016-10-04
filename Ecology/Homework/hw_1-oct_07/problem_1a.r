cranes = read.csv("cranes.csv")

N = cranes$Number
Y = cranes$Year

plot(Y,N,pch=4)

logN = log(N)

regression = lm(logN~Y)

lines(Y,exp(regression$fitted.values))

summary(regression)
