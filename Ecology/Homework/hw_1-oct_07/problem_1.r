cranes = read.csv("cranes.csv")

N = cranes$Number
Y = cranes$Year

logN = log(N)

regression = lm(logN~Y)

lines(Y,exp(regression$fitted.values))

summary(regression)