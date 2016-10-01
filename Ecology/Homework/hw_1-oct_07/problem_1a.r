cranes = read.csv("cranes-2013.csv")

N = cranes$Count
Y = cranes$Year

logN = log(N)

regression = lm(logN~Y)

lines(Y,exp(regression$fitted.values))

summary(regression)
