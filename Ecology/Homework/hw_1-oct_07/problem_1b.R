cranes = read.csv("cranes.csv")

N = cranes$Number
logN = log(N)

logNtp1 = logN[-1]
logNt = logN[-length(logN)]

plot(logNt,logNtp1,pch=4)

regression = lm(logNtp1~logNt)

lines(logNt,regression$fitted.values)

summary(regression)
