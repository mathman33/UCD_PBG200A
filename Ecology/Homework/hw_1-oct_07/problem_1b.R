cranes = read.csv("cranes-2013.csv")

N = cranes$Count
logN = log(N)

logNtp1 = logN[-1]
logNt = logN[-length(logN)]

plot(logNt,logNtp1)

regression = lm(logNtp1~logNt)

lines(logNt,regression$fitted.values)

summary(regression)
