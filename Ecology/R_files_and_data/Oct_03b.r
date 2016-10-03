# bet-hedging 101

#Y = c(3.166, 0.01) # Good year, bad year
Y = c(100, 0.01) # Good year, bad year
S = 0.9 # Seed survival probability
p = 0.5 # Probability of a good year

r = function(g) {
  p*log(Y[1]*g + (1 - g)*S) + (1 - p)*log(Y[2]*g + (1 - g)*S)
}

optimize(f=r, c(0,1), maximum = TRUE)
