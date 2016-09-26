rm(list=ls())

# Load the data
data = read.csv("chickadee.csv")
year = data$Year
N = data$Number

plot(year,N)

y = log(N)
k = length(y)
y.future = y[-1]
y.past = y[-k]

g1 = function(x, a1, a2) x + a1 + a2*exp(x)

model1 = nls(y.future~g1(y.past, a1, a2), start=list(a1=0, a2=0))

g2 = function(x, a1) x + a1

model2 = nls(y.future~g2(y.past, a1), start=list(a1=0))

# check out coefficients with coef(model1) and coef(model2)
# Check out log-likelihood with logLik(model1) and logLik(model2)
# The higher log-likelihood value means that's a better model?
  # Not necessarily....
  # We know Model1 contains Model2, so we know a-priori Model2 will have
  # a lower log-likelihood than Model1.
  # More parameter-rich models are more likely to produce the data....
  # But there's a tradeoff between parameters and simplicity?
  # Run the risk of overfitting
# Check out AIC scores with AIC(model1) and AI
  # Using this is better than simply looking at log-likelihood
  # This is a method (along with BIC)
  # A difference of 3 or 4 (ish) is considered significant
