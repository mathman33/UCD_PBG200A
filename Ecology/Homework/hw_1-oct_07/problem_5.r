rm(list=ls())

RICKER = function(x, a, b) x + a + b*exp(x)
GOMPERTZ = function(x, a, b) a + (b+1)*x
RANDOM_WALK = function(x, a) x + a

data_list <- c("data1.txt", "data2.txt", "data3.txt", "data4.txt", "data5.txt")
for (FILE in data_list) {
  data = read.csv(FILE)
  N = data$N

  y = log(N)
  k = length(y)
  y.future = y[-1]
  y.past = y[-k]
  
  MODEL_GOMPERTZ = nls(y.future~GOMPERTZ(y.past, a, b), start=list(a=0, b=0))
  MODEL_RICKER = nls(y.future~RICKER(y.past, a, b), start=list(a=0, b=0))
  MODEL_RANDOM_WALK = nls(y.future~RANDOM_WALK(y.past, a), start=list(a=0))
  
  print(FILE)
  print("COEFFICIENTS")
  print(coef(MODEL_GOMPERTZ))
  print(coef(MODEL_RICKER))
  print(coef(MODEL_RANDOM_WALK))
  print("LOGLIK")
  print(logLik(MODEL_GOMPERTZ))
  print(logLik(MODEL_RICKER))
  print(logLik(MODEL_RANDOM_WALK))
  print("AIC")
  print(AIC(MODEL_GOMPERTZ))
  print(AIC(MODEL_RICKER))
  print(AIC(MODEL_RANDOM_WALK))
  print("BIC")
  print(BIC(MODEL_GOMPERTZ))
  print(BIC(MODEL_RICKER))
  print(BIC(MODEL_RANDOM_WALK))
}