library(fitdistrplus)

cases=c(37,3,10,2,12,3,8,8,1,rep(0,29))

model1 = fitdist(cases,distr="pois")
model2 = fitdist(cases,distr="nbinom")

# To account for individual heterogeneity, let lambda have a gamma distribution among the population.
# Coupling a gamma prior to a Poisson is called a "negative binomial"
cdfcomp(list(model1,model2))