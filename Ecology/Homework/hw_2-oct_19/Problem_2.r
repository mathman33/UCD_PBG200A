# Number of juveniles
js=c(0.56, 0.64, 0.3, 0.4, 0, 0.38, 0.18, 0.25, 0.44);
# Number of adults
as=c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61); 
# Fecundity
f=c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6);

# Fecundity is divided by two since only females produce offspring (we are assuming a 1-1 sex ratio)
R = as + (f/2)*js
r = mean(log(R))

# Set number of time steps
Tmax = 500
# Set number of replications
reps = 1000
# Set initial condition
N0 = 100
# Initialize a matrix of instances (columns)
N = matrix(N0,nrow=Tmax, ncol=reps)

# Run all simulations at once
for(t in 2:Tmax) {
  N[t,]=N[t-1,]*sample(x=R, size=reps, replace = TRUE)
}
# Plot the results and the mean trend line
matplot(log(N),type="l")
abline(a = log(100), b = mean(log(R)))

