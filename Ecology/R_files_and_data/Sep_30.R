#js=c(0.56, 0.64, 0.3, 0.4, 0, 0.38, 0.18, 0.25, 0.44);
#as=c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61); 
#f=c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6);

# Set the values R can take on
R = c(4, 0.2)
# Set the final time
Tmax = 500
# Set the number of replications
reps = 1000
# Set initial condision
N0 = 100
# Initialize a matrix of instances (columns)
N = matrix(N0,nrow=Tmax, ncol=reps)

# Run all $reps$ instances at once
for(t in 2:Tmax) {
  N[t,]=N[t-1,]*sample(x=R, size=reps, replace = TRUE)
}

# Plot all the results
matplot(log(N),type="l")
# Add the mean trend line
abline(a = log(100), b = mean(log(R)))
