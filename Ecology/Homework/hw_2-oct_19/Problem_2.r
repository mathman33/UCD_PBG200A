R_A = c(1.1, 0.9, 1.2, 0.8)
R_B = c(2.0, 0.4, 3.0, 0.5)

r_A = mean(log(R_A))
r_B = mean(log(R_B))

sig_A = mean((log(R_A) - r_A)^2)
sig_B = mean((log(R_B) - r_B)^2)

# Set number of time steps
Tmax = 25
# Set number of replications
reps = 1000
# Set initial condition
N0 = 500
# Initialize a matrix of instances (columns)
N_A = matrix(N0, nrow=Tmax, ncol=reps)
N_B = matrix(N0, nrow=Tmax, ncol=reps)

# Run all simulations at once
for(t in 2:Tmax) {
  N_A[t,]=N_A[t-1,]*sample(x=R_A, size=reps, replace = TRUE)
  N_B[t,]=N_B[t-1,]*sample(x=R_B, size=reps, replace = TRUE)
}

under100A = 0
under100B = 0
for(r in 1:reps){
  if (N_A[Tmax,r] <= 100){
    under100A = under100A + 1
  }
  if (N_B[Tmax,r] <= 100){
    under100B = under100B + 1
  }
}

#attach(mtcars)
par(mfrow=c(1,2))

# Plot the results and the mean trend line
matplot(log(N_A),type="l",xlab="t")
abline(a = log(N0), b = mean(log(R_A)),lwd=3)
abline(a = log(100), b = 0,lty="dotted",lwd=3)

matplot(log(N_B),type="l",xlab="t")
abline(a = log(N0), b = mean(log(R_B)),lwd=3)
abline(a = log(100), b = 0,lty="dotted",lwd=3)

A_ext = sum(N_A[Tmax,] > 2)/reps
B_ext = sum(N_B[Tmax,] > 2)/reps

threshold=100
M = r_A
V = sig_A
rho=0.5
Vplus=V*(1+2*rho/(1-rho))
z=(log(threshold/N0)-M*25)/(sqrt(25*Vplus))
pnorm(z)