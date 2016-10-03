js=c(0.56, 0.64, 0.3, 0.4, 0, 0.38, 0.18, 0.25, 0.44);
as=c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61); 
f=c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6);

R = as + (f/2)*js

r = mean(log(R))

Tmax = 500
reps = 1000
N0 = 100
N = matrix(N0,nrow=Tmax, ncol=reps)

for(t in 2:Tmax) {
  N[t,]=N[t-1,]*sample(x=R, size=reps, replace = TRUE)
}
matplot(log(N),type="l")
abline(a = log(100), b = mean(log(R)))

