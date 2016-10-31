a = 0.001
b = 2
mu = -2.5

timesteps = 200
reps = 1000

N0 = 67
N = matrix(N0, nrow=timesteps,ncol=reps)

rho = 0.5
sigma = 0.5
eta = matrix(0,nrow=timesteps,ncol=reps)
R = matrix(0.2,nrow=timesteps,ncol=reps)

for (t in 2:timesteps){
  Z = rnorm(reps)
  eta[t,] = rho*eta[t-1,] + sqrt(1- rho^2)*Z
}
for (t in 2:timesteps){
  N[t,] = (exp(mu + sigma*eta[t-1,])*(N[t-1,]^b))/(1 + a*(N[t-1,]^b))
}

extinct = 0
#matplot(N,type="l")
for (r in 1:reps){
  if (N[timesteps-1,r] < 0.0001){
    extinct = extinct + 1
  }
}
fraction = extinct/reps