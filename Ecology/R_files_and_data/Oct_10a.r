A = matrix(c(0,0.55,1.1,0.55),nrow=2)
N1 = c(0,100)
Tmax = 25
N=matrix(NA,nrow=Tmax,ncol=2)
N[1,] = N1

for (t in 2:Tmax){
  N[t,] = A%*%N[t-1,]
}

matplot(N,type="l")