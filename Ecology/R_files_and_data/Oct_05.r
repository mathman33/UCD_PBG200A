js=c(0.56, 0.64, 0.3, 0.4, 0, 0.38, 0.18, 0.25, 0.44);
as=c(0.53, 0.68, 0.71, 0.38, 0.54, 0.69, 0.66, 0.49, 0.61); 
f=c(3.38, 1.27, 2.77, 2.17, 0.05, 4.0, 2.37, 0.5, 1.6);
R=as+(f/2)*js


sim=function(R,rho=0,reps=1000,Tmax=100,N0=100){
  N=matrix(N0,nrow = Tmax+1,ncol = reps)
  Z=R-mean(R)
  Rs=sample(R,reps,replace=TRUE)
  for(t in 2:(Tmax+1)){
    Rs=rho*Rs+sqrt(1-rho^2)*sample(Z,size=reps,replace=TRUE)+(1-rho)*mean(R)
    N[t,]=N[t-1,]*Rs
  }
  return(N)
}

#N0 = 100
#Tmax=100
#hist(log(N[Tmax+1,]),freq=FALSE)
#xs = seq(-15,10,length=50)
#M=mean(log(R))
#V = mean((M-log(R))^2)*(1 + 2*rho/(1 - rho))
#ys = dnorm(xs,mean = M*(Tmax)+log(N0),sd = sqrt(V*Tmax))
#lines(xs, ys, lwd=2,col="red")