A=matrix(c(0, 0, 0,0, 127, 4, 80,
           0.6747, 0.737, 0, 0, 0, 0, 0,
           0, 0.0486, 0.6610, 0, 0, 0, 0,
           0, 0, 0.0147, 0.6907, 0, 0, 0,
           0, 0, 0, 0.0518, 0, 0, 0,
           0, 0, 0, 0, 0.8091, 0, 0,
           0, 0, 0, 0, 0, 0.8091, 0.8089),7,byrow=TRUE)

N1 = c(10000,0,0,0,0,0,0)
Tmax = 100
N=matrix(NA,nrow=Tmax,ncol=7)
N[1,] = N1

for (t in 2:Tmax){
  N[t,] = A%*%N[t-1,]
}

matplot(N,type="l")

# Get the eigenvalues and eigenvectors of A
stuff = eigen(A)
# Grab the largest eigenvalue and its corresponding eigenvector
lambda = Re(stuff$values[1])
v = Re(stuff$vectors[,1])
v = v/sum(v)

# Get the vector of reproductive values
otherstuff = eigen(t(A))
w = Re(otherstuff$vectors[,1])
w = w/sum(v*w)

# Grab sensitivities and elasticities of each entry of A
S = w%o%v
E = S*A/lambda

fecund = E[1,]
survival=c(E[2,1],E[2,2]+E[3,2],E[3,3]+E[4,3],E[4,4]+E[5,4],E[5,5]+E[6,5],E[6,6]+E[7,6],E[7,7])
# survival = c(E[2,]
#for (i in 3:7)survival = survival + E[i,]

plot(survival,type="l",ylim=c(0,max(survival)),xlab="Stage",ylab="Elasticities")
lines(survival,type="p")
lines(fecund,type="p",col="red")
lines(fecund,col="red")
text(3.1,0.11,"Survivorship Elasticities",font=2)
text(3,0.08,"Fecundity Elasticities",col='red',font=2)

# N0 = c(10000,0,0,0,0,0,0)
# 
# APPROX = 10000*0.2469775*lambda^Tmax*v
# 
# par(mfrow=c(1,2))
# 
# barplot(N[Tmax,], names.arg=c("1", "2", "3", "4", "5", "6", "7"),main="Full Model, t=100")
# barplot(APPROX, names.arg=c("1", "2", "3", "4", "5", "6", "7"),main="Approximation, t=100")
# 
# par(mfrow=c(1,1))
# E = abs(N[Tmax,]-APPROX)
# E
# sum(E)
# plot(N[Tmax,],pch=4)
# lines(APPROX,type="p")
