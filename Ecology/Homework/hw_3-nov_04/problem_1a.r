A=matrix(0,55,55)
A[2,1]=0.6747
# small juvs
for (i in 2:8){
  A[i+1,i]=0.7857
}
# large juvs
for (i in 9:16){
  A[i+1,i]=0.6758}
# subadults
for (i in 17:22){
  A[i+1,i]=0.7425}
# novice breeders
A[1,23]=127
A[24,23]=0.8091
# 1st remigrants
A[25,24]=0.8091
A[1,24]=4
# matures
for (i in 25:54){
  A[i+1,i]=0.8091
  A[1,i]=80}
A[1,55]=80

N1 = rep(0, 55)
N1[1] = 10000
Tmax = 100
N=matrix(NA,nrow=Tmax,ncol=55)
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
# 
fecund = E[1,]
survival = diag(E[-1,-ncol(E)])
survival = c(survival, E[55,55])

plot(survival)
lines(fecund,type="p")

fecund_stage = c(fecund[1],sum(fecund[2:8]),sum(fecund[9:16]),sum(fecund[17:22]),fecund[23],fecund[24],sum(fecund[25:55]))
survival_stage = c(survival[1],sum(survival[2:8]),sum(survival[9:16]),sum(survival[17:22]),survival[23],survival[24],sum(survival[25:55]))
                                  
plot(survival_stage,axes=FALSE,type="l",ylim=c(0,max(survival_stage)),xlab="1                     2-8                     9-16               17-22                  23                    24                 25-55",ylab="Elasticities")
axis(1,labels=FALSE)
axis(2,labels=TRUE)
lines(survival_stage,type="p")
lines(fecund_stage,type="p",col="red")
lines(fecund_stage,col="red")
text(3,0.13,"Survivorship Elasticities",font=2)
text(2.9,0.07,"Fecundity Elasticities",col='red',font=2)

APPROX = 10000*w[1]*lambda^Tmax*v
# 
par(mfrow=c(2,1))
# 
barplot(N[Tmax,],main="Full Model, t=100",xlab="i",ylab="N_i(100)")
barplot(APPROX,main="Approximation, t=100",xlab="i",ylab="N_i(100)")
# 
# par(mfrow=c(1,1))
# E = sum(abs(N[Tmax,]-APPROX))
# E
