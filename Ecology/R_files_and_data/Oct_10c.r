A=matrix(c(0, 0, 0,0, 127, 4, 80,
           0.6747, 0.737, 0, 0, 0, 0, 0,
           0, 0.0486, 0.6610, 0, 0, 0, 0,
           0, 0, 0.0147, 0.6907, 0, 0, 0,
           0, 0, 0, 0.0518, 0, 0, 0,
           0, 0, 0, 0, 0.8091, 0, 0,
           0, 0, 0, 0, 0, 0.8091, 0.8089),7,byrow=TRUE)

N1 = c(100,0,0,0,0,0,0)
Tmax = 25
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
