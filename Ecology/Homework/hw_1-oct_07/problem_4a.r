temp = read.table("bellows1d.txt", header=FALSE)
N=temp[,1]
S=temp[,2]

reg = nls(S~a*N/(1+(b*N)^c),start=c(a=1,b=0.01,c=2))

summary(reg)

plot(N,S,col="red")
lines(N,fitted.values(reg),col="blue")