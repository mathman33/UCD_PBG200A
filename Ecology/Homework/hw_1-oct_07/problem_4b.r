a = 0.8068723
b = 0.0148920
c = 4.2470522

S <- function(N){
  num = a*N
  denom = 1 + (b*N)^c
  return(num/denom)
}

M <- function(N, fec){
  return(fec*S(N))
}

fecs = c(1,3,4,6,7,10)
final_time = 30
initial_condition = 20

result = matrix(nrow=length(fecs),ncol=final_time)

for (i in seq(1, length(fecs))){
  f = fecs[i]
  result[i,1] = initial_condition
  for (j in seq(2, final_time)){
    result[i,j] = M(result[i,j-1],f)
  }
}

RESULT = t(result)
plot(RESULT[,6], type="l", col="red",ylim=c(0,350),ylab="Result",xlab="Time")
lines(RESULT[,5], type="l", col="blue")
lines(RESULT[,4], type="l", col="black")
lines(RESULT[,3], type="l", col="orange")
lines(RESULT[,2], type="l", col="magenta")
lines(RESULT[,1], type="l", col="green")

legend("topleft", legend=c("F=1","F=3","F=4","F=6","F=7","F=10"), col=c("green", "magenta", "orange", "black", "blue", "red"), lty=c(1,1,1,1,1,1), bty="n")

