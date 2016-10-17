# superspreaders branching


outbreak=function(mu=2,k=1,Tf=10,reps=50,plot.it=FALSE){
  Tf<-10  # number of generations
  
  
  # simulation
  N<-matrix(1,Tf,reps)
  
  for (i in 1:reps){
    for (t in 1:(Tf-1)){
      N[t+1,i]<-sum(rnbinom(N[t,i],mu=mu,size=k))
    }}
  
  if(plot.it){
    par(mfrow=c(1,2))
    
    barplot(dnbinom(c(0:10),mu=mean,size=k),xlab="# offspring",ylab="frequency")
    
    hist(N[T,],20,col="red",xlab="population size",ylab="frequency",main="")
    
    total<-sum(sign(N[Tf,]))
    print(paste("extinction probability is", 1-total/reps))
    print(paste("conditional population size is", sum(N[T,])/total))
  }
  return(N)
}



