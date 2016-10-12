# Code for estimating growth rates for 
# structured populations in a random environment

# INPUT VARIABLES are:

# demography: array A in which A[,,i] corresponds to the projection matrix for environmental state i
# environmental dynamics (markovian): matrix P for which P[i,j] is the probability of going from environmental state i to environmental state j
# T total run time for estimating the invasion speed, etc.
# seed the random seed used for the estimate

# RETURNS: 
# growth - stochastic growth rate
# conf - [growth-conf,growth+conf] is the 95% confidence interval
# growth.sd - standard deviation in the stochastic growth rate
# v - matrix of reproductive values
# w - matrix of stable stage distributions
# S - sensitivity matrix for stochastic growth rate log(lambdaS) 
# E - elasticity matrix for "lambdaS" 


Lyap=function(A,P,T,seed=0,SIG=FALSE){
  
  # set up 
  no.states=dim(P)[1]; # number of environmental states
  k=dim(A[,,1])[1]; # number of stages
  reps=20; # number of s values to try in [0,smax] for the moment generating matrix M(s)
  
  ##
  ## To follow caswell, we need that "t=0" corresponds to "t=1"
  ##
  
  
  state=1; # initial state
  Rs=numeric(T) 
  states=numeric(T+1) # keeps track of all the states for the reproductive calculation
  states[1]=state
  ws=matrix(0,k,T+1) # holds all the ws!!
  w=matrix(1,k,1)/k; # initial vector
  ws[,1]=w
  if(seed>0)set.seed(seed);
  for(t in 1:T){
    temp=(A[,,state])%*%w;
    Rs[t]=sum(temp)
    w=temp/sum(temp); # update w
    state=sample(c(1:no.states),1,prob=P[state,]); # update state
    ws[,t+1]=w  
    states[t+1]=state
  }
  
  vs=matrix(0,k,T+1)
  vs[,T+1]=matrix(1,k,1)/k
  for(t in (T+1):2){
    v=t(A[,,states[t-1]])%*%vs[,t]
    vs[,t-1]=v/sum(v)
  }
  
  ### the sensitivity, elasticity matrices		
  
  S=matrix(0,k,k)
  for(t in 1:T)S=S+outer(vs[,t+1],ws[,t])/(sum(vs[,t+1]*ws[,t+1])*Rs[t])/T
  
  E=matrix(0,k,k)
  for(t in 1:T)E=E+outer(vs[,t+1],ws[,t])*A[,,states[t]]/(sum(vs[,t+1]*ws[,t+1])*Rs[t])/T
  
  #	S2=matrix(0,k,k)
  #	for(i in 1:T)S2=S2+outer(vs[,t+1],ws[,t])^2/(sum(vs[,t+1]*ws[,t+1])^2*Rs[t]^2)/T
  
  ## estimates for log growth rate, confidence interaval, variance in growth rate
  
  growth=mean(log(Rs)); # estimate for Lyapunov exponent		
  conf=1.96*sqrt(var(log(Rs))/T); # estimate for 95% confidence interval
  
  if(SIG){
    # variance in stochastic growth rate. To estimate this variance, use the observation that log(lambda_S)-sigma^2/2=log(lambda_average) where lambda_average is the expected growth rate of the population size and sigma^2 is the desired variaance. 
    
    # use the "mega-matrix" approach to compute lambda_average 
    
    # create a block diagonal matrix with diagonal blooks A[,,i]
    
    F=matrix(0,k*no.states,k*no.states)
    for(i in 1:no.states){
      lower=(i-1)*k+1
      upper=i*k
      F[lower:upper,lower:upper]=A[,,i]
    }
    
    # the megamatrix B and lambda.average
    
    B=F%*%(P%x%diag(k))
    
    lambda.average=max(abs(eigen(B)$values))
    
    # the estimate for sigma
    
    growth.sd=sqrt(2*(abs(log(lambda.average)-growth)))
  }
  
  if(SIG==FALSE)growth.sd=NA
  
  
  
  return(list(growth=growth,conf=conf,growth.sd=growth.sd,w=ws,v=vs,R=Rs,S=S,E=E))
}



##################################
# illustration of using the code
# based on Silva et al. (1991)
##################################


A=array(0,dim=c(4,4,2))
#Burnt 
A[,,1]=matrix(c(0.08,1.63,2.42,4.4,
                0.21,0.64,0.35,0.16,
                0,0.19,0.43,0.24,
                0,0.03,0.23,0.48),4,byrow=TRUE)
#Unburnt
A[,,2]=matrix(c(0,0.706,0.391,3.59,
                0.018,0.158,0.136,0.093,
                0,0.08,0.07,0.21,
                0,0,0.01,0.07),4,byrow=TRUE)

#Sample P matrix

P=matrix(c(0.9,0.1,0.9,0.1),nrow=2,byrow=TRUE)

