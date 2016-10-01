## creates a bifurcation diagram for the "logistic" map

# parameters 

n<-50000 # number of r values
T<-500 # length of each run
Fmin<-1 # min F value 
Fmax<-10; # max F value
a<-0.8
b<-0.01
c<-4.24

## run code

N<-runif(n)
F<-Fmin+(Fmax-Fmin)*c(1:n)/n


for (i in 1:T){
	N<-F*a*N/(1+(b*N)^c)}

## plotting


plot(F,N,pch=".",col="blue",xlab="F",ylab="abundance")
title("Bellows bifurcation diagram")
