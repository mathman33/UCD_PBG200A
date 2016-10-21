# incidence function model
# extinction probability min{1/A^beta,1}
# colonization probability from i->j: A[j]*exp(-alpha*d[i,j]*A[j])
#

#library(DAAG)
###
### set up
###

# base parameters

k=30 # patches
width=100 # width of geographic square
r.max=4 # max radius of a patch
alpha=0 # see above
beta=0.4
T=40000 # number of patch updates
step=50 # time between animation updates


# create random landscape of patches:
# x=rnorm(k)*width # randomly selects points
# y=rnorm(k)*width
# r=runif(k)*r.mean # randomly selects radii


# alternatively load a file of patch info
load("~/School/UCD/UCD_PBG200A/Ecology/R_files_and_data/IFM.Rdata")


gamma=1/width^2
###
### run simulation
###




A=pi*r^2

e=pmin(1/A^beta,1)




numbers<-c(1:k)/k
state<-matrix(sign(r),k,1) # 0 is empty 1 is occupied
states<-matrix(1,k,T)
states[,1]<-state
xM<-x%*%matrix(1,1,k)
yM<-y%*%matrix(1,1,k)
dist<-sqrt((xM-t(xM))^2+(yM-t(yM))^2)
N<-numeric(T)
N[1]<-sum(state)



par(fg="white",col.axis="white")
plot(x,y,cex=r,pch=21,bg=state+1,col=state+1)
#pause()


set.seed(2)


for (t in 1:(T-1)){
	patch<-ceiling(runif(1)*k)
	temp<-state[patch]
	if (state[patch]==1){state[patch]<-rbinom(1,1,1-e[patch])}
	if (temp==0){
		c<-A[patch]*sum(A*state*exp(-alpha*dist[patch,]))*gamma
		state[patch]<-rbinom(1,1,min(c,1))
		}
		N[t+1]<-sum(state)
		states[,t+1]<-state
	if (t%%step==0){par(fg="white",col.axis="white")
		plot(x,y,cex=r,pch=21,bg=state+1,col=state+1,xlab="",ylab="")
}
}

##
## final plot
##

#quartz()
par(fg="black",col.axis="black")

plot(c(1:T)/k,N,type="l",xlab="time",ylab="number occupied",ylim=c(0,k))






