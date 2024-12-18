source("https://tbb.bio.uu.nl/rdb/grindR/grind.R")
model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR <- r*R*(1 - R/K) - a*R*N/(R+h)
    dN <- c*a*R*N/(R+h) - delta*N
    return(list(c(dR, dN)))  
  }) 
}  

colors[1] = "#d92e5cff" #optioneel, default = 'red'
colors[2] = "#e7c024ff" #optioneel, default = 'blue'



p <- c(r=1,K=1.8,a=1,c=1,delta=0.3,c=1,h=1)  #parameters
s <- c(R=2,N=0.1) #begin-dichtheden van R en N  
plane(xmax=2.2,ymax=2)
run(tstep=0.5, tmax=60,traject=F,col="grey")
#newton(state=c(R=0.5,N=0.5),plot=T)