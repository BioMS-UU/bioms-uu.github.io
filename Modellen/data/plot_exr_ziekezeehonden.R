source("./Modellen/data/Grind.R")

model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dZ <- b*Z*(1 - Z/K) - d*Z - beta*Z*I
    dI <- beta*Z*I - delta*I
    return(list(c(dZ, dI)))  
  }) 
}  

p <- c(b=1,K=1,d=0.01,beta=0.3,delta=0.25) # p is a named vector of parameters
s <- c(Z=1,I=0.01)                # s is the state

run(tmax = 10000)
plane(vector = T)


model <- function(t, state, parms) {
  with(as.list(c(state,parms)), {
    dR <- r*R*(1 - R/K) - a*R*N
    dN <- c*a*R*N - delta*N
    return(list(c(dR, dN)))  
  }) 
}  

p <- c(r=1,K=1,a=1,c=1,delta=0.001) # p is a named vector of parameters
s <- c(R=1,N=0.01)                # s is the state
