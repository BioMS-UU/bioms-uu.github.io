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

run()
plane(vector = T)
