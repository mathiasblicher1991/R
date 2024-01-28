# Useful functions

# Zero-order hold function
zoh <- function(t,x){
  
  N <- length(t)
  t_new <- c(t[1],rep(t[-c(1,N)],each=2),t[N])
  x_new <- rep(x[-N],each=2)
  return(data.frame(t=t_new,x=x_new))
  
}

# Standard Brownian motion
brow <- function(t){
  
  cumsum(c(0,rnorm(length(t)-1,sd=sqrt(diff(t)))))
  
}

# Clear R session
clc <- function(){
  
  rm(list=ls(envir = .GlobalEnv), envir = .GlobalEnv)
  while(dev.cur()>1){
    dev.off()
  }
  cat("\014")
  
}
