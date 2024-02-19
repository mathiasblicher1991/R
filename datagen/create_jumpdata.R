# Datasets for robust testing

# Choose start time
t1 <- as.POSIXct("2022-07-01 00:00",tz="UTC")

# Make a process - geometric brownian motion
set.seed(12)
h <- 1/60 #minutes = 1/60
t <- seq(0,24*14,by=h)
N <- length(t)
r <- 0.05
theta <- 20
x0 <- rnorm(1,theta,sd=3)
x <- rep(x0,N)
dBt <- rnorm(N,0,sqrt(h))
for(i in 2:N){
  x[i] <- x[i-1] + r*(theta-x[i-1])*h + dBt[i]
}
plot(t,x,type="l",col="red")
obs_original <- data.frame(TimeStamp=t1+t*3600,obs=round(x,2))
obs_sampled <- obs_original[seq(1,N,by=1/h),]
plot(obs_original,type="l",col="red")
plot(obs_sampled,type="l",col="dark green",lwd=2)

#write.csv(obs_sampled,"~/Desktop/datagen/jumpydata.csv",row.names=F)

# Forecast times
times <- t1 + 86400*c(0:13)
fc_per_basetime <- 3
nfctot <- length(times)*fc_per_basetime
basetimes <- rep(times,each=fc_per_basetime)
forectimes <- basetimes + 3600*c(6,12,18)

df <- data.frame(BaseTime=basetimes,TimeStamp=forectimes)

# Model 1 - only 1 ensemble member
forecast_data_1 <- df
forecast_data_1$m1 <- sapply(df$BaseTime,function(x){obs_sampled[x==obs_sampled$TimeStamp,]$obs})
head(forecast_data_1)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
points(forecast_data_1$TimeStamp,forecast_data_1$m1,col="red",cex=1.2)

# Model 2 - true model
forecast_data_2 <- df
m <- 20
max_horizon <- max(df$TimeStamp-df$BaseTime)[[1]]
forecast_data_2 <- cbind(forecast_data_2,matrix(numeric(dim(df)[1]*m),nrow=dim(df)[1]))
colnames(forecast_data_2)[-c(1:2)] <- paste0("m",1:m)
set.seed(5678)
for(i in 1:length(times)){
  
  t_temp <- seq(0,max_horizon,by=h)
  N_temp <- length(t_temp)
  X <- matrix(numeric(m*N_temp),ncol=m)
  X[1,] <- obs_sampled[obs_sampled$TimeStamp==times[i],]$obs
  dBt_temp <- matrix(rnorm(N_temp*m,0,sqrt(h)),ncol=m)
  for(j in 2:N_temp){
    X[j,] <- X[j-1,] + r*(theta-X[j-1,])*h + dBt_temp[j,]
  }
  forecast_data_2[forecast_data_2$BaseTime==times[i],-c(1:2)] <- X[seq(1,N_temp,by=6*60),][-1,]
  
}

head(forecast_data_2)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
lines(forecast_data_2$TimeStamp,forecast_data_2[,1+2],col="red",cex=1.2)
lines(forecast_data_2$TimeStamp,forecast_data_2[,2+2],col="blue",cex=1.2)
lines(forecast_data_2$TimeStamp,forecast_data_2[,3+2],col="orange",cex=1.2)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
for(i in 1:20){
  lines(forecast_data_2$TimeStamp,forecast_data_2[,i+2],col="red",cex=1.2)
}

# Model 3 - no BaseTime, true model
forecast_data_3 <- data.frame(TimeStamp=seq(t1,tail(obs_sampled$TimeStamp,1),by=3600*6)[-1])
m <- 5
max_horizon <- 6
forecast_data_3 <- cbind(forecast_data_3,matrix(numeric(dim(df)[1]*4/3*m),nrow=dim(df)[1]*4/3))
colnames(forecast_data_3)[-1] <- paste0("m",1:m)
set.seed(92211)
for(i in 1:dim(forecast_data_3)[1]){
  
  t_temp <- seq(0,max_horizon,by=h)
  N_temp <- length(t_temp)
  X <- matrix(numeric(m*N_temp),ncol=m)
  X[1,] <- obs_sampled[obs_sampled$TimeStamp==(forecast_data_3$TimeStamp[i] - 6*3600),]$obs
  dBt_temp <- matrix(rnorm(N_temp*m,0,sqrt(h)),ncol=m)
  for(j in 2:N_temp){
    X[j,] <- X[j-1,] + r*(theta-X[j-1,])*h + dBt_temp[j,]
  }
  forecast_data_3[i,-c(1)] <- X[N_temp,]
  
}

head(forecast_data_3)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
lines(forecast_data_3$TimeStamp,forecast_data_3[,1+2],col="red",cex=1.2)
lines(forecast_data_3$TimeStamp,forecast_data_3[,2+2],col="blue",cex=1.2)
lines(forecast_data_3$TimeStamp,forecast_data_3[,3+2],col="orange",cex=1.2)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
for(i in 2:6){
  lines(forecast_data_3$TimeStamp,forecast_data_3[,i],col=i,cex=1.2)
}

# Model 4 - hindcast SDE
forecast_data_4 <- data.frame(TimeStamp=seq(t1,tail(obs_sampled$TimeStamp,1),by=3600)[-1])
m <- 100
max_horizon <- 1
forecast_data_4 <- cbind(forecast_data_4,matrix(numeric(dim(df)[1]*4/3*m),nrow=dim(df)[1]*4/3))
colnames(forecast_data_4)[-1] <- paste0("m",1:m)
set.seed(92211)
for(i in 1:dim(forecast_data_4)[1]){
  
  t_temp <- seq(0,max_horizon,by=h)
  N_temp <- length(t_temp)
  X <- matrix(numeric(m*N_temp),ncol=m)
  if(i==1){
    X[1,] <- obs_sampled[obs_sampled$TimeStamp==forecast_data_4$TimeStamp[i],]$obs
  }else{
    X[1,] <- as.numeric(forecast_data_4[i-1,-c(1)])
  }
  
  dBt_temp <- matrix(rnorm(N_temp*m,0,sqrt(h)),ncol=m)
  for(j in 2:N_temp){
    X[j,] <- X[j-1,] + 0.3*(obs_original$obs[(i-1)*60 + j-1]-X[j-1,])*h + dBt_temp[j,]
  }
  forecast_data_4[i,-c(1)] <- X[N_temp,]
  
}

head(forecast_data_4)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
lines(forecast_data_4$TimeStamp,forecast_data_4[,1+2],col="red",cex=1.2)
lines(forecast_data_4$TimeStamp,forecast_data_4[,2+2],col="blue",cex=1.2)
lines(forecast_data_4$TimeStamp,forecast_data_4[,3+2],col="orange",cex=1.2)
plot(obs_sampled,type="l",col="dark green",lwd=1,lty=2)
for(i in 2:(m+1)){
  lines(forecast_data_4$TimeStamp,forecast_data_4[,i],col=i,cex=1.2)
}

# Model 5 - hindcase SDE rescrambled to conceal autocorrelation
forecast_data_5 <- forecast_data_4
for(i in 1:dim(forecast_data_5)[1]){
  u <- sample(1:m,replace=F)
  forecast_data_5[i,-c(1)] <- forecast_data_4[,-c(1)][i,u]
}

jumpdata_forecasts <- list(forecasts=list(Model1=forecast_data_1,
                                          Model2=forecast_data_2,
                                          Model3=forecast_data_3,
                                          Model4=forecast_data_4,
                                          Model5=forecast_data_5),
                           observations=obs_sampled)

save(jumpdata_forecasts,file="~/Desktop/datagen/jumpdata_004.rda")
