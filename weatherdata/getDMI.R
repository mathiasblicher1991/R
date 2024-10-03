library(httr)
library(jsonlite)

getDMIdata <- function(station, parameter, t1, t2, apikey = apikey_dmi){
  
  t1 <- paste0(t1,"T00:00:00Z")
  t2 <- paste0(t2,"T00:00:00Z")
  url_base <- "https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?limit=300000"
  
  url <- paste0(url_base,"&parameterId=",parameter,"&stationId=",
         station,"&datetime=",t1,"/",t2,"&api-key=",apikey)
  response <- GET(url)
  dat <- content(response, as="parsed")
  df <- data.frame(TimeStamp=unlist(lapply(dat$features,function(x){x["properties"]$properties["observed"]})),
                   value=unlist(lapply(dat$features,function(x){x["properties"]$properties["value"]})))
  df <- df[dim(df)[1]:1,]
  return(df)
  
}

getDMIfull <- function(stations, parameters, t1, t2, apikey = apikey_dmi){
  
  result <- list()
  for(i in 1:length(stations)){
    
    station <- stations[i]
    data <- data.frame()
    
    for(j in 1:length(parameters)){
      
      p <- parameters[j]
      newdata <- getDMIdata(station, p, t1, t2, apikey)
      colnames(newdata)[2] <- p
      if(j != 1){
        data <- merge(data, newdata, all = T)
      }else{
        data <- newdata
      }
      
    }
    result[[i]] <- data
    names(result)[i] <- station
    
  }
  
  return(result)
  
}

station_id_to_name <- function(c){
  
  switch(c,
    "06041" = {"skagen_fyr"},
    "06058" = {"hvide_sande"},
    "06079" = {"anholt_havn"},
    "06081" = {"blaavandshuk_fyr"},
    "06168" = {"nakkehoved_fyr"},
    "06180" = {"cph_lufthavn"}
  )
  
}
