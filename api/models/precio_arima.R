library(stats)#
library(forecast)#

precio_arima <- function(despacho_file, escasez_file, n_samples){
  #datos 1 precio de oferta de despacho 
  Y <- read.csv(here::here(paste0('data/', despacho_file)),sep=",")
  Y=Y[,c(1,2)]
  Y$Date <- as.Date(Y$Date,format="%Y-%m-%d")
  
  #datos 2 precio de la escasez de activacion
  Escazes <- read.csv(here::here(paste0('data/', escasez_file)),sep=",")
  Escazes=Escazes[,c(1,2)]
  Escazes$Date <- as.Date(Escazes$Date,format="%Y-%m-%d")
  
  #JuntarDatos
  for (i in 1:nrow(Y)) {
    if (Y$Date[i]==Escazes$Date[i]) {
      Y$Escazes[i] <- Escazes$Value[i]
    }else {
      Y$Escazes[i] <- "NA"
    }}
  
  
  n <- nrow(Y)-31
  x <- n+1
  m <- n+31
  Ytrain <- Y[1:n,]
  Ytest <- Y[x:m,]
  
  
  
  #ARIMA
  serie <- ts(Y$Value,start = c(2017,12,01),frequency = 365)
  modelo <- auto.arima(serie)
  
  serie1 <- ts(Ytrain$Value,start = c(2017,12,01),frequency = 365)
  modelo1 <- auto.arima(serie1)
  
  
  #Metricas
  fitg<- data.frame(predict(modelo1, n.ahead=(nrow(Ytest))))
  df<-data.frame(z=Ytest$Value, zhat=fitg$pred)
  # SSE
  SSE <- sum((df$z -df$zhat)^2)
  # MSE
  MSE <- sum((df$z -df$zhat)^2) /nrow(df)
  # MAPE
  MAPE <- (sum(abs(df$z -df$zhat) /df$z) /(nrow(df))) *100
  
  #pronostico
  
  prediction <- predict(modelo, n.ahead=(n_samples), prediction.interval=F, level=0.9)#intervalo de prediccion
  
  output <- NULL
  temp_df <- data.frame(prediction)
  output$forecast <- list(temp_df$pred)[[1]]
  
  stats <- NULL
  stats$MAPE <- MAPE
  stats$SSE <- SSE
  stats$MSE <- MSE
  
  output$stats <- stats
  return(output)
}