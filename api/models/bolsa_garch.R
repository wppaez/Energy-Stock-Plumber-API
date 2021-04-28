library(tseries)
library(stats)
library(TSA)
library(rugarch)
library(e1071)

bolsa_garch <- function(input_file, n_samples){
  datos<- read.csv(here::here(paste0('data/', input_file)), sep=",")
  train_size <- round(1 *nrow(datos), 0)
  train <- datos[1:train_size,] 
  
  train_size1 <- round(0.9*nrow(datos), 0)
  train1 <- datos[1:train_size1,] 
  test1 <- datos[train_size1:nrow(datos),] 
  
  seriep <- ts(train$mean, start=c(2016,04,13), frequency=365)
  serie1 <- ts(train1$mean, start=c(2016,04,13), frequency=365)
  #modelo arima 
  modelo <- arima(seriep,order = c(3,1,3),method = "ML")
  modelo1 <- arima(serie1,order = c(3,1,3),method = "ML")
  residuos<-residuals(modelo)
  
  ## el modelo ARCH/GARCH escogido es GARCH(0,19)
  spec = ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(0,19)),
                    mean.model = list(armaOrder=c(3,3))) 
  fit=ugarchfit(spec=spec, data=seriep)
  fitted=fitted(fit)
  
  
  spec1 = ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(0,19)),
                     mean.model = list(armaOrder=c(3,3))) 
  fit1=ugarchfit(spec=spec1, data=serie1)
  
  #Metricas de error
  bootp1=ugarchboot(fit1,method=c("Partial","Full")[1],n.ahead = nrow(test1),n.bootpred=1000,n.bootfit=1000)
  s_f1=bootp1@forc@forecast$seriesFor 
  prediction=as.vector(s_f1)
  price<- data.frame(prediction)
  df<-data.frame(z=test1$mean, zhat=price$prediction)
  # SSE
  SSE <- sum((df$z -df$zhat)^2)
  # MSE
  MSE <- sum((df$z -df$zhat)^2) /nrow(df)
  # MAPE
  MAPE <- (sum(abs(df$z -df$zhat) /df$z) /(nrow(df))) *100
  
  
  #Pronostico
  bootp=ugarchboot(fit,method=c("Partial","Full")[1],n.ahead = n_samples,n.bootpred=1000,n.bootfit=1000)
  s_f=bootp@forc@forecast$seriesFor
  precio=as.vector(s_f)
  
  output <- NULL
  temp_df <- data.frame(precio)
  output$forecast <- list(temp_df$precio)[[1]]
  
  stats <- NULL
  stats$MAPE <- MAPE
  stats$SSE <- SSE
  stats$MSE <- MSE
  
  output$stats <- stats
  return(output)
}