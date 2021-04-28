source(here::here("models/bolsa_arima.R"))
source(here::here("models/bolsa_expo.R"))
source(here::here("models/bolsa_garch.R"))
source(here::here("models/bolsa_setar.R"))
source(here::here("models/precio_arima.R"))
source(here::here("models/precio_gaussian.R"))
source(here::here("models/precio_nn.R"))
source(here::here("models/precio_svm.R"))

runModel <- function(variable, model, samples){
  bolsa_file <- "formatted_Precio de Bolsa Nacional.csv"
  despacho_file <- "sized_formatted_Precio de Oferta del Despacho.csv"
  escasez_file <- "sized_formatted_Precio de Escasez de Activacion.csv"
  output <- NULL
  if(variable == "Bolsa de Energia"){
    if(model == "ARIMA"){
      output <- bolsa_arima(bolsa_file, samples)
    } else if (model == "Exponencial Doble") {
      output <- bolsa_expo(bolsa_file, samples)
    } else if (model == "GARCH") {
      output <- bolsa_garch(bolsa_file, samples);
    } else if (model == "SETAR") {
      output <- bolsa_setar(bolsa_file, samples);
    }
  } else if(variable == 'Precio Unitario') {
    if(model == "ARIMA"){
      output <- precio_arima(despacho_file, escasez_file, samples)
    } else if (model == "Proceso Gaussiano") {
      output <- precio_gaussian(despacho_file, escasez_file, samples)
    } else if (model == "SVM") {
      output <- precio_svm(despacho_file, escasez_file, samples)
    } else if (model == "Red Neuronal") {
      output <- precio_nn(despacho_file, escasez_file, samples)
    }
  }
  return(output)
}