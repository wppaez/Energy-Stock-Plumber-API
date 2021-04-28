#* @apiTitle Energy Stock Backend
#* @apiDescription Api utilizada para realizar pronosticos con respecto a 
#* la bolsa de energia y al precio unitario de la energia en la costa atlantico.

source(here::here('common/util.R'))
source(here::here('models/index.R'))


#* Echo
#* @param message the echo message.
#* @get /echo
function(req, message=""){
  logRequest(req)
  list(message = paste0("The echo is: ", message))
}

#*
#* @get /
function(req){
  logRequest(req)
  "Hi from plumber!"
}



#*
#* @get /forecast
#* @param variable The variable to forecast.
#* @param model The forecasting model.
#* @param samples:double The quantity of values to forecast.
function(req, variable="Precio Unitario", model="ARIMA", samples=2){
  logRequest(req)
  output <- runModel(variable, model, as.double(samples))
  print(output)
  output
}


