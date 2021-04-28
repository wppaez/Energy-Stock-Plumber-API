library(plumber)
app <- plumb('api/index.R')
app$run()
