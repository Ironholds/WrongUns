source("config.R")
ignore <- lapply(list.files("./Functions", full.names = T), source)
dir.create(file.path(getwd(),"Graphs"))
main <- function(){
  
  #Grab data
  data <- retrieve()
  
}