complete<- function(directory, id= 1:332){
  filenames <- list.files(directory)
  ids<-c()
  nobs<-c()
  for(i in id){
    filepath<-paste(directory,"/" ,filenames[i], sep="")
    data <- read.csv(filepath, header = TRUE)
    
    completeCases = data[complete.cases(data), ]
    ids <- c(ids, i)
    nobs <- c(nobs, nrow(completeCases))
  }
  data.frame(id=ids,nobs=nobs)
}
