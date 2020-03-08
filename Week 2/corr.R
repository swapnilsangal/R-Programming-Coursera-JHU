corr <- function(directory, threshold=0){
  completes = complete(directory, 1:332)
  complete_above_threshold = subset(completes, nobs>threshold)
  correlations <- c()
  filenames = list.files(directory)
  
  for(i in complete_above_threshold$id){
    filepath<-paste(directory,"/" ,filenames[i], sep="")
    data <- read.csv(filepath, header = TRUE)
    
    completeCases = data[complete.cases(data), ]
    
    if(nrow(completeCases)>=threshold){
      correlations = c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
  correlations
}
