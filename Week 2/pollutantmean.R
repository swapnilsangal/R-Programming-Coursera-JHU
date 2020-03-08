pollutantmean <- function(directory, pollutant, id= 1:332){
  filenames <- list.files(directory)
  pollutants <- c()
  for(i in id){
    filepath<-paste(directory,"/" ,filenames[i], sep="")
    data <- read.csv(filepath, header = TRUE)
    pollutants = na.omit(c(pollutants, data[,pollutant]))
  }
  mean(pollutants)
}
