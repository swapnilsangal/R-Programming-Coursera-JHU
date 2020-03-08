rankall<-function(outcome,num='best'){
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  input_outcome<-outcome
  rank<-num
  
  if(!input_outcome %in% c('heart attack','heart failure','pneumonia')){
    stop('Invalid Outcome')
  }
  
  outcome_data<-data[c('name','state',input_outcome)]
  outcome_data<-outcome_data[outcome_data[input_outcome]!='Not Available',]
  output<-c()
  for(each in unique(outcome_data[,2])){
    each_data<-outcome_data[outcome_data$state==each & outcome_data[input_outcome]!='Not Available',]
    each_data[,input_outcome]<-as.numeric(each_data[,input_outcome])
    each_data<-each_data[order(each_data[input_outcome],each_data['name']),]
    
    if(rank=='best'){
      each_name<-head(each_data,1)$name
    }
    else if(rank=='worst'){
      each_name<-tail(each_data,1)$name
    }
    else if(rank>nrow(each_data)){
      each_name<-NA
    }
    else{
      each_name<-each_data[,'name'][num]
    }
    
    
  output<-rbind(output,c(each_name,each))
  }
  df<-as.data.frame(output)
  colnames(df)<-c('hospital','state')
  return(df[order(df['state']),])
  
}