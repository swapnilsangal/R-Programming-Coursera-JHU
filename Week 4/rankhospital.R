rankhospital<-function(state,outcome,num='best'){
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
  data <- data[c(2, 7, 11, 17, 23)]
  names(data)[1] <- "name"
  names(data)[2] <- "state"
  names(data)[3] <- "heart attack"
  names(data)[4] <- "heart failure"
  names(data)[5] <- "pneumonia"
  
  input_state <- state
  input_outcome <- outcome
  rank<-num
  
  if(!input_state %in% unique(data[['state']])){
    stop('Invalid State')
  }
  if(!input_outcome %in% c('heart attack','heart failure','pneumonia')){
    stop('Invalid Outcome')
  }
  
  outcome_data<-data[c('name','state',input_outcome)]
  outcome_data<-outcome_data[outcome_data$state==input_state & outcome_data[input_outcome]!='Not Available',]
  outcome_data[,input_outcome]<-as.numeric(outcome_data[,input_outcome])
  outcome_data<-outcome_data[order(outcome_data[input_outcome],outcome_data['name']),]
  
  if(rank=='best'){
    return(head(outcome_data,1)$name)
  }
  else if(rank=='worst'){
    return(tail(outcome_data,1)$name)
  }
  else if(rank>nrow(outcome_data)){
    return(NA)
  }
  else{
    return(outcome_data[,'name'][num])
  }
    
}