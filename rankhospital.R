##rank a hospital in that state for that outcome


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  dat<-read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = "Not Available")
    ## Check that state and outcome are valid

  validate_outcome <-c("heart attack", "heart failure", "pneumonia")
  if(!outcome%in%validate_outcome){stop("invalid outcome")}
  
  validate_state <- unique(dat[,"State"])
  if(!state%in%validate_state){stop("invalid state")}
  
  ## Return hospital name in that state with the given rank
  dat_state<-dat[dat$State==state,]
  
  ## 30-day death rate
  if(outcome=="heart attack"){col_num <- 11}
  else if(outcome=="heart failure"){col_num<-17}
  else{col_num<-23}
  

  ##sort the 30-day death rate and the hospital name
  dat_sort<-dat_state[order(as.numeric(dat_state[,col_num]),dat_state[,2],decreasing = FALSE,na.last = NA),]
  if(num=="best"){num<-1}
  if(num=="worst"){num<-nrow(dat_sort)}

  hospital_name <- dat_sort[num,2]
 
  hospital_name
  }
  
  
  
