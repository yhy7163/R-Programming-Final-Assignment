



rankall <- function(outcome, num = "best") {
  ## Read outcome data
  dat<-read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  validate_outcome <-c("heart attack", "heart failure", "pneumonia")
  if(!outcome%in%validate_outcome){stop("invalid outcome")}
  
  validate_state <- sort(unique(dat[,"State"]))
 ## if (!state %in% validate_state) stop("invalid state")
  
  
  if(outcome=="heart attack"){col_num <- 11}
  else if(outcome=="heart failure"){col_num<-17}
  else{col_num<-23}
  
  hospital_name <-character(0)
  
  for(i in seq_along(validate_state)){
   dat_state<-dat[dat$State==validate_state[i],]
  
  

  ## For each state, find the hospital of the given rank
 
  
  #sort the 30-day death rate and the hospital name
    dat_sort<-dat_state[order(as.numeric(dat_state[,col_num]),dat_state[,2],decreasing = FALSE,na.last = NA),]
    this.num = num
    if(this.num=="best"){num<-1}
    if(this.num=="worst"){num<-nrow(dat_sort)}
  
    hospital_name[i] <- dat_sort[this.num,2]
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  data.frame(hospital = hospital_name, state = validate_state, row.names = validate_state)
}