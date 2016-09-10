##1.extract data from cvs file and store as dataframe 
##2. filter data with only a state provided by user (by row)
##3. filter data with given column (by column)
##4. find out the min heart attack/failure/pneumonia hospital
##data??????1:??????Not available???data
##data??????2:validate state
##data??????3:validate outcome,??????3???
##data??????4:character data???numeric
##data??????5:???????????????????????????,sort??????????????????
best <- function(state, outcome) {
  ## Read outcome data
  dat<-read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = "Not Available")

  validate_outcome <-c("heart attack", "heart failure", "pneumonia")
  if(!outcome%in%validate_outcome){stop("invalid outcome")}
  
  validate_state <- unique(dat[,"State"])
  if(!state%in%validate_state){stop("invalid state")}
  
  dat_state<-dat[dat$State==state,]
  
  if(outcome=="heart attack"){col_num <- 11}
  else if(outcome=="heart failure"){col_num<-17}
  else{col_num<-23}
  
  dat_state[,col_num] <-as.numeric(dat_state[,col_num])
  min_colnum<-which.min(dat_state[,col_num])
  
  
  hospital_name<-dat_state[min_colnum,2]
  
  hospital_name

}
