##1.extract data from cvs file and store as dataframe 
##2. filter data with only a state provided by user (by row)
##3. filter data with given column (by column)
##4. find out the min heart attack/failure/pneumonia hospital
##data handling 1:remove Not available
##data handling 2:validate state
##data handling 3:validate outcome, 3 outcomes
##data handling 4:turn character data into numeric
##data handling 5:sort hospital
best <- function(state, outcome) {
  ## Read outcome data
  dat<-read.csv("outcome-of-care-measures.csv",colClasses = "character",na.strings = "Not Available")

  validate_outcome <-c("heart attack", "heart failure", "pneumonia")
  if(!outcome%in%validate_outcome){stop("invalid outcome")}
  
  validate_state <- unique(dat[,"State"])
  if(!state%in%validate_state){stop("invalid state")}
  
  
  if(outcome=="heart attack"){col_num <- 11}
  else if(outcome=="heart failure"){col_num<-17}
  else{col_num<-23}
  
  dat_state<-dat[dat$State==state,]
  
  dat_state[,col_num] <-as.numeric(dat_state[,col_num])
  min_colnum<-which.min(dat_state[,col_num])
  
  
  hospital_name<-dat_state[min_colnum,2]
  
  if (length(hospital_name) > 1) {
    hospitals_sorted <- sort(hospital_name)
    hospitals_sorted[1]
  }
  else {
    hospital_name
  }
}
