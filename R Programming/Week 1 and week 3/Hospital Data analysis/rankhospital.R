rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome datasetwd("C:/Users/RAUNAK KOCHAR/Documents/prog2/")
        filename <- read.csv("rprog-data-ProgAssignment3-data (1)//outcome-of-care-measures.csv")
        flag1 <- 0
        
        match <- filename$State == state
        df <- filename[match,]
        ## Check that state and outcome are valid
        if(state %in% filename$State){
                flag1 <- 1
        }
        if(flag1 == 1){
                
                if(outcome == "heart attack"){
                        dfheartattack <-df[-grep("Not Available", df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]  
                        ordering <-order(as.numeric(as.character(dfheartattack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,dfheartattack$Hospital.Name)))
                        dfsortedheartattack <- dfheartattack[ordering, ]
                        ##finaldf <- dfsortedheartattack
                        
                        
                        if(num == "best"){
                                final = as.character(dfsortedheartattack[1,2])
                        }
                        else if(num == "worst"){
                                final = as.character(dfsortedheartattack[nrow(dfsortedheartattack),2])
                        }
                        else if(num >=1 || num <= nrow(dfsortedheartattack)){
                                final = as.character(dfsortedheartattack[num,2])
                        }
                        else{
                                na <- "NA"
                                na
                        }
                        
                }
                
                else if(outcome == "pneumonia"){
                        dfpneu <-df[-grep("Not Available", df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]  
                        ordering <-order(as.numeric(as.character(dfpneu$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,dfpneu$Hospital.Name)))
                        dfsortedpneu <- dfpneu[ordering, ]
                        ##finaldf <- dfsortedpneu
                        
                        if(num == "best"){
                                final = as.character(dfsortedpneu[1,2])
                        }
                        else if(num == "worst"){
                                final = as.character(dfsortedpneu[nrow(dfsortedpneu)-1,2])
                        }
                        else if(num >=1 || num <= nrow(dfsortedpneu)){
                                final = as.character(dfsortedpneu[num,2])
                        }
                        else{
                                na <- "NA"
                                na
                        }
                        
                }
                
                else if(outcome == "heart failure"){
                        dfheartfail <-df[-grep("Not Available", df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]  
                        ordering <-order(as.numeric(as.character(dfheartfail$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,dfheartfail$Hospital.Name)))
                        dfsortedheartfail <- dfheartfail[ordering, ]
                        ##dfsortedheartfail <- dfheartfail[order(dfheartfail$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,dfheartfail$Hospital.Name),]
                        ##finaldf <- dfsortedheartfail
                        
                        if(num == "best"){
                                final = as.character(dfsortedheartfail[1,2])
                        }
                        else if(num == "worst"){
                                final = as.character(dfsortedheartfail[nrow(dfsortedheartfail)-1,2])
                        }
                        else if(num >=1 || num <= nrow(dfsortedheartfail)){
                                final = as.character(dfsortedheartfail[num,2])
                        }
                        else{
                                na <- "NA"
                                na
                        }
                        
                }
                else{
                        stop("invalid outcome")
                }
                
        }
        else{
                stop("Invalid state")
        }
        final
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}