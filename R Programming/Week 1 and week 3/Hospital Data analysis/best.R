best <- function(state, outcome) {
        ## Read outcome data
        setwd("C:/Users/RAUNAK KOCHAR/Documents/prog2/")
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
                        ordering <-order(as.numeric(as.character(dfheartattack$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)))
                        dfsortedheartattack <- dfheartattack[ordering, ]
                        finaldf <- dfsortedheartattack
                }
                
                else if(outcome == "pneumonia"){
                        dfpneu <-df[-grep("Not Available", df$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]  
                        ordering <-order(as.numeric(as.character(dfpneu$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,dfpneu$Hospital.Name)))
                        dfsortedpneu <- dfpneu[ordering, ]
                        finaldf <- dfsortedpneu
                }
                
                else if(outcome == "heart failure"){
                        dfheartfail <-df[-grep("Not Available", df$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]  
                        ordering <-order(as.numeric(as.character(dfheartfail$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)))
                        dfsortedheartfail <- dfheartfail[ordering, ]
                        finaldf <- dfsortedheartfail
                }
                else{
                        stop("invalid outcome")
                }
                
        }
        ## Return hospital name in that state with lowest 30-day death
        if(flag1 == 1){
                final <- as.character(finaldf[1,2])
                final
        }
        else{
                stop("invalid state")
        }
        
        
        ## rate
}