rankall <- function(outcome, num = "best"){
    df <- read.csv("rprog-data-ProgAssignment3-data (1)//outcome-of-care-measures.csv")
    
    dffinal <- data.frame(hospital = character(), state = character())
    
    s <- split(df, df$State)
    
    if(outcome == "heart attack"){
        for(i in 1:50){
            dff <- as.data.frame(s[[i]])
            dff <- dff[order(as.numeric(as.character(dff$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), dff$Hospital.Name),]
            stt <- as.character(dff[1, 7])
            if(i == 1){
                if(num == "best"){
                    dffinal <- dff[1, c(2, 7)]
                }
                
                else if(num == "worst"){
                    dffinal <- dff[nrow(dff),c(2,7)]
                }
                else if(num <= nrow(dff)){
                    dffinal <- dff[num,c(2,7)]
                }
                else{
                    dffinal <- c("NA", stt)
                }
            }
            
            else{
                if(num == "best"){
                    dffinal <- rbind(dffinal, dff[1,c(2,7)])
                }
                
                else if(num == "worst"){
                    dffinal <- rbind(dffinal, dff[nrow(dff),c(2,7)])
                }
                else if(num <= nrow(dff)){
                    dffinal <- rbind(dffinal, dff[num,c(2,7)])
                }
                else{
                    dffinal <- rbind(dffinal, c("NA", stt))
                }
            }
            
        }
    }
    
    if(outcome == "heart failure"){
        for(i in 1:50){
            dff <- as.data.frame(s[[i]])
            dff <- dff[order(as.numeric(as.character(dff$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), dff$Hospital.Name),]
            stt <- as.character(dff[1, 7])
            if(i == 1){
                if(num == "best"){
                    dffinal <- dff[1, c(2, 7)]
                }
                
                else if(num == "worst"){
                    dffinal <- dff[nrow(dff),c(2,7)]
                }
                else if(num <= nrow(dff)){
                    dffinal <- dff[num,c(2,7)]
                }
                else{
                    dffinal <- c("NA", stt)
                }
            }
            
            else{
                if(num == "best"){
                    dffinal <- rbind(dffinal, dff[1,c(2,7)])
                }
                
                else if(num == "worst"){
                    dffinal <- rbind(dffinal, dff[nrow(dff),c(2,7)])
                }
                else if(num <= nrow(dff)){
                    dffinal <- rbind(dffinal, dff[num,c(2,7)])
                }
                else{
                    dffinal <- rbind(dffinal, c("NA", stt))
                }
            }
            
        }
    }
    
    if(outcome == "pneumonia"){
        for(i in 1:50){
            dff <- as.data.frame(s[[i]])
            dff <- dff[order(as.numeric(as.character(dff$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), dff$Hospital.Name),]
            stt <- as.character(dff[1, 7])
            if(i == 1){
                if(num == "best"){
                    dffinal <- dff[1, c(2, 7)]
                }
                
                else if(num == "worst"){
                    dffinal <- dff[nrow(dff),c(2,7)]
                }
                else if(num <= nrow(dff)){
                    dffinal <- dff[num,c(2,7)]
                }
                else{
                    dffinal <- c("NA", stt)
                }
            }
            
            else{
                if(num == "best"){
                    dffinal <- rbind(dffinal, dff[1,c(2,7)])
                }
                
                else if(num == "worst"){
                    dffinal <- rbind(dffinal, dff[nrow(dff),c(2,7)])
                }
                else if(num <= nrow(dff)){
                    dffinal <- rbind(dffinal, dff[num,c(2,7)])
                }
                else{
                    dffinal <- rbind(dffinal, c("NA", stt))
                }
            }
            
        }
    }
    colnames(dffinal) <- c("hospital", "state")
    dffinal
}