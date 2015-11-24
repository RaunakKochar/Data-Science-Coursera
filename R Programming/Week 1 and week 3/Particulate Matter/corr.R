corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filenames<-list.files(path="C:/Users/RAUNAK KOCHAR/Documents/prog1/specdata",full.names=T)
  df<- data.frame() 
  y <- vector()
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  for(i in 1:332){
    df <- read.csv(filenames[i])
    good <- complete.cases(df)
    dfgood <-data.frame(df[good,])
    if(nrow(dfgood) >= threshold){
      y<-c(y,cor(dfgood$nitrate, dfgood$sulfate))
    }
  }
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  y
}