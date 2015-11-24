complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  filenames<-list.files(path="C:/Users/RAUNAK KOCHAR/Documents/prog1/specdata",full.names=T)
  df<- data.frame()
  ##dfnew<- data.frame()
  ##dffinal<-data.frame()
  table<-data.frame()
  x<-c(id)
  y<-c()
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  for(i in seq_along(id)){
    
    ##y<-c()  
    df<-read.csv(filenames[id[i]])
    good<-complete.cases(df)
    dfgood<-data.frame(df[good,])
    nr<-nrow(dfgood)
    y<-c(y,nr)
    ##x_name<-id
    ##y_name<-nobs
    ##df<-cbind(id<-x,nobs<-nr)
    
    ##  dffinal<-rbind(dffinal,dfnew)
  }
  df<-data.frame(id = x,nobs = y)
  df
  ##require(reshape2)
  ##df<-melt(data.frame(x,y))
  ##colnames(df)<-c(x_name,y_name)
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
}


lm<-function(x){
  x*x
}