pollutantmean <-
function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  #setwd(directory)
  filenames<-list.files(path="C:/Users/RAUNAK KOCHAR/Documents/prog1/specdata",full.names=T)
  df<- data.frame()
  for(i in seq_along(id)){
    ##print(id[i])
    df<- rbind(df,read.csv(filenames[id[i]]))
  }
  mean(df[,pollutant],na.rm=T)
}
