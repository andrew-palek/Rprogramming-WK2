complete<- function(directory, id=1:332){
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return a data frame of the form: 
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs is the number of complete cases
  
  
  ## for function debugging DELETE
##id<-1
 ## directory<-"specdata"
  
  
  idNum<-1
  while (idNum<=length(id)){
    textNum<-as.character(id[idNum])
    
    lID<-nchar(textNum)
    if (lID<2){
      textNum<-paste("0",textNum,sep="")
    }
    if (lID<3){
      textNum<-paste("0",textNum,sep="")
    }
    
    fullPath<-paste("./",directory,"/",textNum,".csv",sep="")
    fileData<-read.csv(fullPath)
    
    
    fileData<-drop_na(fileData)
    
    
    if (idNum==1){
      idList<-c(id[idNum])
      nobsList<-length(fileData[,2])
    }
    if(idNum>1){
      idList<-c(idList,id[idNum])
      nobsList<-c(nobsList,length(fileData[,2]))
    }
    
    idNum<-idNum+1 
  } 
  
  returnData<-data.frame("id"=idList,"nobs"=nobsList)
  print(returnData)
  
}
