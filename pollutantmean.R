pollutantmean<- function(directory, pollutant, id=1:332){
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicaing the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant accross all monitors list in the 'id' vector (ignorning NA values) 
  ## NOTE: Do not round the result!
  
  ## for function debugging DELETE
  ##id<-c(1:10)
  ##directory<-"specdata"
  ##pollutant<-"sulfate"
  
  idNum<-1
  functionOut<-c(1:length(id))
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
    
    
    
    if (pollutant=="sulfate"){
      pIDX<-2
    }
    if (pollutant=="nitrate"){
      pIDX<-3
    }
    fileData<-drop_na(fileData,pIDX)
    if (idNum==1){
    functionOut<-fileData[,pIDX]
    }
    if(idNum>1){
      functionOut<-c(functionOut,fileData[,pIDX])
      
    }
    
    idNum<-idNum+1 
  }  
  returnData<-mean(functionOut)
  print(returnData)
  
  
}
