#the code would produce summary statistics of all the surveys 
#Author: Ashok Sekar
#Date: Nov 11 2015

rm(list=ls())

#datasetset is sense_of_place/Data/main_repository and respective folder 
#naming the respective folder as EE, CT, RPM 

#load libraries
library(xlsx)

# define generic names and values 
folder<-c('RPM','CT','EE') # name of the folders where datasets are present 
outputwd<-c('~/Google Drive/Professional/Sense of Place/Data/Results')
codes_pointer<-c('codes_RPM','codes_CT','codes_EE')

#warning! check if the dates are updated 
dates<-read.xlsx("~/Google Drive/Professional/Sense of Place/Data/surveydates.xlsx",1)


for (i in 1:3)
{
  
  #set working directory
  setwd(paste0('~/Google Drive/Professional/Sense of Place/Data/Main Repository/',folder[i],'/Excel/'))
  
  #load file 
  dataFile<-read.xlsx('Sheet_1.xls',1,as.data.frame = TRUE)
  dataFile<-dataFile[-1,]
  
  
  
  setwd(outputwd) # set output directory
  #name of output files 
  output_names<-c(paste0('SumStats_',folder[i],'_demog.xlsx'), paste0('SumStats_',folder[i],'_travel.xlsx'))
  
  
  #datadictionary
  dictionary<-read.xlsx('~/Google Drive/Professional/Sense of Place/Data/lexicons.xlsx',1)
  
  #rename colnames 
  colnames(dataFile)<-dictionary[,codes_pointer[i]]
  
  
  #date preprocessing 
  
  #convert to dates 
  
  dataFile$StartDate<-as.numeric(as.character(dataFile$StartDate))
  dataFile$EndDate<-as.numeric(as.character(dataFile$EndDate))
  
  #converting start date 
  dataFile$StartDate<-as.POSIXct(as.Date(dataFile$StartDate,"1899-12-30"),tz="GMT")
  #timezone change
  attr(dataFile$StartDate,"tzone")<-"UTC"
 
  #converting end date 
  dataFile$EndDate<-as.POSIXct(as.Date(dataFile$EndDate,"1899-12-30"),tz="GMT")
  attr(dataFile$EndDate,"tzone")<-"UTC"
  
  
  
  
  #choose only demographic variables 
  demog_id<-which(dictionary$category=='demographics')
  demog_data<-dataFile[,c(1,demog_id)] #subset the demographics data 
  #list of all the summary
  summary<-list()
  for (j in 1:length(demog_id))
  {
    summary[[j]]<-data.frame(table(demog_data[,j+1]))
  } 
  names(summary) <- dictionary[,codes_pointer[i]][demog_id]
  
  
  
  #save as an excel file
  lapply(names(summary), 
         function(x) write.xlsx(summary[[x]], 
                                paste0('SumStats_',folder[i],'_demog.xlsx'), sheetName=x, append=TRUE))
  
  
  #choose all travel variables
  travel_id<-which(dictionary$category=='travel')
  travel_data<-dataFile[,c(1,travel_id)]
  
  summary<-list()
  for (j in 1:length(travel_id))
  {
    summary[[j]]<-data.frame(table(travel_data[,j+1]))
  } 
  names(summary) <- dictionary[,codes_pointer[i]][travel_id]
  
  
  #save as an excel file
  lapply(names(summary), 
         function(x) write.xlsx(summary[[x]], 
                                paste0('SumStats_',folder[i],'_travel.xlsx'), sheetName=x, append=TRUE))
  
  
  
}


