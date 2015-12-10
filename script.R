#the code would produce summary statistics of all the surveys 
#Author: Ashok Sekar
#Date: Nov 11 2015

rm(list=ls())

#datasetset is sense_of_place/Data/main_repository and respective folder 
#naming the respective folder as EE, CT, RPM 

#load libraries
library(xlsx)
library(lubridate)





# define generic names and values 
folder<-c('RPM','CT','EE') # name of the folders where datasets are present 
outputwd<-c('~/Google Drive/Professional/Sense of Place/Data/Results')
codes_pointer<-c('codes_RPM','codes_CT','codes_EE')






#warning! check if the dates are updated 
dates<-read.xlsx("~/Google Drive/Professional/Sense of Place/Data/surveydates.xlsx",1)
dates$Start<-force_tz(dates$Start,tzone = "America/New_York")
dates$End<-force_tz(dates$End,tzone = "America/New_York")





#error report
err_report<-data.frame()
for (i in 1:3)
{
  
  #set working directory
  setwd(paste0('~/Google Drive/Professional/Sense of Place/Data/Main Repository/',folder[i],'/Excel/'))
  
  #load file 
  rawFile<-read.xlsx('Sheet_1.xls',1,as.data.frame = TRUE)
  rawFile<-rawFile[-1,]
  
  
  
  setwd(outputwd) # set output directory
  #name of output files 
  output_names<-c(paste0('SumStats_',folder[i],'_demog.xlsx'), paste0('SumStats_',folder[i],'_travel.xlsx'))
  
  
  #datadictionary
  dictionary<-read.xlsx('~/Google Drive/Professional/Sense of Place/Data/lexicons.xlsx',1)
  
  #rename colnames 
  colnames(rawFile)<-dictionary[,codes_pointer[i]]
  
  
  #date preprocessing 
  
  #convert to dates 
  
  rawFile$StartDate<-as.numeric(as.character(rawFile$StartDate))
  rawFile$EndDate<-as.numeric(as.character(rawFile$EndDate))
  
  #converting start date 
  rawFile$StartDate<-as.POSIXct(as.Date(rawFile$StartDate,"1899-12-30"),tz="GMT")
  #timezone change
  attr(rawFile$StartDate,"tzone")<-"America/New_York"
  #converting end date 
  rawFile$EndDate<-as.POSIXct(as.Date(rawFile$EndDate,"1899-12-30"),tz="GMT")
  attr(rawFile$EndDate,"tzone")<-"America/New_York"
  
  #filtering only the survey dates
  datesubset<-subset(dates,Location==folder[i])
  
  
  dateid<-NULL
  for (j in 1:length(datesubset[,1])) 
  {
    id<-which((rawFile$StartDate >= datesubset$Start[j]) & 
                         (rawFile$StartDate <= datesubset$End[j]))
    dateid<-c(dateid,id)
  }
  err_report[1,i]<-length(rawFile$RespID)-length(dateid)
  
  
  #checking for glitch
  #if the last category name is empty then it is glitch 
  glitchid<-which(is.na(rawFile$status))
  err_report[2,i]<-length(glitchid)
  
  
  
  
  #ids to hold on 
  idshold<-setdiff(dateid,glitchid) #removing glitches from the date id
  
  #create data file with ids to hold 
  dataFile<-rawFile[idshold,]
  
  #choose only demographic variables 
  demog_id<-which(dictionary$category=='demographics') #demog_id
  demog_data<-dataFile[,c(1,demog_id)] #subset the demographics data 
  #list of all the summary
  summary<-list()
  for (j in 1:length(demog_id))
  {
    summary[[j]]<-data.frame(table(demog_data[,j+1]))
  } 
  names(summary) <- dictionary[,codes_pointer[i]][demog_id]
  
  
  
  #save as an excel file
  #lapply(names(summary), 
  #       function(x) write.xlsx(summary[[x]], 
  #                              paste0('SumStats_',folder[i],'_demog.xlsx'), sheetName=x, append=TRUE))
  
  
  #choose all travel variables
  travel_id<-which(dictionary$category=='travel') #travel_id
  travel_data<-dataFile[,c(1,travel_id)]
  
  
  summary<-list()
  for (j in 1:length(travel_id))
  {
    summary[[j]]<-data.frame(table(travel_data[,j+1]))
  } 
  names(summary) <- dictionary[,codes_pointer[i]][travel_id]
  
  
  #save as an excel file
  #lapply(names(summary), 
         #function(x) write.xlsx(summary[[x]], 
          #                      paste0('SumStats_',folder[i],'_travel.xlsx'), sheetName=x, append=TRUE))
  
  
  
}


colnames(err_report)<-folder
rownames(err_report)<-c('date','incomplete')
#write.csv(err_report,'err_in_rawdataset.csv')


