require(xlsx)
require(reshape2)
require(lubridate)
require(plyr)
require(likert)
require(R.utils)
require(Rmisc)
require(ggplot2)
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#generating all the resutls for presentation 
load("~/Google Drive/Professional/Sense of Place/Data/Results/work.Rdata")



#save file 
save.image(file = '~/Google Drive/Professional/Sense of Place/Data/Results/work.Rdata')

#keyforthisprogram
key<-data.frame(matrix(nrow = 20,ncol = 3))
colnames(key)<-c('Name','Description','Comments')


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------

#the count of the number of people surveyed 
##load the number of glitches file
err_in_rawdataset<-read.csv("~/Google Drive/Professional/Sense of Place/Data/Results/err_in_rawdataset.csv")
count_respondents<-data.frame(matrix(nrow = 6,ncol = 3))
rownames(count_respondents)<-c('surveyed','trial','trial_error','date_error','incomplete','final')
colnames(count_respondents)<-c('RPM','CT','EE')
##adding count in date_error and incompleteness
count_respondents[c('date_error'),]<-as.numeric(as.integer(err_in_rawdataset[1,2:4]))
count_respondents[c('incomplete'),]<-as.numeric(as.integer(err_in_rawdataset[2,2:4]))
##loading the trial data and quantifyign the count 
trial<-read.xlsx('/Users/ashok/Google Drive/Professional/Sense of Place/Data/Main Repository/RPM/Old_Oct08_Oct10GEV/Excel/Sheet_1.xls',1)
trial<-trial[-1,]
trial$StartDate<-as.POSIXct(as.Date(as.numeric(as.character(trial$StartDate)),"1899-12-30"),tz="GMT")
attr(trial$StartDate,"tzone")<-"America/New_York" #timezone change 
trial$EndDate<-as.POSIXct(as.Date(as.numeric(as.character(trial$EndDate)),"1899-12-30"),tz="GMT")
attr(trial$StartDate,"tzone")<-"America/New_York" #timezone change 
##there are two error combined here...one is GEV tablet and other is trial 
count_respondents[c("trial"),]<-c(as.numeric(table(as.Date(trial$StartDate)))[1],0,0)
count_respondents[c("trial_error"),]<-c(as.numeric(table(as.Date(trial$StartDate)))[2],0,0)
##final 
count_respondents[c("final"),]<-c(as.numeric(table(mainFile$locID))[3],as.numeric(table(mainFile$locID))[1],
                                  as.numeric(table(mainFile$locID))[2]) 
count_respondents[c("surveyed"),]<-colSums(count_respondents[2:6,1:3])
key[1,1:2]<-c('count_respondents','summary of the number of responses')





#to find the number of people who surveyed about other locations
##correcting for NA in the mainFile. from the current analysis they should be errors
mainFile$participation[is.na(mainFile$participation)]<-"Yes"
temp<-table(subFile$interviewLoc,subFile$locID,factor(subFile$participation))
temp<-data.frame(ftable(temp))
temp<-temp[-which(temp[,1]==temp[,2]),] # removing unncessary files 
temp1<-table(mainFile$interviewLoc,mainFile$locID,factor(mainFile$participation))
temp1<-data.frame(ftable(temp1))
temp1<-temp1[-which(temp1[,1]!=temp1[,2]),]
temp2<-rbind(temp,temp1)
temp2<-temp2[order(temp2$Var1),]
colnames(temp2)<-c('interviewLoc','locID','participation','Freq')
##finalresult
ctable_location<-temp2 
rm(temp2,temp1, temp)
key[2,1:2]<-c('ctable_location','contingency table that summarizes count of 
              responses about other locations')





#to find the behavioral statistics 
##behavioral values are available as likert scale values in both the mainFile and subFile. 
##convert the variables to factor datatype 
vars_Likertscale<-as.character(unique(dictionary$Code[which(dictionary$AnswerType=="Likert")])) 
id_Likert_mainFile<-which(colnames(mainFile) %in% vars_Likertscale)
id_Likert_subFile<-which(colnames(subFile) %in% vars_Likertscale)
###workfile for the analysis - first the mainFile 
chk<-mainFile[,c(id_Likert_mainFile)]
for (i in 1:dim(chk)[2])
{
  chk[,i]<-as.character(chk[,i])
  chk[,i]<-revalue(chk[,i],c("1"="Strongly Disagree",
                       "2"="Disagree",
                       "3"="Slightly Disagree",
                       "4"="Neutral",
                       "5"="Slightly Agree",
                       "6"="Agree",
                       "7"="Strongly Agree"))
  #chk[,i]<-factor(chk[,i],levels = c("1","2","3",
                                     #"4","5","6","7"))
  chk[,i]<-factor(chk[,i],levels = c("Strongly Disagree","Disagree","Slightly Disagree",
                                     "Neutral","Slightly Agree","Agree","Strongly Agree"))
}
behavior_mainFile<-cbind(mainFile[,c("RespID","interviewLoc","locID","participation")],chk)
###find the percentage and count values for each behavior attribute
summary_bmainfile<-data.frame(matrix(nrow = 21,ncol = length(colnames(behavior_mainFile))-1))
colnames(summary_bmainfile)<-c("interviewLoc","locID","Scale",colnames(behavior_mainFile[-(1:4)]))
scale<-data.frame(matrix(nrow = 21,ncol = 21))
location<-data.frame(matrix(nrow = 21,ncol = 21))
for (i in 1:(dim(summary_bmainfile)[2]-3))
{
  j<-i+4
  ftable_temp<-ftable(table(behavior_mainFile$interviewLoc,behavior_mainFile[,j]))
  perctable_temp<-data.frame(round(ftable_temp/rowSums(ftable_temp),2))
  perctable_temp<-perctable_temp[order(perctable_temp$Var1,perctable_temp$Var2),]
  scale[,i]<-perctable_temp$Var2
  location[,i]<-perctable_temp$Var1
  summary_bmainfile[,i+3]<-perctable_temp$Freq
}
##final result that summarizes the count of likert score results 
summary_bmainfile$interviewLoc<-perctable_temp$Var1
summary_bmainfile$locID<-perctable_temp$Var1
summary_bmainfile$Scale<-perctable_temp$Var2 
key[3,1:3]<-c('behavior_mainFile','subset of the mainFile containing only 
              behavior responses','values are factors')
key[4,1:3]<-c('summary_bmainfile','summarizes the likert scale values as  
              percentage','also can be summarized as count')





#generating results figure for behavior
##preparing the datafile 
###dividing the dataset into different sop categories 
templist<-list()
sopcategories<-c('aesthetics','attachment','dependence','identity',
                 'satisfaction','social')
vars_sopcategoryID<-list()
vars_sopquestion<-list()
for (i in 1:3)
{
  for (j in 1:length(sopcategories))
  {
  #creating a list of variables for sop category
  vars_sopcategoryID[[sopcategories[j]]]<-as.character(
    unique(dictionary$Code[which(dictionary$categoryID==sopcategories[j])]))
  #creating the temp file that includes only one location and one sopcategory
  temp<-behavior_mainFile[behavior_mainFile$locID==unique(behavior_mainFile$locID)[i],
                          c(vars_sopcategoryID[[sopcategories[j]]])]
  #changing colnames into questions
  ##first getting the appropriate questions 
  vars_sopquestion[[sopcategories[j]]]<-as.character(
    unique(dictionary$Response[dictionary$Code %in% colnames(temp)]))  
  ##changing the colnames 
  colnames(temp)<-vars_sopquestion[[sopcategories[j]]]
  #plot the figure and store as a list   
  filename<-paste0(unique(behavior_mainFile$locID)[i],'_',sopcategories[j],'.pdf')
  plot(likert(temp))+ggtitle(capitalize(sopcategories[j]))
  #ggsave(filename,dpi = 300,width = 14,height = 4)
  dev.off
  #rm(temp)
  }
}





#finding the results for the internet responses
ctable_online<-data.frame(ftable(table(mainFile$interviewLoc,
                        factor(mainFile$online),
                        factor(mainFile$read),
                        factor(mainFile$write))))
colnames(ctable_online)<-c('interviewLoc','online','read','write','count')
key[5,1:2]<-c('ctable_online','contingency table that summarizes count of 
              responders use of internet')





#finding travel behavior to the interview location 
travelID<-as.character(dictionary$Code[dictionary$categoryID=='travel'])
travel_mainFile<-mainFile[,c("RespID","interviewLoc","locID",'participation',travelID)]





#finding demographics 




#-----------------------------------------------
#creating two new datasets
#creating new datasets to work with the survey data 


#the plan is to have two datasets 1) will contain responses about the interviewee's location
#and their demographics 2) will contain interviewee's opinion about other location 


#steps 
##input the datasets 
##rename the colnames appropriately 
##add new colnames to indicate the SITEID and primary LOC data. 
##divide the dataset into two 1) about respondent and respondents idea about a location and 2) resp idea about other locations
##cbind 6 different datasets into only two datasets 
##cleaning the behavioral responses into numbers (don't forget to save)
##get mean values 


#defines the order of location in the surey 
orderLoc<-data.frame(matrix(nrow = 3,ncol = 3)) 
colnames(orderLoc)<-c('pri','sec1','sec2')
orderLoc$pri<-c('RPM','CT','EE')
orderLoc$sec1<-c('CT','RPM','CT')
orderLoc$sec2<-c('EE','EE','RPM')

#input dictionary 
setwd("~/Google Drive/Professional/Sense of Place/Data")
dictionary<-read.xlsx('newlexicons.xlsx',1,as.data.frame = TRUE)
#dictionary$AnswerType<-as.character(dictionary$AnswerType)


#new datasets
primaryfile<-data.frame()
secondaryfile<-data.frame()

for (i in 1:3)
{
  #input the dataset 
  setwd(paste0('~/Google Drive/Professional/Sense of Place/Data/Main Repository/',orderLoc$pri[i],'/Excel/'))
  
  
  rawFile<-read.xlsx('Sheet_1.xls',1,as.data.frame = TRUE)
  rawFile<-rawFile[-1,]
  
#   if (orderLoc$pri[i]==c('RPM'))
#   {
#     trial<-read.xlsx('/Users/ashok/Google Drive/Professional/Sense of Place/Data/Main Repository/RPM/Old_Oct08_Oct10GEV/Excel/Sheet_1.xls',1))
#   }
  
  #conversion of the files
  ##primaryfile
  pfile<-rawFile[,dictionary$Number[dictionary$GroupID==1]]
  colnames(pfile)<-dictionary$Code[dictionary$GroupID==1]
  pfile$interviewLoc<-orderLoc$pri[i] #depending on the filename
  pfile$locID<-orderLoc$pri[i] #depending on filename
  pfile<-pfile[,c(1,2,3,61,62,4:60)]
  
  primaryfile<-rbind(primaryfile,pfile) #the main file
  rm(pfile)
  
  ##secondaryfile
  sfile<-data.frame()
  for (j in 2:3)
  {
    tempfile<-rawFile[,c(1,dictionary$Number[dictionary$GroupID==j])]
    tempfile<-tempfile[-1,]
    colnames(tempfile)<-c(as.character(dictionary$Code[1]),as.character(dictionary$Code[dictionary$GroupID==j]))
    tempfile$interviewLoc<-orderLoc$pri[i] #refer to name
    tempfile$locID<-orderLoc[i,j] # refer to filename 
    sfile<-rbind(sfile,tempfile)
  }
  #sorting and reorganizing the dataset 
  require(plyr)
  sfile<-sfile[,c(1,25,26,2:24)]
  secondaryfile<-rbind(secondaryfile,sfile)
  secondaryfile<-arrange(secondaryfile,RespID)
  
}

#changing the datatype in the primaryfile 

##likert scale answers will be converted to (0,1 to 7) or (No Comment, Strongly Disagree to Strongly Agree)
##plan: find those columns w/ likert scale values replace them with corresponding values 

#convert all data into character 
#primaryfile<-data.frame(lapply(primaryfile,as.character))

#for likert scale based colums replace them accordingly 
##primary file
for (i in 1:dim(primaryfile)[2])
{
  if (dictionary$AnswerType[min(which(dictionary$Code==colnames(primaryfile[i])))]=="Likert")
  {
    primaryfile[,i]<-revalue(primaryfile[,i], c("-2"="2", "-3"="3","-5"="5","-6"="6","Strongly Disagree (1)"="1",
                 "Strongly Agree (7)"="7","Neutral (4)"="4",
                 "<span style=\"color: #0000ff;\"><strong>No Comment</strong></span>"="NA")) 
                  #revalue is from plyr package 
  }
  
}
##seondary file 
for (i in 1:dim(secondaryfile)[2])
{
  if (dictionary$AnswerType[min(which(dictionary$Code==colnames(secondaryfile[i])))] =="Likert")
  {
    secondaryfile[,i]<-revalue(secondaryfile[,i], c("-2"="2", "-3"="3","-5"="5","-6"="6","Strongly Disagree (1)"="1",
                                                "Strongly Agree (7)"="7","Neutral (4)"="4",
                                                "<span style=\"color: #0000ff;\"><strong>No Comment</strong></span>"="NA")) 
    #revalue is from plyr package 
  }
}


#removing non-confrming dates and glitches 
##first the dates 

dates<-read.xlsx("~/Google Drive/Professional/Sense of Place/Data/surveydates.xlsx",1)
dates$Start<-force_tz(dates$Start,tzone = "America/New_York")
dates$End<-force_tz(dates$End,tzone = "America/New_York")

#converting start date 
primaryfile$StartDate<-as.POSIXct(as.Date(as.numeric(as.character(primaryfile$StartDate)),"1899-12-30"),tz="GMT")
#timezone change
attr(primaryfile$StartDate,"tzone")<-"America/New_York"
#converting end date 
primaryfile$EndDate<-as.POSIXct(as.Date(as.numeric(as.character(primaryfile$EndDate)),"1899-12-30"),tz="GMT")
attr(primaryfile$EndDate,"tzone")<-"America/New_York"

dates$interval<-as.interval(dates$Start,dates$End)
primaryfile$interval<-as.interval(primaryfile$StartDate,primaryfile$EndDate)

dateid<-data.frame() # date id provide the list of ids not needed 
for (i in 1:dim(primaryfile)[1])
{
  dateid[1,i]<-sum(int_overlaps(primaryfile$interval[i],dates$interval[dates$Location==primaryfile$interviewLoc[i]]))
}
dateid<-which(dateid[1,]==0)

#checking for glitch
#if the last category name is empty then it is glitch 
glitchid<-which(is.na(primaryfile$status))

err_respID<-as.character(primaryfile$RespID[c(dateid,glitchid)])#save these id's

#creating new datasets 
mainFile<-primaryfile[!primaryfile$RespID %in% err_respID,]
subFile<-secondaryfile[!secondaryfile$RespID %in% err_respID,]




############-----------------------------------------------






