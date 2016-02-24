
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

folder<-"J:/MacLabUsers/HLADIK SHARED/Projects/Exosomes/Tewari U01 project/Claire Nanosight Tewari sample pdf and excel"

files = list.files(folder, pattern = ".csv")


filesPath <- paste(folder, "/", files, sep= "")

nanosight <- lapply(filesPath,read.delim, stringsAsFactors = FALSE, row.names = NULL, sep = ",")

names(nanosight)<-str_replace(files, pattern="-ExperimentSummary.csv", replacement="")


#here is a function to extract the data from the .csv files
#sometimes I did 3 standard measurements and sometimes I did 5.
#For the ones with 3, avg is located at 60,1 and std error at 60,2.
#For the ones with 5, avg is at 82,1 and std error is at 82,2.
#The sample name is always at 5,2


extractData<- function(df){

data.frame(col1=as.character(df[5,2]),
    col2= as.numeric(ifelse(df[75,1]=="Average",df[82,1],df[60,1])),#for if there were 3 or 5 measurements

    col3=as.numeric(ifelse(df[75,1]=="Average",df[82,2],df[60,2]))
)
  

}


#apply the extractData function to the nanosight files list
dat<-lapply(nanosight, FUN=extractData)

#make the list  into one df with the file name as the id column
dat<-bind_rows(dat,.id="FullName")

#change the column names
colnames(dat)<-c("FullName","Sample","Average","StdError")


#extract all the pieces of information that I want from the FullName
#and Sample columns

datEdit<-dat%>%
  mutate(Nanosight_Date = str_extract(FullName,"(2016).+\\s"))%>%
  mutate(PTID = str_extract(FullName,"\\d{3}(X|\\d)(F|S)?"))%>%
  mutate(PTID=str_replace(PTID,"F",""))%>%
  mutate(PTID= str_replace(PTID,"S",""))%>%
  mutate(Sample = str_replace(Sample, "-", ":"))%>%
  mutate(Dilution = str_extract(Sample,"(1:)\\d(\\d{2}|k)"))%>%
  mutate(Dilution = str_replace(Dilution, "k","000"))%>%
  mutate(F_or_S = str_extract(Sample,"[F|S]" ))%>%
  mutate(Protocol_Step = str_extract(Sample,"(C|E|D?D)1?"))%>%
  mutate(Measurement = str_extract(Sample,"standard(1|2)?"))%>%
  mutate(Calculated_Sample_Concentration = Average * as.numeric(sapply(strsplit(Dilution, ":"),
                                        function(x) x[[2]])))%>%
  mutate(Captures = ifelse(
    str_detect(Measurement,"standard(1|2)")==TRUE, "3 x 60s","5 x 60s"))%>% # captures per standard
  select(FullName,Nanosight_Date,PTID,F_or_S, Protocol_Step,
         Dilution,Measurement,Captures, Average, StdError,
         Calculated_Sample_Concentration)




names(datEdit)[c(1,9)]<-c("File_Name","Average_Concentration")


write.csv(datEdit, "J:/MacLabUsers/HLADIK SHARED/Projects/Exosomes/Tewari U01 project/Compiled Tewari Nanosight Data.csv")

## CHECK #####         
#I'm pretty sure that the entry for PTID 2246 should actually be
#for 2446 since that is what is written in my manual sample log
#and there are no other entries for 2246 AND LV said there aren't any
#other similar PTIDs so I changed the file name 23Feb16.

#I am adding a column clarifying how many measurements were taken
#for each standard


