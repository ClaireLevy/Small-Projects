
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)

folder<-"J:/MacLabUsers/HLADIK SHARED/Projects/Exosomes/Tewari U01 project/Claire Nanosight Tewari sample pdf and excel"

files = list.files(folder, pattern = ".csv")


filesPath <- paste(folder, "/", files, sep= "")

nanosight <- lapply(filesPath,read.delim, stringsAsFactors = FALSE, row.names = NULL, sep = ",")

names(nanosight)<-str_replace(files, pattern="-ExperimentSummary.csv", replacement="")
#here is a functio to extract the data from the .csv files
#sometimes I did 3 standard measurements and sometimes I did 5.
#For the ones with 3, avg is located at 60,1 and std error at 60,2.
#For the ones with 5, avg is at 82,1 and std error is at 82,2.
#The sample name is always at 5,2


extractData<- function(df){

data.frame(col1=as.character(df[5,2]),
    col2= ifelse(df[75,1]=="Average",df[82,1],df[60,1]),#for if there were 3 or 5 measurements

    col3=ifelse(df[75,1]=="Average",df[82,2],df[60,2]))

}

dat<-lapply(nanosight, FUN=extractData)

dat<-bind_rows(dat,.id="FullName")

colnames(dat)<-c("FullName","Sample","Average","StdError")


#extract just the date portion out of the FullName column

dat<-dat%>%
  mutate(Nanosight_Date = str_extract(FullName,"(2016).+\\s"))%>%
  mutate(PTID = str_extract(FullName,"\\d{3}(X|\\d)(F|S)?"))%>%
  mutate(PTID=str_replace(PTID,"F",""))%>%
  mutate(PTID= str_replace(PTID,"S",""))



#now I also want a column that is the multiplier to get the TRUE concentration
#i.e. if make a 1:300 dilution, the concentration should be multiplied by 300

datShort$Sample<- as.character(datShort$Sample)

datShort<-mutate(datShort, Multiplier=unlist(strsplit(datShort$Sample, split='-', fixed=TRUE))[2])

datShort$Multiplier<-as.numeric(datShort$Multiplier)

datShort<-mutate(datShort,Final.Concentration = Multiplier* Average.Concentration)

