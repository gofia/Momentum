Data<-read.csv("allData.csv") #³adujemy dane NYSE
Data<-Data[,!is.na(Data[1,])] #bierzemy tylko spó³ki z pe³n¹ histori¹ od 1981-01-30 do 2014-12-31

#Naprawiamy kalendarz
Data[,1]<-as.Date(Data[,1])
library(dplyr)
library(zoo)
Data$Date<-Data[,1]
full.dates <- as.Date(min(Data$Date):max(Data$Date), origin = "1970-01-01")
db <- data.frame(Date = full.dates)
Data <- left_join(db, Data)
Data$X.1<-NULL

#zamieniamy puste komórki wartoœciami z t-1
for(s in 1:(length(Data))){
    for (t in 1:length(Data[,s])){
        if(is.na(Data[t,s])){
            Data[t,s]<-Data[t-1,s]
        }
    }
}

#zamieniamy wektory cen zamkniêcia wektorami stóp zwrotu
logar<-function(vec){ 
    Return<-log(vec[-1]/vec)
    Return<-c(NA,(Return[1:(length(Return)-1)]))
    return(Return)
}

logData<-Data
for(i in 1:(length(Data)-1) ){
    logData[,i+1]<-as.data.frame(logar(as.numeric(Data[,i+1])))
}

#czyœcimy tabelê
logData<-logData[2:length(logData[,1]),]
row.names(logData)<-as.Date(logData[,1])
logData<-logData[,2:length(logData)]

#tworzymy tabelê ze skumulowanymi stopami zwrotu
sumData<-logData
for(s in 1:(length(logData))){
    for (t in 1:length(logData[,s])){
        sumData[t,s]<-sum(logData[1:t,s])
    }
}
