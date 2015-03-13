#MOMENTUM
#
#Strategia 12/3, 12 miesiêcy "rank", 1 tydzieñ "skip", 3 miesi¹c "invest"
#Decyzje inwestycyjne podejmowane bêd¹ ostatniego dnia dnia miesi¹ca

#Tworzymy wektor miesiêcy 
for (t in 1:length(sumData[,1])){
    sumData$Month[t]<-months(as.Date((row.names(sumData)[t])))
}

#Tworzymy wektor okresów (1 okres-1 miesi¹c)
m<-1
for (t in 1:length(sumData[,1])){
    if(t>1){
        if(sumData$Month[t]!=sumData$Month[t-1]){
            m<-m+1
        }
    }
    sumData$Period[t]<-m
}

#Selekconujemy ostatnie dni miesi¹ca, oraz 7 dni je poprzedzaj¹ce 
sumData$newPeriod<-1:length(sumData[,1])
sumData$Skip<-rep(NA,length(sumData[,1]))
for (t in 1:length(sumData[,1])){
    if(t<length(sumData[,1])){
        if(sumData$Period[t]!=sumData$Period[t+1]){
            sumData$newPeriod[t]<-sumData$Period[t]
            sumData$Skip[t-7]<-sumData$Period[t]
        }else{
            sumData$newPeriod[t]<-NA
        }
    }else{
        sumData$newPeriod[t]<-sumData$Period[t]
        sumData$Skip[t-7]<-sumData$Period[t]
    }
}
#Tworzymy tabelê ze skumulowanymi stopami zwrotu dla ka¿dego ostatniego dnia miesi¹ca, oraz 7 poprzedzaj¹cego go dnia 
tablePeriods<-sumData[!is.na(sumData$newPeriod),]
tableSkip<-sumData[!is.na(sumData$Skip),]
sumData[is.na(sumData)] <- 0

#Obliczamy wartoœæ Momentum dla ka¿dego okresu
momData<-logData[0,]
for(t in 1:length(tableSkip[,1])){
    if(t>=13){ #Liczymy Momentum od 01-02-1990
        for(s in 1:length(logData)){
            momData[t-12,s]<-tableSkip[t,s]-tablePeriods[t-12,s] #Dla ka¿dego 7 dnia poprzedzaj¹cego ostatni dzieñ miesi¹ca odejmujemy wartoœæ skumulowanej stopy zwrotu sprzed 12 miesiêcy
        }
    }
}

#Wybieramy sk³adniki portfela
momComp<-momData
ro<-round(length(logData[1,])/10)
for(t in 1:length(momData[,1])){
    r<-rank(momData[t,])
    for (s in 1:length(momData)){
        if(r[s]<=ro){
            momComp[t,s]<-(-1) #Nadajemy wartoœæ -1 spó³ek z najgorszym momentum
        }else if(r[s]>(length(logData[1,])-ro)){
            momComp[t,s]<-1 #Nadajemy wartoœæ 1 dla spó³ek z najlepszym momentum
        }else{
            momComp[t,s]<-0 #Nadajemy wartoœæ 0 dla pozosta³ych spó³ek
        }
    }
}

#Tworzymy portfele Momentum
Momentum<-data.frame()
LongP<-data.frame()
ShortP<-data.frame()

for(t in seq(from=1,to=300,by=3)){
    assign(paste0('Momentum', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Momentum dla ka¿dego miesi¹ca
    assign(paste0('Long', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Long dla ka¿dego miesi¹ca
    assign(paste0('Short', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Short dla ka¿dego miesi¹ca
    for (s in 1:length(momData)){
        if(momComp[t,s]==1){
            assign(paste0('Long'), 
                   logData[sumData$Period %in% c(t+15,t+13,t+14),s]) #Tworzymy wektor stóp zwrotu dla akcji kupowanej
            assign(paste0('Long', '_', 'all'),
                   cbind(get(paste0('Long', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcjê do porfela Long
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcjê do porfela Momentum
            
        }else if (momComp[t,s]==-1){
            assign(paste0('Short'), 
                   -logData[sumData$Period %in% c(t+15,t+13,t+14),s]) #Tworzymy wektor stóp zwrotu dla akcji sprzedawanej
            assign(paste0('Short', '_', 'all'),
                   cbind(get(paste0('Short', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcjê do porfela Short
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcjê do porfela Momentum
        }
        rm(Long)
        rm(Short)
    }
    Momentum<-rbind(Momentum,get(paste0('Momentum', '_', 'all')))
    LongP<-rbind(LongP,get(paste0('Long', '_', 'all')))
    ShortP<-rbind(ShortP,get(paste0('Short', '_', 'all')))
    rm('Momentum_all')
    rm('Long_all')
    rm('Short_all')
}