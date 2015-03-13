#MOMENTUM
#
#Strategia 12/3, 12 miesi�cy "rank", 1 tydzie� "skip", 3 miesi�c "invest"
#Decyzje inwestycyjne podejmowane b�d� ostatniego dnia dnia miesi�ca

#Tworzymy wektor miesi�cy 
for (t in 1:length(sumData[,1])){
    sumData$Month[t]<-months(as.Date((row.names(sumData)[t])))
}

#Tworzymy wektor okres�w (1 okres-1 miesi�c)
m<-1
for (t in 1:length(sumData[,1])){
    if(t>1){
        if(sumData$Month[t]!=sumData$Month[t-1]){
            m<-m+1
        }
    }
    sumData$Period[t]<-m
}

#Selekconujemy ostatnie dni miesi�ca, oraz 7 dni je poprzedzaj�ce 
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
#Tworzymy tabel� ze skumulowanymi stopami zwrotu dla ka�dego ostatniego dnia miesi�ca, oraz 7 poprzedzaj�cego go dnia 
tablePeriods<-sumData[!is.na(sumData$newPeriod),]
tableSkip<-sumData[!is.na(sumData$Skip),]
sumData[is.na(sumData)] <- 0

#Obliczamy warto�� Momentum dla ka�dego okresu
momData<-logData[0,]
for(t in 1:length(tableSkip[,1])){
    if(t>=13){ #Liczymy Momentum od 01-02-1990
        for(s in 1:length(logData)){
            momData[t-12,s]<-tableSkip[t,s]-tablePeriods[t-12,s] #Dla ka�dego 7 dnia poprzedzaj�cego ostatni dzie� miesi�ca odejmujemy warto�� skumulowanej stopy zwrotu sprzed 12 miesi�cy
        }
    }
}

#Wybieramy sk�adniki portfela
momComp<-momData
ro<-round(length(logData[1,])/10)
for(t in 1:length(momData[,1])){
    r<-rank(momData[t,])
    for (s in 1:length(momData)){
        if(r[s]<=ro){
            momComp[t,s]<-(-1) #Nadajemy warto�� -1 sp�ek z najgorszym momentum
        }else if(r[s]>(length(logData[1,])-ro)){
            momComp[t,s]<-1 #Nadajemy warto�� 1 dla sp�ek z najlepszym momentum
        }else{
            momComp[t,s]<-0 #Nadajemy warto�� 0 dla pozosta�ych sp�ek
        }
    }
}

#Tworzymy portfele Momentum
Momentum<-data.frame()
LongP<-data.frame()
ShortP<-data.frame()

for(t in seq(from=1,to=300,by=3)){
    assign(paste0('Momentum', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Momentum dla ka�dego miesi�ca
    assign(paste0('Long', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Long dla ka�dego miesi�ca
    assign(paste0('Short', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+15,t+13,t+14),]))) #Tworzymy portfel Short dla ka�dego miesi�ca
    for (s in 1:length(momData)){
        if(momComp[t,s]==1){
            assign(paste0('Long'), 
                   logData[sumData$Period %in% c(t+15,t+13,t+14),s]) #Tworzymy wektor st�p zwrotu dla akcji kupowanej
            assign(paste0('Long', '_', 'all'),
                   cbind(get(paste0('Long', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcj� do porfela Long
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcj� do porfela Momentum
            
        }else if (momComp[t,s]==-1){
            assign(paste0('Short'), 
                   -logData[sumData$Period %in% c(t+15,t+13,t+14),s]) #Tworzymy wektor st�p zwrotu dla akcji sprzedawanej
            assign(paste0('Short', '_', 'all'),
                   cbind(get(paste0('Short', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcj� do porfela Short
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcj� do porfela Momentum
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