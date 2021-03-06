#MOMENTUM
#
#Strategia MOM12-2, 12 miesi�cy "rank", 1 tydzie� "skip", 1 miesi�c "invest"
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
skip<-0
sumData$newPeriod<-1:length(sumData[,1])
sumData$Skip<-rep(NA,length(sumData[,1]))
for (t in 1:length(sumData[,1])){
    if(t<length(sumData[,1])){
        if(sumData$Period[t]!=sumData$Period[t+1]){
            sumData$newPeriod[t]<-sumData$Period[t]
            sumData$Skip[t-skip]<-sumData$Period[t]
        }else{
            sumData$newPeriod[t]<-NA
            sumData$Skip[t]<-NA
        }
    }else{
        sumData$newPeriod[t]<-sumData$Period[t]
        sumData$Skip[t-skip]<-sumData$Period[t]
    }
}
#Tworzymy tabel� ze skumulowanymi stopami zwrotu dla ka�dego ostatniego dnia miesi�ca, oraz 7 poprzedzaj�cego go dnia 
tablePeriods<-sumData[!is.na(sumData$newPeriod),]
tableSkip<-sumData[!is.na(sumData$Skip),]
sumData[is.na(sumData)] <- 0

#Obliczamy warto�� Momentum dla ka�dego okresu
p<-12
momData<-logData[0,]
for(t in 1:length(tableSkip[,1])){
    if(t>=(p+1)){
        for(s in 1:length(logData)){
            momData[t-p,s]<-tableSkip[t-1,s]-tablePeriods[t-p,s] #Dla ka�dego 7 dnia poprzedzaj�cego ostatni dzie� miesi�ca odejmujemy warto�� skumulowanej stopy zwrotu sprzed 12 miesi�cy
        }
    }
}

#Wybieramy sk�adniki portfela
r<-momData
ro<-round(length(r)/10)
for(t in 1:length(momData[,1])){
    r[t,]<-rank(momData[t,])
}
r[r<=ro]<--1 #Nadajemy warto�� -1 dla sp�ek z najgorszym momentum
r[r>(length(logData[1,])-ro)]<-1 #Nadajemy warto�� 1 dla sp�ek z najlepszym momentum
r[r!=1 & r!=-1]<-0 #Nadajemy warto�� 0 dla pozosta�ych sp�ek
momComp<-r

#Tworzymy portfele Momentum
Momentum<-data.frame()
LongP<-data.frame()
ShortP<-data.frame()

for(t in seq(from=1,to=length(momComp[,1]),by=1)){
    assign(paste0('Momentum', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Momentum dla ka�dego miesi�ca
    assign(paste0('Long', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Long dla ka�dego miesi�ca
    assign(paste0('Short', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Short dla ka�dego miesi�ca
    for (s in 1:length(momData)){
        if(momComp[t,s]==1){
            assign(paste0('Long'), 
                   logData[sumData$Period %in% c(t+13),s]) #Tworzymy wektor stop zwrotu dla akcji kupowanej
            assign(paste0('Long', '_', 'all'),
                   cbind(get(paste0('Long', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcj� do porfela Long
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcj� do porfela Momentum
            
        }else if (momComp[t,s]==-1){
            assign(paste0('Short'), 
                   -logData[sumData$Period %in% c(t+13),s]) #Tworzymy wektor stop zwrotu dla akcji sprzedawanej
            assign(paste0('Short', '_', 'all'),
                   cbind(get(paste0('Short', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcj� do porfela Short
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcj� do porfela Momentum
        }
    }
    Momentum<-rbind(Momentum,get(paste0('Momentum', '_', 'all')))
    LongP<-rbind(LongP,get(paste0('Long', '_', 'all')))
    ShortP<-rbind(ShortP,get(paste0('Short', '_', 'all')))
}