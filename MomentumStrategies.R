#MOMENTUM
#
#Strategia MOM12-2, 12 miesiêcy "rank", 1 tydzieñ "skip", 1 miesi¹c "invest"
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
#Tworzymy tabelê ze skumulowanymi stopami zwrotu dla ka¿dego ostatniego dnia miesi¹ca, oraz 7 poprzedzaj¹cego go dnia 
tablePeriods<-sumData[!is.na(sumData$newPeriod),]
tableSkip<-sumData[!is.na(sumData$Skip),]
sumData[is.na(sumData)] <- 0

#Obliczamy wartoœæ Momentum dla ka¿dego okresu
p<-12
momData<-logData[0,]
for(t in 1:length(tableSkip[,1])){
    if(t>=(p+1)){
        for(s in 1:length(logData)){
            momData[t-p,s]<-tableSkip[t-1,s]-tablePeriods[t-p,s] #Dla ka¿dego 7 dnia poprzedzaj¹cego ostatni dzieñ miesi¹ca odejmujemy wartoœæ skumulowanej stopy zwrotu sprzed 12 miesiêcy
        }
    }
}

#Wybieramy sk³adniki portfela
r<-momData
ro<-round(length(r)/10)
for(t in 1:length(momData[,1])){
    r[t,]<-rank(momData[t,])
}
r[r<=ro]<--1 #Nadajemy wartoœæ -1 dla spó³ek z najgorszym momentum
r[r>(length(logData[1,])-ro)]<-1 #Nadajemy wartoœæ 1 dla spó³ek z najlepszym momentum
r[r!=1 & r!=-1]<-0 #Nadajemy wartoœæ 0 dla pozosta³ych spó³ek
momComp<-r

#Tworzymy portfele Momentum
Momentum<-data.frame()
LongP<-data.frame()
ShortP<-data.frame()

for(t in seq(from=1,to=length(momComp[,1]),by=1)){
    assign(paste0('Momentum', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Momentum dla ka¿dego miesi¹ca
    assign(paste0('Long', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Long dla ka¿dego miesi¹ca
    assign(paste0('Short', '_', 'all'), 
           data.frame(row=row.names(logData[sumData$Period%in% c(t+13),]))) #Tworzymy portfel Short dla ka¿dego miesi¹ca
    for (s in 1:length(momData)){
        if(momComp[t,s]==1){
            assign(paste0('Long'), 
                   logData[sumData$Period %in% c(t+13),s]) #Tworzymy wektor stop zwrotu dla akcji kupowanej
            assign(paste0('Long', '_', 'all'),
                   cbind(get(paste0('Long', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcjê do porfela Long
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Long')))) #Dodajemy akcjê do porfela Momentum
            
        }else if (momComp[t,s]==-1){
            assign(paste0('Short'), 
                   -logData[sumData$Period %in% c(t+13),s]) #Tworzymy wektor stop zwrotu dla akcji sprzedawanej
            assign(paste0('Short', '_', 'all'),
                   cbind(get(paste0('Short', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcjê do porfela Short
            assign(paste0('Momentum', '_', 'all'),
                   cbind(get(paste0('Momentum', '_', 'all')),
                         x=get(paste0('Short')))) #Dodajemy akcjê do porfela Momentum
        }
    }
    Momentum<-rbind(Momentum,get(paste0('Momentum', '_', 'all')))
    LongP<-rbind(LongP,get(paste0('Long', '_', 'all')))
    ShortP<-rbind(ShortP,get(paste0('Short', '_', 'all')))
}