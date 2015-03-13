###Rysujemy wykresy dla wszystkich strategii
Tperformance<-as.data.frame(Momentum[,1])
performance<-rowMeans(LongP[,2:34])

perf<-data.frame(performance)
for (t in 1:length(performance))
{
    perf[t,1]<-sum(performance[1:t])
}
Tperformance[,2]<-perf
colnames(Tperformance)[2]<-"Long"
###
performance<-rowMeans(ShortP[,2:33])

perf<-data.frame(performance)
for (t in 1:length(performance))
{
    perf[t,1]<-sum(performance[1:t])
}
Tperformance[,3]<-perf
colnames(Tperformance)[3]<-"Short"
###
performance<-rowMeans(Momentum[,2:66])

perf<-data.frame(performance)
for (t in 1:length(performance))
{
    perf[t,1]<-sum(performance[1:t])
}
Tperformance[,4]<-perf
colnames(Tperformance)[4]<-"Momentum"
###
Tperformance$sumData<-rowMeans(
    sumData[(length(Data[,1])-length(Tperformance[,1])):
                (length(Data[,1])-1),1:652])
###
yrange<-range(c(Tperformance[,2],Tperformance[,3],Tperformance[,4],Tperformance$sumData))

windows()
plot(Tperformance[,2], type="l",col="red",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(Tperformance[,3], type="l",col="blue",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(Tperformance[,4], type="l",col="green",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(rep(0,length(Tperformance[,1])), type="l",col="black",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(rep(1,length(Tperformance[,1])), type="l",col="black",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(rep(-1,length(Tperformance[,1])), type="l",col="black",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")
par(new=TRUE)
plot(Tperformance$sumData, type="l",col="yellow",ylim=yrange,ylab = "Return",xlab="Time from 1990-02-01 to 2014-12-31")

legend("topleft",lty=c(1,1,1,1),col=c("red","blue","green","yellow"),legend=c("Long_only","Short_only","Momentum","Market"))

dev.copy(png,file="PERFORMANCEplot.png")
dev.off()
dev.off()