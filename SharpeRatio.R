#Rysujemy wykres SHARPEplot
#Dla uproszczenia zak³adaj¹c okres=30 dni
SharpeData<-momData[1:300,1]
SharpeMomentum<-momData[1:300,1]
for(t in 1:300){
    SharpeData[t]<-mean(Tperformance$sumData[(t*1):(t*30)])/
        sd(Tperformance$sumData[(t*1):(t*30)])
    SharpeMomentum[t]<-mean(Tperformance$Momentum[(t*1):(t*30)])/
        sd(Tperformance$Momentum[(t*1):(t*30)])
}
yrange<-range(c(SharpeData, SharpeMomentum))
windows()
plot(ylim=yrange,SharpeData,type="l",col="red",xlab="Period",ylab="Sharpe ratio")
par(new=TRUE)
plot(ylim=yrange,SharpeMomentum,type="l",col="green",xlab="Period",ylab="Sharpe ratio")
legend("topright",lty=c(1,1),col=c("green","red"),legend=c("Momentum_Sharpe","Market_Sharpe"))

dev.copy(png,file="SHARPEplot.png")
dev.off()
dev.off()