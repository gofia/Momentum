#Rysujemy wykres SHARPEplot
#Dla uproszczenia zak³adaj¹c okres=30 dni
p<-300

SharpeData<-c(1:(length(momData[,1])*30/p))
SharpeMomentum<-c(1:(length(momData[,1])*30/p))

for(t in 1:(length(momData[,1])*30/p)){
    if(t>1)
    {
        SharpeData[t]<-mean(Tperformance$sumData[((t-1)*p):(t*p)])/
            sd(Tperformance$sumData[((t-1)*p):(t*p)])
        SharpeMomentum[t]<-mean(Tperformance$Momentum[((t-1)*p):(t*p)])/
            sd(Tperformance$Momentum[((t-1)*p):(t*p)])
    }
}

yrange<-range(c(SharpeData, SharpeMomentum))
windows()
plot(ylim=yrange,SharpeData,type="l",col="red",xlab="Period",ylab="Sharpe ratio", main="Sharpe Ratio - period=300d")
par(new=TRUE)
plot(ylim=yrange,SharpeMomentum,type="l",col="green",xlab="Period",ylab="Sharpe ratio", main="Sharpe Ratio - period=300d")
legend("topright",lty=c(1,1),col=c("green","red"),legend=c("Momentum_Sharpe","Market_Sharpe"))

dev.copy(png,file="SHARPEplot300d.png")
dev.off()
dev.off()

