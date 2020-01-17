#data = read.csv("ItalyMonth.csv")
data = read.csv("tabella.csv")
ts = ts(data, frequency = 12, start = 1990)
plot(ts)
acf(ts)

#Analisi Decomposizione
ts.d = decompose(ts)
plot(ts.d)
plot(ts.d$seasonal)
lines(ts.d$trend, col = "blue")
lines(ts.d$random, col = "red")
par(xpd=TRUE)
legend_order <- matrix(1:3,ncol=3,byrow = TRUE)
legend(1990, 4e+07, legend=c("seasonal", "trend", "random")[legend_order],lty=1,col=c("black", "blue", "red")[legend_order],cex=1, bty="n", ncol=3)
par(xpd=FALSE)

par(bg="black")
m_ts=matrix(ts,12,30)
ts.plot(m_ts, col=heat.colors(30))
lines(rowMeans(m_ts),lwd=3,col="white")
par(bg="white")
 
ts.sd=vector("numeric",12)
for(i in 1:12){
	ts.sd[i]=sd(m_ts[i,])
} 
ts.m=rowMeans(m_ts)
plot(ts.m,pch=20,type="b",ylim=range(c(ts.m-3*ts.sd,ts.m+3*ts.sd)))
arrows(1:12,ts.m-ts.sd,1:12,ts.m+ts.sd,length=0.02,angle=90,code=3,col="green3")
points(ts.m+ts.sd,type="b",pch=20,col="gray")
points(ts.m-ts.sd,type="b",pch=20,col="gray")
lines(ts.d$figure+mean(ts),col="red") 

#HoltWinters
#Entrambi sono fedeli alla serie e non catturano il trend.
ts.se=HoltWinters(ts,beta=F,gamma=F)
plot(ts.se)
ts.set=HoltWinters(ts,gamma=F)
plot(ts.set)

#Proviamo a catturare il trend
x=1:24
coefficients(lm(ts[1:24]~x))
plot(HoltWinters(ts,alpha=0.07,gamma=F,l.start=12181677,b.start=161082))

ts.hw = HoltWinters(ts)
plot(ts.hw$fitted)
ts.stl = stl(ts(data[1:357,1], frequency=12, start = 1990), "periodic")
ts.plot(ts.stl$time.series[,2],ts.hw$fitted[,2],col=c("black","red"))
legend(1990, 1.8e+07, legend=c("level", "trend"),lty=1,col=c("red", "black"),cex=1)
ts.plot(ts.stl$time.series[,1],ts.hw$fitted[,4],col=c("black","red"))
legend(1990, 3e+07, legend=c("season stl", "season hw"),lty=1,col=c("black", "red"),cex=1)

plot(ts.hw,predict(ts.hw,12),main="Previsione a 12 mesi")
plot(ts.hw,predict(ts.hw,24),main="Previsione a 24 mesi")

ts.hw.r=residuals(ts.hw)
plot(ts.hw,predict(ts.hw,12))
lines(predict(ts.hw,12)+quantile(ts.hw.r,0.05),col="green3")
lines(predict(ts.hw,12)+quantile(ts.hw.r,0.95),col="green3")

plot(ts.hw,predict(ts.hw,12))
lines(predict(ts.hw,12)+qnorm(0.05,mean(ts.hw.r),sd(ts.hw.r)),col="blue")
lines(predict(ts.hw,12)+qnorm(0.95,mean(ts.hw.r),sd(ts.hw.r)),col="blue")

plot(ts.hw,predict(ts.hw,12))
lines(predict(ts.hw,12)+qnorm(0.05,mean(ts.hw.r),sd(ts.hw.r)),col="blue")
lines(predict(ts.hw,12)+qnorm(0.95,mean(ts.hw.r),sd(ts.hw.r)),col="blue")
lines(predict(ts.hw,12)+quantile(ts.hw.r,0.05),col="green3")
lines(predict(ts.hw,12)+quantile(ts.hw.r,0.95),col="green3")

#Analisi dei redisui
plot(ts.hw.r,type="p",pch=20)
plot(ts.hw$fitted[,1],ts.hw.r,pch=20)

#VarSpiegata
start(ts)
end(ts)
start(ts.hw.r)
end(ts.hw.r)
1-var(ts.hw.r)/var(window(ts,c(1991,1)))

acf(ts.hw.r)

hist(ts.hw.r,20,freq=F)
lines(density(ts.hw.r))
lines(sort(ts.hw.r),dnorm(sort(ts.hw.r),mean(ts.hw.r),sd(ts.hw.r)),col="red")
qqnorm(ts.hw.r)
qqline(ts.hw.r)
shapiro.test(ts.hw.r)

#Autovalidazione
l=length(ts)
n=12
pred=rep(0,n)
for(i in 1:n){
t<-ts(ts[1:(l-i)],frequency=12)
ts.hw<-HoltWinters(t)
pred[n-i+1]=predict(ts.hw,1)
}
real=ts[(l-n+1):l]
sqrt(var(pred-real)/var(real))

#Esaminiamo le differenze graficamente
ts.plot(real)
lines(pred,col="blue")
legend(1, 4e+07, legend=c("Valori Reali", "Valori Predetti"),lty=1,col=c("black", "blue"),cex=1)


#Esaminiamo errore su un singolo periodo
ts.w1=window(ts,end=c(2018,12))
ts.w2=window(ts,start = c(2019,1))
ts.hw=HoltWinters(ts.w1)
ts.hw.p=predict(ts.hw,12)
sqrt(var(ts.hw.p-ts.w2)/var(ts.w2))

#visualizziamo
plot(ts.w2)
lines(ts.hw.p,col="blue")
#A causa della mancanza dei valori negli ultimi 3 mesi del 2019 la serie predetta è stata
#traslata di 3 mesi, facendola partire dal 2018/10 invece che dal 2019/1
lines(ts(pred,frequency=12,start=c(2018,10)),col="red")

