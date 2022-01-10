hdata<-read.csv("C:/Users/cheru/Downloads/AAPL.csv")
hdat<-hdata[,16]
plot.ts(hdat,lwd=3,col=c(4),
        main="Time plot AAPL index",
        ylab="",xlab="days")
optdata<-read.csv("C:/Users/cheru/OneDrive/Documents/AAPL2.csv")
pmkt<-0.5*(optdata[,5]+optdata[,6])
cmkt<-0.5*(optdata[,22]+optdata[,23])
strike<-optdata[,3]
plot(strike,cmkt,lwd=3,col=c(2),ylab="Payoff",
     main="Observed European option price ", xlab="Strike
Price")
points(strike,pmkt,cex=0.8,col=c(4))
abline(v=121,lwd=2,col=c(1))

fit1<-lm(cmkt~pmkt+strike);fit1
summary(fit1)
eror<-fit1$residuals
hist(eror,probability=T)
shapiro.test(eror)
shapiro.test(rnorm(1000))
qqnorm(eror)
qqline(eror)