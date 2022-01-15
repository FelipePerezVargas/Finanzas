################################################################
####                                                        ####  
####                     VOLATILIDAD                        ####
####                                                        ####
################################################################
library(ggplot2)
library(plotly)
library(tseries)
library(quantmod)
library(PortfolioAnalytics)
library(moments)
library(fPortfolio)
library(xts)


#Cargamos los datos 

MXX<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/MXX.csv')
Alsea<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/ALSEA.MX.csv')
Bimbo<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/BIMBOA.MX.csv')
Cemex<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/CEMEXCPO.MX.csv')
Walmart<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/KOFUBL.MX.csv')
Coca<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/WMT.MX.csv')

portafolio<-matrix(c(MXX$Adj.Close, Alsea$Adj.Close, Bimbo$Adj.Close,
                     Cemex$Adj.Close, Coca$Adj.Close, Walmart$Adj.Close),ncol=6)
colnames(portafolio)<-c('MXX','Alsea','Bimbo','Cemex','Coca','Walmart')
portafolio<-as.data.frame(portafolio)



fecha<-as.Date(MXX$Date)
par(mfrow=c(2,3))
for (i in 1:length(portafolio)) {
  plot(fecha, portafolio[,i], type='l',
       col=i, xlab='Fecha', ylab='Precio MXN',
       main=print(paste('Histórico', colnames(portafolio[i]))))  
}

#MXX
RM<-diff(log(as.numeric(portafolio$MXX)))
RM<-na.omit(RM)
hist(RM, main='Histrograma IPC', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RM), col='red')
RMS<-round(skewness(RM),2)
RMK<-round(kurtosis(RM),2)
RMN<-jarque.test(as.vector(RM))
vol_RM<-sd(RM)
risk_RM<-mean(RM)

VaR_RM<-VaR(RM, p=0.95);VaR_RM
CVaR_RM<-CVaR(RM, p=0.95);CVaR_RM
x=seq(min(RM),max(RM),length=100)
y=dnorm(x,mean=mean(RM),sd=sd(RM))
plot(x,y, type='l')
x=seq(CVaR_RM,VaR_RM,length=100)
y=dnorm(x,mean=mean(RM),sd=sd(RM))
lines(x,y, col='blue')

polygon(c(CVaR_RM,x,VaR_RM),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')


#Alsea
RA<-diff(log(as.numeric(portafolio$Alsea)));RA
RA<-na.omit(RA)
hist(RA, main='Histrograma Alsea', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RA), col='red')
round(skewness(RA),2)
round(kurtosis(RA),2)
jarque.test(as.vector(RA))
vol_RA<-sd(RA)
risk_RA<-mean(RA)

VaR_RA<-VaR(RA, p=0.95);VaR_RM
CVaR_RA<-CVaR(RM, p=0.95);CVaR_RM
x=seq(min(RA),max(RA),length=100)
y=dnorm(x,mean=mean(RA),sd=sd(RA))
plot(x,y, type='l')
x=seq(CVaR_RA,VaR_RA,length=100)
y=dnorm(x,mean=mean(RA),sd=sd(RA))
lines(x,y, col='blue')
polygon(c(CVaR_RA,x,VaR_RA),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')


# Bimbo 
RB<-diff(log(as.numeric(portafolio$Bimbo)));RB
RB<-na.omit(RB)
hist(RB, main='Histrograma Bimbo', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RB), col='red')
round(skewness(RB),2)
round(kurtosis(RB),2)
jarque.test(as.vector(RB))
vol_RB<-sd(RB)
risk_RB<-mean(RB)

VaR_RB<-VaR(RB, p=0.95);VaR_RB
CVaR_RB<-CVaR(RB, p=0.95);CVaR_RB
x=seq(min(RB),max(RB),length=100)
y=dnorm(x,mean=mean(RB),sd=sd(RB))
plot(x,y, type='l')
x=seq(CVaR_RB,VaR_RB,length=100)
y=dnorm(x,mean=mean(RB),sd=sd(RB))
lines(x,y, col='blue')
polygon(c(CVaR_RB,x,VaR_RB),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')

#Cemex
RC<-diff(log(as.numeric(portafolio$Cemex)));RC
RC<-na.omit(RC)
hist(RC, main='Histrograma Cemex', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RC), col='red')
round(skewness(RC),2)
round(kurtosis(RC),2)
jarque.test(as.vector(RC))
vol_RC<-sd(RC)
risk_RC<-mean(RC)

VaR_RC<-VaR(RC, p=0.95);VaR_RC
CVaR_RC<-CVaR(RC, p=0.95);CVaR_RC
x=seq(min(RC),max(RC),length=100)
y=dnorm(x,mean=mean(RC),sd=sd(RC))
plot(x,y, type='l')
x=seq(CVaR_RC,VaR_RC,length=100)
y=dnorm(x,mean=mean(RC),sd=sd(RC))
lines(x,y, col='blue')
polygon(c(CVaR_RC,x,VaR_RC),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')
#Coca-cola

RCC<-diff(log(as.numeric(portafolio$Coca)));RCC
RCC<-na.omit(RCC)
hist(RCC, main='Histrograma Cemex', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RCC), col='red')
round(skewness(RCC),2)
round(kurtosis(RCC),2)
jarque.test(as.vector(RCC))
vol_RCC<-sd(RCC)
risk_RCC<-mean(RCC)

VaR_RCC<-VaR(RCC, p=0.95);VaR_RCC
CVaR_RCC<-CVaR(RCC, p=0.95);CVaR_RCC
x=seq(min(RCC),max(RCC),length=100)
y=dnorm(x,mean=mean(RCC),sd=sd(RCC))
plot(x,y, type='l')
x=seq(CVaR_RCC,VaR_RCC,length=100)
y=dnorm(x,mean=mean(RCC),sd=sd(RCC))
lines(x,y, col='blue')
polygon(c(CVaR_RCC,x,VaR_RCC),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')


#Walmart
RW<-diff(log(as.numeric(portafolio$Walmart)));RW
RW<-na.omit(RW)
hist(RW, main='Histrograma Cemex', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RW), col='red')
round(skewness(RW),2)
round(kurtosis(RW),2)
jarque.test(as.vector(RW))
vol_RW<-sd(RW)
risk_RW<-mean(RW)

VaR_RW<-VaR(RW, p=0.95);VaR_RW
CVaR_RW<-CVaR(RW, p=0.95);CVaR_RW
x=seq(min(RW),max(RW),length=100)
y=dnorm(x,mean=mean(RW),sd=sd(RW))
plot(x,y, type='l')
x=seq(CVaR_RW,VaR_RW,length=100)
y=dnorm(x,mean=mean(RW),sd=sd(RW))
lines(x,y, col='blue')
polygon(c(CVaR_RW,x,VaR_RW),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')



#Cambiamos los nombres de los ancabezados o columnas

portafolio<-matrix(c(MXX$Date,MXX$Adj.Close, Alsea$Adj.Close, Bimbo$Adj.Close,
                     Cemex$Adj.Close, Walmart$Adj.Close, Coca$Adj.Close),ncol=6)
colnames(portafolio)<-c('Fecha','IPC','Alsea','Bimbo','Cemex','Coca','Walmart')
portafolio<-as.data.frame(portafolio)
portafolio<-na.omit(portafolio)

w<-c(0.2,0.2,0.2,0.2,0.1,0.1)
VaR(portafolio, p=0.95, weights = w)

Retornos<-data.frame(RM,RA,RB,RC,RCC,RW)
Retornos<-na.omit(Retornos)
colnames(Retornos)<-c('Alsea','Bimbo','Cemex','Coca','Walmart')


Retornos1<-data.frame(RM,RA,RB,RC,RCC,RW)
Retornos1<-na.omit(Retornos1)

VaR(Retornos, p=0.95)


cor(Retornos1)

cov(Retornos1)




for (i in 2:length(Retornos)) {
  plot(Retornos[,1], Retornos[,i], type='l',
       col=i, xlab='Fecha', ylab='Retornos Diarios')  
}
summary(Retornos)


Riesgo<-vector()
Volatilidad<-vector()
for (i in 2:7) {
 Riesgo[i]<-  sd(Retornos[,i])
 Volatilidad[i]<-mean(Retornos[,i])
  plot(Riesgo[-1], Volatilidad[-1], col=1:6, xlab='Riesgo', ylab='Volatilidad', 
      main='Gráfica de Riesgo-Rendimiento Portafolio', xlim=c(0,0.05))
  labels <- colnames(portafolio)
  text(Riesgo[-1], Volatilidad[-1], labels, cex = 0.7, pos = 1, col=1:6)
  abline(v=0, h=0, col='gray', lty=3)
 }







# Calculamo el riesgo y el rendimiento del portafolio 





# Una vez instalado vamos a obtener datos históricos de alguna empresa
# Para esto usamos el comando getSymbols ####

names(MXX)<-c('Fecha', 'Apertura','Cierre',
               'Alto','Bajo', 'Ajustado','Volumen')

#Ahora podemos graficar la serie de tiempo
mxx<-ggplot(data = MXX, aes(x = MXX$Fecha, y = MXX$Cierre))+
  geom_line(color = "#00AFBB", size = 0.5, na.rm=TRUE)+
  scale_x_date(date_labels = "%b/%y")+
  ggtitle("Evolución del IPC \n 5 años") +
  xlab("Fecha") + ylab("Precio (MXN)")+
  stat_smooth(colour="green")+
  theme_economist()
#Lo convierto a un grafico interactivo 
ggplotly(mxx)


# Portafolio<-data.frame(MXX$Fecha, )



plot(MXX$Ajustado, xlab='Fecha', ylab='Precio (MXN)',
     main='Evolución del IPC \n 5 Años', type='l')
x<-log(MXX$Ajustado)
plot(x, type='l')
 # Calculamos los retornos
plot(diff(x), type='l')
na.remove(diff(x))

acf(na.remove(diff(x)), type ="covariance", plot = TRUE)

acf(na.remove(diff(x)), type ="correlation", plot = TRUE)





# grid.arrange(AirTempDaily, AirTempMonthly, ncol=1)

# p+scale_x_date(date_labels = "%b")
# p+scale_x_date(date_labels = "%Y %b %d")
# p+scale_x_date(date_labels = "%W")
# p+scale_x_date(date_labels = "%m-%Y")








ss <- subset(economics, date > as.Date("2006-1-1"))
ggplot(data = ss, aes(x = date, y = pop)) + 
  geom_line(color = "#FC4E07", size = 2)


plot(x, mxx.ts[,2], type='l', xlab='Fecha')
# Estabilización de la varianza
x = log(mxx.ts)
plot(x)

# Eliminación de la tendencia
dif1.x = diff(x)
plot(dif1.x)

#Eliminación de la estacionalidad
dif12.dif1.x = diff(dif1.x, lag=12)
dif12.dif1.x<-na.remove(dif12.dif1.x)
plot(dif12.dif1.x)

which(is.na(dif12.dif1.x))
serie_estacionaria<-dif12.dif1.x
serie_estacionaria<- na.remove(serie_estacionaria)

autocovarianza<-acf(serie_estacionaria, type ="covariance"
                    , plot = FALSE)
plot(autocovarianza)

autocorrelacion<-acf(serie_estacionaria, 
                     type ="correlation", plot = FALSE)
autocorrelacion
acf <-acf(serie_estacionaria, lag.max = 12)
pacf <- pacf(serie_estacionaria, lag.max = 12)







