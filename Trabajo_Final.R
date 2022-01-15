# Considere un bono que expira dentro de 26 años, que paga un cupón semestral 
# y cuya tasa cupón es de 8 % anual y tiene un rendimiento del 6%  anual 

# Calcula la duración con periodos semestrales

r<-0.03  # Rendimiento
c<-0.04  # Cupón
n<-52 #Número de periodos

b1<-(1+r)/r
b2<-n*(c-r)
b3<-(1+r)**n
Dm<-b1+(1+r+b2)/(c*(b3-1)+r)
Dm

Dm<- -(1+r)/r+(1+r+(n*(c-r)))/(c*(((1+r)**n) -1)+r)
Dm

# Calcular la Convexidad
b0<-(1+r);b0
b1<-(1+r+r*n)/(1+r);b1
b2<-n*(n+1)*b0*r**2*(r-c);b2
b3<-r**2*b0*2;b3
b5<-b0**n;b5
b4<-c*(b5 - 1);b4

C<-(2*c*b0**2*(b5 -b1)+b2)/(b3*(b4+r))
C

# Se tiene un bono con duración de 12.76 a un precio de $101.5, la tasa de interés
# vigente es de 8% anual  y la volatilidad en el rendimiento de tasas es de 2%,
# calcula el VaR con un nivel de confianza del 99%
         

VaR<- -2.33*101.5*0.08*12.76*0.02*sqrt(1);VaR*100





rm(list = ls())
library(readr)
library(ggplot2)
library(plotly)
library(PortfolioAnalytics)
library(moments)
library(fPortfolio)
library(quantmod)
# Cargamos los datos de los precios hisóricos de cierre 
P1<-read_csv("C:/Users/Admin/Desktop/Riesgo_Maestría/Portafolio1.csv")
# Graficamos el comportamiento de los precios históricos
p1<-data.frame(P1)

historicos<-ggplot(p1, aes(x=Fecha)) +
        geom_line(aes(y=AMD), color = 'aquamarine', size = 1) + 
        geom_line(aes(y=BABA), color = 'azure4', size = 1) +
        geom_line(aes(y=DPZ), color = 'cornflowerblue', size = 1) +
        geom_line(aes(y=FB), color = 'brown1', size = 1) +
        geom_line(aes(y=SAM), color = 'orange', size = 1) +
        labs(x='Fecha', y='Precio (MXN)') +
        ggtitle('Portafolio de Inversión')
        
ggplotly(historicos)



#Alsea


#Alsea
RA<-diff(log(as.numeric(p1$Alsea)));RA
RA<-na.omit(RA)
# par(mfrow=c(1,3))
plot(p1$Fecha[-1],RA, type='l', main='Rendimiento Alsea',
     xlab='Fecha', ylab='Rendimiento')
abline(h=0, col='red')
hist(RA, main='Histrograma Alsea', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='green')
lines(density(RA), col='red')
round(skewness(RA),2)
round(kurtosis(RA),2)
jarque.test(as.vector(RA))
vol_RA<-sd(RA);vol_RA
risk_RA<-mean(RA);risk_RA

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
RB<-diff(log(as.numeric(p1$Bimbo)));RB
RB<-na.omit(RB)
plot(p1$Fecha[-1],RB, type='l', main='Rendimiento Bimbo',
     xlab='Fecha', ylab='Rendimiento')
abline(h=0, col='red')
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
RC<-diff(log(as.numeric(p1$Cemex)));RC
RC<-na.omit(RC)
plot(p1$Fecha[-1],RC, type='l', main='Rendimiento Cemex',
     xlab='Fecha', ylab='Rendimiento')
abline(h=0, col='red')
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

RCC<-diff(log(as.numeric(p1$Coca_cola)));RCC
RCC<-na.omit(RCC)
plot(p1$Fecha[-1],RCC, type='l', main='Rendimiento Coca-Cola',
     xlab='Fecha', ylab='Rendimiento')
abline(h=0, col='red')
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
RW<-diff(log(as.numeric(p1$Walmart)));RW
RW<-na.omit(RW)
plot(p1$Fecha[-1],RW, type='l', main='Rendimiento Walmart',
     xlab='Fecha', ylab='Rendimiento')
abline(h=0, col='red')
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


#Calculo del Valor en Riesgo (VaR) de cada una de las acciones
#con nivel de confianza del 99 % para un horizonte de de tiempo
#de un día
precios<-c(p1$Alsea[1], p1$Bimbo[1],p1$Cemex[1],
           p1$Coca_cola[1], p1$Walmart[1])
f99<-2.33
t<-1
#Alsea
VaR_Alsea<-f99*p1$Alsea[1]*vol_RA*sqrt(t/252);VaR_Alsea*100
#Bimbo
VaR_Bimbo<-f99*p1$Bimbo[1]*vol_RB*sqrt(t/252);VaR_Bimbo*100
#Cemex
VaR_Cemex<-f99*p1$Cemex[1]*vol_RC*sqrt(t/252);VaR_Cemex*100
#Coca Cola
VaR_Coca<-f99*p1$Coca_cola[1]*vol_RCC*sqrt(t/252);VaR_Coca*100
#Walmart
VaR_Walmart<-f99*p1$Walmart[1]*vol_RW*sqrt(t/252);VaR_Walmart*100

#Ahora vamos a calcular el VaR de las siguientes combinaciones
# compuestas por dos acciones de las 5 que tenemos, es decir
# 12,13,14,15,23,24,25,34,35,45

#Para el caso del portafolio de las acciones 1 y 2 se tiene 
#Primero calculamos la distribución de pesos
w1<-seq(0, 1, by=0.001);w1
w2<-1-w1;w2

cor12<-cor(p1$Alsea,p1$Bimbo);cor12

r12<-w1*risk_RA+w2*risk_RB;12
v12<-w1**2*vol_RA**2+w2**2*vol_RB**2+2*w1*w2*cor12*vol_RA*vol_RB;v12
v12<-sqrt(v12);v12
plot(v12,r12)

df<-data.frame(rp,v12, ncol=2)

g<-ggplot(df, aes(x=v12, y=rp))+geom_point()
g<-ggplotly(g);g














