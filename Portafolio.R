library(quantmod)
library(PortfolioAnalytics)
library(moments)
library(ggplot2)
library(plotly)
# Descargamos las acciones de interés
# Cargamos la base de datos

portafolio1<-read.csv('C:/Users/Admin/Desktop/Riesgo_Maestría/Portafolio1.csv')

# Calculamos los retornos  ####

#Alsea
RA<-diff(log(portafolio1$Alsea));RA
# Bimbo 
RB<-diff(log(portafolio1$Bimbo));RB
#Cemex
RC<-diff(log(portafolio1$Cemex));RC
#Coca-cola
RCO<-diff(log(portafolio1$Coca.cola));RCO
#Walmart
RW<-diff(log(portafolio1$Walmart));RW

# Calculamos el riesgo y el rendimiento #####

# Alsea####
rA<-mean(RA);rA
reA<-sd(RA);reA
# Bimbo ####
rB<-mean(RB);rB
reB<-sd(RB);reB|
# Cemex
rC<-mean(RC);rC
reC<-sd(RC);reC
# Coca-cola
rCO<-mean(RCO);rCO
reCO<-sd(RCO);reCO
# Walmart
rW<-mean(RW);rW
reW<-sd(RW);reW

Riesgo<-c(rA,rB, rCO, rW)*100
Rendimiento<-c(reA,reB, reCO, reW)*100

dfr<-data.frame(Riesgo, Rendimiento)

gg<-ggplot(data=dfr, aes(x=dfr$Riesgo, y=dfr$Rendimiento))+
    geom_point()
ggplotly(gg)


plot(Riesgo,Rendimiento, col='red', pch=16, main='Grafica Riesgo-Rendimiento')

#Prueba de Asimetría
S<-(sum((rvec-mean(rvec))^{3}))/(length(rvec)*sd(rvec)^{3});S
#Prueba de Kurtosis en busca peso de las colas
K<-(sum((rvec-mean(rvec))^{4}))/(length(rvec)*sd(rvec)^{4});K
#Prueba de normalidad asimetría=0 y Kurtosis=3 en los errores
JB<-(length(rvec)/6)*(S^{2}+(K-3)^{2}/4);JB

# Alsea
hist(RA)
round(skewness(RA),2)
round(kurtosis(RA),2)
jarque.test(RA)

# Bimbo
hist(RB)
round(skewness(RB),2)
round(kurtosis(RB),2)
jarque.test(RB)

# Cemex
hist(RC)
round(skewness(RC),2)
round(kurtosis(RC),2)
jarque.test(RC)
# Coca-cola
hist(RCO)
round(skewness(RCO),2)
round(kurtosis(RCO),2)
jarque.test(RCO)
# Coca-cola
hist(RW)
round(skewness(RW),2)
round(kurtosis(rvec),2)
jarque.test(rvec)

#Realizamos la matriz para calcular las correlaciones y covarianzas

MRE<-matrix(c(RA,RB,RC,RCO,RW), ncol=5);MRE
cov(MRE)
cor(MRE)



#Pesos del portafolio 

w<-c(0.2,0.2,0.2,0.2,0.2)

REP<-w%*%Rendimiento;REP
RIP<-sqrt(w%*%(cov(MRE)%*%w));RIP






# Alsea
# Bimbo
# Cemex
# Coca-cola
# Walmart