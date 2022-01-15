rm(list = ls())
library(readr)
library(ggplot2)
library(plotly)
library(PortfolioAnalytics)
library(fPortfolio)
library(quantmod)
library(moments)

P1<-read_csv("C:/Users/Admin/Desktop/Riesgo_Maestría/Portafolio1.csv")

# Una vez que cargamos los datos realizamos un gráfico donde se puedan visualizar la evolución de los precios históricos de los activos seleccionados

# Graficamos el comportamiento de los precios históricos, pero primero 
# convertimos en un data frame para poder usar el paquete ggplot2 y plotly
p1<-data.frame(P1)
#Gráfico de acciones
historicos<-ggplot(p1, aes(x=Fecha)) +
  geom_line(aes(y=AMD), color = 'aquamarine', size = 1) + 
  geom_line(aes(y=BABA), color = 'azure4', size = 1) +
  geom_line(aes(y=DPZ), color = 'cornflowerblue', size = 1) +
  geom_line(aes(y=FB), color = 'brown1', size = 1) +
  geom_line(aes(y=SAM), color = 'orange', size = 1) +
  labs(x='Fecha', y='Precio (MXN)') +
  ggtitle('Portafolio de Inversión')
#Gráfico interactivo de la evolución de los precios históricos        
ggplotly(historicos)

# Ahora vamos a calcular los rendimientos de cada uno de los activos

# AMD
R1<-diff(log(as.numeric(p1$AMD))) #Calculamos los retornos de cada uno de los activos
R1<-na.omit(R1) # Quitar los objetos sin  valores asignados
# BABA
R2<-diff(log(as.numeric(p1$BABA)))
R2<-na.omit(R2)
# DPZ
R3<-diff(log(as.numeric(p1$DPZ)))
R3<-na.omit(R3)
# FB
R4<-diff(log(as.numeric(p1$FB)))
R4<-na.omit(R4)
# SAM
R5<-diff(log(as.numeric(p1$SAM)))
R5<-na.omit(R5)
# MXX
R6<-diff(log(as.numeric(p1$MXX)))
R6<-na.omit(R6)

ren<-data.frame(p1$Fecha[-1], R1, R2,R3,R4,R5)
# Gráfico de los retornos de los activos
#Gráfico de acciones
rhistoricos<-ggplot(ren, aes(x=p1$Fecha[-1])) +
  geom_line(aes(y=R1), color = 'aquamarine', size = 0.5) + 
  geom_line(aes(y=R2), color = 'azure4', size = 0.5) +
  geom_line(aes(y=R3), color = 'cornflowerblue', size = 0.5) +
  geom_line(aes(y=R4), color = 'brown1', size = 0.5) +
  geom_line(aes(y=R5), color = 'orange', size = 0.5) +
  labs(x='Fecha', y='Rendimientos') +
  ggtitle('Retornos del Portafolio de Inversión')
#Gráfico interactivo de la evolución de los precios históricos        
ggplotly(rhistoricos)

# Ahora calculamos la Volatilidad de cada uno de los activos, calculando la desviación estándar de cada uno de los activos

# AMD
vol_R1<-sd(R1)
# BABA
vol_R2<-sd(R2)
# DPZ
vol_R3<-sd(R3)
# FB
vol_R4<-sd(R4)
# SAM
vol_R5<-sd(R5)


# Ahora calculamos el rendimiento de cada uno de los activos 

# AMD
ren_R1<-mean(R1)
# BABA
ren_R2<-mean(R2)
# DPZ
ren_R3<-mean(R3)
# FB
ren_R4<-mean(R4)
#SAM
ren_R5<-mean(R5)

# Ahora realizamos la gráfica de Riesgo-Rendimiento para saber cual es más riesgoso.

Rendimiento<-c(ren_R1, ren_R2, ren_R3, ren_R4, ren_R5)
Volatilidad<-c(vol_R1, vol_R2, vol_R3, vol_R4, vol_R5)
# 
# Creamos una tabla para que nos imprima los resultados
RV<-matrix(c(vol_R1, ren_R1,
             vol_R2, ren_R2,
             vol_R3, ren_R3,
             vol_R4, ren_R4,
             vol_R5, ren_R5), ncol=2, nrow =5, byrow = TRUE)
colnames(RV)<-c('Volatilidad', 'Rendimiento')
rownames(RV)<-c('AMD', 'BABA', 'DPZ', 'FB', 'SAM');RV
TRR<-data.frame(Stocks=c('AMD', 'BABA', 'DPZ', 'FB', 'SAM'),Volatilidad,Rendimiento)
# Graficamos la volatilidad vs el rendimiento 
RR<-ggplot(data=TRR, aes(x=Volatilidad, y=Rendimiento,colour=Rendimiento ))+
  geom_point()+
  labs(x='Volatilidad', y='Rendimientos') +
  ggtitle('Riesgo-Rendimiento del Portafolio de Inversión')
ggplotly(RR)

# Ahora vamos a calcular el el histograma de frecuencias de los rendimiento 
# para posteriormente calcular VaR

library(moments)
# AMD

#Prueba de Asimetría

S1<-(sum((R1-mean(R1))^{3}))/(length(R1)*sd(R1)^{3});S1 #Manual

s1<-round(skewness(R1),3) # Usando el paquete moments

#Prueba de Kurtosis en busca peso de las colas
K1<-(sum((R1-mean(R1))^{4}))/(length(R1)*sd(R1)^{4});K1
k1<-round(kurtosis(R1),3)


#Prueba de normalidad asimetría y Kurtosis en los errores
JB1<-(length(R1)/6)*(s1^{2}+(k1-3)^{2}/4);round(JB1,3)

# Calcular el histograma de frecuencias 
hist(R1, main='Histrograma AMD', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(R1), col='red')
abline(v=mean(R1), col='#5F9EA0', lwd=2)

# BABA
#Prueba de Asimetría
S2<-(sum((R2-mean(R2))^{3}))/(length(R2)*sd(R2)^{3});S2
s2<-round(skewness(R2),2)
#Prueba de Kurtosis en busca peso de las colas
K2<-(sum((R2-mean(R2))^{4}))/(length(R2)*sd(R2)^{4});K2
k2<-round(kurtosis(R2),2)
#Prueba de normalidad asimetría y Kurtosis en los errores
JB2<-(length(R2)/6)*(s2^{2}+(k2-3)^{2}/4);JB2

hist(R2, main='Histrograma BABA', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(R2), col='red')
abline(v=mean(R2), col='#5F9EA0', lwd=2)

# DPZ
#Prueba de Asimetría
S3<-(sum((R3-mean(R3))^{3}))/(length(R3)*sd(R3)^{3});S3

#Prueba de Kurtosis en busca peso de las colas
K3<-(sum((R3-mean(R3))^{4}))/(length(R3)*sd(R3)^{4});K3

#Prueba de normalidad asimetría y Kurtosis en los errores
JB3<-(length(R3)/6)*(s3^{2}+(k3-3)^{2}/4);JB3

#Creamos el histograma de frecuencias para cada activo
hist(R3, main='Histrograma DPZ', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(R3), col='red')
abline(v=mean(R3), col='#5F9EA0', lwd=2)


# FB
#Prueba de Asimetría
S4<-(sum((R4-mean(R4))^{3}))/(length(R4)*sd(R4)^{3});S4

#Prueba de Kurtosis en busca peso de las colas
K4<-(sum((R4-mean(R4))^{4}))/(length(R4)*sd(R4)^{4});K4

#Prueba de normalidad asimetría y Kurtosis en los errores
JB4<-(length(R4)/6)*(s4^{2}+(k4-3)^{2}/4);JB4

hist(R4, main='Histrograma FB', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(R4), col='red')
abline(v=mean(R4), col='#5F9EA0', lwd=2)

# SAM
#Prueba de Asimetría
S5<-(sum((R5-mean(R5))^{3}))/(length(R5)*sd(R5)^{3});S5

#Prueba de Kurtosis en busca peso de las colas
K5<-(sum((R5-mean(R5))^{4}))/(length(R5)*sd(R5)^{4});K5

#Prueba de normalidad asimetría y Kurtosis en los errores
JB5<-(length(R5)/6)*(S5^{2}+(K5-3)^{2}/4);JB5

hist(R5, main='Histrograma SAM', ylab='Frecuencia',  
     freq = FALSE, breaks = "FD", col='darkorchid4')
lines(density(R5), col='red')
abline(v=mean(R5), col='#5F9EA0', lwd=2)

PE<-matrix(c(S1,K1-3,JB1,
             S2,K2-3,JB2,
             S3,K3-3,JB3,
             S4,K4-3,JB4,
             S5,K5-3,JB5), ncol=3, nrow =5, byrow = TRUE)
colnames(PE)<-c('Simetría', 'Kurtosis', 'Normalidad')
rownames(PE)<-c('AMD', 'BABA', 'DPZ', 'FB', 'SAM');PE

plot(PE, col=1:5, pch=19, )
text(PE[,1]+0.2, PE[,2], c('AMD', 'BABA', 'DPZ', 'FB', 'SAM'),
     col=1:5,font=0.5, xlab='Simetria', ylab='Kurtosis')



# Ahora podemos calcular el Valor en Riesgo de cada una de
# las acciones

# Definición de VaR
# El valor en riesgo es una técnica estadística para medir el
# riesgo financiero de una inversión. Indica la probabilidad 
# (normalmente 1% o 5%) de sufrir una determinada pérdida 
# durante un periodo de tiempo (normalmente 1 día, 1 semana o 1 mes).
# También se le conoce comúnmente como VaR (Value at Risk)

# Definición de CVaR
# El expected shortfall (tail VaR) es la pérdida esperada por una cartera
# en un horizonte temporal determinado una vez superado el VAR medido por 
# el nivel de confianza elegido.

#Para este caso vamos a ocupar un nivel de confianza  de 95%


# AMD
#Valor en Riesgo 
VaR_R1<-VaR(R1, p=0.95);VaR_R1
#VaR condicional o specting shortfall
CVaR_R1<-CVaR(R1, p=0.95);CVaR_R1
# Representación Grafica
x=seq(min(R1),max(R1),length=100)
y=dnorm(x,mean=mean(R1),sd=sd(R1))
plot(x,y, type='l', col='blue', xlab='Variación', ylab='Probailidad',
     main='Valor en Riesgo (VaR)  de AMD')
x=seq(CVaR_R1,VaR_R1,length=100)
y=dnorm(x,mean=mean(R1),sd=sd(R1))
lines(x,y, col='blue')
polygon(c(CVaR_R1,x,VaR_R1),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')

# BABA
VaR_R2<-VaR(R2, p=0.95);VaR_R2
CVaR_R2<-CVaR(R2, p=0.95);CVaR_R2
x=seq(min(R2),max(R2),length=100)
y=dnorm(x,mean=mean(R2),sd=sd(R2))
plot(x,y, type='l', col='blue', xlab='Variación', ylab='Probailidad',
     main='Valor en Riesgo (VaR)  de BABA')
x=seq(CVaR_R2,VaR_R2,length=100)
y=dnorm(x,mean=mean(R2),sd=sd(R2))
lines(x,y, col='blue')
polygon(c(CVaR_R2,x,VaR_R2),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')



# DPZ
VaR_R3<-VaR(R3, p=0.95);VaR_R3
CVaR_R3<-CVaR(R3, p=0.95);CVaR_R3
x=seq(min(R3),max(R3),length=100)
y=dnorm(x,mean=mean(R3),sd=sd(R3))
plot(x,y, type='l', col='blue', xlab='Variación', ylab='Probailidad',
     main='Valor en Riesgo (VaR)  de DPZ')
x=seq(CVaR_R3,VaR_R3,length=100)
y=dnorm(x,mean=mean(R3),sd=sd(R3))
lines(x,y, col='blue')
polygon(c(CVaR_R3,x,VaR_R3),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')


# FB
VaR_R4<-VaR(R4, p=0.95);VaR_R4
CVaR_R4<-CVaR(R4, p=0.95);CVaR_R4
x=seq(min(R4),max(R4),length=100)
y=dnorm(x,mean=mean(R4),sd=sd(R4))
plot(x,y, type='l', col='blue', xlab='Variación', ylab='Probailidad',
     main='Valor en Riesgo (VaR)  de FB')
x=seq(CVaR_R4,VaR_R4,length=100)
y=dnorm(x,mean=mean(R4),sd=sd(R4))
lines(x,y, col='blue')
polygon(c(CVaR_R4,x,VaR_R4),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')

# SAM
VaR_R5<-VaR(R5, p=0.95);VaR_R5
CVaR_R5<-CVaR(R5, p=0.95);CVaR_R5
x=seq(min(R5),max(R5),length=100)
y=dnorm(x,mean=mean(R5),sd=sd(R5))
plot(x,y, type='l', col='blue', xlab='Variación', ylab='Probailidad',
     main='Valor en Riesgo (VaR)  de SAM')
x=seq(CVaR_R5,VaR_R5,length=100)
y=dnorm(x,mean=mean(R5),sd=sd(R5))
lines(x,y, col='blue')
polygon(c(CVaR_R5,x,VaR_R5),c(0,y,0),col="red")
abline(h=0, v=0, lty=3, col='gray')


VaR_s<-matrix(c(VaR_R1,CVaR_R1,
                VaR_R2,CVaR_R2,
                VaR_R3,CVaR_R3,
                VaR_R4,CVaR_R4,
                VaR_R5,CVaR_R5), ncol=2, nrow =5, byrow = TRUE)
colnames(VaR_s)<-c('VaR', 'CVaR')
rownames(VaR_s)<-c('AMD', 'BABA', 'DPZ', 'FB', 'SAM');VaR_s

plot(VaR_s, col=1:5, pch=19, )
text(VaR_s[,1]+0.003, VaR_s[,2], c('AMD', 'BABA', 'DPZ', 'FB', 'SAM'),
     col=1:5,font=0.5, xlab='Simetria', ylab='Kurtosis')


####################################################################
####                                                            ####
####     Conformación de un portafolio de dos y n activos       ####
####                                                            ####
####################################################################


# Ahora vamos a calcular el VaR de las siguientes combinaciones
# compuestas por dos acciones de las 5 que tenemos, es decir
# 12,13,14,15,23,24,25,34,35,45

# Para esto primero distribuimos los pesos
w1<-seq(0, 1, by=0.01);w1
w2<-1-w1

# AMD y BABA

# Ahora realizamos el calculo de la correlación 
cor12<-cor(p1$AMD,p1$BABA);cor12
# Calculamos los rendimientos del portafolio
r12<-w1*ren_R1+w2*ren_R2;r12
# Calculamos la volatilidad del portafolio
v12<-w1**2*vol_R1**2+w2**2*vol_R2**2+2*w1*w2*cor12*vol_R1*vol_R2
v12<-sqrt(v12);v12 #Volatilidad
df<-data.frame(r12,v12, ncol=2)
# Graficamos la volatilidad vs rendimiento para encontrar la curva eficiente donde se tiene el menor riesgo y su  rendimiento asociado
g12<-ggplot(df, aes(x=v12, y=r12))+geom_point()
g12<-ggplotly(g12);g12


# VaR del portafolio
CL<- 1.65    #Nivel de confianza del 95%
S_12<-p1[1,2]+p1[1,3] # Valor del portafolio 12 AMD y BABA
VaR_12<-CL*v12*S_12*sqrt(1/252); VaR_12


Com_VaR_12<-matrix(c(w1, w2,VaR_12),ncol=3, nrow=length(w1), byrow=FALSE)
colnames(Com_VaR_12)<-c('w1', 'w2','VaR 12')


# Ahora vamos a calcular el riesgo y volatilidad de un portafolio
# compuesto  por n activos.
# 
# Sea una matriz cuadrada en la cual la diagonal está compuesta por
# las volatilidades (desviaciones estándar) de cada activo del
# portafolio  y los elementos fuera de la diagonal sean ceros
  
# Para eso seguimos los siguientes pasos 

# Paso 1 
# Composición de la matriz de volatilidades

# Vamos a definir una combinación de pesos para nuestro portafolio
# Pesos del portafolio
wp<-matrix(c(10,10,10,10,60), ncol=1, nrow=5, byrow = TRUE) #Vector fila
nc<-1.65  # Nivel de confianza
# usando los rendimientos por separado de cada uno de los activos
pf<-matrix(c(ren_R1,ren_R2, ren_R3, ren_R4, ren_R5), ncol=5) 
#Cálculo de la matriz de correlaciones [C]
corp<-cor(pf);corp 
# Calculamos la matriz de covarianzas 
 covp<-cov(pf);covp
# Calculamos la matriz de volatilidades del portafolio [\sigma]
 mvop<-matrix(c(vol_R1,0,0,0,0,
                0,vol_R2,0,0,0,
                0,0,vol_R3,0,0,
                0,0,0,vol_R4,0,
                0,0,0,0,vol_R5), ncol=5, nrow=5);mvop # Cálculo de la matriz de volatilidades [\sigma]
# [Sigma]=[vol][C][vol]
# Calculo de \sigma mayuscula
Sigma<-mvop%*%corp%*%mvop;Sigma  
# Una vez que tenemos esos elementos podemos calcular la volatilidad 
# del portafolio con la siguiente expresión: 
sigma_p<-sqrt(t(wp)%*%Sigma%*%wp);sigma_p # Cálculo de la volatilidad del Portafolio
# Finalmente calculamos el VaR del portafolio 
# para esto debemos de conocer el la suma del valor del portafolio 
# considerando una acción 
precio_p<-p1[1,2]+p1[1,3]+p1[1,4]+p1[1,5]+p1[1,6]
Var_p<-CL*(sigma_p/100)*precio_p*sqrt(1/252);Var_p # Cálculo del Valor en Riesgo del portafolio
print(paste('El valor en riesgo del portafolio es del', round(Var_p,4),
            '% con un nivel de confianza del 95%'))





 
 
















