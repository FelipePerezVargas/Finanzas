# Uso del paquete quantmod

#  Lo primero que vamos a hacer es instalar el paquete quatmod
# install.packages('quantmod')
# El siguiente paso es cargar la librer�a ####
library(quantmod)

# Una vez instalado vamos a obtener datos hist�ricos de alguna empresa
# Para esto usamos el comando getSymbols ####
s1<-getSymbols('BBAJIOO.MX')

# En esta caso genera una serie de tiempo compuesta por: precio de apertura,
# precio m�ximo, precio m�nimo, precio de cierre, columna de acciones y precios
# ajustados. 

# Podemos seleccionar la fuente de donde queremos obtener la informaci�n ####
s1<-getSymbols('BBAJIOO.MX', source='yahoo')
# La fuentes que soporta el comando son FED, Yahoo, Google y cargar desde bases 
#  de datos

# Podemos agregar la selecci�n de un rango de fechas en el formato a�o/d�a/mes ####
s1<-getSymbols('BBAJIOO.MX', from = '2020-01-01') 

# Esta serie de tiempo es una cadena de caracteres que no podemos manipulas,
# as� que tenemos que cambiar a una variable/dataframe la serie de tiempo ####
s1<-getSymbols('BBAJIOO.MX', auto.assign = F)


# Podemos combinar los comandos anteriores ####

s1<-getSymbols('BBAJIOO.MX', source='yahoo',
               from = '2020-01-01', 
               auto.assign = FALSE)
# Podemos conocer los primeros 6 valores ####
head(s1)
# Podemos conocer los �ltimos 6 valores ####
tail(s1)
# Podemos conocer y/o almacenar el �ltimo valor ####
last(s1)[,4]
# POdemos hacer un gr�fico simple de los precios hist�ricos de la acci�n con 
# el comando plot() no editable ####
plot(s1) #Nos gr�fica el volumen 
# Podemos gr�ficar cualquiera de las columnas usando sq$columnadeseada ####
s1<-na.omit(A1) #Omitimos NA
plot(s1$BBAJIOO.MX.Open)
hist(s1$BBAJIOO.MX.Open)
plot(s1$BBAJIOO.MX.High)
hist(s1$BBAJIOO.MX.High)
plot(s1$BBAJIOO.MX.Low)
hist(s1$BBAJIOO.MX.Low)
plot(s1$BBAJIOO.MX.Close)
hist(s1$BBAJIOO.MX.Close)
plot(s1$BBAJIOO.MX.Volume)
hist(s1$BBAJIOO.MX.Volume)
plot(s1$BBAJIOO.MX.Adjusted)
hist(s1$BBAJIOO.MX.Adjusted)

# Podemos crear otros tipos de gr�ficos usando el paquete quantmod

# add_area(s1$BBAJIOO.MX.Adjusted)

# Podemos calcular los rendimientos diarios, semanales, mensuales y anuales

# Retornos diario ####
s1.d<-dailyReturn(s1$BBAJIOO.MX.Close)
# Retornos semanales ####
s1.s<-weeklyReturn(s1$BBAJIOO.MX.Close)
# Retornos mensuales ####
s1.m<-monthlyReturn(s1$BBAJIOO.MX.Close)
# Retornos trimestrales ####
s1.t<-quarterlyReturn(s1$BBAJIOO.MX.Close)
# Retornos anuales ####
s1.a<-annualReturn(s1$BBAJIOO.MX.Close)




# Calculo del VaR ####
A1<-getSymbols('AAPL',src='yahoo',from='2015-11-24',to='2020-11-27', 
               auto.assign = FALSE )[,c(4)]

chartSeries(A1) #Hacemos el gr�fico de los precios
A1<-na.omit(A1) #Omitimos NA
names(A1)<-'AAPL' #Renombramos los precios
logret<-diff(log(A1$AAPL)) #Calculamos los retornos diarios
# Retorno diarios
logret<-diff(log(A1$AAPL))[-1] # Quitamos el �ltimo valor
mu<-round(mean(logret),8);mu #Calculamos el rendimiento 
sig<-round(sd(logret),8);sig #Calculamos la riesgo-volatilidad

rvec <- as.vector(logret)
hist(rvec)
plot(rvec, lty=2, col='red')




# Modelo 1  considerando mu y sigma
var<-round(qnorm(0.01,mu,sig),6);var
ES<-mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01;ES
#Valor en riesgo en un d�a 
1000*(exp(var)-1)
# Especting Shortfall
#M�xima perdida en un d�a 
1000*(exp(ES)-1)



#### Predicci�n de los retornos diarios, semanales, etc.. #### 
# M�todo 1 ####
# Obtenemos los precios de cualquier acci�n
s2<-getSymbols('CX', source='yahoo',
                    from = '2015-01-01', 
                   auto.assign = FALSE, adjust =  TRUE)
# Calculamos los retornos, en este caso lo vamos a hacer semanales
s2.s<-weeklyReturn(s2$CX.Close)
# Gr�ficamos para analizar el comportamiento
plot(s2.s)
# Realizamos un histograma para saber si tienen distribuci�n normal
hist(s2.s)
# obtenemos el �ltimo valor de la serie para poder calibrar el modelo
val1<-last(s2$CX.Close)

# Creamos la ventana donde vamos a alojar los gr�ficos
plot(NULL, 
     xlim = c(2021.4,2021.7), #Un trimestre
     ylim = c(1.5,15),    #Establecemos los precios
     xlab = 'Tiempo',
     ylab = 'Valor Cemex')
abline(h=min(s2$CX.Close), col='blue', lty=2)   #Precio m�nimo  
abline(h=max(s2$CX.Close), col='red',lty=2)  #Rendimiento m�ximo
abline(h=min(val1), col='green', lty=2)   #Precio m�nimo 
# Ciclo para obtener las posibles trayectorias de los retorno
for (i in 1:10) {
  #Muestra para calcular una distribuci�n de retornos
  #Debemos convertir en vector para poder trabajar con los datos
   rmuestra<-sample(as.vector(s2.s), 12, replace = TRUE)
  #Sumamos uno pasa saber si estamos obteniendo ganancia o perdidas 
   rmuestra1<-1+rmuestra
  #Calculamos la suma acumulada 
   rmuestra2<-cumprod(rmuestra1)
  #Calculamos el precio final 
   val2<-rmuestra2*as.numeric(val1)
  #Predicci�n de los retornos de la acci�n 
   val3<-ts(val2,start = c(2021,26), frequency = 52)
  #Gr�ficamos la acci�n  
   
   lines(val3, col = i)
}

# M�todo 2 ####
# 

# Creamos la ventana donde vamos a alojar los gr�ficos 
plot(NULL, 
     xlim = c(2021.4,2021.7), #Un trimestre
     ylim = c(1.5,15),    #Establecemos los precios
     xlab = 'Tiempo',
     ylab = 'Valor Cemex')
abline(h=min(s2$CX.Close), col='blue', lty=2)   #Precio m�nimo  
abline(h=max(s2$CX.Close), col='red',lty=2)  #Rendimiento m�ximo
abline(h=min(val1), col='green', lty=2)   #Precio m�nimo 

# Creamos un contenedor para que se almacenen los resultados
out2 <- rep(0,10)
# Ciclo para obtener las posibles trayectorias de los retorno
for (i in 1:10) {
  #Muestra para calcular una distribuci�n de retornos
  #Debemos convertir en vector para poder trabajar con los datos
  rmuestra<-sample(as.vector(s2.s), 12, replace = TRUE)
  rmuestra1<-1+rmuestra
  rmuestra2<-cumprod(rmuestra1)
  val2<-rmuestra2*as.numeric(val1)
# Variable bandera
  flag1<-ifelse(val2 < 5, 1,0) #Estableces el precio de perdida 
#  
out1[i]=max(flag1)  
  val3<-ts(val2,start = c(2021,26), frequency = 52)
  lines(val3, col = i)
  abline(h=5, col ='yellow', lwd=2, lty=2)
}

mean(out1)



# M�todo 3 ####
# 

# Creamos la ventana donde vamos a alojar los resultados
plot(NULL, 
     xlim = c(2021.4,2021.7), #Un trimestre
     ylim = c(1.5,15),    #Establecemos los precios
     xlab = 'Tiempo',
     ylab = 'Valor Cemex')
abline(h=min(s2$CX.Close), col='blue', lty=2)   #Precio m�nimo  
abline(h=max(s2$CX.Close), col='red',lty=2)  #Rendimiento m�ximo
abline(h=min(val1), col='green', lty=2)   #Precio m�nimo 

# Creamos un contenedor para que se almacenen los resultados
out2 <- matrix(0,nrow=1000, ncol=12)
# Ciclo para obtener las posibles trayectorias de los retorno
for (i in 1:1000) {
  #Muestra para calcular una distribuci�n de retornos
  #Debemos convertir en vecto para poder trabajar con los datos
  rmuestra<-sample(as.vector(s2.s), 12, replace = TRUE)
  rmuestra1<-1+rmuestra
  rmuestra2<-cumprod(rmuestra1)
  val2<-rmuestra2*as.numeric(val1)
  #  
  out2[i,]<-val2  
  val3<-ts(val2,start = c(2021,26), frequency = 52)
  lines(val3, col = i)
  abline(h=5, col ='yellow', lwd=2, lty=2)
}


bound1<- apply(out2, 2, quantile, c(0.025, 0.5,0.0975))
lbound1<-ts(bound1[1,], start = c(2021, 26), frequency = 52)
lines(lbound1, col = 'red', lty=2, lwd=2)
ubound1<-ts(bound1[3,], start = c(2021, 26), frequency = 52)
lines(ubound1, col = 'seagreen', lty=2, lwd=2)
mbound1<-ts(bound1[2,], start = c(2021, 26), frequency = 52)
lines(mbound1, col = 'darkblue', lty=2, lwd=2)








# Rendimiento de un portafolio de un portafolio ####

# Rendimiento de un portafolio compuesto por dos acciones ####


ticker<- c('AAPL','GOOGL','TGT','AMZN','TM')


portafolio1<-getSymbols(ticker, auto.assign = FALSE,
                       from='2015-01-01')
portafolio2<-portafolio1[,4]
portafolio3<-weeklyReturn(portafolio2)


for (i in 2:length(ticker)) {
  portafolio1<-getSymbols(ticker[i], auto.assign = FALSE,
                         from='2015-01-01')  
portafolio2<-portafolio1[,4]
 portafolio3a<-weeklyReturn(portafolio2)
 portafolio3<-cbind(portafolio3,portafolio3a )
}

names(portafolio3)<-ticker
#Calculamos la media para cada acci�n
apply(portafolio3, 2, mean)
# Calculamos la desviaci�n est�ndar para cada acci�n
apply(portafolio3, 2, sd)
# Calculamos la covarianza
cov(portafolio3)
# Calculamos la correlaci�n entre las acciones
cor(portafolio3)
# Escogemos los pesos de cada acci�n que conforma el portafolio
pesos1<-c(0.2,0.3,0.15,0.25,0.10)
# Calculamos el valor medio de cada una de las acciones
media_np<-apply(portafolio3, 2, mean)
# Calculamos el pedo 
peso_accion<-media_np%*%pesos1
riesgo<-t(pesos1)%*%cov(portafolio3)%*%pesos1

# Optimizaci�n de un portafolio ####

# Instalar el paquete fPortfolio
# install.packages(fPortfolio)
library(fPortfolio)

minp1<-minvariancePortfolio(as.timeSeries(portafolio3),
                            spec = portfolioSpec(),
                            constraints = 'LongOnly')
minp1

pesos2<-c(0.0905, 0.0647, 0.1659, 0.1127 ,0.5662)

media_p<-media_np%*%pesos2
riesgo2<-t(pesos2)%*%cov(portafolio3)%*%pesos2
  

# Simulaci�n del precio de una acci�n ####
p<-vector()
p[1]<-100
p[2]<- p[1]+rnorm(1,0,5)
p[2]

t<-365
for (i in 2:t) {
  p[i]<-p[i-1]+rnorm(1,0,5)
}
p
ts.plot(p)
hist(p)


# Simulaci�n del precio de dos acciones ####
s<-2
p<-matrix(nrow = t, ncol = s)
p[1,]<-100
head(p)

for (j in 1:s) {
  for (i in 2:t) {
    p[i,j]<-p[i-1,j]+rnorm(1,0,5)
  }
}
head(p)
ts.plot(p)
hist(p)
tail(p)

# Simulaci�n de n acciones ####

s<-1000
p<-matrix(nrow = t, ncol = s)
p[1,]<-100
head(p)

for (j in 1:s) {
  for (i in 2:t) {
    p[i,j]<-p[i-1,j]+rnorm(1,0,5)
  }
}
head(p)
ts.plot(p)
hist(p)
tail(p)


pfinal<-p[365,]
mean(pfinal)
sd(pfinal)
mean(pfinal)+c(1,1)*qnorm(0.975)*sd(pfinal)

pfinal<-pfinal[order(pfinal, decreasing = T)]
pfinal
pfinal[0.95*s]
sum(pfinal< -58.63763)/s






























