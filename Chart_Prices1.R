##################################################################
###                                                            ###  
###               Gr�ficos con ajustes vitales                 ###
###                                                            ###
##################################################################

library(quantmod)
library(dplyr)
library(highcharter)
Al<-getSymbols('ALSEA.MX',src='yahoo', from="2016-03-12", 
               auto.assign = FALSE)
# Podemos dibujar la l�nea de tendencia 
#lineChart(Al)
hchart(Al)
basicStats(Al)
# Podemos realizar un filtro para seleccionar una fecha en especifico
# A�o
lineChart(Al, subset = '2020')
# Periodo de a�os dentro de la base de datos
lineChart(Al, subset = '2020::2021')
# A�os y mes en especifico 
lineChart(Al, subset = '2020-03::2021-03')
#Fecha en especifico
lineChart(Al, subset = '2021-01-01::2021-03-12')

# Tambi�n podemos dibujar las l�nea de tendencia con barras para 
barChart(Al, subset = '2021')
# Con los datos hist�ricos se puede realizar un gr�fico de velas
candleChart(Al, subset = '2021-01-01::2021-03-12')

# POdemos graficar con ajustes de decisi�n 
# SMA Medias M�viles Simples
lineChart(Al, subset = '2020-03::2021-03')
addSMA(n=30, col='blue')
addSMA(n=200, col='red')
# EMA Medias M�viles Exponenciales
lineChart(Al, subset = '2020-03::2021-03')
addEMA(n=30, col='blue')
addEMA(n=200, col='red')
# Agregamos las bandas de confiabilidad
# La representaci�n gr�fica de las bandas
# de Bollinger son dos curvas que envuelven 
# el gr�fico de precios. Se calcula a partir 
# de una media m�vil (simple o exponencial) 
# sobre el precio de cierre a la que envuelven
# dos bandas que se obtienen de a�adir y sustraer
# al valor de la media K desviaciones est�ndar 
# (habitualmente, K = 2). 
# La distancia entre las curvas superior e inferior,
# igual a cuatro desviaciones est�ndar, es por lo tanto
# una medida de la volatilidad del precio del activo.
# 
# De acuerdo con el an�lisis t�cnico, el que los precios
# sobrepasen las bandas indica que el mercado est� sobrecomprado
# (si lo hacen por arriba) o sobrevendido (si lo hacen por abajo).

lineChart(Al, subset = '2020-03::2021-03')
addBBands(n=30, sd=2)
# Agregamos los momentos
lineChart(Al, subset = '2020-03::2021-03')
addMomentum(n=1)

lineChart(Al, subset = '2020-03::2021-03')
addROC(n=7)

# RSI = (Relative StrenghtIndex). Es uno de los osciladores m�s utilizados en an�lsis t�cnico.
# En su c�lculo se suelen utilizan datos de 14 sesiones, aunque se podr�an utilizar datos de m�s sesiones. Creado por r J. Welles Wilder, Jr.
# El oscilador RSI puede tomar valores entre 0 y 100.
# Si el valor de RSI supera 70 la acci�n est� sobrevalorada (se�al de venta).
# Si el valor de RSI es inferior 30 la acci�n est� subvaluada ( se�al de compra)



lineChart(Al, subset = '2020-03::2021-03')
addRSI(n=7, maType = 'EMA')
# 
# MACD = "Moving Average Convergence Divergence" 
# (Convergencia/divergencia de Medias M�viles) es un indicador creado por G�rard Appel.
# La se�al MACD est� constituida por dos l�neas que oscilan alrededor de la l�nea de 0:
# La primera linea MACD es la diferencia entre dos medias m�viles exponenciales; utilizamos
# habitualmente las medias m�viles exponenciales de 12 y 26 d�as.
# 
# La segunda l�nea, dicha l�nea de se�al, es la media m�vil exponencial de la diferencia, 
# toma en   cuenta 9 d�as. Estas dos l�neas se llaman MACD (Moving Average Converge Divergencia).
# 
# Tambi�n existe MACD histograma, mostrando la diferencia de las 2 medias m�viles.
# Gr�ficamente MACD se presenta as�:
# La se�al MACD est� constituida por dos l�neas que oscilan alrededor de la l�nea de 0:
# La primera linea MACD es la diferencia entre dos medias m�viles exponenciales; utilizamos
# habitualmente las medias m�viles exponenciales de 12 y 26 d�as.
# 
# La segunda l�nea, dicha l�nea de se�al, es la media m�vil exponencial de la diferencia,
# toma en   cuenta 9 d�as. Estas dos l�neas se llaman MACD (Moving Average Converge Divergencia).
# Tambi�n existe MACD histograma, mostrando la diferencia de las 2 medias m�viles.


lineChart(Al, subset = '2020-03::2021-03')
addMACD(fast=12,slow = 26, signal = 9,type = 'EMA')




# Aisnaci�n de precios hist�ricos 
price<-Cl(Al) # Precio de cierre
r<-price/Lag(price)-1   # Porcentaje de cambio del precio 
delta<-0.005 # Umbral de decisi�n 
signal<-c(0) #Primer dato 

for (i in 2:length(price)) {
  if(r[i]> delta){
    signal[i]<-1
  }else
    signal[i]<-0
}

head(signal, n=10)
signal<-reclass(signal, price)
head(signal, n=10)
lineChart(Al, subset = '2021-02::2021-03')
addTA(signal, type='S', col='red')
library(PerformanceAnalytics)
trade<-Lag(signal,1)
ret1<-dailyReturn(Al)*trade;ret1
names(ret1)<-'filter'

charts.PerformanceSummary(ret1,main = 'Regla de Naive')


