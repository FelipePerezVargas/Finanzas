##################################################################
###                                                            ###  
###               Gráficos con ajustes vitales                 ###
###                                                            ###
##################################################################

library(quantmod)
library(dplyr)
library(highcharter)
Al<-getSymbols('ALSEA.MX',src='yahoo', from="2016-03-12", 
               auto.assign = FALSE)
# Podemos dibujar la línea de tendencia 
#lineChart(Al)
hchart(Al)
basicStats(Al)
# Podemos realizar un filtro para seleccionar una fecha en especifico
# Año
lineChart(Al, subset = '2020')
# Periodo de años dentro de la base de datos
lineChart(Al, subset = '2020::2021')
# Años y mes en especifico 
lineChart(Al, subset = '2020-03::2021-03')
#Fecha en especifico
lineChart(Al, subset = '2021-01-01::2021-03-12')

# También podemos dibujar las línea de tendencia con barras para 
barChart(Al, subset = '2021')
# Con los datos históricos se puede realizar un gráfico de velas
candleChart(Al, subset = '2021-01-01::2021-03-12')

# POdemos graficar con ajustes de decisión 
# SMA Medias Móviles Simples
lineChart(Al, subset = '2020-03::2021-03')
addSMA(n=30, col='blue')
addSMA(n=200, col='red')
# EMA Medias Móviles Exponenciales
lineChart(Al, subset = '2020-03::2021-03')
addEMA(n=30, col='blue')
addEMA(n=200, col='red')
# Agregamos las bandas de confiabilidad
# La representación gráfica de las bandas
# de Bollinger son dos curvas que envuelven 
# el gráfico de precios. Se calcula a partir 
# de una media móvil (simple o exponencial) 
# sobre el precio de cierre a la que envuelven
# dos bandas que se obtienen de añadir y sustraer
# al valor de la media K desviaciones estándar 
# (habitualmente, K = 2). 
# La distancia entre las curvas superior e inferior,
# igual a cuatro desviaciones estándar, es por lo tanto
# una medida de la volatilidad del precio del activo.
# 
# De acuerdo con el análisis técnico, el que los precios
# sobrepasen las bandas indica que el mercado está sobrecomprado
# (si lo hacen por arriba) o sobrevendido (si lo hacen por abajo).

lineChart(Al, subset = '2020-03::2021-03')
addBBands(n=30, sd=2)
# Agregamos los momentos
lineChart(Al, subset = '2020-03::2021-03')
addMomentum(n=1)

lineChart(Al, subset = '2020-03::2021-03')
addROC(n=7)

# RSI = (Relative StrenghtIndex). Es uno de los osciladores más utilizados en análsis técnico.
# En su cálculo se suelen utilizan datos de 14 sesiones, aunque se podrían utilizar datos de más sesiones. Creado por r J. Welles Wilder, Jr.
# El oscilador RSI puede tomar valores entre 0 y 100.
# Si el valor de RSI supera 70 la acción está sobrevalorada (señal de venta).
# Si el valor de RSI es inferior 30 la acción está subvaluada ( señal de compra)



lineChart(Al, subset = '2020-03::2021-03')
addRSI(n=7, maType = 'EMA')
# 
# MACD = "Moving Average Convergence Divergence" 
# (Convergencia/divergencia de Medias Móviles) es un indicador creado por Gérard Appel.
# La señal MACD está constituida por dos líneas que oscilan alrededor de la línea de 0:
# La primera linea MACD es la diferencia entre dos medias móviles exponenciales; utilizamos
# habitualmente las medias móviles exponenciales de 12 y 26 días.
# 
# La segunda línea, dicha línea de señal, es la media móvil exponencial de la diferencia, 
# toma en   cuenta 9 días. Estas dos líneas se llaman MACD (Moving Average Converge Divergencia).
# 
# También existe MACD histograma, mostrando la diferencia de las 2 medias móviles.
# Gráficamente MACD se presenta así:
# La señal MACD está constituida por dos líneas que oscilan alrededor de la línea de 0:
# La primera linea MACD es la diferencia entre dos medias móviles exponenciales; utilizamos
# habitualmente las medias móviles exponenciales de 12 y 26 días.
# 
# La segunda línea, dicha línea de señal, es la media móvil exponencial de la diferencia,
# toma en   cuenta 9 días. Estas dos líneas se llaman MACD (Moving Average Converge Divergencia).
# También existe MACD histograma, mostrando la diferencia de las 2 medias móviles.


lineChart(Al, subset = '2020-03::2021-03')
addMACD(fast=12,slow = 26, signal = 9,type = 'EMA')




# Aisnación de precios históricos 
price<-Cl(Al) # Precio de cierre
r<-price/Lag(price)-1   # Porcentaje de cambio del precio 
delta<-0.005 # Umbral de decisión 
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


