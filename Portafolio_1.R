#####################################################
#                                                   #
# Con este c�digo se pueden gestionar y optimizar   #
#      portafolios de inversi�n en acciones         #
#                                                   #
#####################################################

# Preliminares
remove(list = ls())
#setwd("~/GitHub/Finance/Porfolio_Optimizartion")

library(quantmod) # Modelado Financiero
library(PerformanceAnalytics)
library(PortfolioAnalytics)

################################### EJEMPLO CON UNA ACCION ###############################################

# Descarga de datos
gruma <- getSymbols.yahoo("GRUMAB.MX", from="2015-01-01", auto.assign = FALSE)

#Grafico b�sico
chartSeries(gruma, type = "auto",subset = NULL, show.grid = TRUE, 
            name = NULL,time.scale = NULL,log.scale = FALSE, TA = 'addVo()',
            TAsep=';', line.type = "l",bar.type = "ohlc", 
            theme = chartTheme("black"), layout = NA,major.ticks='auto',
            minor.ticks=TRUE, yrange=NULL,
            plot=TRUE, color.vol = TRUE, multi.col = F,)

# Calculo de rendimientos
gruma_ret <- na.omit(dailyReturn(gruma[,6], type = "log")) # Solo el cierre ajustado

# Grafico rendimientos acumulados
chart.CumReturns(gruma_ret)

####################################### MAS ACCIONES ######################################################

# Selecci�n de activos
nombres <- c("AMXL.MX","ELEKTRA.MX","GCARSOA1.MX","PE&OLES.MX", "CX", 
             "MFRISCOA-1.MX", "TLEVISACPO.MX","GRUMAB.MX", "ALSEA.MX", 
             "BIMBOA.MX", "ALFAA.MX", "GMEXICOB.MX", "GFNORTEO.MX")

# Obtenci�n de datos
precios <- NULL
for(i in nombres){
  precios <- cbind(precios, getSymbols.yahoo(i, from="2015-01-01", periodicity = "daily", auto.assign=F)[,6])
}


# Grafico de los precios
chart.TimeSeries(scale(precios))

# Comprobaci�n de datos faltantes la base
colSums(is.na(precios))

# Calculo de rendimientos individuales de portafolio est�tico
rendimientos_individuales <- na.omit(ROC(precios))

# Precios de referencia
ipc <- getSymbols.yahoo("^MXX", from="2015-01-01", periodicity = "daily", auto.assign=F)[,6]

# Calculo de rendimientos IPC
rendimientos_ipc <- na.omit(ROC(ipc)) # tasa de cambio continua
colnames(rendimientos_ipc) <- "rendimientos_ipc"
head(rendimientos_ipc)
colSums(is.na(ipc))
tail(rendimientos_ipc)

# Rendimientos ponderados del portafolio
rendimientos_portafolio_estatico <- Return.portfolio(rendimientos_individuales)
chart.CumReturns(rendimientos_portafolio_estatico)

### M�tricas b�sicas del portafolio ###

# Potencial de diversificaci�n
CAPM.beta(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252) # > 1 mas riesgoso que el IPC

# Rendimiento de exceso ajustado a la tasa de riesgo libre de 6%
CAPM.jensenAlpha(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252)
# >0 le ganas al IPC

# Rendimiento por unidad de riesgo ajustado
SharpeRatio(rendimientos_portafolio_estatico, Rf=0.06/252)

# Rendimientos, Riesgo y SharpeRatio anualizados
table.AnnualizedReturns(rendimientos_portafolio_estatico)
table.CalendarReturns(rendimientos_portafolio_estatico)

### Especificaci�n modelo din�mico ###

# Especificamos el portafolio
portf <- portfolio.spec(colnames(rendimientos_individuales))

# A�adimos restricciones
portf <- add.constraint(portf, type = "weight_sum", min_sum=0.99, max_sum=1.01) 
portf <- add.constraint(portf, type = "box", min=0.01, max=.99) # para cada activo individual
portf <- add.constraint(portf, type = "transaction_cost", ptc = 0.01) # Costo de transacci�n proporcional del 1%

# A�adimos objetivos {anualizados}
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type = "return", name ="mean") #maximizar el rendimiento
portf <- add.objective(portf, type = "risk", name= "StdDev", target = 0.1) #Minimizar el riesgo {volatilidad diria}

#
## Backtesting Optimizaci�n
#

# Portafolios aleatorios
pa <- random_portfolios(portf, 10000, "sample") #genera 10000 portafolios

# Rebalanceo
op_reb <- optimize.portfolio.rebalancing(rendimientos_individuales,
                                         portf, 
                                         optimize_method = "random",
                                         rp=pa,
                                         rebalance_on = "months",
                                         training_period = 30, 
                                         rolling_window = 14) 


# Visualizaci�n los portafolios rebalanceados {pesos din�micos}
chart.Weights(op_reb, main="Pesos Rebalanceados en el tiempo")
x <- extractWeights(op_reb)

### Comparaci�n ###

# Pesos equilibrados
pesos_equilibrados <- rep(1/ncol(rendimientos_individuales),ncol(rendimientos_individuales)) # pesos equilibrados
portafolio_equilibrado <- Return.portfolio(rendimientos_individuales, weights =pesos_equilibrados)
colnames(portafolio_equilibrado) <- "Portafolio Est�tico"

# Pesos rebalanceados
pesos_rebalanceados <- extractWeights(op_reb)
porfolio_rebalanceado <- Return.portfolio(rendimientos_individuales, weights = pesos_rebalanceados)
colnames(porfolio_rebalanceado) <- "Portafolio Din�mico Rebalanceado"

# Grafico de desempe�o en el tiempo
desempe�o <- cbind(porfolio_rebalanceado, portafolio_equilibrado, rendimientos_ipc)
x11()
charts.PerformanceSummary(desempe�o, main = "Desempe�o en el tiempo", legend.loc = "center" )

### FIN ###