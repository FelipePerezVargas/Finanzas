#####################################################
#                                                   #
# Con este código se pueden gestionar y optimizar   #
#      portafolios de inversión en acciones         #
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

#Grafico básico
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

# Selección de activos
nombres <- c("AMXL.MX","ELEKTRA.MX","GCARSOA1.MX","PE&OLES.MX", "CX", 
             "MFRISCOA-1.MX", "TLEVISACPO.MX","GRUMAB.MX", "ALSEA.MX", 
             "BIMBOA.MX", "ALFAA.MX", "GMEXICOB.MX", "GFNORTEO.MX")

# Obtención de datos
precios <- NULL
for(i in nombres){
  precios <- cbind(precios, getSymbols.yahoo(i, from="2015-01-01", periodicity = "daily", auto.assign=F)[,6])
}


# Grafico de los precios
chart.TimeSeries(scale(precios))

# Comprobación de datos faltantes la base
colSums(is.na(precios))

# Calculo de rendimientos individuales de portafolio estático
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

### Métricas básicas del portafolio ###

# Potencial de diversificación
CAPM.beta(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252) # > 1 mas riesgoso que el IPC

# Rendimiento de exceso ajustado a la tasa de riesgo libre de 6%
CAPM.jensenAlpha(rendimientos_portafolio_estatico, rendimientos_ipc, Rf=0.06/252)
# >0 le ganas al IPC

# Rendimiento por unidad de riesgo ajustado
SharpeRatio(rendimientos_portafolio_estatico, Rf=0.06/252)

# Rendimientos, Riesgo y SharpeRatio anualizados
table.AnnualizedReturns(rendimientos_portafolio_estatico)
table.CalendarReturns(rendimientos_portafolio_estatico)

### Especificación modelo dinámico ###

# Especificamos el portafolio
portf <- portfolio.spec(colnames(rendimientos_individuales))

# Añadimos restricciones
portf <- add.constraint(portf, type = "weight_sum", min_sum=0.99, max_sum=1.01) 
portf <- add.constraint(portf, type = "box", min=0.01, max=.99) # para cada activo individual
portf <- add.constraint(portf, type = "transaction_cost", ptc = 0.01) # Costo de transacción proporcional del 1%

# Añadimos objetivos {anualizados}
portf <- add.constraint(portf, type="full_investment")
portf <- add.constraint(portf, type="long_only")
portf <- add.objective(portf, type = "return", name ="mean") #maximizar el rendimiento
portf <- add.objective(portf, type = "risk", name= "StdDev", target = 0.1) #Minimizar el riesgo {volatilidad diria}

#
## Backtesting Optimización
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


# Visualización los portafolios rebalanceados {pesos dinámicos}
chart.Weights(op_reb, main="Pesos Rebalanceados en el tiempo")
x <- extractWeights(op_reb)

### Comparación ###

# Pesos equilibrados
pesos_equilibrados <- rep(1/ncol(rendimientos_individuales),ncol(rendimientos_individuales)) # pesos equilibrados
portafolio_equilibrado <- Return.portfolio(rendimientos_individuales, weights =pesos_equilibrados)
colnames(portafolio_equilibrado) <- "Portafolio Estático"

# Pesos rebalanceados
pesos_rebalanceados <- extractWeights(op_reb)
porfolio_rebalanceado <- Return.portfolio(rendimientos_individuales, weights = pesos_rebalanceados)
colnames(porfolio_rebalanceado) <- "Portafolio Dinámico Rebalanceado"

# Grafico de desempeño en el tiempo
desempeño <- cbind(porfolio_rebalanceado, portafolio_equilibrado, rendimientos_ipc)
x11()
charts.PerformanceSummary(desempeño, main = "Desempeño en el tiempo", legend.loc = "center" )

### FIN ###