rm(list=ls())
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(ROI)
#Cargamos las acciones que van a conformar el portafolio

Acciones<- c('FB','AAPL','AMZN','NFLX','NVDA')

# Obtenemos los precios históricos de cada una de las acciones y nos quedamos con
# con los precios de cierre ajustados
portafolio<-NULL
for (Acciones in Acciones) {
  portafolio<-cbind(portafolio, 
                    getSymbols.yahoo(Acciones, 
                    auto.assign = FALSE,from='2016-03-12',
                    periodicity ='daily')[,4])  
}



#Calculamos los retornos 
Ret_por<-na.omit(ROC(portafolio))
#Creamos nuestro vector que va a almacenar la información del portafolio 
port_op<-portfolio.spec(colnames(Ret_por))

#Agregamos las condiciones de optimización 
#Lo primero que tenemos que hacer es que la suma de los pesos sea igual a uno
port_op<-add.constraint(port_op, type = 'weight_sum',
                             min_sum=1, max_sum=1)
# Limitamos a que el porcentaje de inversión por cada acción se encuentre en los intervalos de
# 10% y 40% 
port_op<-add.constraint(port_op, type = 'box', min=0.05
                             , max=0.50)
# Imponemos los objetivos con los que se va a optimizar el portafolio
# El primero es que este con respecto a los retornos 
port_op<-add.objective(port_op, type ='return'
                             , name='mean')
# El segundo objetivo es con respecto al riego
port_op<-add.objective(port_op,type='risk', name='StdDev')

# Optimizamos el portafolio
opt_portafolio<-optimize.portfolio(Ret_por, port_op,
                                   optimize_method='ROI' , trace = TRUE );opt_portafolio

# Me muestra el gráfico de pesos para cada acción
chart.Weights(opt_portafolio)

# Me gráfica los portafolios que quiero optimizar
ef<-extractEfficientFrontier(opt_portafolio, match.col = 'StdDev',
                             n.portfolios =10, risk_aversion = NULL  )

chart.EfficientFrontier(ef, match.col = "StdDev", n.portfolios = 10,
                        cex.axis = 0.8, element.color = "blue", main = "Frontera Eficiente",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 1.8)

chart.EF.Weights(ef, colorset = NULL,
                 n.portfolios = 10, by.groups = FALSE, match.col = "StdDev",
                 main = "Optimización de un Portafolio",
                 cex.lab = 0.8, cex.axis = 0.8, cex.legend = 0.8, legend.labels = NULL,
                 element.color = "darkgray", legend.loc = "topright")

# Ejercicio 2


#Cargamos las acciones que van a conformar el portafolio

Acciones<- c('DELL','GOOG','CSCO','MSFT','JNPR')

# Obtenemos los precios históricos de cada una de las acciones y nos quedamos con
# con los precios de cierre ajustadps
portafolio<-NULL
for (Acciones in Acciones) {
  portafolio<-cbind(portafolio, 
                    getSymbols.yahoo(Acciones, 
                                     auto.assign = FALSE,from='2016-03-12',
                                     periodicity ='daily')[,4])  
}

#Calculamos los retornos 
Ret_por<-na.omit(ROC(portafolio))
#Creamos nuestro vector que va a almacenar la infromación del portafolio 
port_op<-portfolio.spec(colnames(Ret_por))


# Calculamos el valor en riesgo del portafolio por separado
# Método de precios históricos
VaR(Ret_por, p=.99, method="historical")
# Método Gaussiano
VaR(Ret_por, p=.99, method="gaussian")
# Método modificado  Cornish Fisher considerando que no hay normalidad
VaR(Ret_por, p=.99, method="modified")
# Ahora usamos  p=.99
VaR(Ret_por, p=.99)
# Que es equivalente a  alfa=.01
VaR(Ret_por, p=.01)
# Quitamos outliers
VaR(Ret_por, clean="boudt")
# Agregamos el componente de peso para el VaR del portafolio
VaR(Ret_por, clean="boudt", portfolio_method="component")
chart.VaRSensitivity(Ret_por[,1,drop=FALSE], 
                     methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"), 
                     colorset=bluefocus, lwd=2)
ES(Ret_por, p = 0.01)
table.Drawdowns(Ret_por)
chart.Drawdown(Ret_por)
skewness(Ret_por)  
kurtosis(Ret_por)
#La  variabilidad de los retornos abajo de la media
SemiDeviation(Ret_por)  


#Agregamos las condiciones de optimización 
#Lo primero que tenemos que hacer es que la suma de los pesos sea igual a uno
port_op<-add.constraint(port_op, type = 'weight_sum',
                        min_sum=1, max_sum=1)
# Limitamos a que el porcentaje de inverión por cada acción se encuentre en los intervalos de
# 10% y 40% 
port_op<-add.constraint(port_op, type = 'box', min=0.10
                        , max=0.40)
# Imponemos los objetivos con los que se va a optimizar el portafolio
# El primero es que este con respecto a los retornos 
port_op<-add.objective(port_op, type ='return'
                       , name='mean')
# El segundo objetivo es con respecto al riego
port_op<-add.objective(port_op,type='risk', name='StdDev')

# Optimizamos el portafolio
opt_portafolio<-optimize.portfolio(Ret_por, port_op,
                                   optimize_method='ROI' , trace = TRUE );opt_portafolio

# Me muestra el gráfico de pesos para cada acción
chart.Weights(opt_portafolio)

# Me gráfica los portafolios que quiero optimizar
ef<-extractEfficientFrontier(opt_portafolio, match.col = 'StdDev',
                             n.portfolios =10, risk_aversion = NULL  )

chart.EfficientFrontier(ef, match.col = "StdDev", n.portfolios = 10,
                        cex.axis = 0.8, element.color = "blue", main = "Frontera Eficiente",
                        RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                        chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                        cex.assets = 0.8)
# Ahora se analizan las combinaciónes de los portafolios que se estan simulando
chart.EF.Weights(ef, colorset = NULL,
                 n.portfolios = 10, by.groups = FALSE, match.col = "StdDev", main = "",
                 cex.lab = 0.8, cex.axis = 0.8, cex.legend = 0.8, legend.labels = NULL,
                 element.color = "darkgray", legend.loc = "topright")




# now use Gaussian
VaR(edhec, p=.95, method="gaussian")

# now use modified Cornish Fisher calc to take non-normal distribution into account
VaR(edhec, p=.95, method="modified")


# chart.RiskReward(opt_portafolio,
#                  neighbors = NULL, return.col = "mean", risk.col = "StdDev",
#                  chart.assets = FALSE, element.color = "darkgray", cex.axis = 0.8,
#                  xlim = NULL, ylim = NULL)
# 
# 
# chart.RiskBudget(opt_portafolio, match.col = "StdDev",
#                  risk.type = "absolute", main = "Risk Budget", plot.type = "line",
#                  cex.axis = 0.8, cex.lab = 0.8, element.color = "darkgray", las = 3,
#                  ylim = NULL, colorset = NULL, legend.loc = NULL, cex.legend = 0.8)



library(fPortfolio)
library(caTools)
library(timeSeries)

Acciones<- c('DELL','GOOG','CSCO','MSFT','JNPR')
portafolio<-NULL
for (Acciones in Acciones) {
  portafolio<-cbind(portafolio, 
                    getSymbols.yahoo(Acciones, 
                                     auto.assign = FALSE,from='2016-03-12',
                                     periodicity ='daily')[,4])  
}

P_cierre<-portafolio[apply(portafolio, 1, function(x) all(!is.na(x))),]

retornos<-as.timeSeries(tail(P_cierre,-1)/as.numeric(head(P_cierre,1))-1)
frontera<-portfolioFrontier(retornos)
plot(frontera)

getStatistics(frontera)$mean
cor(retornos)
cov(retornos)



puntos_ret_riesgo<-frontierPoints(frontera);puntos_ret_riesgo
puntos_anualizados<-data.frame(targetRisk=puntos_ret_riesgo[, 'targetRisk']*sqrt(252),
                               targetReturn=puntos_ret_riesgo[,'targetReturn']*252)

plot(puntos_anualizados)









