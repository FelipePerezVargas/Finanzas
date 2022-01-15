library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

Acciones<- c('ALSEA.MX','BIMBOA.MX','CMXC','WALMEX.MX')

portafolio<-NULL
for (Acciones in Acciones) {
  portafolio<-cbind(portafolio, getSymbols(Acciones, 
                          auto.assign = FALSE,
                          from='2015-01-01', periodicity ='daily')[,6] )  
 }

# Podemos hacer lo sgraficos de cada una de las acciones
chartSeries(portafolio[,1])
chartSeries(portafolio[,2])
chartSeries(portafolio[,3])
chartSeries(portafolio[,4])


# Calculamos los retornos 
s1.d<-dailyReturn(portafolio)
ret_portafolio<-na.omit(diff(log(portafolio)))
ret_portafolio1<-ROC(portafolio)



riesgo<-vector()
for (i in 1:4) {
  riesgo[i]<-mean(ret_portafolio1[,i])
  
}
mean(ret_portafolio$ALSEA.MX.Adjusted)

rendimiento<-vector()
for (i in 1:4) {
  rendimiento[i]<-sd(ret_portafolio[,i])
  
}

plot(riesgo, rendimiento)

colSums(ret_portafolio1)

retornos_portafolio<-Return.portfolio(ret_portafolio1)
retornos_portafolio<-na.omit(ret_portafolio1)
CAPM.beta(retornos_portafolio,ret_Referencia, 0.04/252 )
CAPM.jensenAlpha(retornos_portafolio,ret_Referencia, 0.04/252)
SharpeRatio(retornos_portafolio, 0.04/252)
table.AnnualizedReturns(retornos_portafolio)
table.CalendarReturns(retornos_portafolio)

portafolio_c<-portfolio.spec(colnames(retornos_portafolio))

portafolio_c<-add.constraint(portafolio_c, type = 'weight_sum',
                             min_sum=1, max_sum=1)
portafolio_c<-add.constraint(portafolio_c, type = 'box', min=0.10
                             , max=0.40)
portafolio_c<-add.constraint(portafolio_c, type ='return'
                             , return_target=mean(retornos_portafolio))

portafolio_c<-add.constraint(portafolio_c,type='risk', name='StdDev')

opt_portafolio<-optimize.portfolio(retornos_portafolio, portafolio_c,
                                   optimize_method='ROI' , trace = TRUE )





