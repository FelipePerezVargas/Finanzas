
####################################################
##                                                ##
##            Optimización de Portafolio          ##
##                                                ##
####################################################

library(tseries)
library(fPortfolio)
library(knitr)
library(kableExtra)
library(quantmod)


####Cargamos el indice de referencia####
Indice<- get.hist.quote(instrument = "^MXX", 
                        start=as.Date("2015-02-19"), 
                        end=as.Date("2021-02-19"), quote = "AdjClose")

plot(Indice, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre") 
title(main="Precios Históricos del Índice ^MXX 500 [2015-2021]")
summary(Indice)

#### Seleccionamos los activos a conformar nuestro portafolio ####
#Microsoft
MSFT<- get.hist.quote(instrument = "MSFT", 
                      start=as.Date("2015-02-19"), 
                      end=as.Date("2021-02-19"), quote = "AdjClose")

plot(MSFT, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de Microsoft Corporation [2015-2021]")
#Johnson & Johnson
JNJ<- get.hist.quote(instrument = "JNJ", 
                     start=as.Date("2015-02-19"), 
                     end=as.Date("2021-02-19"), quote = "AdjClose")

plot(JNJ, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de Johnson & Johnson [2015-2021]")

#Mastercard
MA<- get.hist.quote(instrument = "MA", 
                    start=as.Date("2015-02-19"), 
                    end=as.Date("2021-02-19"), quote = "AdjClose")


plot(MA, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de Mastercard Incorporated [2015-2021]")
#Pepsi
PEP<- get.hist.quote(instrument = "PEP", 
                     start=as.Date("2015-02-19"), 
                     end=as.Date("2021-02-19"), quote = "AdjClose")

plot(PEP, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de PepsiCo Inc. [2015-2021]")
#Walmart
WMT<- get.hist.quote(instrument = "WMT", 
                     start=as.Date("2015-02-19"), 
                     end=as.Date("2021-02-19"), quote = "AdjClose")

plot(WMT, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de Walmart Corporate [2015-2021]")

#McDonald's
MCD<- get.hist.quote(instrument = "MCD", 
                     start=as.Date("2015-02-19"), 
                     end=as.Date("2021-02-19"), quote = "AdjClose")

plot(MCD, col="deepskyblue", xlab="Fecha", ylab="Precios de Cierre")
title(main="Histórico de McDonald's Corporation [2015-2021]")

#### Conformamos el portafolio ####
Portafolio <- merge(MSFT,JNJ,MA,PEP,WMT,MCD, all = FALSE) 


#Asignamos nombres a cada activo
names(Portafolio)<-c("MSFT","JNJ","MA", "PEP", "WMT","MCD")
names(Portafolio)
plot(Portafolio, main=" ", col="deepskyblue", xlab="Fecha")
title(main="Histórico de Cartera")

#Calculamos los retornos del índice ####
RetornoIndice<-diff(log(Indice))
head(RetornoIndice)
tail(RetornoIndice)


plot(RetornoIndice, main=" ", col="deepskyblue", xlab="Fecha", ylab="Rendimientos")
title(main="Rendimientos del Indice S&P 500")
# Graficar el histograma para saber su distribución

#Calculamos los rendimientos del portafolio
Rendimientos<-diff(log(Portafolio))
head(Rendimientos,10)
tail(Rendimientos,10)

plot(Rendimientos, main=" ", col="deepskyblue", xlab="Fecha")
title(main='Rendimientos de la Portafolio')

summary(Rendimientos)

#Calculamos el Riesgo-Rendimiento ####
RendimientoPromedio <- c(mean(RetornoIndice),
                        mean(Rendimientos$MSFT),
                        mean(Rendimientos$JNJ),
                        mean(Rendimientos$MA),
                        mean(Rendimientos$PEP),
                        mean(Rendimientos$WMT),
                        mean(Rendimientos$MCD))

Volatilidad <- c(sd(RetornoIndice),
                sd(Rendimientos$MSFT),
                sd(Rendimientos$JNJ),
                sd(Rendimientos$MA),
                sd(Rendimientos$PEP),
                sd(Rendimientos$WMT),
                sd(Rendimientos$MCD))
#Estas líneas son para visualizar las relaciones riesgo-rendimiento
Cuadro = data.frame(cbind(RendimientoPromedio,Volatilidad))
colnames(Cuadro)<- c("GSPC","MSFT", "JNJ", "MA", "PEP", "WMT", "MCD")
plot(Cuadro)

#Estas líneas son para los cálculos
Cuadro = data.frame(rbind(RendimientoPromedio,Volatilidad))
colnames(Cuadro)<- c("GSPC","MSFT", "JNJ", "MA", "PEP", "WMT", "MCD")
plot(Cuadro)

Cuadro*100  
#Calculamos el valor en Riesgo de cada activo ####
var(RetornoIndice)*100
var(Rendimientos$MSFT)*100
var(Rendimientos$JNJ)*100
var(Rendimientos$MA)*100
var(Rendimientos$PEP)*100
var(Rendimientos$WMT)*100
var(Rendimientos$MCD)*100
#Calculamos la covarianza de los rendimientos ####
Cov <- cov(Rendimientos)
Cov
#Calculamos la correlación de los rendimientos
corr <- cor(Rendimientos) 
corr

#### Ahora podemos hacer graficos que nos ayuden a tomar decisiones  ####

library(gplots)

generate_heat_map <- function(correlationMatrix, title)
{
  
  heatmap.2(x = correlationMatrix,    
            cellnote = correlationMatrix,   
            main = title,           
            symm = TRUE,            
            dendrogram="none",      
            Rowv = FALSE,           
            trace="none",           
            density.info="none",        
            notecol="black")          
}

corr1 <- round(cor(Rendimientos)*100, 2)
generate_heat_map(corr1,"Mapa de calor: Correlaciones")

#### Ahora vamos a analizar el portafolio desde la teoría de Markov ####

markov<-portfolioSpec()
#Definimos la tasa libre de riesgo
setRiskFreeRate(markov)<- 0.001 #Tasa libre de riesgo
# Definimos la cantida de portafolios que queremos simular
setNFrontierPoints(markov) <- 20 #Cantidad de portafolios en frontera
#Imponemos restricciones
constraints="LongOnly"

Frontera <- portfolioFrontier(as.timeSeries(Rendimientos),spec=markov,
                              constraints )
Frontera


frontierPlot(Frontera)
grid()
tangencyPoints(Frontera, pch = 19, col = "red", cex=2)
tangencyLines(Frontera, col="grey", pch=19, cex=2)
minvariancePoints(Frontera, col="blue", pch=19, cex=2)
monteCarloPoints(Frontera, mCsteps=10, col="#0098D5", cex=0.001)
frontierPlot(Frontera,)



col <- qualiPalette(ncol(Rendimientos), "Pastel1")
weightsPlot(Frontera, col=col)


efPortfolio <- efficientPortfolio(as.timeSeries(Rendimientos),markov,constraints)
efPortfolio


tgPortfolio <- tangencyPortfolio(as.timeSeries(Rendimientos),markov,constraints)
tgPortfolio


weightsPie(efPortfolio, col=col )
mtext(text = "Portafolio eficiente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)


weightsPie(tgPortfolio, col=col)
mtext(text = "Portafolio tangente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)









