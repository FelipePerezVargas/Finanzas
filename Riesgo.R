rm(list=ls())

###################################################
###                                             ###
###           Distribución Binomial             ###
###                                             ###
###################################################

#Un agente de seguros vende polizas a cinco personas de la misma edad 
#y que disfrutan de buena salud. Segun las tablas actuales, la probabilidad 
#de que una persona en estas condiciones viva 30 años o mas es de 2/3. 
#Hallese la probabilidad de que, transcurridos 30 años vivan:

n <- 5 #Número de ensayos
p<- 0.8 #Probailidad 
x <- 0:n # K número de éxitos
#par(mfrow=c(2,2))

prob <- dbinom(x,n,p);prob #Función de Probabilidad
proba<-pbinom(x, size = n, prob = p);proba #Funciónm de distribución
probp<-qbinom(x, size = n, prob = p);probp # Percentiles 
barplot(prob,col = "red",ylim = c(0,.4),names.arg=x,main="Distribución binomial")

#####  Simulación  ######
xVal<-names(table(rbinom(10000,5,0.6)));xVal
barplot(as.vector(table(rbinom(100,5,0.6))),
        main="Simulación Distribución Binomial\n")


#Un estudio determinó que 40$\%$ de los alumnos de una universidad
#comen en alguna de las cafeterías de tu carrera. Si una tarde se escogen
#al azar 8 estudiantes de dicha carrera, determina la probabilidad de que
#hayan comido en alguna de las cafeterías de tu campus:

n <- 8 #Número de ensayos
p<- 0.4 #Probailidad 
x <- 0 # K número de éxitos

prob <- dbinom(x,n,p);prob
sum(dbinom(x,n,p))
barplot(prob,col = "red",ylim = c(0,.8),
        main="Distribución binomial")

#####  Simulación  ######
xVal<-names(table(rbinom(100,8,0.4)));xVal
barplot(as.vector(table(rbinom(100,8,0.4))),
        main="Simulación Distribución Binomial\n")

#Si se auditan 12 empresas y la probabilidad de que una de ellas esté 
#en quiebra es de 0,15, ¿cuál es el
#número esperado de empresas en quiebra? ¿Y su desviación típica?

n <- 12 #Número de ensayos
p<- 0.15 #Probailidad 
x <- 0:n # K número de éxitos

prob <- dbinom(x,n,p);prob
sum(dbinom(x,n,p))
barplot(prob,col = "red",ylim = c(0,.8),
        main="Distribución binomial")

#####  Simulación  ######
xVal<-names(table(rbinom(100,12,0.15)));xVal
barplot(as.vector(table(rbinom(100,12,0.15))),
        main="Simulación Distribución Binomial\n")


#Si ya se conoce que solo el 3% de los alumnos de Contabilidad son muy inteligentes
#¿Calcular laprobabilidad de que si tomamos 100 alumnos al azar 5 de ellos sean muy 
#inteligentes.


##############################################################
##                                                         ###    
##                 Distribución de Poisson                 ###
##                                                         ###
##############################################################
#un banco recibe en promedio 6 cheques sin fondo por día,
#¿cuáles son las probabilidades de que reciba, a) cuatro 
#cheques sin fondo en un día dado, b) 10 cheques sin fondos
#en cualquiera de dos días consecutivos?
po<-dpois(0:5, lambda = 2)
barplot(po,col = "red",ylim = c(0,.18),
        main="Distribución Poisson")
#barplot(rpois(15, lambda = 6))

#Una compañía telefónica recibe llamadas a razón de 4 por minuto.
#calcular la probabilidad de :
#a)Recibir 2 llamadas en un minuto
#b)No recibir ninguna llamada en un minuto
#c)Recibir menos de 3 llamadas en un minuto
#d) Recibir más de 3 llamadas en un minuto
po<-dpois(0:2, lambda = 4);po
sum(po)
barplot(po,col = "red",ylim = c(0,.18),
        main="Distribución Poisson")
table(dpois(0:2, lambda = 4))

#En una empresa el término medio de accidentes es de 3 por mes
#Calcular la probabilidad de
#a) Que no ocurra ningún accidente en un mes
#b) que como máximo ocurran 2 accidentes en un mes
#c) Que ocurran 30 accidentes en un año
#d) que ocurran 8 accidentes en un trimestre
po<-dpois(0:30, lambda = 3);po
sum(po)
barplot(po,col = "red",ylim = c(0,.25),
        main="Distribución Poisson")


#### Simulación Poisson ####
rpois(0:3, lambda = 3)



#################  Distribución Hipergeometrica #########################
#De un grupo de 20 ingenieros con doctorado, se eligen 10 aleatoriamente 
#con el fin de contratarlos. ¿Cuál es la probabilidad de que entre los 10 
#seleccionados, estén los 5 mejores del grupo de 20?
N<-20 #Muestra 
M<-10
n<-20-10 #n=N-M
k<-5 #Subconjunto 
x <- 0:5

Po<- dhyper(x=x, m=M, n=n, k=k);Po
barplot(Po)


barplot(rhyper(0:5, 20, 10, 5))

#Suponga que un proceso de control de calidad se
#inspecciona un lote de 10 artículos, de los cuales
#son defectuosos. Si se eligen 5 artículos al
#azar y sin reemplazo.
#a)Calcule la probabilidad de elegir 2 artículos defectuosos
#b)Calcule la probabilidad de elegir a los más 2 arrtículos 
#defectuosos
x<-0:2
N<-10
M<-4
k<-5
Po<- dhyper(x=x, m=M, n=N-M, k=k);Po  #a)
barplot(Po) 
sum(Po)  #b)


######## Simulación 
set.seed(53535)
N <- 100
y_rhyper <- rhyper(N, m = 50, n = 20, k = 30) 
hist(y_rhyper,breaks = 70,main = "")
     
#En una línea de control de calidad se revisan 10 artículos determinando que hay 3 que
#no cumplen con las especificaciones. Si se escogen al azar 2 artículos identifique los
#parámetros de la ley y halle la esperanza de la variable aleatoria, que describe el
#número de piezas correctas de las dos escogidas.     

Po<- dhyper(x=0, m=7, n=3, k=2)
P0


library(ggplot2)
library(dplyr)
options(scipen = 999, digits = 2) # sig digits
x = 14
m = 70
n = 30
k = 20
density = dhyper(x = 1:20, m = m, n = n, k = k)
data.frame(red = 1:20, density) %>%
  mutate(red14 = ifelse(red == 14, "x = 14", "other")) %>%
 ggplot(aes(x = factor(red), y = density, fill = red14)) +
  geom_col() +
  geom_text(
    aes(label = round(density,2), y = density + 0.01),
    position = position_dodge(0.9),
    size = 3,
    vjust = 0
  ) +
  labs(title = "PMF of X = x Red Balls",
       subtitle = "Hypergeometric(k = 20, M = 70, N = 30)",
       x = "Number of red balls (x)",
       y = "Density")
p<-ggplotly(p);p

#xVal<-names(table(rbinom(1000,10,0.8)))
#barplot(as.vector(table(rbinom(1000,10,.8))),names.arg =xVal,
#          main="Simulated Binomial Distribution\n (n=10,p=0.5)")

#dpois(0:30, lambda = 12)
#ppois(10:20, lambda = 12)
#qpois(seq(0.1, 0.9, 0.1), lambda = 12)

library(tidyr)
library(ggplot2)
library(dplyr)
options(scipen = 999, digits = 2) # sig digits

x = 14
m = 7000
n = 3000
k = 20

d_binom <- dbinom(x = 1:20, size = k, prob = m / (m + n))
df_binom <- data.frame(x = 1:20, Binomial = d_binom)
p <- ggplot(df_binom, aes(x = x, y = Binomial)) +
  geom_col()

d_hyper_100 <- dhyper(x = 1:20, m = 70, n = 30, k = k)
d_hyper_250 <- dhyper(x = 1:20, m = 175, n = 75, k = k)
d_hyper_500 <- dhyper(x = 1:20, m = 350, n = 150, k = k)
d_hyper_1000 <- dhyper(x = 1:20, m = 700, n = 300, k = k)
df_hyper = data.frame(x = 1:20, 
                      Hyper_100 = d_hyper_100, 
                      Hyper_250 = d_hyper_250, 
                      Hyper_500 = d_hyper_500, 
                      Hyper_1000 = d_hyper_1000)
df_hyper_tidy <- gather(df_hyper, key = "dist", value = "density", -c(x))
p + 
  geom_line(data = df_hyper_tidy, aes(x = x, y = density, color = dist)) +
  labs(title = "Hypergeometric Distribution Appoximation to Binomial",
       subtitle = "Hypergeometric approaches Binomial as population size increases.",
       x = "Number of successful observations (x)",
       y = "Density")






#En lo anterior hemos visto que la función de probabilidad viene precedida de
#la letra d, la función de distribución de la letra p, los percentiles de la letra q
#y finalmente generamos con la función que empieza por r. Esto es así en R para
#cualquier distribuci´on de probabilidad.


##########################################################################
###                                                                    ###
###                         Distribución Normal                        ###
###                                                                    ###
##########################################################################

pnorm(c(27), mean=28, sd=1, lower.tail=TRUE)
qnorm(p = 0.5,mean = 1,sd = 2)

#valores al azar de la distribución normal
randNorm <- rnorm(300000)
#calculo de su densidad
randDensity <- dnorm(randNorm)
#gráfica
library(ggplot2)
ggplot(data.frame(x = randNorm, y = randDensity)) + 
  aes(x = x, y = y) +
  geom_point() + 
  labs(x = "Random Normal Variable", y = "Densidad")

ggplot(data.frame(x = randNorm), aes(x = x)) +
  geom_histogram(binwidth = 0.1) +
  labs(x = "Random Normal Variable", y = "Frecuencia")

fecha<-c('Ene','Feb', 'Mar', 'Abr','May','Jun', 'Jul',
         'Ago','Sep','Oct','Nov','Dic')
precio<-c(10,10.4,11.8,12.6,12.1,12.5,12.9,11.6,11.9,12.6,12.1,11.8)
#Calculamos su valor medio
m<-mean(precio);m
# Calculamos su varianza
v<-precio-m;v
v2<-v**2;v2
#Varianza de la suma de los cuadrados
var<-sum(v2)/12;var
# Regresasr a la dimensión del precio
# La desviación tipica o stándar es
var1<-sqrt(var);var1
#La desviación estándar es el promedio de 
#variaciones del precio. 

