

################################################
###      Teorema de Pitagoras                ###
################################################

a<-3   #Cateto adyacente
b<-4   #Cateto opuesto
c<-sqrt(a**2+b**2) #Hipotenusa   

theta<-asin(b/c)*(180/pi)  # Ángulo con respecto la horizontal en grados
theta<-acos(a/c)*(180/pi)

print(paste("La magnitud del vector es",c,"y la dirección es",round(theta,2),'°'))


#Calculo usando la información de los vectores

# cx<-A*cos()(180/pi)
# cy<-A*sin()


l<-c(-5,-2,3,8)
po<-c(94,3,2,1)
pro<-c(0.94,0.03,0.02,0.01)
pra<-c(0.94,0.97,0.99,1.0)

qnorm(0.95)
mu<-10
sig<-5
qnorm(0.98,10,5)
mu+sig*qnorm(0.98)
qnorm(0.99,1,3)
mu+sig*qnorm(0.95)


# What is the 95% VaR a N(0,1)
qnorm(0.95,0,1)
qnorm(0.95)
# What is the 95% VaR a N(0,3)
qnorm(0.99,1,3)


hist(data$losses, col=3, main = 'Losses Histogram', xlab = 'Observations', ylab = 'Frecuency')

round(quantile(data$losses, 0.95),4)

q<-round(quantile(data$losses, 0.95,type=3),4)

length(which(data$losses>=q))

length(which(data$losses>=q))/length(data$losses)



