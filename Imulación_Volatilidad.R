############################################################################
####                                                                    ####
####  Volatilidad Histórica para la Fijación de precios de una Acción   ####
####                                                                    ####
############################################################################


set.seed(6360)
S <- 1065
r <- 0.0125
vol <- 0.2
a <- 0.95
c <- 0.15
p <- -0.3
K <- seq(100,2000,100)
TTM <- 1
t <- 1/250
shock_1 <- matrix(0, ncol = 1000, nrow = 250)
for (i in 1:ncol(shock_1)) {
 shock_1[,i] <- rnorm(250, mean = 0, sd = 1)
}
shock_2 <- matrix(0, ncol = 1000, nrow = 250)
for (i in 1:ncol(shock_2)) {
 shock_2[,i] <- rnorm(250, mean = 0, sd = 1)
}


vol_matrix <- matrix(0, ncol = 1000, nrow = 250)
colnames(vol_matrix) <- paste("Sim", 1:ncol(vol_matrix))
rownames(vol_matrix) <- paste("Day", 1:250)
vol_null <- vol**2
for (j in 1:ncol(vol_matrix)) {
  
  vol_matrix[1,j] <- vol_null+c*sqrt(t*abs(vol_null))*(p*shock_1[1,j]+sqrt(1-p^2)*shock_2[1,j])
  
  for (i in 2:nrow(vol_matrix)) {
    vol_matrix[i,j] <- vol_matrix[i-1,j] + a*(vol_null-vol_matrix[i-1,j])*t+c*
      sqrt(t*abs(vol_matrix[i-1,j]))*(p*shock_1[i,j] + sqrt(1-p^2)*shock_2[i,j])
  }
}


## modelación de cambios de precios usando BS

price_matrix <- matrix(0, ncol = 1000, nrow = 250)
colnames(price_matrix) <- paste("Sim", 1:ncol(price_matrix))
rownames(price_matrix) <- paste(" Day", 1:250)
price_null <- S    
for (j in 1:ncol(price_matrix)) {
  
  price_matrix[1,j] <-price_null*exp((r-0.5*vol_null)*t+sqrt(t*vol_null)*shock_1[1,j])
  
  for (i in 2:nrow(price_matrix)) {
    price_matrix[i,j] <- price_matrix[i-1,j]*exp((r-0.5*abs(vol_matrix[i-1,j]))*t + 
                                                   sqrt(t*abs(vol_matrix[i-1,j]))*shock_1[i,j])
  }
}

   matplot(price_matrix, type = 'l',pch = NULL,
       main = '1000 Simulación de por el método de volatilidad histórica',
       xlab = "Días",
       ylab = 'Previo')

   
   
   
   
   