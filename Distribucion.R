 par(mfrow=c(2,1))
 Días=0:100
 ReturnX=numeric(101)
 ReturnX[5]=-100
 ReturnX[61]=-100
 ReturnX[79]=-100
 ReturnX[95]=-100
 plot(Días,ReturnX,type="l")

 Días=0:100
 ReturnY=numeric(101)
 ReturnY[5]=-100
 ReturnY[25]=-100
 ReturnY[42]=-100
 ReturnY[60]=-100
 plot(Días,ReturnY,type="l")
 
 
 
 p = 1/5
 n = 12
 k = 4
 dbinom(k,size=n,prob=0.2)
 prob <- NULL
 for(k in 0:4){
   prob <- c(prob,dbinom(k,n,p))
   prob
 }
 prob
 sum(prob)
 sum(dbinom(0:4,n,p))
 sum(dbinom(0:4,n,p))
 curve(dbinom(k,size=n,prob=0.2), -8,8)
 