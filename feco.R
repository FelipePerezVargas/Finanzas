#################################################
###### Funciones de Formulas Econom�tricas ######
#################################################

### Valor Futuro ###
# PV= Valor presente
# FV=Valor Futuro
# r= Interes por periodo
# N= N�mero de periiodos

FV<-function(PV,r,N){
  FV<-PV*(1+r)^{N}
  print(paste('El valor futuro es $',FV, 'Pesos'))
}
FV(100,0.05,2)


### Valor Futuro (Frecuancia de composici�n) ###
# PV= Valor presente
# FV=Valor Futuro
# rs= Interes Anual Declarado
# m= N�mero de periodos compuestos por a�o
# Na= N�mero de a�os

FV<-function(PV,rs,Na,m){
  FV<-PV*(1+rs/m)^{m*Na}
  print(paste('El valor futuro es $',round(FV,2), 'Pesos'))
}
FV(10000,0.08,2,4)

### Valor Futuro (Compuesto Continuo) ###
# PV= Valor presente
# FV=Valor Futuro
# rs= Interes Anual Declarado
# m= N�mero de periodos compuestos por a�o
# Na= N�mero de a�os

FV<-function(PV,rs,N){
  FV<-PV*exp(rs*N)
  print(paste('El valor futuro es $',round(FV,2), 'Pesos'))
}
FV(10000,0.08,2)


### Tasa Anual Efectiva ###




### Valor Futuro ###
### Valor Futuro ###
### Valor Futuro ###
### Valor Futuro ###