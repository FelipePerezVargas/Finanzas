rm(list=ls())
library(ggplot2)
library(plotly)


beta<-seq(0,2,by=0.001)
rm<-4.3886
tlr<-2.4155
re<-3.73734435
bbajio<-0.67

capm<-tlr+beta*(rm-tlr)
r<-data.frame(beta,capm)

p<-ggplot(r, aes(beta, capm))+ geom_line()+geom_point()+
  geom_hline(yintercept = c(re, 2.4155))+geom_vline(xintercept = c(0.67));p
p<-ggplotly(p);p
