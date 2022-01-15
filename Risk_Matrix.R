rm(list=ls())

# Load libraries

library(ggplot2)
library(plotly)
library(dplyr)

# Load the data

risk <- read.csv("/Users/Admin/Documents/Riesgo/risk.csv")
# Creating heatmap background for Risk Matrix

# setting the score in order to calculate the risk level
Likelihood_score <- rep(c(1,2,4,6,12),5)
Consequence_score <- rep(c(1,2,4,6,12),each=5)
Likelihood <- rep(c(1:5),5)
Consequence <- rep(c(1:5),each=5)
df <- data.frame(Likelihood,Consequence)
df <- mutate(df, risk_score = Consequence_score * Likelihood_score,
             Risk = case_when(risk_score >= 0 & risk_score < 6 ~ 1,
                              risk_score >= 6 & risk_score < 12 ~ 2,
                              risk_score >= 12 & risk_score < 32  ~ 3,
                              risk_score >= 32 ~ 4) )


# plotting 
risk_p<- ggplot(df,aes(x =Likelihood, y =Consequence, fill=Risk))+
  geom_tile()+
  scale_fill_gradientn(colours = c("red", "orange","#EEEE00","#008000"),guide=FALSE)+
  scale_x_continuous(trans = "reverse",name= "Frecuencia",breaks = 0:5, expand = c(0, 0))+
  scale_y_continuous(trans = "reverse",name = "Consecuencia",breaks = 0:5, expand = c(0, 0))+
  #coord_fixed()+
  theme_bw()+
  geom_hline(yintercept = seq(1.5,5.5), color = "white")+
  geom_vline(xintercept = seq(1.5,5.5), color = "white")+
  ggtitle("Matriz Interactiva de Risgo de 5x5")+
  theme(legend.position="bottom")+
  guides(color=guide_legend(title="Plantas Seleccionadas"))+
  geom_jitter(data = risk,
              # position = "jitter",
              inherit.aes = FALSE, width= 0.3,height = 0.3,
              aes(y = Consequence,
                  x = Likelihood, 
                  col = Type,
                  text = paste("<b>ID#:</b>",ID,"<br>",
                               "<b>Risk:</b>",Riesgo,"<br>",
                               "<b>Type:</b>",Tipo,"<br>",
                               "<b>Interim Action:</b>",Interim)))+
  scale_color_manual(values = c("#9400D3","#009fdf","#aaaaaa")
  )

config(ggplotly(risk_p,tooltip = "text", width = 950,height = 750), displayModeBar=FALSE, collaborate = FALSE) %>%
  layout(margin=list(l=90,b=50),
         legend = list(orientation = "h",y = -0.15, x = 0))
