#Diagrama de Causa y Efecto- Diagrama de Pescado
# Paso o se cargan las paquetes indicados
library(ggplot2)
library(qcc) 
library(SixSigma)
library(ggQC)

### Paso 1 se define el objetivo 
### 
#Objetivo
effect<-'Aprobación del curso de\n Gestión de Proyectos'

#Paso 2 Se identifican los porsibles riesgos que eviten el alcance del objetivo
causes.head<-c('Recursos', 'Docentes', 'Estudiantes',
               'Contenidos Curriculares', 'Institución Educativa', 'Medio Ambiente')

#Paso 3 Se evaluan los posibles riesgos 
causes<-vector(mode = 'list', length = length(causes.head))
causes[1]<-list(c('Acceso a los equipos de computo', 'Pocos libros especializados'))
causes[2]<-list(c('Uso inadecuado de los recursos', 'Procesos de evaluación inadecuado', 'Estrategias de clase inadecuadas', 'Especialista en el tema'))
causes[3]<-list(c('Poco interés', 'Uso del celular en clase', 'No busca ayuda del docente', 'No asiste', 'No hace y no entrega tareas'))
causes[4]<-list(c('Demasiados temas', 'Poco tiempo para temas dificiles', 'Contenidos Inadecuados','COntinidos no actualizados'))
causes[5]<-list(c('Poca capacitación de los docentes','Negligencia en el manejo de los problemas', 'Desconocimiento del problema'))
causes[6]<-list(c('Reuniones con compañeros de la escuela', 'Distracción por personas en el celular', 'Ruido externo', 'Inseguridad'))

#Paso 4 Se realiza el Diagrama de pescado
ss.ceDiag(effect,causes.head,causes,sub='Autor: Felipe Pérez Vargas', ss.col = c('blue','red'))
#### Paso 5 Se evaluan los riesgos por ejemplo FMAE(Failure Mode Analysis and Effects - Análisis de Modo de Falla y Efecto)


  




######################################################################################


effect <- "Objetivo"
causes.gr <- c("R1", "R2", "R3", "R4", 
               "R5", "R6", 'R-n')
causes <- vector(mode = "list", length = length(causes.gr))
causes[1] <- list(c("C1", "C2", "C3"))
causes[2] <- list(c("C1", "C2", "C3"))
causes[3] <- list(c("C1", "C2", "C3"))
causes[4] <- list(c("C1", "C2", "C3"))
causes[5] <- list(c("C1", "C2", "C3"))
causes[6] <- list(c("C1", "C2", "C3"))
ss.ceDiag(effect, causes.gr, causes, sub = "Autor")

############################ Reducción de dimensionalidad #############################################

RPN <- c(512,430,480,350,80,20, 50,490,200,10)
Causas <- c('ER1', 'ER2', 'ER3', 'ER4', 'ER5', 'ER6','ER7', 'ER8', 'ER9','ER10')
myDf <- data.frame(count=RPN, Riesgo=Causas, stringsAsFactors = FALSE)
myDf <- myDf[order(myDf$count, decreasing=TRUE), ]
myDf$Causas <- factor(myDf$count, levels=myDf$count)
myDf$cumulative <- cumsum(myDf$count)
ggplot(myDf, aes(x=myDf$Causas))+
  geom_bar(aes(y=myDf$count), fill='blue', stat="identity")+
  geom_point(aes(y=myDf$cumulative/10), color = rgb(0, 1, 0), pch=16, size=1) +
  geom_path(aes(y=myDf$cumulative/10, group=1), colour="slateblue1", lty=3, size=0.9) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title = "Grafica de Pareto ", subtitle = "Reducción de Dimensión",
       x = 'Causas', y ='RPN')
  
  

Risk <- data.frame(
  KPI = c('ER1', 'ER2', 'ER3', 'ER4', 'ER5', 'ER6','ER7', 'ER8', 'ER9','ER10'),
  Time = c(512,430,480,350,80,20, 50,490,200,10)) 
ggplot(Risk, aes(x = KPI, y = Time)) +
 stat_pareto(point.color = "red",
                    point.size = 3,
                    line.color = "black",
                    bars.fill = c("blue", "orange"),
                    show.legend = NA) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+
  theme(plot.title = element_text(hjust = 0.5))
  

counts <- c(512,430,480,350,80,20, 50,490,200,10)
tags <- c('ER1', 'ER2', 'ER3', 'ER4', 'ER5', 'ER6','ER7', 'ER8', 'ER9','ER10')
df <- data.frame(counts=counts,tags=tags,stringsAsFactors = FALSE)
df <- df[order(df$counts,decreasing=TRUE), ]
df$tags <- factor(df$tags, levels=df$tags)
df$cumulative <- cumsum(df$counts)
df$cumulative <- 100 * df$cumulative/tail(df$cumulative, n=1)
scaleRight <- tail(df$cumulative, n=1)/head(df$counts, n=1)
ggplot(df, aes(x=df$tags)) +
  geom_bar(aes(y=df$counts), fill='deepskyblue4', stat="identity") +
  geom_path(aes(y=df$cumulative/scaleRight, group=1),colour="red", size=0.9) +
  geom_point(aes(y=df$cumulative/scaleRight, group=1),colour="red") +
  scale_y_continuous(sec.axis = sec_axis(~.*scaleRight, name = "Acumulado (%)")) +
  theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
  labs(title="Reducción de Dimensión ", subtitle="Riesgo de NO aprobar una Materia", x="", y='')

  