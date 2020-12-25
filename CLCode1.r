rm(list=ls()) 
#install.packages(c("plyr", "dplyr", "ggplot2", "ggbiplot", "devtools", "ggfortify", "tidyverse", "grid", "scales"))
#install.packages("tidyverse", lib = "C:\\Program Files\\R\\R-3.4.2\\library")
# install.packages("Hmisc")
library(plyr)
library(dplyr)
library(ggplot2)
library(ggbiplot)
library(devtools)
library(ggfortify)
library(tidyverse)
library(grid)
library(scatterplot3d)
library(scales)
library(ggplot2)
library(hrbrthemes)
library(gridExtra)
library("papeR")
library(knitr)
library(e1071)
library(timeDate)
library(bizdays)
library(lubridate)
library(gplots)
library(tidyr)
library(magrittr)
library(Hmisc)
#Base daily#
setwd("C:\\Users\\Jrinaldi\\Downloads\\Tesis")
data = read.csv('OilDaily.csv', sep = ',')
data1 <- data[6:2642,c(1:9)]
names(data1)<- c("Date", "CL1", "CL2", "CL3", "CL4", "CL5", "CL6", "CL7", "CL8")
data1$CL1 <- as.numeric(as.character(data1$CL1))
data1$CL2 <- as.numeric(as.character(data1$CL2))
data1$CL3 <- as.numeric(as.character(data1$CL3))
data1$CL4 <- as.numeric(as.character(data1$CL4))
data1$CL5 <- as.numeric(as.character(data1$CL5))
data1$CL6 <- as.numeric(as.character(data1$CL6))
data1$CL7 <- as.numeric(as.character(data1$CL7))
data1$CL8 <- as.numeric(as.character(data1$CL8))
data1$CAL1 <- as.numeric(as.character(data1$CL2)) - as.numeric(as.character(data1$CL1))
data1$CAL2 <- as.numeric(as.character(data1$CL4)) - as.numeric(as.character(data1$CL3))
data1$CAL3 <- as.numeric(as.character(data1$CL6)) - as.numeric(as.character(data1$CL5))
data1$CAL4 <- as.numeric(as.character(data1$CL8)) - as.numeric(as.character(data1$CL7))

data1$Date<-as.Date(data1$Date, format = "%m/%d/%Y")

###############################################
###Gráficos de series de tiempo de CL1 a CL8###
###############################################

c <- ggplot(data1, aes(x=Date, y=CL1)) 
c <- c + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c <- c + theme_ipsum()  
c <- c + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c <- c + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'), limits = as.Date(c("2010-04-01","2020-04-14")))
c <- c + scale_y_continuous(limits=as.numeric(c(0,120)))
c <- c + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c <- c+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c <- c + labs(x ="Fecha", y = "CL1")
plot(c)

c1 <- ggplot(data1, aes(x=Date, y=CL2)) 
c1 <- c1 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c1 <- c1 + theme_ipsum() 
c1 <- c1 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c1 <- c1 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c1 <- c1 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c1 <- c1+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c1 <- c1 + labs(x ="Fecha", y = "CL2")
plot(c1)

c2 <- ggplot(data1, aes(x=Date, y=CL3)) 
c2 <- c2 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c2 <- c2 + theme_ipsum()  
c2 <- c2 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c2 <- c2 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c2 <- c2 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c2 <- c2+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c2 <- c2 + labs(x ="Fecha", y = "CL3")
plot(c2)

c3 <- ggplot(data1, aes(x=Date, y=CL4)) 
c3 <- c3 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c3 <- c3 + theme_ipsum() 
c3 <- c3 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c3 <- c3 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c3 <- c3 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c3 <- c3+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c3 <- c3 + labs(x ="Fecha", y = "CL4")
plot(c3)

c4 <- ggplot(data1, aes(x=Date, y=CL5)) 
c4 <- c4 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c4 <- c4 + theme_ipsum()   
c4 <- c4 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c4 <- c4 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c4 <- c4 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c4 <- c4+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c4 <- c4 + labs(x ="Fecha", y = "CL5")
plot(c4)

c5 <- ggplot(data1, aes(x=Date, y=CL6)) 
c5 <- c5 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c5 <- c5 + theme_ipsum()  
c5 <- c5 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c5 <- c5 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c5 <- c5 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c5 <- c5+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c5 <- c5 + labs(x ="Fecha", y = "CL6")
plot(c5)

c6 <- ggplot(data1, aes(x=Date, y=CL7)) 
c6 <- c6 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c6 <- c6 + theme_ipsum()   
c6 <- c6 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c6 <- c6 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c6 <- c6 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c6 <- c6+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c6 <- c6 + labs(x ="Fecha", y = "CL7")
plot(c6)

c7 <- ggplot(data1, aes(x=Date, y=CL8)) 
c7 <- c7 + geom_line( color="gray9", size=2, alpha=0.9, linetype=1)
c7 <- c7 + theme_ipsum()  
c7 <- c7 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c7 <- c7 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
c7 <- c7 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c7 <- c7+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c7 <- c7 + labs(x ="Fecha", y = "CL8")
plot(c7)

grid.arrange(c, c1, c2, c3, nrow=2, ncol=2) 
grid.arrange(c4, c5, c6, c7, nrow=2, ncol=2)

###############################################
#Gráficos de series de tiempo de los calendars#
###############################################

p <- ggplot(data1, aes(x=Date, y=CAL1)) 
p <- p + geom_line( color="midnightblue", size=2, alpha=0.9, linetype=1)
p <- p + theme_ipsum() #+ ggtitle("CL2-CL1 desde 04-2010 hasta 04-2020, valores diarios")  
p <- p + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
  )  
p <- p + scale_y_continuous(limits=as.numeric(c(-3,3)))
p <- p + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('6 months'),limits = as.Date(c("2010-04-14","2020-04-14")))
p <- p + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
p <- p+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p <- p + labs(x ="Fecha", y = "CL2-CL1")
plot(p)

p1 <- ggplot(data1, aes(x=Date, y=CAL2)) 
p1 <- p1 + geom_line( color="midnightblue", size=2, alpha=0.9, linetype=1)
p1 <- p1 + theme_ipsum() #+ ggtitle("CL4-CL3 desde 04-2010 hasta 04-2020, valores diarios")  
p1 <- p1 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
p1 <- p1 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
p1 <- p1 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
p1 <- p1 + theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p1 <- p1 + labs(x ="Fecha", y = "CL4-CL3")
plot(p1)

p2 <- ggplot(data1, aes(x=Date, y=CAL3)) 
p2 <- p2 + geom_line( color="midnightblue", size=2, alpha=0.9, linetype=1)
p2 <- p2 + theme_ipsum() #+ ggtitle("CL6-CL5 desde 04-2010 hasta 04-2020, valores diarios")  
p2 <- p2 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
p2 <- p2 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
p2 <- p2 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
p2 <- p2 + theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p2 <- p2 + labs(x ="Fecha", y = "CL6-CL5")
plot(p2)

p3 <- ggplot(data1, aes(x=Date, y=CAL4)) 
p3 <- p3 + geom_line( color="midnightblue", size=2, alpha=0.9, linetype=1)
p3 <- p3 + theme_ipsum() #+ ggtitle("CL8-CL7 desde 04-2010 hasta 04-2020, valores diarios")  
p3 <- p3 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
p3 <- p3 + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('8 months'))
p3 <- p3 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
p3 <- p3 + theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
p3 <- p3 + labs(x ="Fecha", y = "CL8-CL7")
plot(p3)


grid.arrange(p, p1, p2, p3, nrow=2, ncol=2)

###############################################
#Tablas de estadísticas descriptivas###########
###############################################

data2 <- data1[-c(2620:2637), c(10:13)] 
summarize(data2, type = "numeric")
print(xtable(summarize(data2, type = "numeric")))
#Procesar salida en Overleaf o cualquier IDE de Latex formateando las columnas

###############################################
#Densidades####################################
###############################################

h <- qplot(data2$CAL1, geom = "density",  
           xlab = "CAL1",  
           fill=I("midnightblue"), 
           col=I("midnightblue"), 
           alpha=I(1),
           xlim=c(-3,3)) 
h <- h + theme(axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
h <- h + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
h <- h+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
h <- h + labs(x ="CAL1")
plot(h)

h1 <- qplot(data2$CAL2, geom = "density",  
           xlab = "CAL2",  
           fill=I("midnightblue"), 
           col=I("midnightblue"), 
           alpha=I(1),
           xlim=c(-3,3)) 
h1 <- h1 + theme(axis.title.x = element_text(size=12, face="bold",hjust=0.5),
               axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
h1 <- h1 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
h1 <- h1+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
h1 <- h1 + labs(x ="CAL2")
plot(h1)

h2 <- qplot(data2$CAL3, geom = "density",  
            xlab = "CAL3",  
            fill=I("midnightblue"), 
            col=I("midnightblue"), 
            alpha=I(1),
            xlim=c(-3,3)) 
h2 <- h2 + theme(axis.title.x = element_text(size=12, face="bold",hjust=0.5),
                 axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
h2 <- h2 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
h2 <- h2+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
h2 <- h2 + labs(x ="CAL3")
plot(h2)

h3 <- qplot(data2$CAL4, geom = "density",  
            xlab = "CAL4",  
            fill=I("midnightblue"), 
            col=I("midnightblue"), 
            alpha=I(1),
            xlim=c(-3,3)) 
h3 <- h3 + theme(axis.title.x = element_text(size=12, face="bold",hjust=0.5),
                 axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
h3 <- h3 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
h3 <- h3+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
h3 <- h3 + labs(x ="CAL4")
plot(h3)


grid.arrange(h, h1, h2, h3, nrow=2, ncol=2)

#Curtosis y valores mopdales###################

kurtosis(data2$CAL1) 
kurtosis(data2$CAL2) 
kurtosis(data2$CAL3)
kurtosis(data2$CAL4) 

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(data2$CAL1)
getmode(data2$CAL2)
getmode(data2$CAL3)
getmode(data2$CAL4)

###############################################
#Patrones en torno a los rollovers#############
###############################################
data2 <- data1[-c(2620:2637), c(1,10:13)] 
data2$Date<-as.Date(data2$Date, format = "%m/%d/%Y")
data2$days2roll = numeric(length(data2$Date))

# Creo variable days2roll, que indica cuantos trading days quedan para el roll. Vale 0 el último día del mes

for (i in 1:length(data2$Date)){
  
  m = month(data2$Date[i])
  
  t = 0
  
  while (m == month(data2$Date[i+t+1]) & i < 2610){
    
    t = t+1
    
  }
  
  data2$days2roll[i] = t
  
}

nbr_rolls = sum(data2$days2roll==14)

mat1 = matrix(nrow = nbr_rolls, ncol = 14*2+1)

mat3 = matrix(nrow = nbr_rolls, ncol = 14*2+1)

mat5 = matrix(nrow = nbr_rolls, ncol = 14*2+1)

mat7 = matrix(nrow = nbr_rolls, ncol = 14*2+1)

roll = 0 #Numero de roll

for (i in 1:length(data2$Date)){
  
  if (data2$days2roll[i] == 14){
    
    roll = roll + 1
    
    mat1[roll,] = data2$CAL1[i:(i+14*2)]-data2$CAL1[i+14]
    
    mat3[roll,] = data2$CAL2[i:(i+14*2)]-data2$CAL2[i+14]
    
    mat5[roll,] = data2$CAL3[i:(i+14*2)]-data2$CAL3[i+14]
    
    mat7[roll,] = data2$CAL4[i:(i+14*2)]-data2$CAL4[i+14]
    
  }
  
}


# Agrupo todos los 120 rolls en dataframes con t+/-14 como columnas

dfCAL1 = as.data.frame(mat1)

dfCAL2 = as.data.frame(mat3)

dfCAL3 = as.data.frame(mat5)

dfCAL4 = as.data.frame(mat7)

names(dfCAL1)<- c("-14", "-13", "-12", "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
names(dfCAL2)<- c("-14", "-13", "-12", "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
names(dfCAL3)<- c("-14", "-13", "-12", "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")
names(dfCAL4)<- c("-14", "-13", "-12", "-11", "-10", "-9", "-8", "-7", "-6", "-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")

# Reshaping las matrices, preparando para graficar

dfCAL1_long <- gather(dfCAL1, Days2roll, dfCAL1, factor_key=TRUE)
ggplot(dfCAL1_long, aes(x=x = Days2roll, y=dfCAL1))+ 
  #stat_summary(fun.data='mean_sdl',
               fun.args = list(mult = 1),
               geom = 'smooth', se = TRUE)#+
  stat_summary(geom="line", fun.y='mean', linetype="dashed")#+
  stat_summary(geom="point", fun.y='mean', color="red")

#dfCAL1Stats = as.data.frame(tapply(dfCAL1_long$dfCAL1, dfCAL1_long$Days2roll, summary))




# dfCAL1Means = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = mean)
# dfCAL1SD = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = sd)
# names(dfCAL1Means)<- c("days", "Mean")
# names(dfCAL1SD)<- c("days", "SD")
# 
# m <- ggplot(dfCAL1Means, aes(x=days, y=Mean)) 
# m <- m + geom_line( color="black", size=2, alpha=0.9, linetype=1)
# m <- m + theme_ipsum()  
# m <- m + theme(
#   plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
#   axis.title.x = element_text(size=12, face="bold",hjust=0.5),
#   axis.title.y = element_text(size=12, face="bold",hjust=0.5)
# )  
# m <- m + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) 
# m <- m + scale_y_continuous(limits=as.numeric(c(-0.05,0.05)))
# m <- m + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
# m <- m+ theme(
#   panel.background = element_rect(fill = "gray90", colour = "white",
#                                   size = 2, linetype = "solid"),
#   panel.grid.major = element_line(size = 0.5, linetype = 'solid',
#                                   colour = "white"), 
#   panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
#                                   colour = "white")
# )
# m <- m + labs(x ="Días", y = "C(t)-C(0)")
# 
# 
# plot(m)


#CAL1Stats <- merge(dfCAL1Means,dfCAL1SD,by="days")
#CAL1Stats$days <- as.numeric(as.character(CAL1Stats$days))



#ddply(dfCAL1_long, "Days2roll", summarise, mean = mean(dfCAL1))


