rm(list=ls()) 
# install.packages(c("devtools", "dplyr", "ggplot2", "ggbiplot", "devtools", "ggfortify", "tidyverse", "grid", "hrbrthemes", "papeR", "e1071", "timeDate", "bizdays", "lubridate", "gplots", "tidyr"))
# install.packages("gplots", verbose=T)
# install.packages("xtable")
#library(plyr)
#library(dplyr)
#library(ggplot2)
#library(devtools)
#library(ggfortify)
#library(tidyverse)
#library(grid)
#library(scales)
#library(hrbrthemes)
#library(gridExtra)
#library("papeR")
#library(knitr)
# library(e1071)
# library(timeDate)
# library(bizdays)
# library(lubridate)
# library(gplots)
# library(tidyr)
# library(magrittr)
# library(Hmisc)
# library(sandwich)
# library(xtable)
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
#Pruebas de normalidad#########################
###############################################

shapiro.test(data1$CAL1)
shapiro.test(data1$CAL2)
shapiro.test(data1$CAL3)
shapiro.test(data1$CAL4)


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
dfCAL2_long <- gather(dfCAL2, Days2roll, dfCAL2, factor_key=TRUE)
dfCAL3_long <- gather(dfCAL3, Days2roll, dfCAL3, factor_key=TRUE)
dfCAL4_long <- gather(dfCAL4, Days2roll, dfCAL4, factor_key=TRUE)


dfCAL1_long[is.na(dfCAL1_long)] = 0
sum(is.na(dfCAL1_long$dfCAL1))
dfCAL2_long[is.na(dfCAL2_long)] = 0
sum(is.na(dfCAL2_long$dfCAL2))
dfCAL3_long[is.na(dfCAL3_long)] = 0
sum(is.na(dfCAL3_long$dfCAL3))
dfCAL4_long[is.na(dfCAL4_long)] = 0
sum(is.na(dfCAL4_long$dfCAL4))


# Newey-West

NeweyWest(lm(dfCAL1_long$dfCAL1~1))*3480
NeweyWest(lm(dfCAL2_long$dfCAL2~1))*3480
NeweyWest(lm(dfCAL3_long$dfCAL3~1))*3480
NeweyWest(lm(dfCAL4_long$dfCAL4~1))*3480

rm(list=ls()) 
setwd("C:\\Users\\Jrinaldi\\Desktop\\Reflexiones\\Tesis\\Tesis MFIN")
data = read.csv('RollsNeweyWest.csv', sep = ',')

data <- data[, c(1,2,4,5,6,8,9,10,12,13,14,16,17)]
names(data)<- c("Días", "CAL1 Mean", "t-stat", "p-value", "CAL2 Mean", "t-stat", "p-value", "CAL3 Mean", "t-stat", "p-value","CAL4 Mean", "t-stat", "p-value")


<<results=tex>>
  xtable(data)

#Colapso a medias y cómputo de desvíos

dfCAL1Means = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = mean)
dfCAL1Means = as.data.frame(dfCAL1Means)
names(dfCAL1Means)<- c("days","C(t)-C(0)") 

dfCAL1SD = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = sd)
dfCAL1SD = as.data.frame(dfCAL1SD)
names(dfCAL1SD)<- c("days","SD") 


dfCAL1p25 = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = quantile, probs  = 0.25)
dfCAL1p25 = as.data.frame(dfCAL1p25)
names(dfCAL1p25)<- c("days","p5") 

dfCAL1p75 = aggregate( dfCAL1_long$dfCAL1 ~  dfCAL1_long$Days2roll, FUN = quantile, probs  = 0.75)
dfCAL1p75 = as.data.frame(dfCAL1p75)
names(dfCAL1p75)<- c("days","p75") 

dfCAL2Means = aggregate( dfCAL2_long$dfCAL2 ~  dfCAL2_long$Days2roll, FUN = mean)
dfCAL2Means = as.data.frame(dfCAL2Means)
names(dfCAL2Means)<- c("days","C(t)-C(0)") 

dfCAL2SD = aggregate( dfCAL2_long$dfCAL2 ~  dfCAL2_long$Days2roll, FUN = sd)
dfCAL2SD = as.data.frame(dfCAL2SD)
names(dfCAL2SD)<- c("days","SD)") 

dfCAL2p25 = aggregate( dfCAL2_long$dfCAL2 ~  dfCAL2_long$Days2roll, FUN = quantile, probs  = 0.25)
dfCAL2p25 = as.data.frame(dfCAL2p25)
names(dfCAL2p25)<- c("days","p25") 

dfCAL2p75 = aggregate( dfCAL2_long$dfCAL2 ~  dfCAL2_long$Days2roll, FUN = quantile, probs  = 0.75)
dfCAL2p75 = as.data.frame(dfCAL2p75)
names(dfCAL2p75)<- c("days","p75") 

dfCAL3Means = aggregate( dfCAL3_long$dfCAL3 ~  dfCAL3_long$Days2roll, FUN = mean)
dfCAL3Means = as.data.frame(dfCAL3Means)
names(dfCAL3Means)<- c("days","C(t)-C(0)") 

dfCAL3SD = aggregate( dfCAL3_long$dfCAL3 ~  dfCAL3_long$Days2roll, FUN = sd)
dfCAL3SD = as.data.frame(dfCAL3SD)
names(dfCAL3SD)<- c("days","SD") 

dfCAL3p25 = aggregate( dfCAL3_long$dfCAL3 ~  dfCAL3_long$Days2roll, FUN = quantile, probs  = 0.25)
dfCAL3p25 = as.data.frame(dfCAL3p25)
names(dfCAL3p25)<- c("days","p25") 

dfCAL3p75 = aggregate( dfCAL3_long$dfCAL3 ~  dfCAL3_long$Days2roll, FUN = quantile, probs  = 0.75)
dfCAL3p75 = as.data.frame(dfCAL3p75)
names(dfCAL3p75)<- c("days","p75") 

dfCAL4Means = aggregate( dfCAL4_long$dfCAL4 ~  dfCAL4_long$Days2roll, FUN = mean)
dfCAL4Means = as.data.frame(dfCAL4Means)
names(dfCAL4Means)<- c("days","C(t)-C(0)") 

dfCAL4SD = aggregate( dfCAL4_long$dfCAL4 ~  dfCAL4_long$Days2roll, FUN = sd)
dfCAL4SD = as.data.frame(dfCAL4SD)
names(dfCAL4SD)<- c("days","SD") 

dfCAL4p25 = aggregate( dfCAL4_long$dfCAL4 ~  dfCAL4_long$Days2roll, FUN = quantile, probs  = 0.25)
dfCAL4p25 = as.data.frame(dfCAL4p25)
names(dfCAL4p25)<- c("days","p25") 

dfCAL4p75 = aggregate( dfCAL4_long$dfCAL4 ~  dfCAL4_long$Days2roll, FUN = quantile, probs  = 0.75)
dfCAL4p75 = as.data.frame(dfCAL4p75)
names(dfCAL4p75)<- c("days","p75") 



#Luego de trasladar a un CSV los resultados por problemas técnicos, los levanto nuevamente como etapa previa a graficar
rm(list=ls()) 
setwd("C:\\Users\\Jrinaldi\\Desktop\\Reflexiones\\Tesis\\Tesis MFIN")
data = read.csv('RollsCSV.csv', sep = ',')

dataRolls <- data[,c(1:17)]
names(dataRolls)<- c("t", "C1_t", "DE1", "C1_5", "C1_95", "C2_t", "DE2", "C2_5", "C2_95","C3_t", "DE3", "C3_5", "C3_95","C4_t", "DE4", "C4_5", "C4_95")





r1 <- ggplot(dataRolls, aes(x=t, y=dataRolls$C1_t)) 
r1 <- r1 + geom_line(aes(y = dataRolls$C1_t), color="midnightblue", size=2, alpha=0.9, linetype=1)
r1 <- r1 + geom_line(aes(y = dataRolls$C1_5), color="red3", size=1.5, alpha=0.75,linetype = "dashed")
r1 <- r1 + geom_line(aes(y = dataRolls$C1_95), color="red3", size=1.5, alpha=0.75, linetype = "dashed")
r1 <- r1 + geom_point()
r1 <- r1 + theme_ipsum() 
r1 <- r1 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
r1 <- r1 + scale_x_continuous(minor_breaks = seq(-14, 14, 1), breaks=c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
r1 <- r1 + scale_y_continuous(minor_breaks = seq( round(min(dataRolls$C1_5), digits=2),  round(max(dataRolls$C1_95), digits=2), 0.02), breaks=seq(round(min(dataRolls$C1_5), digits=2), round(max(dataRolls$C1_95), digits=2), 0.02))
r1 <- r1 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
r1 <- r1 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
r1 <- r1+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 1, linetype = 'solid',
                                  colour = "white")
)
r1 <- r1 + labs(x ="Cantidad de días hasta fin de mes", y = "CAL1: Media de C(t)-C(0) y Q3-Q1 en USD")
plot(r1)

r2 <- ggplot(dataRolls, aes(x=t, y=dataRolls$C2_t)) 
r2 <- r2 + geom_line(aes(y = dataRolls$C2_t), color="midnightblue", size=2, alpha=0.9, linetype=1)
r2 <- r2 + geom_line(aes(y = dataRolls$C2_5), color="red3", size=1.5, alpha=0.75,linetype = "dashed")
r2 <- r2 + geom_line(aes(y = dataRolls$C2_95), color="red3", size=1.5, alpha=0.75, linetype = "dashed")
r2 <- r2 + geom_point()
r2 <- r2 + theme_ipsum() 
r2 <- r2 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
r2 <- r2 + scale_x_continuous(minor_breaks = seq(-14, 14, 1), breaks=c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
r2 <- r2 + scale_y_continuous(minor_breaks = seq( round(min(dataRolls$C2_5), digits=2),  round(max(dataRolls$C2_95), digits=2), 0.02), breaks=seq(round(min(dataRolls$C2_5), digits=2), round(max(dataRolls$C2_95), digits=2), 0.02))
r2 <- r2 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
r2 <- r2 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
r2 <- r2+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 1, linetype = 'solid',
                                  colour = "white")
)
r2 <- r2 + labs(x ="Cantidad de días hasta fin de mes", y = "CAL2: Media de C(t)-C(0) y Q3-Q1 en USD")
plot(r2)

r3 <- ggplot(dataRolls, aes(x=t, y=dataRolls$C3_t)) 
r3 <- r3 + geom_line(aes(y = dataRolls$C3_t), color="midnightblue", size=2, alpha=0.9, linetype=1)
r3 <- r3 + geom_line(aes(y = dataRolls$C3_5), color="red3", size=1.5, alpha=0.75,linetype = "dashed")
r3 <- r3 + geom_line(aes(y = dataRolls$C3_95), color="red3", size=1.5, alpha=0.75, linetype = "dashed")
r3 <- r3 + geom_point()
r3 <- r3 + theme_ipsum() 
r3 <- r3 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
r3 <- r3 + scale_x_continuous(minor_breaks = seq(-14, 14, 1), breaks=c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
r3 <- r3 + scale_y_continuous(minor_breaks = seq( round(min(dataRolls$C3_5), digits=2),  round(max(dataRolls$C3_95), digits=2), 0.02), breaks=seq(round(min(dataRolls$C3_5), digits=2), round(max(dataRolls$C3_95), digits=2), 0.02))
r3 <- r3 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
r3 <- r3 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
r3 <- r3+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 1, linetype = 'solid',
                                  colour = "white")
)
r3 <- r3 + labs(x ="Cantidad de días hasta fin de mes", y = "CAL3: Media de C(t)-C(0) y Q3-Q1 en USD")
plot(r3)


r4 <- ggplot(dataRolls, aes(x=t, y=dataRolls$C4_t)) 
r4 <- r4 + geom_line(aes(y = dataRolls$C4_t), color="midnightblue", size=2, alpha=0.9, linetype=1)
r4 <- r4 + geom_line(aes(y = dataRolls$C4_5), color="red3", size=1.5, alpha=0.75,linetype = "dashed")
r4 <- r4 + geom_line(aes(y = dataRolls$C4_95), color="red3", size=1.5, alpha=0.75, linetype = "dashed")
r4 <- r4 + geom_point()
r4 <- r4 + theme_ipsum() 
r4 <- r4 + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
r4 <- r4 + scale_x_continuous(minor_breaks = seq(-14, 14, 1), breaks=c(-14,-13,-12,-11,-10,-9,-8,-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
r4 <- r4 + scale_y_continuous(minor_breaks = seq( round(min(dataRolls$C4_5), digits=2),  round(max(dataRolls$C4_95), digits=2), 0.02), breaks=seq(round(min(dataRolls$C4_5), digits=2), round(max(dataRolls$C4_95), digits=2), 0.02))
r4 <- r4 + theme(axis.line = element_line(colour="black", size=1, linetype="solid"))
r4 <- r4 + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
r4 <- r4+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 1, linetype = "solid"),
  panel.grid.major = element_line(size = 1, linetype = 'solid',
                                  colour = "white")
)
r4 <- r4 + labs(x ="Cantidad de días hasta fin de mes", y = "CAL4: Media de C(t)-C(0) y Q3-Q1 en USD")
plot(r4)


grid.arrange(r1, r2, r3, r4, nrow=2, ncol=2)
