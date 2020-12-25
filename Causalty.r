rm(list=ls()) 
# install.packages("xts")
# library(xts)
#Base daily#
setwd("C:\\Users\\Jrinaldi\\Desktop\\Reflexiones\\Tesis\\Tesis MFIN\\XLS")
data = read.csv('NetPosi.csv', sep = ',')
# class(data$Dates)
data$Dates<-as.Date(data$Dates, format = "%m/%d/%Y")
# class(data$Dates)

c <- ggplot(data, aes(x=Dates, y=CALENDAR1)) 
c <- c + geom_line( color="gray9", size=2, alpha=0.9, linetype=1) +ggtitle("CAL1") 
c <- c + theme_ipsum()  
c <- c + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
c <- c + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('24 weeks'), limits = as.Date(c("2010-04-02","2020-04-02")))
c <- c + scale_y_continuous(limits=as.numeric(c(-3,4)))
c <- c + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
c <- c+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
c <- c + labs(x ="Fecha", y = NULL )
plot(c)

e <- ggplot(data, aes(x=Dates, y=ESPECULADORES.NET)) 
e <- e + geom_line( color="darkgreen", size=2, alpha=0.9, linetype=1) +ggtitle("ESPECULADORES: Posiciones netas de compra") 
e <- e + theme_ipsum()  
e <- e + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
e <- e + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('24 weeks'), limits = as.Date(c("2010-04-02","2020-04-02")))
e <- e + scale_y_continuous(limits=())
e <- e + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
e <- e+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
e <- e + labs(x ="Fecha", y= NULL)
plot(e)


co <- ggplot(data, aes(x=Dates, y=COBERTURISTAS.NET)) +ggtitle("COBERTURISTAS: Posiciones netas de compra") 
co <- co + geom_line( color="red", size=2, alpha=0.9, linetype=1)
co <- co + theme_ipsum()  
co <- co + theme(
  plot.title = element_text(family="Times", face="bold", size=16,hjust=0.5),
  axis.title.x = element_text(size=12, face="bold",hjust=0.5),
  axis.title.y = element_text(size=12, face="bold",hjust=0.5)
)  
co <- co + theme(axis.line = element_line(colour="black", size=1, linetype="solid")) + scale_x_date(labels = date_format("%m-%y"), breaks = date_breaks('24 weeks'), limits = as.Date(c("2010-04-02","2020-04-02")))
co <- co + scale_y_continuous(limits=())
co <- co + theme(axis.text.x=element_text(angle=45,vjust = 1, hjust =1)) 
co <- co+ theme(
  panel.background = element_rect(fill = "gray90", colour = "white",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
co <- co + labs(x ="Fecha", y = NULL)
plot(co)

grid.arrange(c, e, co, nrow=3, ncol=1)


# Como series de tiempo
dataTS <- xts(data[,2:4], order.by=as.Date(data[,1]),"%m/%d/%Y")
class(dataTS)
start(dataTS)
end(dataTS)

frequency(dataTS)
summary(dataTS) #Para análisis exploratorio de datos
plot(dataTS$CALENDAR1)


names(data)<- c("Fecha", "CAL1", "ESP_NET", "COB_NET")
NET<-ts(data)
end(NET)

