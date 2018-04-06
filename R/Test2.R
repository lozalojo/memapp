library(mem)
data(flucyl)

i.data <- flucyl[1]
i.method = 4
i.param = NA

# derivada

i.timing <- memtiming(i.data, i.method = i.method, i.param = i.param)
i.timing$optimum.map
i.curva.map = i.timing$map.curve
x<-i.curva.map[,1]
y<-i.curva.map[,2]
y.s<-loess(y~x)$fitted
cambio.signo<-abs(diff(sign(diff(diff(y.s)))))
if (any(cambio.signo!=0)){
  optimo<-1+which.max(cambio.signo)
  resultados<-i.curva.map[x==optimo,]
}else{
  optimo<-NA
  resultados<-rep(NA,5)
}
resultados


timdata <- memtiming(i.data, i.method = i.method, i.param = i.param)
temp1<-c(0,timdata$map.curve[,2])
y.100<-min((1:length(temp1))[round(temp1,2)==100])
y<-temp1[1:y.100]
y.s<-mem:::suavizado(y,1)
d.y<-diff(y.s)
d.x<-1:length(d.y)
dgrafgg<-data.frame(weeks=d.x, slope=d.y)

# Calculate ticks for x
axis.x.range.original <- range(dgrafgg$weeks)
axis.x.otick <- optimal.tickmarks(axis.x.range.original[1], axis.x.range.original[2], 15, i.valid.ticks=1:5, i.include.min = T)
axis.x.range <- axis.x.otick$range
axis.x.ticks <- axis.x.otick$tickmarks
axis.x.labels <- axis.x.otick$tickmarks
# Range y fix
axis.y.range.original <- range(dgrafgg$slope)
axis.y.otick <- optimal.tickmarks(axis.y.range.original[1], axis.y.range.original[2], 10)
axis.y.range <- axis.y.otick$range
axis.y.ticks <- axis.y.otick$tickmarks
axis.y.labels <- axis.y.otick$tickmarks  

gplot<-ggplot(dgrafgg) +
  geom_line(aes(x=weeks, y=slope), color="#808080", linetype=1, size=1) +
  geom_point(aes(x=weeks,y=slope), color="#808080", size=3, shape=21, fill="#808080", stroke = 0.1) +
  geom_hline(yintercept=timdata$param.param ,col="#800080",lwd=1.5, linetype=2) +
  geom_segment(aes(x = timdata$optimum.map[1], y = 0, xend = timdata$optimum.map[1], yend = max(dgrafgg$slope,na.rm=T)),col="#FFB401",lwd=1) +
  geom_segment(aes(x = min(dgrafgg$weeks), y = dgrafgg$slope[timdata$optimum.map[1]], xend = max(dgrafgg$weeks), yend =dgrafgg$slope[timdata$optimum.map[1]]),col="#FFB401",lwd=1) +
  geom_point(aes(x=timdata$optimum.map[1],y=dgrafgg$slope[timdata$optimum.map[1]]), color="#980043", size=4, shape=1) +
  scale_x_continuous(breaks=axis.x.ticks, limits = axis.x.range, labels = axis.x.labels) +
  scale_y_continuous(breaks=axis.y.ticks, limits = axis.y.range, labels = axis.y.labels) +
  labs(title = i.textMain, x = i.textX, y = i.textY) +
  ggthemes::theme_few() +
  theme(plot.title = element_text(hjust = 0.5))
p<-list(plot=gplot, gdata=dgrafgg)