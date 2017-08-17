#create a figure or series of figures that tell interesting story about data
#What is the best time of day to fly to minimize delays?
#What is the best time of year to fly to minimize delays?
#How do patterns of flights to different destinations or parts of the country change over the course of the year?
#What are the bad airports to fly to?

library(ggplot2)
library(lattice)
library(grid)
library(gridExtra)

airline<- read.csv('https://raw.githubusercontent.com/jgscott/STA380/master/data/ABIA.csv',header=TRUE)
names(airline)
attach(airline)

###departure delays###
dest_dep_delay<-aggregate(DepDelay~Origin, airline, mean)

dest_dep_delay[order(dest_dep_delay$DepDelay),]

top20worst_depdelays<-tail(dest_dep_delay[order(dest_dep_delay$DepDelay),],20)
top20worst_depdelays$Origin <- factor(top20worst_depdelays$Origin, levels = top20worst_depdelays$Origin[order(top20worst_depdelays$DepDelay)])

ggplot(top20worst_depdelays, aes(Origin, DepDelay)) + geom_bar(stat='identity')

####loop for all the months###  

p<-list()
for (i in 1:12) {
  dest_dep_delay<-aggregate(DepDelay~Origin, subset(airline, airline['Month']==i), mean)
  #sort(dest_dep_delay$DepDelay, decreasing=TRUE)
  dest_dep_delay[order(dest_dep_delay$DepDelay),]
  
  top20worst_depdelays<-tail(dest_dep_delay[order(dest_dep_delay$DepDelay),],10)
  top20worst_depdelays$Origin <- factor(top20worst_depdelays$Origin, levels = top20worst_depdelays$Origin[order(top20worst_depdelays$DepDelay)])
  
  par(mfrow=c(3,4))
  p[[i]]<-ggplot(top20worst_depdelays, aes(Origin, DepDelay)) + geom_bar(stat='identity')+ggtitle(i)+scale_fill_brewer(palette='Blues')
}
do.call(grid.arrange,p)


