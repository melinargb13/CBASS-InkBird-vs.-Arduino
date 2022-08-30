setwd("D:/Desktop/UoK/Tesis/r_thesis/Zoox_Counts")

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

zoox_counts <-read.table("barplot2.txt", sep = "\t",header = TRUE)
zoox_counts$temp<-as.factor(zoox_counts$temp) #check capitals 
zoox_counts$system<-as.factor(zoox_counts$system)
zoox_counts$Sym_cm2<-as.factor(zoox_counts$Sym_cm2)

zoox_barplot<- ggplot(zoox_counts, aes(x=temp, y=Sym_cm2))
zoox_barplot+geom_bar(stat = "identity", aes(fill=system), position='dodge')+scale_fill_manual(values = c("darkgoldenrod1", "royalblue2") ) +
  theme(legend.position="bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#zoox_barplot+theme(panel.background = element_blank())
#r <- barplot(zoox_counts, col = rainbow(20),main='short y axis',ann=FALSE,axes=FALSE)
#usr <- par("usr")
#par(usr=c(usr[1:2], 4000, 200000))
#axis(2,at=seq(4000,200000,5000))

zoox_barplot +                                   # Draw ggplot2 barchart with manual y-axis
  ylim= c(4000, 2000000)

