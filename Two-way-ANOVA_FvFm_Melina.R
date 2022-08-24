setwd("D:/Desktop/UoK/Tesis/r_thesis/FvFm_ED50s")

library(lmerTest)
library(emmeans)
library(sjPlot)
library(drc)
library(ggplot2)
library(Rmisc)

#### Load and view file
input<- read.delim("PAM_Ed_StatsAnalysis.txt", sep= '\t')
View(input)
str(input)

data_T1<-subset(input, Timepoint== "T1")
data_T2<-subset(input, Timepoint== "T2")

Fv_FmANOVA1<-aov(PAM~Temperature*Treatment, data=data_T1) 
anova(Fv_FmANOVA1)

Fv_FmANOVA2<-aov(PAM~Temperature*Treatment, data=data_T2) 
anova(Fv_FmANOVA2)

#Temperature is a significant factor
#Treatment (Arduino vs. InkBird) is not a significant factor 

#Another way to do it#

twoANOVA <-aov(PAM~Treatment * factor(Temperature), data=input)
summary(twoANOVA)
