library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(egg)

setwd("D:/Desktop/UoK/Tesis/r_thesis/FvFm_ED50s")
input<-read.table("PAM_Ed_StatsAnalysis.txt",  sep = "\t", header = T)

input$Temperature=as.numeric(input$Temperature) # we need temperature as numeric
input$PAM=as.numeric(input$PAM) # PAM as numeric

#remove comment column
input<-input[, c(1:6) ]

#remove rows with missing data
input<-input[complete.cases(input), ]

#create a group to define colony ID
input$Colony=as.factor(paste(input$Timepoint,input$Treatment, sep = "_"))
levels(input$Colony)

#fit the PAM data from each colony to the response curve considering replicate number

#### Arduino T1
DRCpamrep_Ard_T1<- drm(PAM ~ Temperature, data=input[input$Colony=="T1_Arduino",], curveid=Replicate,
                fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
                upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

summary(DRCpamrep_Ard_T1)

compParm(DRCpamrep_Ard_T1, 'ED50')
compParm(DRCpamrep_Ard_T1, 'ED50', "-")

DRCpamrep_Ard_T1$coefficients[7:9]
ED(DRCpamrep_Ard_T1, c(50))[,1]

#### Inkbird T1
DRCpamrep_Ink_T1<- drm(PAM ~ Temperature, data=input[input$Colony=="T1_Inkbird",], curveid=Replicate,
                    fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
                    upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

summary(DRCpamrep_Ink_T1)

compParm(DRCpamrep_Ink_T1, 'ED50')
compParm(DRCpamrep_Ink_T1, 'ED50', "-")

DRCpamrep_Ink_T1$coefficients[7:9]
ED(DRCpamrep_Ink_T1, c(50))[,1]

#### Arduino T2
DRCpamrep_Ard_T2<- drm(PAM ~ Temperature, data=input[input$Colony=="T2_Arduino",], curveid=Replicate,
                       fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
                       upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

summary(DRCpamrep_Ard_T2)

compParm(DRCpamrep_Ard_T2, 'ED50')
compParm(DRCpamrep_Ard_T2, 'ED50', "-")

DRCpamrep_Ard_T2$coefficients[7:9]
ED(DRCpamrep_Ard_T2, c(50))[,1]

#### Inkbird T2
DRCpamrep_Ink_T2<- drm(PAM ~ Temperature, data=input[input$Colony=="T2_Inkbird",], curveid=Replicate,
                    fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
                    upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

summary(DRCpamrep_Ink_T2)

compParm(DRCpamrep_Ink_T2, 'ED50')
compParm(DRCpamrep_Ink_T2, 'ED50', "-")

DRCpamrep_Ink_T2$coefficients[7:9]
ED(DRCpamrep_Ink_T2, c(50))[,1]

# merging ED50 groups

RepCoeffs<-data.frame("ED50"=c(DRCpamrep_Ard_T1$coefficients[7:9],DRCpamrep_Ard_T2$coefficients[7:9], DRCpamrep_Ink_T1$coefficients[7:9], DRCpamrep_Ink_T2$coefficients[7:9]), "Colony"=c(rep("T1_Arduino",3), rep("T2_Arduino",3), rep("T1_Inkbird",3), rep("T2_Inkbird",3)), "Timepoint"=c(rep("T1",3), rep("T2", 3), rep("T1",3), rep("T2", 3)), "Replicate"=c(rep("1",1), rep("2",1), rep("3", 1), rep("1",1), rep("2",1), rep("3", 1)))
write.table(RepCoeffs, "RepCoeffs_ED50s.txt", sep= "\t")
aggregate(ED50 ~ Colony, data=RepCoeffs, mean)

#testing for normality
aggregate(ED50 ~ Replicate, data=RepCoeffs, FUN= function(x) shapiro.test(x)$p.value)

shapiro.test(RepCoeffs$ED50)

#T2_Arduino is not normal but overall ED50 data distributes normally

#testing for unequal variance
bartlett.test(ED50 ~ Colony, data=RepCoeffs)

#Fail to reject null hypothesis, assume homogeneity of variences

RepCoeffs$transformed_ED50s<-sqrt(max(1+RepCoeffs$ED50)-RepCoeffs$ED50) #try different transformations, vegan permanova 

#separate into T1 and T2 then run ttests
RepCoeffs_T1<- subset(RepCoeffs, RepCoeffs$Timepoint == "T1")
ED50.ttest<- t.test(transformed_ED50s ~ Colony, data=RepCoeffs_T1, alternative= "greater") #alternative of greater makes it one-tailed which is good for small sample size
ED50.ttest
ED50.ttest<- t.test(ED50 ~ Colony, data=RepCoeffs_T1, alternative= "greater") #use transformed data or not?
ED50.ttest
ED50.ManWhit<- wilcox.test(ED50 ~ Colony, data=RepCoeffs_T1)
ED50.ManWhit

RepCoeffs_T2<- subset(RepCoeffs, RepCoeffs$Timepoint == "T2")
aggregate(transformed_ED50s ~ Colony, data=RepCoeffs_T2, FUN= function(x) shapiro.test(x)$p.value)

ED50.ttest<- t.test(transformed_ED50s ~ Colony, data=RepCoeffs_T2, alternative= "greater") #alternative of greater makes it one-tailed which is good for small sample size
ED50.ttest
ED50.ttest<- t.test(ED50 ~ Colony, data=RepCoeffs_T2, alternative= "greater") #alternative of greater makes it one-tailed which is good for small sample size
ED50.ttest
ED50.ManWhit<- wilcox.test(ED50 ~ Colony, data=RepCoeffs_T2)
ED50.ManWhit

#anov_rep<- aov(transformed_ED50s ~ Colony, data= RepCoeffs)
#summary(anov_rep)
#TukeyHSD(anov_rep)

