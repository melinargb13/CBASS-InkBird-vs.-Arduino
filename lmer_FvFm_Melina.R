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

#define as factors
data<-transform(input, 
                Treatment=as.factor(Treatment), 
                Timepoint=as.factor(Timepoint),
                Temperature=as.factor(Temperature),
                Replicate=as.factor(Replicate))
str(data)
names(data)

#separate by timepoint
data_T1<-subset(data, Timepoint== "T1")
data_T2<-subset(data, Timepoint== "T2")

##################################################################################
#########T1 model & pairwise comparisons of temperature treatments for T1#########
##################################################################################

PAM<-lmer(PAM ~ Treatment*Temperature + (1|Replicate), data=data_T1)
sjPlot::plot_model(PAM, type="diag")
step(PAM, reduce.random=FALSE)
plot(PAM) # Residual vs Fitted values
qqnorm(resid(PAM)); qqline(resid(PAM)) # qq plot to check for normal distribution of residuals
hist(resid(PAM)) # histogram of residuals to check for normal distribution of residuals

shapiro.test(data_T1$PAM) #we assume normal distribution

PAM_final<-lmer(PAM ~ Temperature + (1|Replicate),data=data_T1)
anova(PAM)

print(emmeans(PAM_final, list(pairwise ~ Temperature)), adjust = c("bonferroni"))

##################################################################################
#########T2 model & pairwise comparisons of temperature treatments for T2#########
##################################################################################

PAM2<-lmer(PAM ~ Treatment*Temperature + (1|Replicate), data=data_T2)
sjPlot::plot_model(PAM2, type="diag")
step(PAM2, reduce.random=FALSE)
plot(PAM2) # Residual vs Fitted values
qqnorm(resid(PAM2)); qqline(resid(PAM2)) # qq plot to check for normal distribution of residuals
hist(resid(PAM2)) # histogram of residuals to check for normal distribution of residuals

shapiro.test(data_T2$PAM) # p<0.05 we do not assume normal distribution

PAM2_final<-lmer(PAM ~ Temperature + (1|Replicate),data=data_T2)
anova(PAM2_final)

print(emmeans(PAM2_final, list(pairwise ~ Temperature)), adjust = c("bonferroni"))

#################################################################################
######T1 model & pairwise comparisons of systems within each temp treatment######
#################################################################################

PAM<-lmer(PAM ~ Treatment*Temperature + (1|Replicate), data=data_T1)
sjPlot::plot_model(PAM, type="diag")
step(PAM, reduce.random=FALSE)

PAM_final<-lmer(PAM ~ Temperature * Treatment + (1|Replicate),data=data_T1)
anova(PAM)

print(emmeans(PAM_final, list(pairwise ~ Treatment|Temperature)), adjust = c("bonferroni"))

#################################################################################
######T2 model & pairwise comparisons of systems within each temp treatment######
#################################################################################

PAM2<-lmer(PAM ~ Treatment*Temperature + (1|Replicate), data=data_T2)
sjPlot::plot_model(PAM2, type="diag")
step(PAM2, reduce.random=FALSE)

PAM2_final<-lmer(PAM ~ Temperature * Treatment + (1|Replicate),data=data_T2)
anova(PAM2_final)

print(emmeans(PAM2_final, list(pairwise ~ Treatment|Temperature)), adjust = c("bonferroni"))
