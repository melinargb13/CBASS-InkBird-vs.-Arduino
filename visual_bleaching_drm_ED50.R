##################################################################
#### DRMs and ED50s based on visual bleaching assessment scores###
##################################################################

setwd("D:/Desktop/UoK/Tesis/r_thesis/Visual_Bleaching")

library(lmerTest)
library(emmeans)
library(sjPlot)
library(drc)
library(Rmisc)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plyr)
library(reshape2)

Vis_bleaching<-read.table("Visual_bleaching_assesment_Final_T2.txt", sep = "\t",header = TRUE)
str(Vis_bleaching)

Vis_bleaching$Label<-as.factor(Vis_bleaching$Label)
Vis_bleaching$System<-as.factor(Vis_bleaching$System)
Vis_bleaching$Species<-as.factor(Vis_bleaching$Species)
Vis_bleaching$Geno<-as.factor(Vis_bleaching$Geno)
Vis_bleaching$Replicate<-as.factor(Vis_bleaching$Replicate)

Sty_data<-subset(Vis_bleaching, Species=="Stylophora")

######################################## 
#### Stylophora DRCs ####
######################################## 

levels(Sty_data$System)
Sty_data$System = factor(Sty_data$System,levels(Sty_data$System)[c(4,3,6,1,2,5)])
# correct N to S order is: "Eilat"  "AlWajh"  "Yanbu"  "AlFahal"  "AlQunfudhah"  "Obock"   

Sty_data$Temp=as.numeric(Sty_data$Temp) # we need temperature as numeric

#changed to arduino 
Sty_Arduino_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$System=="Arduino",], fct = LL.3())
summary(Sty_Arduino_pop)
plot(Sty_Arduino_pop)

Sty_Arduino <- drm(Median ~ Temp, data = Sty_data[Sty_data$System=="Arduino",], curveid=System, fct = LL.3())

summary(Sty_Arduino)
plot(Sty_Arduino)

#extract coeffs by geno, then compute 95% CIs
Sty_Arduino_genocoeffs_50<-data.frame(ED(Sty_Arduino, c(50)))
Sty_Arduino_coeff_mean<-mean(Sty_Arduino_genocoeffs_50$Estimate)
Sty_Arduino_coeff_mean

Sty_Arduino_summary<-data.frame(CI(Sty_Arduino_genocoeffs_50$Estimate, ci=0.95))
Sty_Arduino_coeff_lower<-Sty_Arduino_summary[3,]
Sty_Arduino_coeff_upper<-Sty_Arduino_summary[1,]

#### Sty - AlWajh ####

Sty_Inkbird_pop <- drm(Median ~ Temp, data = Sty_data[Sty_data$System=="Inkbird",], fct = LL.3(names = c('Slope', 'Max', 'ED50')))
summary(Sty_Inkbird_pop)
plot(Sty_Inkbird_pop)

Sty_Inkbird <- drm(Median ~ Temp, data = Sty_data[Sty_data$System=="Inkbird",], curveid=System, fct = LL.3(names = c('Slope', 'Max', 'ED50')))
summary(Sty_Inkbird)
plot(Sty_Inkbird)

#extract coeffs by geno, then compute 95% CIs
Sty_Inkbird_genocoeffs_50<-data.frame(ED(Sty_Inkbird, c(50)))
Sty_Inkbird_coeff_mean<-mean(Sty_Inkbird_genocoeffs_50$Estimate)

Sty_Inkbird_coeff_mean

Sty_Inkbird_summary<-data.frame(CI(Sty_Inkbird_genocoeffs_50$Estimate, ci=0.95))
Sty_Inkbird_coeff_lower<-Sty_Inkbird_summary[3,]
Sty_Inkbird_coeff_upper<-Sty_Inkbird_summary[1,]

############################################################################################
#### Combine genotpye-ED50s into dataframe for statistical analysis ####
############################################################################################

Sty_Geno_ED50s <- data.frame(cbind(Sty_Arduino_genocoeffs_50[,1],Sty_Inkbird_genocoeffs_50[,1]))

Sty_Geno_ED50s<-Sty_Geno_ED50s %>% 
  dplyr::rename(Arduino= X1,
                Inkbird=X2,)

Sty_Geno_ED50s$Geno<-as.factor(1:nrow(Sty_Geno_ED50s))
str(Sty_Geno_ED50s)

Sty_Geno_ED50s_long<-melt(Sty_Geno_ED50s, id="Geno")

Sty_Geno_ED50s_long<-Sty_Geno_ED50s_long %>% 
  dplyr::rename(System= variable,
                ED50=value)

Sty_ED50_mod<-aov(ED50 ~ System, Sty_Geno_ED50s_long)  
summary(Sty_ED50_mod)
TukeyHSD(Sty_ED50_mod)

############################################################################################
#### Combine ED50 data plus predict curves from models for plotting ####
############################################################################################

Sty_coeff_means<-data.frame(Sty_Inkbird_coeff_mean, Sty_Arduino_coeff_mean)
Sty_coeff_lowers<-data.frame(Sty_Inkbird_coeff_lower, Sty_Arduino_coeff_lower)
Sty_coeff_uppers<-data.frame(Sty_Inkbird_coeff_upper, Sty_Arduino_coeff_upper)

Sty_Inkbird_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_Inkbird_pred = as.data.frame(predict(Sty_Inkbird_pop, newdata = Sty_Inkbird_preddata, interval = 'confidence'))
Sty_Inkbird_preddata = data.frame(Sty_Inkbird_preddata, Median = Sty_Inkbird_pred$Prediction, Lower = Sty_Inkbird_pred$Lower, Upper = Sty_Inkbird_pred$Upper)

Sty_Arduino_preddata = data.frame(temp = seq(30,39, length.out = 100))
Sty_Arduino_pred = as.data.frame(predict(Sty_Arduino_pop, newdata = Sty_Arduino_preddata, interval = 'confidence'))
Sty_Arduino_preddata = data.frame(Sty_Arduino_preddata, Median = Sty_Arduino_pred$Prediction, Lower = Sty_Arduino_pred$Lower, Upper = Sty_Arduino_pred$Upper)

#### PLOT Stylophora ####
Sty_data$Temp<-as.character(Sty_data$Temp)
Sty_data$Temp<-as.numeric(Sty_data$Temp)

Sty_plot<- ggplot() +
  geom_jitter(data = Sty_data, aes(x = Temp, y = Median, color = System), size = 3, width = 0.50) +
  scale_x_continuous(limits=c(29,42), breaks=c(30,32,34,36,38,40)) +
  scale_y_continuous(limits=c(0, 120), breaks=c(0, 20, 40, 60, 80, 100)) +
  
  geom_line(data = Sty_Arduino_preddata, aes(x = temp, y = Median), color = 'royalblue2', lwd=0.8, show.legend = FALSE) +
  geom_ribbon(data = Sty_Arduino_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue2', lwd=0.8 , linetype=2, alpha = 0.2) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_Arduino_coeff_mean), color = 'royalblue2', lwd=0.8, show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_Arduino_coeff_lower, xmax=Sty_coeff_uppers$Sty_Arduino_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'royalblue2',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_Arduino_coeff_mean, digits = 2)), x = 31, y = 30, show.legend = FALSE, color = 'royalblue2') +
  
  geom_line(data = Sty_Inkbird_preddata, aes(x = temp, y = Median), color = 'darkgoldenrod1', lwd= 0.8, show.legend = FALSE) +
  geom_ribbon(data = Sty_Inkbird_preddata, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', lwd=0.8, linetype=2, alpha = 0.1) +
  geom_vline(data = Sty_coeff_means, aes(xintercept = Sty_Inkbird_coeff_mean), color = 'darkgoldenrod1', lwd=0.8, show.legend = FALSE) +
  annotate("rect", xmin=Sty_coeff_lowers$Sty_Inkbird_coeff_lower, xmax=Sty_coeff_uppers$Sty_Inkbird_coeff_upper, ymin=-Inf, ymax=Inf, fill= 'darkgoldenrod1',  alpha = 0.1) +
  geom_text(data = Sty_coeff_means, aes(label=round(Sty_Inkbird_coeff_mean, digits = 2)), x = 31, y = 25, show.legend = FALSE, color = 'darkgoldenrod1') +
  
  ggtitle(" ") +
  scale_color_manual(values=c("royalblue2", "darkgoldenrod1", "darkorange1", "red3", "springgreen1")) +
  ylab("Pigmentation (%)") +
  xlab("Temperature (°C)")

Sty_plot_final<-Sty_plot + theme(panel.background = element_blank())
Sty_plot_final + theme(axis.line = element_line(colour = "black"))

Sty_plot_1 <- Sty_plot

