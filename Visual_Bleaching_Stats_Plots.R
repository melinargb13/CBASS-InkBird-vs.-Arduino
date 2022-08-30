#setwd to source file location

setwd("D:/Desktop/UoK/Tesis/r_thesis/Visual_Bleaching")
#install.packages('caret')
library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(car)
library(ggpubr)
library(moments)
library(caret)

######################################################
######## Stylophora Vis Bleaching Assesment T1 ####### 
######################################################

Full_Physiology1<-read.table("Visual_bleaching_assesment_Final_T2.txt",  sep = "\t", header = T) 
#Full_Physiology <-read.csv("Visual_bleaching_assesment-Luigi_T1")
Full_Physiology1$Temp<-as.factor(Full_Physiology1$Temp) #check capitals 
Full_Physiology1$System<-as.factor(Full_Physiology1$System)
Full_Physiology1$Species<-as.factor(Full_Physiology1$Species)

str(Full_Physiology1)

Sty_phys1<-subset(Full_Physiology1, Species == 'Stylophora')
skewness(Sty_phys1$Median, na.rm = TRUE)

Sty_phys_p2<-(Sty_phys1$Median)^(2)

Sty_phys1$Sty_phys_p2 <- Sty_phys_p2

#ggdensity(Sty_phys1, x = "Sty_phys_log", fill = "lightgray", title = "Sty_Phys") +
#  scale_x_continuous(limits = c(0, 0.2)) +
#  stat_overlay_normal_density(color = "red", linetype = "dashed")

shapiro.test(Sty_phys_p2)
bartlett.test(Sty_phys_p2 ~ Temp, data=Sty_phys1)

visual_bleaching1<-lmer(Sty_phys_p2 ~ Temp*System + (1|Replicates/System), data = Sty_phys1)
sjPlot::plot_model(visual_bleaching1, type="diag") #check for homoscedasticity
qqnorm(resid(visual_bleaching1)); qqline(resid(visual_bleaching1)) # qq plot to check for normal distribution of residuals
hist(resid(visual_bleaching1)) # histogram of residuals to check for normal distribution of residuals
anova(visual_bleaching1) 
rand(visual_bleaching1)
step(visual_bleaching1, reduce.random = F)

#shapiro.test(Sty_phys1$Median) #we do not assume normal distribution
#bartlett.test(Median ~ Temp, data=Sty_phys1) #we assume homogeneity of variances?
#leveneTest(Median ~ Temp, data=Sty_phys1) #we assume equal variances?

#visual_bleaching_final1<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys)
#anova(visual_bleaching_final1)

print(emmeans(visual_bleaching1, list(pairwise ~ Temp)), adjust = c("mvt"))
print(emmeans(visual_bleaching1, list(pairwise ~ System|Temp)), adjust = c("mvt"))
print(emmeans(visual_bleaching1, list(pairwise ~ Temp|System)), adjust = c("mvt"))

visual_bleaching1 <- ggplot(data=Sty_phys1, 
                  aes(x=Temp, y=Median, label=Temp, fill=System)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "System") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=1, fatten=1.4) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1.5),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(10, "lines")) + xlab(label = "temperature (°C)") + ylab(label = "Pigmentation (%)")+
  theme(axis.text.x = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=15, face="plain"),
        legend.position="bottom")

visual_bleaching1
x <- visual_bleaching1

######################################################
######## Stylophora Vis Bleaching Assesment T2 ####### 
######################################################

Full_Physiology2<-read.table("Visual_bleaching_assesment_Final_T2.txt",  sep = "\t", header = T) 
#Full_Physiology <-read.csv("Visual_bleaching_assesment-Luigi_T1")
Full_Physiology2$Temp<-as.factor(Full_Physiology2$Temp) #check capitals 
Full_Physiology2$System<-as.factor(Full_Physiology2$System)
Full_Physiology2$Species<-as.factor(Full_Physiology2$Species)

str(Full_Physiology2)

Sty_phys2<-subset(Full_Physiology2, Species == 'Stylophora')

#Sty_sym_log<-log(Sty_phys$Sym_cm2)
visual_bleaching2<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys2)
sjPlot::plot_model(visual_bleaching2, type="diag") #check for homoscedasticity
qqnorm(resid(visual_bleaching2)); qqline(resid(visual_bleaching2)) # qq plot to check for normal distribution of residuals
hist(resid(visual_bleaching2)) # histogram of residuals to check for normal distribution of residuals
anova(visual_bleaching2) 
rand(visual_bleaching2)
step(visual_bleaching2, reduce.random = F)

shapiro.test(Sty_phys2$Median) #we do not asume normal distribution


#bartlett.test(Median ~ Temp, data=Sty_phys2) #we do not assume homogeneity of variances
#leveneTest(Median ~ Temp, data=Sty_phys2) #we assume equal variances?

anova(visual_bleaching2)

print(emmeans(visual_bleaching2, list(pairwise ~ Temp|System)), adjust = c("bonferroni"))
print(emmeans(visual_bleaching2, list(pairwise ~ System|Temp)), adjust = c("bonferroni"))

print(emmeans(visual_bleaching2, list(pairwise ~ Temp)), adjust = c("bonferroni"))


#Plot

visual_bleaching2 <- ggplot(data=Sty_phys2, 
                           aes(x=Temp, y=Median, label=Temp, fill=System)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "System") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=1, fatten=1.4) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1.5),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(10, "lines")) + xlab(label = "temperature (°C)") + ylab(label = "Pigmentation (%)")+
  theme(axis.text.x = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 13, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 13, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=15, face="plain"),
        legend.position="bottom")

visual_bleaching2 
y <- visual_bleaching2

#merge plots

plots_final <- ggarrange(visual_bleaching1 + theme(text = element_text(size = 6)), visual_bleaching2 + theme(text = element_text(size = 2)), labels = c("T0", "T2"), ncol = 2, nrow = 1)
plots_final


#END

