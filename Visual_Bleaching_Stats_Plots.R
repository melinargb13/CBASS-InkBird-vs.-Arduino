#setwd to source file location

setwd("D:/Desktop/UoK/Tesis/r_thesis/Visual_Bleaching")

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

######################################################
######## Stylophora Vis Bleaching Assesment T1 ####### 
######################################################

Full_Physiology<-read.table("Visual_bleaching_assesment_T1.txt",  sep = "\t", header = T) 
#Full_Physiology <-read.csv("Visual_bleaching_assesment-Luigi_T1")
Full_Physiology$Temp<-as.factor(Full_Physiology$Temp) #check capitals 
Full_Physiology$System<-as.factor(Full_Physiology$System)
Full_Physiology$Species<-as.factor(Full_Physiology$Species)

str(Full_Physiology)

Sty_phys<-subset(Full_Physiology, Species == 'Stylophora')

#Sty_sym_log<-log(Sty_phys$Sym_cm2)
visual_bleaching1<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys)
sjPlot::plot_model(visual_bleaching1, type="diag") #check for homoscedasticity
qqnorm(resid(visual_bleaching1)); qqline(resid(visual_bleaching1)) # qq plot to check for normal distribution of residuals
hist(resid(visual_bleaching1)) # histogram of residuals to check for normal distribution of residuals
anova(visual_bleaching1) 
rand(visual_bleaching1)
step(visual_bleaching1, reduce.random = F)

shapiro.test(Sty_phys$Median) #we do not asume normal distribution

visual_bleaching_final1<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys)
anova(visual_bleaching_final1)

print(emmeans(visual_bleaching_final1, list(pairwise ~ Temp|System)), adjust = c("mvt"))
print(emmeans(visual_bleaching_final1, list(pairwise ~ System|Temp)), adjust = c("mvt"))

visual_bleaching1 <- ggplot(data=Sty_phys, 
                  aes(x=Temp, y=Median, label=Temp, fill=System)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "System") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines")) + xlab(label = "temperature (°C)") + ylab(label = "Percentage of Pigmentation")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")

visual_bleaching1
x <- visual_bleaching1

######################################################
######## Stylophora Vis Bleaching Assesment T2 ####### 
######################################################


Full_Physiology<-read.table("Visual_bleaching_assesment_T2.txt",  sep = "\t", header = T) 
#Full_Physiology <-read.csv("Visual_bleaching_assesment-Luigi_T1")
input
Full_Physiology$Temp<-as.factor(Full_Physiology$Temp) #check capitals 
Full_Physiology$System<-as.factor(Full_Physiology$System)
Full_Physiology$Species<-as.factor(Full_Physiology$Species)

str(Full_Physiology)

Sty_phys<-subset(Full_Physiology, Species == 'Stylophora')

#Sty_sym_log<-log(Sty_phys$Sym_cm2)
visual_bleaching2<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys)
sjPlot::plot_model(visual_bleaching2, type="diag") #check for homoscedasticity
qqnorm(resid(visual_bleaching2)); qqline(resid(visual_bleaching2)) # qq plot to check for normal distribution of residuals
hist(resid(visual_bleaching2)) # histogram of residuals to check for normal distribution of residuals
anova(visual_bleaching2) 
rand(visual_bleaching2)
step(visual_bleaching2, reduce.random = F)

shapiro.test(Sty_phys$Median) #we do not asume normal distribution

visual_bleaching_final2<-lmer(Median ~ Temp*System + (1|Replicates/System), data = Sty_phys)
anova(visual_bleaching_final2)

print(emmeans(visual_bleaching_final2, list(pairwise ~ Temp|System)), adjust = c("mvt"))
print(emmeans(visual_bleaching_final2, list(pairwise ~ System|Temp)), adjust = c("mvt"))

#Plot

visual_bleaching2 <- ggplot(data=Sty_phys, 
                           aes(x=Temp, y=Median, label=Temp, fill=System)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "System") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~Species, space = "free", scales = "free")+ 
  theme_bw() +
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, colour = "black", size=2) +
  theme(line= element_line(size = 1),
        axis.line = element_line(colour = "black"),
        axis.ticks.length = unit(0.2 , "cm"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(), 
        strip.text.x = element_text(color = "black", size = 12, angle = 0, hjust = 0, vjust = 0.5, face = "plain"),
        panel.spacing = unit(3, "lines")) + xlab(label = "temperature (°C)") + ylab(label = "Percentage of Pigmentation")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")

visual_bleaching2 
y <- visual_bleaching2


plots_final <- ggarrange(visual_bleaching1 + theme(text = element_text(size = 10)), visual_bleaching2 + theme(text = element_text(size = 10)), labels = c("T1", "T2"), ncol = 2, nrow = 1)
plots_final


#END