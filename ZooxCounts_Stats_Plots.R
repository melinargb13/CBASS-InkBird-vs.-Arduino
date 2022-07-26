#Code for Figure S4 of "Empirically derived thermal thresholds of four coral species along the Red Sea"
#Statistical results are also summarized in Table S4

#setwd to source file location

setwd("D:/Desktop/UoK/Tesis/r_thesis")

library(lmerTest)
library(emmeans)
library(sjPlot)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)

Full_Physiology <-read.csv("zoox_counts_melina.csv")
Full_Physiology$temp<-as.factor(Full_Physiology$temp) #check capitals 
Full_Physiology$system<-as.factor(Full_Physiology$system)
Full_Physiology$species<-as.factor(Full_Physiology$species)

str(Full_Physiology)

Sty_phys<-subset(Full_Physiology, species == 'Stylophora')

#### Stylophora #### STATS 

#IS THE ANOVA CONSIDERING THAT MY DATA IS LOG TRANSFORMED?? IS IT OK IF ON MY GRAPHS THE VALUES ARE NOT LOG TRANSFORMED FOR THE SYM
#DENSITIES?

Sty_sym_log<-log(Sty_phys$Sym_cm2)
Sty_sym<-lmer(Sty_sym_log ~ temp*system + (1|rep/system), data = Sty_phys)
sjPlot::plot_model(Sty_sym, type="diag")
anova(Sty_sym) 
rand(Sty_sym)
step(Sty_sym, reduce.random = F)

Sty_sym_final<-lmer(Sty_sym_log ~ temp*system + (1|rep/system), data = Sty_phys)
anova(Sty_sym_final)

print(emmeans(Sty_sym_final, list(pairwise ~ temp|system)), adjust = c("mvt"))
print(emmeans(Sty_sym_final, list(pairwise ~ system|temp)), adjust = c("mvt"))

#plot
print(levels(Sty_phys$system))
#Sty_phys$system = factor(Sty_phys$system,levels(Sty_phys$system)[c(4,5,1)]) 

# Colours-> Eilat, Al Wajh, Yanbu, AlFahal, Al Q, Obock
# "royalblue2", "darkgoldenrod1", "darkorange1", "red3", "darkorchid4", "springgreen1")

Sty_sym <- ggplot(data=Sty_phys, 
                  aes(x=temp, y=Sym_cm2, label=temp, fill=system)) +
  scale_fill_manual(values = c("royalblue2", "darkorange1", "red3"), drop = FALSE, name = "system") +
  stat_boxplot(geom ='errorbar', width = 0.7, lwd=0.7) +
  geom_boxplot(width=0.7, lwd=0.7, fatten=1) +
  expand_limits(y = 0)+
  facet_grid(~species, space = "free", scales = "free")+ 
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
        panel.spacing = unit(3, "lines")) + xlab(label = "temperature (°C)") + ylab(label = "Symbiont density (cm^2)")+
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=12, face="plain"),
        legend.position="bottom")
  
Sty_sym 

#END