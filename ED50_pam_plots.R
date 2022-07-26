setwd("D:/Desktop/UoK/Tesis/r_thesis/FvFm_ED50s")

library(drc)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(egg)

input<-read.table("PAM.txt",  sep = "\t", header = T) 

input$Temperature=as.numeric(input$Temperature) # we need temperature as numeric
input$PAM=as.numeric(input$PAM) # PAM as numeric

#############################################
## fitting the curves for each individual ## 
#############################################
#remove comment column
input<-input[, c(1:6) ]

#remove rows with missing data
input<-input[complete.cases(input), ]

#create a group to define colony ID
input$Colony=as.factor(paste(input$Strain,input$Treatment, sep = "_")) #,input$Replicate, sep = "_")) # replicate per treatment per strain
levels(input$Colony)

#fit the PAM data from each colony to the response curve

# drm  Demo to one colony
drm(PAM ~ Temperature, data=input[input$Colony=="T1_Arduino",],
    fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
    upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

### now to all colonies in a loop
mod1<-lapply(unique(input$Colony), 
             function(x) drm(PAM ~ Temperature, data=input[input$Colony==x,],
                             fct = LL.3(names = c('Slope', 'Max', 'ED50')),
                             upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30)))

#############################
## extract and plot ED50s ## 
#############################

#extract ED50 
ed50_list<-lapply(c(1:length(mod1)), function(x) mod1[[x]][["coefficients"]][["ED50:(Intercept)"]])
ed50_df<-as.data.frame(do.call(rbind, ed50_list))
ed50_df$Sample=unique(input$Colony)
ed50_df=tidyr::separate(ed50_df,Sample,into =c("Strain", "Treatment" ),sep = "_",remove = FALSE,extra = "merge") #, "Replicate"),sep = "_",remove = FALSE,extra = "merge")
ed50_df$Treatment=factor(ed50_df$Treatment, levels=c("Arduino", "Inkbird"))
colnames(ed50_df)[1]="ED50"
#write.table(ed50_df, "VTK2022_ED50.txt", row.names = F, sep = "\t")

#plot 1 ED50 dsitributions x treatment 
treatment_colours<-c("#C3BF6D", "#4ea8de")
ggplot(ed50_df, aes(x=Treatment, y=ED50, fill =Treatment)) +
  geom_boxplot(outlier.colour="black", outlier.shape=16,
               outlier.size=2, lwd=0.8) + theme_bw() +
  scale_fill_manual(values=treatment_colours) + facet_grid(~Strain)

#### statistic comparison on ED50s
#check for normality 
hist(ed50_df$ED50) #Gaussian shape? No
shapiro.test(ed50_df$ED50) #normality? Yes

hist(sqrt(max(1+ed50_df$ED50)-ed50_df$ED50)) 
shapiro.test(sqrt(max(1+ed50_df$ED50)-ed50_df$ED50)) 

#Based on Shapiro-Wilk normality test, data is not significantly different from normal 
# p-value > 0.05

#transform variables before doing the anova
ed50_df$transformed_ED50s<-sqrt(max(1+ed50_df$ED50)-ed50_df$ED50)

shapiro.test(ed50_df$transformed_ED50s)

#Overall anova.
anova_all=aov(transformed_ED50s ~ Strain*Treatment, data = ed50_df)
summary(anova_all)

#t-tests multiple comparisons between treatments

T1_ED50s<-ed50_df[ed50_df$Strain=="T1", ]
T2_ED50s<-ed50_df[ed50_df$Strain=="T2", ]

pairwise.t.test(T1_ED50s$transformed_ED50s,T1_ED50s$Treatment, p.adj = "fdr")
pairwise.t.test(T2_ED50s$transformed_ED50s,T2_ED50s$Treatment, p.adj = "fdr")

#######################
## plot ED50 curves ## 
#######################

# temperature ranges 
temp_x<- seq(30, 41, length = 100)

# prediction of the fitted values corresponding to the range of temperatures above
pred1<-lapply(mod1, function(x) predict(x, data.frame(Temperature = temp_x)))
pred_df<-as.data.frame(do.call(rbind, pred1))
colnames(pred_df)= round(temp_x, digits = 2)
pred_df$Sample=unique(input$Colony)
pred_df_long=reshape2::melt(pred_df, id.vars=c("Sample"))
colnames(pred_df_long)[2:3]<-c("Temperature", "Fv/Fm")
pred_df_long=tidyr::separate(pred_df_long,Sample,into =c("Strain", "Treatment"),sep = "_",remove = FALSE,extra = "merge")  # , "Replicate"),sep = "_",remove = FALSE,extra = "merge")
pred_df_long$Treatment=factor(pred_df_long$Treatment, levels=c("Arduino", "Inkbird"))
pred_df_long$Temperature=as.numeric(as.character(pred_df_long$Temperature))
pred_df_long$group=paste(pred_df_long$Strain, pred_df_long$Treatment)


#calculate mean ED50 per treatment
ED50_means<-ed50_df %>% 
  group_by(Strain, Treatment) %>%
  summarise(mean=mean(ED50), sd=sd(ED50)) %>%
  unite(Group, c(Treatment), sep = "-", remove = FALSE)
ED50_means$group=paste(ED50_means$Strain, ED50_means$Treatment)

pred_df_long$meanED50=round(ED50_means$mean[match(pred_df_long$group,ED50_means$group)], 2)

curve_T1_input<-pred_df_long[pred_df_long$Strain=="T1",]

modA <- drm(PAM ~ Temperature, data=input[input$Colony=="T1_Arduino",],
            fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
            upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))


modI <- drm(PAM ~ Temperature, data=input[input$Colony=="T1_Inkbird",],
            fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
            upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

curve_T1_input_preddata_1 = data.frame(temp = seq(30,41, length.out = 100))
curve_T1_input_pred_1 = as.data.frame(predict(modA, newdata = curve_T1_input_preddata_1, interval = 'confidence'))
curve_T1_input_preddata_1 = data.frame(curve_T1_input_preddata_1, Median = curve_T1_input_pred_1$Prediction, Lower = curve_T1_input_pred_1$Lower, Upper = curve_T1_input_pred_1$Upper)

curve_T1_input_preddata_2 = data.frame(temp = seq(30,41, length.out = 100))
curve_T1_input_pred_2 = as.data.frame(predict(modI, newdata = curve_T1_input_preddata_2, interval = 'confidence'))
curve_T1_input_preddata_2 = data.frame(curve_T1_input_preddata_2, Median = curve_T1_input_pred_2$Prediction, Lower = curve_T1_input_pred_2$Lower, Upper = curve_T1_input_pred_2$Upper)

input_t1<-input[input$Strain=="T1",]
input_t2<-input[input$Strain=="T2",]


x<- ggplot() +
  geom_line( data = curve_T1_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color=Treatment), size = 0.6) +
  geom_ribbon(data = curve_T1_input_preddata_1, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.1, size=0.6) +
  geom_ribbon(data = curve_T1_input_preddata_2, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue2', linetype=2, alpha = 0.1, outline.type = "both", size=0.6) +
  geom_segment(data = curve_T1_input, aes(x =meanED50, y = 0, xend = meanED50, yend = 0.65,color=Treatment), linetype=3, size=0.7) +
  #geom_smooth (data = curve_T1_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color=Treatment), method=glm , se =F) +
  geom_text(data = curve_T1_input, mapping=aes(x=meanED50, y=0.68, label=meanED50, color=Treatment), size=5, angle=90, hjust=0, check_overlap = T, show_guide = F) +
  guides(fill=guide_legend())+
  theme(panel.grid = element_blank()) +
  theme_classic() +
  scale_color_manual(values=treatment_colours)+
  scale_x_continuous(breaks=c(30, 34, 36, 39), limits = c(30,41), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.02,0.8), expand = c(0, 0)) + labs(color='') +
  scale_color_manual(values=c("darkgoldenrod1", "royalblue2")) +
  geom_jitter(data = input_t1, aes(x = Temperature, y = PAM, color = Treatment), size = 1.5, width = 0.25) 
  
x 
t1 <- x


curve_T2_input<-pred_df_long[pred_df_long$Strain=="T2",]

modC <- drm(PAM ~ Temperature, data=input[input$Colony=="T2_Arduino",],
            fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
            upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))


modH <- drm(PAM ~ Temperature, data=input[input$Colony=="T2_Inkbird",],
            fct = LL.3(names = c('Slope', 'Max', 'ED50')), 
            upperl = c(120, 0.72, 40), lowerl = c(10, 0.55, 30))

curve_T2_input_preddata_1 = data.frame(temp = seq(30,41, length.out = 100))
curve_T2_input_pred_1 = as.data.frame(predict(modC, newdata = curve_T2_input_preddata_1, interval = 'confidence'))
curve_T2_input_preddata_1 = data.frame(curve_T2_input_preddata_1, Median = curve_T2_input_pred_1$Prediction, Lower = curve_T2_input_pred_1$Lower, Upper = curve_T2_input_pred_1$Upper)

curve_T2_input_preddata_2 = data.frame(temp = seq(30,41, length.out = 100))
curve_T2_input_pred_2 = as.data.frame(predict(modH, newdata = curve_T2_input_preddata_2, interval = 'confidence'))
curve_T2_input_preddata_2 = data.frame(curve_T2_input_preddata_2, Median = curve_T2_input_pred_2$Prediction, Lower = curve_T2_input_pred_2$Lower, Upper = curve_T2_input_pred_2$Upper)



y<- ggplot() +
  geom_line( data = curve_T2_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color=Treatment) , size = 0.6) +
  geom_ribbon(data = curve_T2_input_preddata_1, aes(x = temp, ymin=Lower, ymax=Upper), color = 'darkgoldenrod1', linetype=2, alpha = 0.1, size=0.6) +
  geom_ribbon(data = curve_T2_input_preddata_2, aes(x = temp, ymin=Lower, ymax=Upper), color = 'royalblue2', linetype=2, alpha = 0.1, outline.type = "both", size=0.6) +
  geom_segment(data = curve_T2_input, aes(x = meanED50, y = -0.22, xend = meanED50, yend = 1,color=Treatment), linetype=3, size=0.7) +
  #geom_smooth (data = curve_T2_input, aes(x=Temperature, y=`Fv/Fm`, group=Sample, color=Treatment), method=glm , se =F) +
  geom_text(data = curve_T2_input, mapping=aes(x=meanED50, y=1.1, label=meanED50, color=Treatment), size=3, angle=90, hjust=0, check_overlap = T, show_guide = F) +
  guides(fill=guide_legend())+
  theme(panel.grid = element_blank()) +
  theme_classic() +
  scale_color_manual(values=treatment_colours)+
  scale_x_continuous(breaks=c(30, 34, 36, 39), limits = c(30,41), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.22,1.23), expand = c(0, 0)) + labs(color='') +
  scale_color_manual(values=c("darkgoldenrod1", "royalblue2")) +
  geom_jitter(data = input_t2, aes(x = Temperature, y = PAM, color = Treatment), size = 1.5, width = 0.25) 

y 
t2 <- y


T3 <- ggarrange(t1 + theme(text = element_text(size = 10)), t2 + theme(text = element_text(size = 10)), labels = c("T1", "T2"), ncol = 2, nrow = 1)

png(file="Plot/PAM_T1.png",
    width =1600, height=1200)
t1 + theme(text = element_text(size = 30)) 
dev.off()

png(file="Plot/PAM_T2.png",
    width =1600, height=1200)
t2 + theme(text = element_text(size = 30))
dev.off()

png(file="Plot/PAM.png",
    width =2000, height=1200)
T3 
dev.off()

