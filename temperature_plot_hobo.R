library(ggplot2)
library(tidyr)
library(egg)

#temperature plot from hobo output
temp2 <- readxl::read_excel(path = "D:/Desktop/UoK/Tesis/r_thesis/hobo_input_R_T5_24042022.xlsx" , sheet = 2)

#make table longer (N.B.: assumes you have headers formatted "Run_Time, T30, T34, T36, T39")
templong2 <- pivot_longer(temp2,starts_with("T", ignore.case = FALSE, vars = NULL))

### changes the names from abbreviation to full version
templong2$name <- gsub("T_I30", "30°C - Inkbird", templong2$name)
templong2$name <- gsub("T_I34", "34°C - Inkbird", templong2$name)
templong2$name <- gsub("T_I36", "36°C - Inkbird", templong2$name)
templong2$name <- gsub("T_I39", "39°C - Inkbird", templong2$name)
templong2$name <- gsub("T_A30", "30°C - Arduino", templong2$name)
templong2$name <- gsub("T_A34", "34°C - Arduino", templong2$name)
templong2$name <- gsub("T_A36", "36°C - Arduino", templong2$name)
templong2$name <- gsub("T_A39", "39°C - Arduino", templong2$name)

##add "facets" informations (see ggplot)
templong2$name2 <- ""
templong2$name2 = templong2$name
templong2 <- templong2 %>% separate(name2,sep = "-", c('Temperatures', 'Instrument'))


a2 <- ggplot(data = templong2, aes(run_time, value, colour = Instrument, group = name)) + ###change colour to = Temperatures if you want each temp to be different colours 
  geom_line() + #theme(panel.grid = element_blank()) + 
  scale_y_continuous(breaks = c (39,36,34,30), minor_breaks = NULL ) +
  scale_x_datetime(date_breaks = "4 hours", date_minor_breaks = "1 hour", date_labels = "%H:%M") +  #####if R reads spreadsheet time as time and date use this row and comment the one below (if not, invert comment)
  #scale_x_discrete(breaks = c( "00:00", "04:00", "08:00", "12:00", "16:00", "20:00", 24:00")) +
  ylab("Temperature Profile") +
  xlab("run time")+
  labs(color='Temperatures') + #label for the legend
  facet_grid(~Instrument) + ####how you split the graphs(you can change "Temperatures" to  "Instrument" for example)
  ggtitle("HOBO Inkbird vs Arduino") ####title for the plot
a2 

#if you want to join two or more immages 
# t2 <- ggarrange( a2,a2,ncol = 1,  nrow = 2)
# t2

####to save the immage to file
png( file="Plot/Hobo - Inkbird vs Arudino.png",
     width =2000, height=1200)
a2 + theme(text = element_text(size = 20))
dev.off()