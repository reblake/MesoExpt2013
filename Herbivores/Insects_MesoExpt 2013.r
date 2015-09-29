#############################################
###    Mesocosm Experiment 2013           ###
###    Insect Data                        ###
###    Script by Rachael Blake, Jan 2014  ###
#############################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Insects")

ins <- read.csv("Insects_MesoExpt 2013.csv")
head(ins)
tail(ins)

# Exploring data
hist(ins$Prok_Abun)

library(car)
# ANOVA Type III
options(contrasts=c("contr.sum","contr.poly"))
ProkAbun <- lm(Prok_Abun ~ Oil*Corexit*Herbivore, data=ins)       
Anova(ProkAbun, type="III") # calculates ANOVA table with Type III SS


# PLOT
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

ins$Chem1 <- factor(ins$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
colors <- c("green","red","yellow","orange")
Prok <- ggplot(data=ins, aes(x=Herbivore, y=Prok_Abun, fill=Chem1)) + 
                  geom_boxplot() + theme_bw() +
                  theme(panel.grid=element_blank(),legend.key=element_blank(),
                        legend.background=element_blank(),legend.text=element_text(size=18),
                        legend.position=c(.95, .85),axis.text=element_text(size=20),
                        panel.border=element_blank(),axis.line=element_line(color='black'),
                        panel.background=element_blank(),plot.background=element_blank(),
                        axis.title.y=element_text(vjust=-0.02)) +
                  scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  
Prok





