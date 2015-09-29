######################################
###  Meso Experiment 2013
###  Script by Rachael E. Blake
###  Sept. 2014
######################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Oiling stuff")

OI <- read.csv("OilConc_FINAL_MesoExpt 2013.csv")
head(OI)
tail(OI)
names(OI)

OI_red <- OI[-c(53,54),] # These are the samples Buffy Meyer thinks were switched or mis-labeled.
names(OI_red)


library(ggplot2) ; library(plyr) ; library(grid) ; library(scales) ; library(reshape2)

#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
theme_boxplot <- function(base_size = 12){
  theme_bw(base_size) %+replace%
    theme(legend.key.size=unit(15,"points"),
          legend.text=element_text(size=I(13)),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="none",
          plot.margin=unit(c(0.25,2,0.25,2), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
          panel.border=element_rect(colour='black', fill = NA),
          panel.margin=unit(0,"lines"),
          axis.ticks.length=unit(1,"mm"),
          axis.ticks.margin = unit(0, "lines"),
          axis.text=element_text(size=15),
          axis.title.x=element_text(hjust=.55, vjust=-.01, size=17),
          axis.title.y=element_text(size=17, angle=90, hjust=.56, vjust=-.001),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x=element_text(size=14),
          strip.background=element_rect(colour='black', fill='white'))
}  
##########################################
##########


colors <- c("green","red","yellow","orange")
# Total Alkanes       
OI$Chem1 <- factor(OI$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
TtlAlk <- ggplot(data=OI, aes(x=Herbivore, y=Total.Alkanes, fill=Chem1)) + 
                 geom_boxplot() + theme_boxplot() + ylab("Total Alkanes") +
                 theme(legend.background=element_blank(),
                       legend.text=element_text(size=18), legend.position=c(.95, .85),
                       axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                       panel.border=element_blank(),axis.line=element_line(color='black')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))
TtlAlk

# Total Aromatics
TtlAro <- ggplot(data=OI, aes(x=Herbivore, y=Total.Aromatics, fill=Chem1)) + 
                 geom_boxplot() + theme_boxplot() + ylab("Total Aromatics") +
                 theme(legend.background=element_blank(),
                       legend.text=element_text(size=18), legend.position=c(.95, .85), 
                       axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                       panel.border=element_blank(),axis.line=element_line(color='black')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))
TtlAro

########## Taking the Mean of the values for each time step for each treatment
attach(OI)
#Taking means of all values
meanAK <- aggregate(OI[,c(10,11)], by=list(Time_Step, Chem), FUN=mean, na.rm=FALSE)
library(plyr) ; meanAK1 <- arrange(meanAK, Group.1) #sorts data by Time_Step
meanAK1

#Getting the treatment columns
TreatOIAK <- OI[,c(2,3,6:7)]
TreatOIAKK <- arrange(TreatOIAK, Time_Step, Chem) 
TreatOIAKK  
TreatSOIAK <- unique(TreatOIAKK)
TreatSOIAK

#Binding the treatment columns with the data columns
AKMean <- cbind(TreatSOIAK, meanAK1[,c(3:4)])
AKMean

detach(OI)
#############
colors <- c("NC"="green","Core"="red","Oil"="yellow","OilCore"="orange")
# Alkanes over Time
AlkOverTime <- ggplot(data=AKMean, aes(x=Time_Step, y=Total.Alkanes, colour=factor(Chem))) +
                      geom_line(size=2) + theme_boxplot() +
                      theme(legend.key=element_blank(), legend.background=element_blank(),
                            legend.text=element_text(size=18), legend.position=c(.95, .85),
                            axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                            panel.border=element_blank(),axis.line=element_line(color='black'),
                            panel.background=element_blank(),plot.background=element_blank()) +
                      scale_colour_manual(values=colors, guide=guide_legend(title = NULL)) 
AlkOverTime      

# Aromatics over Time
AroOverTime <- ggplot(data=AKMean, aes(x=Time_Step, y=Total.Aromatics, colour=Chem)) +
                      geom_line(size=2) + theme_boxplot() +
                      theme(legend.key=element_blank(), legend.background=element_blank(),
                            legend.text=element_text(size=18), legend.position=c(.95, .85),
                            axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                            panel.border=element_blank(),axis.line=element_line(color='black'),
                            panel.background=element_blank()) +
                      scale_colour_manual(values=colors, guide=guide_legend(title = NULL))
AroOverTime      

############ Bringing in some plant data ############################
# getting only the oil samples from the end of the experiment
EndOil <- subset(OI, OI$Time_Step==5)
EndOil1 <- arrange(EndOil, Chem, Herbivore)
EndOil1

# plant data
setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\")

Means <- read.csv("ALL DATA_SEM_MesoExpt2013.csv")
head(Means)
str(Means)
names(Means)

arrange(Means[,c(2,4,7,18,19)], Chem, Herbivore)

#############
attach(Means)
#Taking means of all values
mean_all <- aggregate(Means[,c(10:19,27:29)], by=list(Chem, Herbivore), FUN=mean, na.rm=FALSE)
library(plyr) ; mean_all1 <- arrange(mean_all, Group.1) #sorts data by 
mean_all1
mean_all1[,c(1,2,12)]

#Getting the treatment columns
TreatM <- Means[,c(1,4:7)]
TreatMs <- arrange(TreatM, Chem, Herbivore) 
TreatMs 
TreatSMs <- unique(TreatMs)
TreatSMs

#Binding the treatment columns with the data columns
Means_every <- cbind(TreatSMs, mean_all1[,-c(1,2)])
Means_every

detach(Means)
#############

MeansOilPhoto <- cbind(EndOil1[,c(1:11)], Means_every)
MeansOilPhoto
MeansOilPhoto[,c(4,6,9,13,16,25,26)]

# Alkanes with Photosynthesis
PAk <- ggplot(MeansOilPhoto, aes(y=Photo, x=Total.Alkanes)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
PAk

# Fit a linear model to the data and save the model object:
mod1 <- lm(Total.Alkanes~Photo, data=MeansOilPhoto)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
rout1 <- list(paste('Fitted model:',round(coef(mod1)[1],3),' + ',
                     round(coef(mod1)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(mod1)[['r.squared']],3),
                     sep=''))
PAk2 <- ggplot(MeansOilPhoto, aes(Total.Alkanes,Photo)) + geom_point() +
               geom_smooth(method=lm) + theme_boxplot() + 
               geom_text(aes(x=50, y=10, label=rout1[[2]]), 
                         hjust=0, parse=TRUE)
PAk2


# Aromatics with Photosynthesis
PAr <- ggplot(MeansOilPhoto, aes(y=Photo, x=Total.Aromatics)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
PAr

#
mod2 <- lm(Total.Aromatics~Photo, data=MeansOilPhoto)
rout2 <- list(paste('Fitted model:',round(coef(mod2)[1],3),' + ',
                     round(coef(mod2)[2],3), 'x',sep = ''),
              paste('R^2 == ',round(summary(mod2)[['r.squared']],3),
                     sep=''))
PAr2 <- ggplot(MeansOilPhoto, aes(Total.Aromatics,Photo)) + geom_point() +
               geom_smooth(method=lm) + theme_boxplot() + 
               geom_text(aes(x=5000, y=9, label=rout2[[2]]), 
                         hjust=0, parse=TRUE)
PAr2

###############
# Fv / Fm 
FAk <- ggplot(MeansOilPhoto, aes(y=Fv.Fm, x=Total.Alkanes)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
FAk
#
FAr <- ggplot(MeansOilPhoto, aes(y=Fv.Fm, x=Total.Aromatics)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
FAr


###############
# Live Stem Dry Weight
LvStmAk <- ggplot(MeansOilPhoto, aes(y=LiveStemDryWgt_g, x=Total.Alkanes)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
LvStmAk
#
LvStmAr <- ggplot(MeansOilPhoto, aes(y=LiveStemDryWgt_g, x=Total.Aromatics)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
LvStmAr

################
# Dead Stem Dry Weight
DdStmAk <- ggplot(MeansOilPhoto, aes(y=DeadStemDryWgt_g, x=Total.Alkanes)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
DdStmAk
#
DdStmAr <- ggplot(MeansOilPhoto, aes(y=DeadStemDryWgt_g, x=Total.Aromatics)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
DdStmAr

###############
# Insect abundance 
ProkAk <- ggplot(MeansOilPhoto, aes(y=Prok_Abun, x=Total.Alkanes)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth(se=FALSE) +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
ProkAk
#
ProkAr <- ggplot(MeansOilPhoto, aes(y=Prok_Abun, x=Total.Aromatics)) +
               geom_point(shape=5, size=4) + theme_boxplot() +
               geom_smooth(se=FALSE) +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
ProkAr

##############
# Snail Weight change
LitAk <- ggplot(MeansOilPhoto, aes(y=SnailWgt_per_Day, x=Total.Alkanes)) +
                geom_point(shape=5, size=4) + theme_boxplot() +
                geom_smooth(se=FALSE) +
                theme(legend.key=element_blank(), legend.background=element_blank(),
                      legend.text=element_text(size=18), legend.position=c(.95, .85),
                      axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                      panel.border=element_blank(),axis.line=element_line(color='black'),
                      panel.background=element_blank())
LitAk
#

colors <- c("green","red","yellow","orange")

LitAr <- ggplot(MeansOilPhoto, aes(y=SnailWgt_per_Day, x=Total.Aromatics)) +
                geom_point(shape=16, size=4, aes(colour=Chem)) + theme_boxplot() +
                geom_smooth(se=FALSE) + 
                theme(legend.key=element_blank(), legend.background=element_blank(),
                      legend.text=element_text(size=18), legend.position=c(.95, .85),
                      axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                      panel.border=element_blank(),axis.line=element_line(color='black'),
                      panel.background=element_blank())
LitAr

#############



















