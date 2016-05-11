######################################
###  Meso Experiment 2013
###  Script by Rachael E. Blake
###  Sept. 2014
######################################
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales) ; library(reshape2)


OI <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Oiling stuff/OilConc_FINAL_MesoExpt 2013.csv")
head(OI)
tail(OI)
names(OI)

OI_red <- OI[-c(53,54),] # These are the samples Buffy Meyer thinks were switched or mis-labeled.
names(OI_red)


#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
theme_boxplot <- function(base_size = 12){
  theme_bw(base_size) %+replace%
    theme(legend.key.size=unit(15,"points"),
          legend.text=element_text(size=I(13)),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="none",
          plot.margin=unit(c(0.5,1,0.5,1), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
          panel.border=element_blank(),
          panel.margin=unit(0,"lines"),
          axis.ticks.length=unit(1,"mm"),
          axis.text.x = element_text(margin=margin(5,0,0,0)),
          axis.text.y = element_text(margin=margin(0,5,0,0)),
          axis.text=element_text(size=13),
          axis.title.x=element_text(size=17, margin=margin(15,0,0,0)), 
          axis.title.y=element_text(size=15, angle=90, margin=margin(0,15,0,0)), 
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x=element_text(size=14),
          strip.background=element_rect(colour='black', fill='white'),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
}   
##########################################
##########

colors <- c("green","red","yellow","orange")

# Total Hydrocarbons
OI_tot <- OI %>%
          select(Field.ID.., Date, Time_Step, Bucket, Treat, Chem, Oil, Corexit, Herbivore,
                 Total.Alkanes, Total.Aromatics) %>%
          mutate(Total.Hydrocarbon = Total.Alkanes + Total.Aromatics)
OI_tot$Chem1 <- factor(OI_tot$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))

TtlHydro <- ggplot(data=OI_tot, aes(x=Herbivore, y=Total.Hydrocarbon, fill=Chem1)) + 
                 geom_boxplot() + theme_boxplot() + ylab("Total Hydrocarbons") +
                 theme(legend.background=element_blank(),
                       legend.text=element_text(size=18), legend.position=c(.95, .85),
                       axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                       panel.border=element_blank(), axis.line=element_line(color='black')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))
TtlHydro

# Total Alkanes       
OI$Chem1 <- factor(OI$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
TtlAlk <- ggplot(data=OI, aes(x=Herbivore, y=Total.Alkanes, fill=Chem1)) + 
                 geom_boxplot() + theme_boxplot() + ylab("Total Alkanes") +
                 theme(legend.background=element_blank(),
                       legend.text=element_text(size=18), legend.position=c(.95, .85),
                       axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                       panel.border=element_blank(), axis.line=element_line(color='black')) +
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
AKMean <- OI %>%
          select(Date,Time_Step, Chem, Oil, Corexit, Total.Alkanes, Total.Aromatics) %>%
          group_by(Date,Time_Step, Chem, Oil, Corexit) %>%
          summarise_each(funs(mean)) %>%
          ungroup()
         


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
Means <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/ALL_DATA_SEM_MesoExpt2013.csv")
head(Means)
str(Means)
names(Means)

arrange(Means[,c(2,4,7,18,19)], Chem, Herbivore)

Means_every <- Means %>%
               select(-Bucket, -Treat, -Snail, -Insect, 
                      -ProkAbunScaled, -LogProkAbunScaled, -SqrtProkAbunScaled,
                      -LogDeadStemDryWgt, -LiveStemDryScaled, -TtlStemNumScaled,
                      -SnailWgtScaled,-Photo_Scaled, -Fv.Fm_Scaled, -LvRootDryWgt, 
                      -DdRootDryWgt, -LogDdRootDryWgt, -LvRootDryWgt_Scaled) %>%
               group_by(Week, Chem, Oil, Corexit, Herbivore) %>%
               summarise_each(funs(mean)) %>%
               ungroup() %>%
               arrange(Week, Chem, Herbivore)

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
               geom_point(shape=15, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
FAk
#
FAr <- ggplot(MeansOilPhoto, aes(y=Fv.Fm, x=Total.Aromatics)) +
               geom_point(shape=15, size=4) + theme_boxplot() +
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
               geom_point(shape=15, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
LvStmAk
#
LvStmAr <- ggplot(MeansOilPhoto, aes(y=LiveStemDryWgt_g, x=Total.Aromatics)) +
               geom_point(shape=15, size=4) + theme_boxplot() +
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
               geom_point(shape=15, size=4) + theme_boxplot() +
               geom_smooth() +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
DdStmAk
#
DdStmAr <- ggplot(MeansOilPhoto, aes(y=DeadStemDryWgt_g, x=Total.Aromatics)) +
               geom_point(shape=15, size=4) + theme_boxplot() +
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
               geom_point(shape=15, size=4, aes(colour=Chem)) + theme_boxplot() +
               geom_smooth(se=FALSE) +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
ProkAk
#
ProkAr <- ggplot(MeansOilPhoto, aes(y=Prok_Abun, x=Total.Aromatics)) +
               geom_point(shape=15, size=4, aes(colour=Chem)) + theme_boxplot() +
               geom_smooth(se=FALSE) +
               theme(legend.key=element_blank(), legend.background=element_blank(),
                     legend.text=element_text(size=18), legend.position=c(.95, .85),
                     axis.text=element_text(size=20), axis.title.y=element_text(vjust=0),
                     panel.border=element_blank(),axis.line=element_line(color='black'),
                     panel.background=element_blank())
ProkAr

##############
# Snail Weight change
colors <- c("green","red","yellow","orange")

LitAk <- ggplot(MeansOilPhoto, aes(y=SnailWgt_per_Day, x=Total.Alkanes)) +
                geom_point(shape=15, size=4, aes(colour=Chem)) + theme_boxplot() +
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



















