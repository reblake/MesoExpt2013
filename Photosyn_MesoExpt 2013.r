##############################################################
##  Photosynthesis and Fv/Fm Measurements                   ##
##  Mesocosm Multi-Stressor Experiment 2013                 ##
##  Script by Rachael Blake, September 2013                 ##
##############################################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\LICOR Files_Meso Expt\\")

PhotoALL <- read.csv("LICOR_PhotosynMeas_MesoExpt 2013.csv")
names(PhotoALL)
head(PhotoALL)
tail(PhotoALL)

####Taking Mean of the three measurements in each bucket#######

# Convert the date column so it can be used in R
PhotoALL$Date1 <- as.Date(PhotoALL$Date, format="%d-%b") 

# REMOVE May 20th LIGHT data..it is wacky for some reason I can't figure out
#PhotoALL1 <- PhotoALL[!PhotoALL$Date %in% c("20-May"),]


attach(PhotoALL)
#Taking means of all values
meanp <- aggregate(PhotoALL[,-c(1:9)], by=list(Date,MeasType,Bucket.Number), FUN=mean, na.rm=FALSE)
library(plyr) ; meanp1 <- arrange(meanp, Group.3, Date1, Group.2) #sorts data by bucket and date
meanp1[1:50,]

#Getting the treatment columns
TreatAllp <- PhotoALL[,c(91,1,2,4:9)]
TreatAllpp <- arrange(TreatAllp, Bucket.Number, Date1, MeasType) 
TreatAllpp[1:30,]  
TreatSAllp <- unique(TreatAllpp)
TreatSAllp[1:50,]
tail(TreatSAllp)

#Binding the treatment columns with the data columns
#Pmn <- meanp[,-c(1:3)]
PMean <- cbind(TreatSAllp, meanp1)
PMean[25:50,1:12]

detach(PhotoALL)

write.csv(PMean,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\LICOR Files_Meso Expt\\Photo_Mean_MesoExpt2013.csv")

#############################################################
#############################################################

PMean1 <- read.csv("LICOR_Photo_Mean_MesoExpt2013.csv")

# Just looking at the data
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
qplot(y=PhiPS2, x=Fv..Fm., data=PMean1)
qplot(y=PhiPS2, x=PhiCO2, data=PMean1)

# Subsetting for the Light and Dark measurements
Light <- PMean1[PMean1$MeasType=="Light",]  ;  Light[1:30,]
Dark <- PMean1[PMean1$MeasType=="Dark",]  ;  Dark[1:30,]


###### FINAL DATA (END OF EXPERIMENT) #########################

#subsetting final light data
FinalLight <- Light[Light$Date %in% c("2-Jul","3-Jul","5-Jul","6-Jul"),]
head(FinalLight) ; tail(FinalLight)
  
#subsetting final dark data
FinalDark <- Dark[Dark$Date %in% c("2-Jul","3-Jul","5-Jul","6-Jul"),]   
head(FinalDark)

# Looking at data
qplot(x=FinalLight$Fv..Fm.,y=FinalLight$PhiPS2)


## Plotting the final data
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
#
FinalLight$Chem1 <- factor(FinalLight$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Photosynthesis
LightPlot <- ggplot(data=FinalLight, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.95, .85),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         
# Photochemical quenching
LightPlota <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
                     geom_boxplot() + theme_bw() +
                     theme(panel.grid=element_blank())
# Nonphotochemcial quenching
LightPlotb <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
                     geom_boxplot() + theme_bw() +
                     theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.1, .85),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         
# Phi PSII
LightPlotc <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
                     geom_boxplot() + theme_bw() +
                     theme(panel.grid=element_blank())

# Fv/Fm
FinalDark$Chem1 <- factor(FinalDark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))

DarkPlot <- ggplot(data=FinalDark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
                   geom_boxplot() + theme_bw() +
                   theme(panel.grid=element_blank(),legend.key=element_blank(),
                         legend.background=element_blank(),legend.text=element_text(size=18),
                         legend.position=c(.1, .2),axis.text=element_text(size=20),
                         panel.border=element_blank(),axis.line=element_line(color='black'),
                         panel.background=element_blank(),plot.background=element_blank()) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         


# Final Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  

FnlLightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=FinalLight)
             #contrasts=list(Oil=contr.sum, Corexit=contr.sum, Herbivore=contr.sum))
summary(FnlLightA)#$coeff  # give summary of all coefficients - useful to see what's missing
vif(FnlLightA)  # calculates variance inflation factor to determine multicollinearity
Anova(FnlLightA, type="III") # calculates ANOVA table with Type III SS
# Photochemcial quenching  
FnlLightqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=FinalLight)
Anova(FnlLightqP, type="III") # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
FnlLightqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=FinalLight)
Anova(FnlLightqN, type="III") # calculates ANOVA table with Type III SS
# Phi PSII
FnlLightPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=FinalLight)
Anova(FnlLightPhi, type="III") # calculates ANOVA table with Type III SS

# Final Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

FnlDarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=FinalDark)
             #contrasts=list(Oil=contr.sum, Corexit=contr.sum, Herbivore=contr.sum))
summary(FnlDarkA) #$coeff  # give summary of all coefficients - useful to see what's missing
vif(FnlDarkA)  # calculates variance inflation factor to determine multicollinearity
Anova(FnlDarkA, type="III") # calculates ANOVA table with Type III SS


###### INITIAL DATA (WEEK 1 OF EXPERIMENT) #########################

#subsetting initial light data
#NOTE: Use "Light" subsetted above
InitialLight <- Light[Light$Date %in% c("20-May","21-May","23-May","24-May"),]
head(InitialLight)
  
#subsetting initial dark data
#NOTE: Use "Dark" subsetted above
InitialDark <- Dark[Dark$Date %in% c("20-May","21-May","23-May","24-May"),]   
head(InitialDark)

# Looking at data
qplot(x=InitialLight$Fv..Fm.,y=InitialLight$PhiPS2)


## Plotting the initial data
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
#
InitialLight$Chem1 <- factor(InitialLight$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Photosynthesis
LightPlot2 <- ggplot(data=InitialLight, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
                     geom_boxplot() + theme_bw() +
                     theme(panel.grid=element_blank())
# Photochemical quenching 
LightPlot2a <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank())
# Nonphotochemical quenching
LightPlot2b <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank())
# Phi PSII
LightPlot2c <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank())
# Fv/Fm
InitialDark$Chem1 <- factor(InitialDark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
colors <- c("green","red","yellow","orange")
DarkPlot2 <- ggplot(data=InitialDark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + scale_fill_manual(values=colors) +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.title=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.1, .2),axis.text=element_text(size=20),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank())

# Initial Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  

InitLightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=InitialLight)
summary(InitLightA)#$coeff  # give summary of all coefficients - useful to see what's missing
vif(InitLightA)  # calculates variance inflation factor to determine multicollinearity
Anova(InitLightA, type="III") # calculates ANOVA table with Type III SS
# Photochemical quenching 
InitLightAqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAqP, type="III") # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
InitLightAqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAqN, type="III") # calculates ANOVA table with Type III SS
# Phi PSII
InitLightAPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAPhi, type="III") # calculates ANOVA table with Type III SS

# Initial Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

InitDarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=InitialDark)
Anova(InitDarkA, type="III") # calculates ANOVA table with Type III SS


###### DATA FROM WEEK 2 OF EXPERIMENT #########################
#subsetting week 2 light data
#NOTE: Use "Light" subsetted above
Wk2Light <- Light[Light$Date %in% c("27-May","28-May","30-May","31-May"),]
head(Wk2Light)
  
#subsetting week 2 dark data
#NOTE: Use "Dark" subsetted above
Wk2Dark <- Dark[Dark$Date %in% c("27-May","28-May","30-May","31-May"),]   
head(Wk2Dark)

# Looking at data
qplot(x=Wk2Light$Fv..Fm.,y=Wk2Light$PhiPS2)


## Plotting the Week 2 data
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
#
colors <- c("green","red","yellow","orange")
Wk2Light$Chem1 <- factor(Wk2Light$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Photosynthesis
LightPlot3 <- ggplot(data=Wk2Light, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
                     geom_boxplot() + theme_bw() + 
                     theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.95, .85),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))                   
#ggsave(file="Wk2Photo.tiff", plot=LightPlot3, path="C:/Users/rblake/Desktop/")  
# Photochemical quenching 
LightPlot3a <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() + 
                      theme(panel.grid=element_blank(),legend.key=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
                     
# Nonphotochemical quenching
LightPlot3b <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() + 
                      theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.95, .85),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 

# Phi PSII
LightPlot3c <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() + 
                      theme(panel.grid=element_blank(),legend.key=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 

# Fv/Fm
Wk2Dark$Chem1 <- factor(Wk2Dark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))

DarkPlot3 <- ggplot(data=Wk2Dark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + 
                    theme(panel.grid=element_blank(),legend.key=element_blank()) +
                    scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
                    

# Week 2 Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  
# Photosynthesis
Wk2LightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=Wk2Light)
Anova(Wk2LightA, type="III") # calculates ANOVA table with Type III SS
summary(Wk2LightA)#$coeff  # give summary of all coefficients - useful to see what's missing
vif(Wk2LightA)  # calculates variance inflation factor to determine multicollinearity
# Photochemical quenching
Wk2LightAqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=Wk2Light)
Anova(Wk2LightAqP, type="III") # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
Wk2LightAqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=Wk2Light)
Anova(Wk2LightAqN, type="III") # calculates ANOVA table with Type III SS
# Phi PSII
Wk2LightAPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=Wk2Light)
Anova(Wk2LightAPhi, type="III") # calculates ANOVA table with Type III SS

# Week 2 Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

Wk2DarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=Wk2Dark)
Anova(Wk2DarkA, type="III") # calculates ANOVA table with Type III SS


###### DATA FROM WEEK 3 OF EXPERIMENT #########################
#subsetting week 3 light data
#NOTE: Use "Light" subsetted above
Wk3Light <- Light[Light$Date %in% c("3-Jun","4-Jun","5-Jun","6-Jun"),]
head(Wk3Light)
  
#subsetting week 3 dark data
#NOTE: Use "Dark" subsetted above
Wk3Dark <- Dark[Dark$Date %in% c("3-Jun","4-Jun","5-Jun","6-Jun"),]   
head(Wk3Dark)

# Looking at data
qplot(x=Wk3Light$Fv..Fm.,y=Wk3Light$PhiPS2)


## Plotting the Week 3 data
#library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
#
colors <- c("green","red","yellow","orange")
Wk3Light$Chem1 <- factor(Wk3Light$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Photosynthesis
LightPlot4 <- ggplot(data=Wk3Light, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
                     geom_boxplot() + theme_bw() +
                     theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.1, .2),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# Photochemical quenching
LightPlot4a <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank())
# Nonphotochemical quenching
LightPlot4b <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.1, .2),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# Phi PSII
LightPlot4c <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
                      geom_boxplot() + theme_bw() +
                      theme(panel.grid=element_blank())

# Fv/Fm
Wk3Dark$Chem1 <- factor(Wk3Dark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))

DarkPlot4 <- ggplot(data=Wk3Dark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                           legend.background=element_blank(),legend.text=element_text(size=18),
                           legend.position=c(.1, .2),axis.text=element_text(size=20),
                           panel.border=element_blank(),axis.line=element_line(color='black'),
                           panel.background=element_blank(),plot.background=element_blank()) +
                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 

# Week 3 Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  
# Photosynthesis
Wk3LightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightA, type="III") # calculates ANOVA table with Type III SS
summary(Wk3LightA)#$coeff  # give summary of all coefficients - useful to see what's missing
vif(Wk3LightA)  # calculates variance inflation factor to determine multicollinearity
# Photochemcial quenching 
Wk3LightAqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAqP, type="III") # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
Wk3LightAqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAqN, type="III") # calculates ANOVA table with Type III SS
# Phi PSII
Wk3LightAPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAPhi, type="III") # calculates ANOVA table with Type III SS

# Week 2 Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

Wk3DarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=Wk3Dark)
Anova(Wk3DarkA, type="III") # calculates ANOVA table with Type III SS


#########################################
##### Effect size calculation ###########
#########################################
# Reference: Schielzeth H (2010) Simple means to improve the interpretability of regression 
# coefficients.  Methods in Ecology & Evolution 1:103-113.
#########################################
# must read in PMean first, and create Final Light and Final Dark - see above
head(PMean1)
head(FinalLight)
head(FinalDark)
head(InitialLight)
head(InitialDark)
head(Wk2Light)
head(Wk2Dark)
head(Wk3Light)
head(Wk3Dark)


## Final Week 8 data
# center and scale all data to get effect sizes
sc_fl <- scale(FinalLight[,c(12:33)], center=T, scale=T)
head(sc_fl)
FnlLgt_sc <- cbind(FinalLight[,c(1:10)], sc_fl)
head(FnlLgt_sc)

Photo8_effsz <- lm(Photo ~ Oil*Corexit*Herbivore, data=FnlLgt_sc)
summary(Photo8_effsz)


## Initial Week 1 data
# center and scale all data to get effect sizes
sc_in <- scale(InitialLight[,c(12:33)], center=T, scale=T)
head(sc_in)
InLgt_sc <- cbind(InitialLight[,c(1:10)], sc_in)
head(InLgt_sc)

Photo1_effsz <- lm(Photo ~ Oil*Corexit*Herbivore, data=InLgt_sc)
summary(Photo1_effsz)
####
sc_ind <- scale(InitialDark[,c(12:33)], center=T, scale=T)
head(sc_ind)
InDk_sc <- cbind(InitialDark[,c(1:10)], sc_ind)
head(InDk_sc)

Photo1d_effsz <- lm(Fv.Fm ~ Oil*Corexit*Herbivore, data=InDk_sc)
summary(Photo1d_effsz)


## Initial Week 2 data
# center and scale all data to get effect sizes
sc_2 <- scale(Wk2Light[,c(12:33)], center=T, scale=T)
head(sc_2)
Wk2Lgt_sc <- cbind(Wk2Light[,c(1:10)], sc_2)
head(Wk2Lgt_sc)

Photo2_effsz <- lm(Photo ~ Oil*Corexit*Herbivore, data=Wk2Lgt_sc)
summary(Photo2_effsz)
####
sc_q2 <- scale(Wk2Light[,c(12:33)], center=T, scale=T)
head(sc_q2)
Wk2LqN_sc <- cbind(Wk2Light[,c(1:10)], sc_q2)
head(Wk2LqN_sc)

Photo2q_effsz <- lm(qN ~ Oil*Corexit*Herbivore, data=Wk2LqN_sc)
summary(Photo2q_effsz)










