##############################################################
##  Photosynthesis and Fv/Fm Measurements                   ##
##  Mesocosm Multi-Stressor Experiment 2013                 ##
##  Script by Rachael Blake, September 2013                 ##
##############################################################

# Load libraries
library(plyr) ; library(ggplot2) ; library(dplyr) ; library(grid) ; library(scales)
library(car) ; library(gridExtra) ; library(tidyr)

# Read in data file of all photosynthesis data
PhotoALL <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/LICOR_Files_Meso_Expt/LICOR_PhotosynMeas_MesoExpt_2013.csv", header=TRUE)
names(PhotoALL) ; head(PhotoALL) ; tail(PhotoALL)

# dplyr() doesn't like dates, so leaving this column as factors, 
# but don't want to lose how I changed output format of as.Date.
#PhotoALL$Date <- format(as.Date(PhotoALL$Date, format="%d-%b"),"%d-%b-2013") 

# REMOVE May 20th LIGHT data..it is wacky for some reason I can't figure out
#PhotoALL1 <- PhotoALL[!PhotoALL$Date %in% c("20-May"),]

# Taking Mean of the three measurements in each bucket #######
PMean <- PhotoALL %>%
         group_by(Date,MeasType,Bucket.Number,Treatment,Chem,Oil,Corexit,Herbivore) %>%
         summarise_each(funs(mean),-HHMMSS) %>%
         ungroup() %>% 
         filter(Date != "20-May") %>%
         mutate(Week = ifelse((Date %in% c("20-May","21-May","23-May","24-May")),'Initial (Week 1)',
                       ifelse((Date %in% c("27-May","28-May","30-May","31-May")),'Week 2',
                       ifelse((Date %in% c("3-Jun","4-Jun","5-Jun","6-Jun")),'Week 3',
                       ifelse((Date %in% c("2-Jul","3-Jul","5-Jul","6-Jul")),'Final (Week 8)',"")))),
                WeekB = ifelse((Date %in% c("20-May","21-May","23-May","24-May","27-May",
                                            "28-May","30-May","31-May")),'Initial',
                        ifelse((Date %in% c("3-Jun","4-Jun","5-Jun","6-Jun")),"Week 3",
                        ifelse((Date %in% c("2-Jul","3-Jul","5-Jul","6-Jul")),'Final',"")))
                )
         
# for ordering the plots
PMean$Chem1 <- factor(PMean$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
PMean$Week1 <- factor(PMean$Week, levels=c('Initial (Week 1)', 'Week 2', 'Week 3', 'Final (Week 8)'))
PMean$WeekBb <- factor(PMean$WeekB, levels=c('Initial','Week 3','Final'))
levels(PMean$Week1) <- paste0(" \n", levels(PMean$Week1) , "\n ")
levels(PMean$WeekBb) <- paste0(" \n", levels(PMean$WeekBb) , "\n ")


#write.csv(PMean,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\LICOR Files_Meso Expt\\Photo_Mean_MesoExpt2013.csv")

PMean_long <- PMean %>%
              select(Date, MeasType, Bucket.Number, Treatment, Chem, Oil, Corexit, Herbivore, Photo, Fv.Fm, 
                     qP, qN, Week, Week1, Chem1, WeekBb) %>%
              gather(VarType, Value, -c(Date, MeasType, Bucket.Number, Treatment, Chem, Oil, Corexit, Herbivore,
                                        Week, Week1, Chem1, WeekBb)) %>%
              filter(Week != "Week 3") %>%
              filter(VarType != "qN" | Week != "Initial (Week 1)") %>%
              filter(VarType != "qP" | Week != "Initial (Week 1)") %>%
              filter(VarType != "Photo" | Week != "Initial (Week 1)") %>%
              filter(VarType != "Fv.Fm" | Week != "Week 2") %>%
              filter(VarType != "Fv.Fm" | MeasType != "Light") %>%
              filter(VarType != "qP" | MeasType != "Dark") %>%
              filter(VarType != "qN" | MeasType != "Dark") %>%
              filter(VarType != "Photo" | MeasType != "Dark") %>%
              mutate_each(funs(factor), VarType)

levels(PMean_long$VarType) <- paste0(" \n", levels(PMean_long$VarType), "\n ")


#############################################################
# Subsetting the data

PMean_L <- PMean %>%
           filter(MeasType=="Light")


InitialLight <- PMean %>%
                filter(MeasType=="Light",
                       Week == "Initial (Week 1)")

InitialDark <- PMean %>%
               filter(MeasType=="Dark",
                      Week == "Initial (Week 1)")

Wk2Light <- PMean %>%
            filter(MeasType=="Light",
                   Week == "Week 2")

Wk2Dark <- PMean %>%
           filter(MeasType=="Dark",
                  Week == "Week 2") 

Wk3Light <- PMean %>%
            filter(MeasType=="Light",
                   Week == "Week 3")

Wk3Dark <- PMean %>%
           filter(MeasType=="Dark",
                  Week == "Week 3")
  
FinalLight <- PMean %>%
              filter(MeasType=="Light",
                     Week == "Final (Week 8)")

FinalDark <- PMean %>%
             filter(MeasType=="Dark",
                    Week == "Final (Week 8)")

###############################################################
# Plotting one response variable through time
colors <- c("green","red","yellow","orange")

# Fv/Fm
FvFmPlot <- ggplot(data=PMean, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
                  geom_boxplot() + theme_bw() + facet_wrap(~ Week1) +
                  theme(strip.text.x=element_text(size=14),
                        strip.text.x=element_text(size=14, angle=90),
                        strip.background=element_rect(fill="white"),
                        panel.grid=element_blank(),legend.key=element_blank(),
                        legend.background=element_blank(),legend.text=element_text(size=12),
                        legend.position=c(.93, .1),axis.text=element_text(size=18),
                        panel.border=element_blank(),axis.line=element_line(color='black'),
                        panel.background=element_blank(),plot.background=element_blank(),
                        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                   scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  




# NOTE: Have to remove May 20th data...it's weird from some reason
PMean_L_sub <- PMean_L %>% 
               filter(Date != "20-May") %>%
               select(Date, MeasType, Treatment, Chem, Herbivore, Fv.Fm, Photo, qN, qP, Week, Chem1, Week1, WeekB)

# and then still include all treatment combos so the plotting comes out correctly
table(PMean_L_sub$Herbivore, PMean_L_sub$Chem1, PMean_L_sub$Week1)

complete <- expand.grid(Chem1 = unique(PMean_L_sub$Chem1),
                        Herbivore = unique(PMean_L_sub$Herbivore),
                        Week1 = unique(PMean_L_sub$Week1)) 

PMean_L_sub2 <- full_join(complete, PMean_L_sub, by=c("Chem1", "Herbivore", "Week1")) %>%
                mutate(Photo = ifelse(is.na(Photo), -10, Photo),
                       qN = ifelse(is.na(qN), -10, qN),
                       qP = ifelse(is.na(qP), -10, qP))

table(PMean_L_sub2$Herbivore, PMean_L_sub2$Chem1, PMean_L_sub2$Week1)

#levels(PMean_L_sub2$Week1) <- paste0(" \n", levels(PMean_L_sub2$Week1) , "\n ")

# Photosynthesis
PhotoPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=Photo)) + 
                    geom_boxplot(aes(fill=Chem1)) + theme_bw() + facet_wrap(~ WeekB, ncol=2) +
                    coord_cartesian(ylim = c(0, 30) + c(-.25, .25)) +
                    theme(strip.text.x=element_text(size=14),
                          strip.text.x=element_text(size=14, angle=90),
                          strip.background=element_rect(fill="white"),
                          panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=12),
                          legend.position=c(.9, .4),axis.text=element_text(size=18),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                     scale_fill_manual(values=colors, labels = c("NC","Core","Oil","OilCore"),
                                       guide=guide_legend(title = NULL), drop=FALSE) 



 
# qN 
qNPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=qN)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_bw() + facet_wrap(~ Week1, ncol=2) +
                 coord_cartesian(ylim = c(1.5, 3.5) + c(-.25, .25)) +
                 theme(strip.text.x=element_text(size=14),
                       strip.text.x=element_text(size=14, angle=90),
                       strip.background=element_rect(fill="white"),
                       panel.grid=element_blank(),legend.key=element_blank(),
                       legend.background=element_blank(),legend.text=element_text(size=12),
                       legend.position=c(.1, .9),axis.text=element_text(size=18),
                       panel.border=element_blank(),axis.line=element_line(color='black'),
                       panel.background=element_blank(),plot.background=element_blank(),
                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  



# qP
qPPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=qP)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_bw() + facet_wrap(~Week1, ncol=2) +
                 coord_cartesian(ylim = c(0.25, 0.75) + c(-.25, .25)) +
                 theme(strip.text.x=element_text(size=14),
                       strip.text.x=element_text(size=14, angle=90),
                       strip.background=element_rect(fill="white"),
                       panel.grid=element_blank(),legend.key=element_blank(),
                       legend.background=element_blank(),legend.text=element_text(size=12),
                       legend.position=c(.1, .9),axis.text=element_text(size=18),
                       panel.border=element_blank(),axis.line=element_line(color='black'),
                       panel.background=element_blank(),plot.background=element_blank(),
                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  


# large plot of all
whPlot <- ggplot(data=PMean_long, aes(x=Herbivore, y=Value)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_bw() + 
                 facet_grid(VarType~WeekBb, scales="free") +
                 theme(strip.text.x=element_text(size=14),
                       strip.text.x=element_text(size=14, angle=90),
                       strip.background=element_rect(fill="white"),
                       panel.grid=element_blank(),legend.key=element_blank(),
                       legend.background=element_blank(),legend.text=element_text(size=12),
                       legend.position=c(.1, .9),axis.text=element_text(size=18),
                       panel.border=element_blank(),axis.line=element_line(color='black'),
                       panel.background=element_blank(),plot.background=element_blank(),
                       axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                       axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                 scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  
whPlot



###### FINAL DATA (END OF EXPERIMENT) #########################

## Plotting the final data
# colors <- c("green","red","yellow","orange")
# FinalLight$Chem1 <- factor(FinalLight$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# # Photosynthesis
# LightPlot <- ggplot(data=FinalLight, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
#                     geom_boxplot() + theme_bw() +
#                     theme(panel.grid=element_blank(),legend.key=element_blank(),
#                           legend.background=element_blank(),legend.text=element_text(size=18),
#                           legend.position=c(.95, .85),axis.text=element_text(size=20),
#                           panel.border=element_blank(),axis.line=element_line(color='black'),
#                           panel.background=element_blank(),plot.background=element_blank(),
#                           axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
#                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         
# # Photochemical quenching
# LightPlota <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() +
#                      theme(panel.grid=element_blank())
# # Nonphotochemcial quenching
# LightPlotb <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() +
#                      theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.1, .85),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank(),
#                            axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
#                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         
# # Phi PSII
# LightPlotc <- ggplot(data=FinalLight, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() +
#                      theme(panel.grid=element_blank())
# 
# # Fv/Fm
# FinalDark$Chem1 <- factor(FinalDark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# 
# DarkPlot <- ggplot(data=FinalDark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
#                    geom_boxplot() + theme_bw() +
#                    theme(panel.grid=element_blank(),legend.key=element_blank(),
#                          legend.background=element_blank(),legend.text=element_text(size=18),
#                          legend.position=c(.1, .2),axis.text=element_text(size=20),
#                          panel.border=element_blank(),axis.line=element_line(color='black'),
#                          panel.background=element_blank(),plot.background=element_blank(),
#                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
#                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))         


# Final Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))



FnlLightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=FinalLight)
             #contrasts=list(Oil=contr.sum, Corexit=contr.sum, Herbivore=contr.sum))
summary(FnlLightA)#$coeff  # give summary of all coefficients - useful to see what's missing
#vif(FnlLightA)  # calculates variance inflation factor to determine multicollinearity
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
#vif(FnlDarkA)  # calculates variance inflation factor to determine multicollinearity
Anova(FnlDarkA, type="III") # calculates ANOVA table with Type III SS


###### INITIAL DATA (WEEK 1 OF EXPERIMENT) #########################
# Should remove all May 20th data - it's whacky for some reason


# ## Plotting the initial data
# InitialLight$Chem1 <- factor(InitialLight$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# # Photosynthesis
# LightPlot2 <- ggplot(data=InitialLight, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() +
#                      theme(panel.grid=element_blank())
# # Photochemical quenching 
# LightPlot2a <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() +
#                       theme(panel.grid=element_blank())
# # Nonphotochemical quenching
# colors <- c("green","red","yellow","orange")
# LightPlot2b <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() + scale_fill_manual(values=colors) +
#                       theme(panel.grid=element_blank(),legend.key=element_blank(),
#                             legend.title=element_blank(),
#                             legend.background=element_blank(),legend.text=element_text(size=18),
#                             legend.position=c(.1, .2),axis.text=element_text(size=20),
#                             panel.border=element_blank(),axis.line=element_line(color='black'),
#                             panel.background=element_blank(),plot.background=element_blank(),
#                             axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                             axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
# 
# # Phi PSII
# LightPlot2c <- ggplot(data=InitialLight, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() +
#                       theme(panel.grid=element_blank())
# # Fv/Fm
# InitialDark$Chem1 <- factor(InitialDark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# colors <- c("green","red","yellow","orange")
# DarkPlot2 <- ggplot(data=InitialDark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
#                     geom_boxplot() + theme_bw() + scale_fill_manual(values=colors) +
#                     theme(panel.grid=element_blank(),legend.key=element_blank(),
#                           legend.title=element_blank(),
#                           legend.background=element_blank(),legend.text=element_text(size=18),
#                           legend.position=c(.1, .2),axis.text=element_text(size=20),
#                           panel.border=element_blank(),axis.line=element_line(color='black'),
#                           panel.background=element_blank(),plot.background=element_blank(),
#                           axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                           axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# Initial Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

InitLightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=InitialLight)
summary(InitLightA)#$coeff  # give summary of all coefficients - useful to see what's missing
#vif(InitLightA)  # calculates variance inflation factor to determine multicollinearity
Anova(InitLightA, type="III", singular.ok = T) # calculates ANOVA table with Type III SS
# Photochemical quenching 
InitLightAqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAqP, type="III", singular.ok = T) # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
InitLightAqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAqN, type="III", singular.ok = T) # calculates ANOVA table with Type III SS
# Phi PSII
InitLightAPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=InitialLight)
Anova(InitLightAPhi, type="III", singular.ok = T) # calculates ANOVA table with Type III SS

# Initial Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

InitDarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=InitialDark)
Anova(InitDarkA, type="III") # calculates ANOVA table with Type III SS


###### DATA FROM WEEK 2 OF EXPERIMENT #########################


# ## Plotting the Week 2 data
# colors <- c("green","red","yellow","orange")
# Wk2Light$Chem1 <- factor(Wk2Light$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# # Photosynthesis
# LightPlot3 <- ggplot(data=Wk2Light, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() + 
#                      theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.95, .85),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank(),
#                            axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
#                      scale_fill_manual(values=colors, guide=guide_legend(title = NULL))                   
# #ggsave(file="Wk2Photo.tiff", plot=LightPlot3, path="C:/Users/rblake/Desktop/")  
# # Photochemical quenching 
# LightPlot3a <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() + 
#                       theme(panel.grid=element_blank(),legend.key=element_blank()) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
#                      
# # Nonphotochemical quenching
# LightPlot3b <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() + 
#                       theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.95, .85),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank(),
#                            axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
#                            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# 
# # Phi PSII
# LightPlot3c <- ggplot(data=Wk2Light, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() + 
#                       theme(panel.grid=element_blank(),legend.key=element_blank()) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# 
# # Fv/Fm
# Wk2Dark$Chem1 <- factor(Wk2Dark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# 
# DarkPlot3 <- ggplot(data=Wk2Dark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
#                     geom_boxplot() + theme_bw() + 
#                     theme(panel.grid=element_blank(),legend.key=element_blank()) +
#                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
#                     

# Week 2 Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))
 
# Photosynthesis
Wk2LightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=Wk2Light)
Anova(Wk2LightA, type="III") # calculates ANOVA table with Type III SS
summary(Wk2LightA)#$coeff  # give summary of all coefficients - useful to see what's missing
#vif(Wk2LightA)  # calculates variance inflation factor to determine multicollinearity
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

# ## Plotting the Week 3 data
# #
# colors <- c("green","red","yellow","orange")
# Wk3Light$Chem1 <- factor(Wk3Light$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# # Photosynthesis
# LightPlot4 <- ggplot(data=Wk3Light, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
#                      geom_boxplot() + theme_bw() +
#                      theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.1, .2),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank()) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# # Photochemical quenching
# LightPlot4a <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(qP), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() +
#                       theme(panel.grid=element_blank())
# # Nonphotochemical quenching
# LightPlot4b <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(qN), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() +
#                       theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.1, .2),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank()) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 
# # Phi PSII
# LightPlot4c <- ggplot(data=Wk3Light, aes(x=Herbivore, y=as.numeric(PhiPS2), fill=Chem1)) + 
#                       geom_boxplot() + theme_bw() +
#                       theme(panel.grid=element_blank())
# 
# # Fv/Fm
# Wk3Dark$Chem1 <- factor(Wk3Dark$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# 
# DarkPlot4 <- ggplot(data=Wk3Dark, aes(x=Herbivore, y=as.numeric(Fv.Fm), fill=Chem1)) + 
#                     geom_boxplot() + theme_bw() +
#                     theme(panel.grid=element_blank(),legend.key=element_blank(),
#                            legend.background=element_blank(),legend.text=element_text(size=18),
#                            legend.position=c(.1, .2),axis.text=element_text(size=20),
#                            panel.border=element_blank(),axis.line=element_line(color='black'),
#                            panel.background=element_blank(),plot.background=element_blank()) +
#                       scale_fill_manual(values=colors, guide=guide_legend(title = NULL)) 

# Week 3 Photosynthesis (Light) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  
# Photosynthesis
Wk3LightA <- lm(Photo ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightA, type="III") # calculates ANOVA table with Type III SS
summary(Wk3LightA)#$coeff  # give summary of all coefficients - useful to see what's missing
#vif(Wk3LightA)  # calculates variance inflation factor to determine multicollinearity
# Photochemcial quenching 
Wk3LightAqP <- lm(as.numeric(qP) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAqP, type="III") # calculates ANOVA table with Type III SS
# Nonphotochemical quenching
Wk3LightAqN <- lm(as.numeric(qN) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAqN, type="III") # calculates ANOVA table with Type III SS
# Phi PSII
Wk3LightAPhi <- lm(as.numeric(PhiPS2) ~ Oil*Corexit*Herbivore, data=Wk3Light)
Anova(Wk3LightAPhi, type="III") # calculates ANOVA table with Type III SS

# Week 3 Fv/Fm (Dark) ANOVA
# ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

Wk3DarkA <- lm(as.numeric(Fv.Fm) ~ Oil*Corexit*Herbivore, data=Wk3Dark)
Anova(Wk3DarkA, type="III") # calculates ANOVA table with Type III SS




##############
#grid.arrange(DarkPlot2, DarkPlot3, DarkPlot4, ncol=2, nrow=2)

###############











