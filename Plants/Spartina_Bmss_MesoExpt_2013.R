################################################################
### June 6, 2013                                             ###
### Spartina Biomass Meso Expt 2013                          ###
### Rachael E. Blake                                         ###
################################################################
library(plyr) ; library(dplyr) ; library(ggplot2) ; library(grid) ; library(scales) 
library(car)


SP <- read.csv("D:/Documents/LSU/MesoExp_2013/Sp_Biomass/SpBmss_MesoExpt 2013.csv")
head(SP) ; tail(SP) ; names(SP)

HgtMean <- SP %>%
           select(-SI_Sample, -Influor, -Live_Dead) %>%
           group_by(Week, Bucket, Treat, Chem, Oil, Corexit, Herbivore) %>%
           summarise_each(funs(mean)) %>%
           ungroup() %>%
           arrange(Bucket, Week)
     

#write.csv(HgtMean,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sp_Biomass\\SpBmss_Meso2013.csv")

###################################################################

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

######################################
## Using the mean data
Stems <- HgtMean
head(Stems) ; names(Stems)




###### Week 0 ANOVA Type III SS
Wk0 <- subset(Stems, Stems$Week==0)#, select=TtlStemNum:Herbivore)
Wk0

options(contrasts=c("contr.sum","contr.poly"))
# Stem Height
Wk0Hgt <- lm(StemHgt_cm ~ Oil*Corexit*Herbivore, data=Wk0)       
Anova(Wk0Hgt, type="III") # calculates ANOVA table with Type III SS
# Stem Count
Wk0Stem <- lm(TtlStemNum ~ Oil*Corexit*Herbivore, data=Wk0)
Anova(Wk0Stem, type="III")

###### Week 2 ANOVA Type III SS
Wk2 <- subset(Stems, Stems$Week==2)#, select=TtlStemNum:Herbivore)
Wk2

options(contrasts=c("contr.sum","contr.poly"))
# Stem Height
Wk2Hgt <- lm(StemHgt_cm ~ Oil*Corexit*Herbivore, data=Wk2)       
summary(Wk2Hgt)  # give summary of all - useful to see what's missing
vif(Wk2Hgt)  # calculates variance inflation factor  to determine multicollinearity
Anova(Wk2Hgt, type="III") # calculates ANOVA table with Type III SS
# Stem Count
Wk2Stem <- lm(TtlStemNum ~ Oil*Corexit*Herbivore, data=Wk2)
summary(Wk2Stem)
vif(Wk2Stem)
Anova(Wk2Stem, type="III")


Wk2$Chem1 <- factor(Wk2$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Stem Heights
Wk2PlotH <- ggplot(data=Wk2, aes(x=Herbivore, y=StemHgt_cm, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank())
# Stem Number
Wk2PlotS <- ggplot(data=Wk2, aes(x=Herbivore, y=TtlStemNum, fill=Chem1)) + 
                   geom_boxplot() + theme_bw() +
                   theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.95, .85),axis.text=element_text(size=20),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                    scale_fill_manual(values=colors, guide=guide_legend(title = NULL))

###### Week 4 ANOVA Type III SS
Wk4 <- subset(Stems, Stems$Week==4)#, select=TtlStemNum:Herbivore)
Wk4

options(contrasts=c("contr.sum","contr.poly"))
# Stem Height
Wk4Hgt <- lm(StemHgt_cm ~ Oil*Corexit*Herbivore, data=Wk4)       
Anova(Wk4Hgt, type="III") # calculates ANOVA table with Type III SS
# Stem Count
Wk4Stem <- lm(TtlStemNum ~ Oil*Corexit*Herbivore, data=Wk4)
Anova(Wk4Stem, type="III")


Wk4$Chem1 <- factor(Wk4$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
colors <- c("green","red","yellow","orange")
# Stem Heights
Wk4PlotH <- ggplot(data=Wk4, aes(x=Herbivore, y=StemHgt_cm, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank())
# Stem Number
Wk4PlotS <- ggplot(data=Wk4, aes(x=Herbivore, y=TtlStemNum, fill=Chem1)) + 
                   geom_boxplot() + theme_bw() +
                   theme(panel.grid=element_blank(),legend.key=element_blank(),
                         legend.background=element_blank(),legend.text=element_text(size=18),
                         legend.position=c(.95, .85),axis.text=element_text(size=20),
                         panel.border=element_blank(),axis.line=element_line(color='black'),
                         panel.background=element_blank(),plot.background=element_blank(),
                         axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                         axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                   scale_fill_manual(values=colors, guide=guide_legend(title = NULL))

###### Week 6 ANOVA Type III SS
Wk6 <- subset(Stems, Stems$Week==6)#, select=TtlStemNum:Herbivore)
Wk6

options(contrasts=c("contr.sum","contr.poly"))
# Stem Height
Wk6Hgt <- lm(StemHgt_cm ~ Oil*Corexit*Herbivore, data=Wk6)       
Anova(Wk6Hgt, type="III") # calculates ANOVA table with Type III SS
# Stem Count
Wk6Stem <- lm(TtlStemNum ~ Oil*Corexit*Herbivore, data=Wk6)
Anova(Wk6Stem, type="III")


Wk6$Chem1 <- factor(Wk6$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
colors <- c("green","red","yellow","orange")
# Stem Heights
Wk6PlotH <- ggplot(data=Wk6, aes(x=Herbivore, y=StemHgt_cm, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank())
# Stem Number
Wk6PlotS <- ggplot(data=Wk6, aes(x=Herbivore, y=TtlStemNum, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.95, .85),axis.text=element_text(size=20),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  

###### Week 8 ANOVA Type III SS
Wk8 <- subset(Stems, Stems$Week==8)#, select=TtlStemNum:Herbivore)
Wk8

options(contrasts=c("contr.sum","contr.poly"))
# Stem Height
Wk8Hgt <- lm(StemHgt_cm ~ Oil*Corexit*Herbivore, data=Wk8)       
Anova(Wk8Hgt, type="III") # calculates ANOVA table with Type III SS
# Stem Count
Wk8Stem <- lm(TtlStemNum ~ Oil*Corexit*Herbivore, data=Wk8)
Anova(Wk8Stem, type="III")


Wk8$Chem1 <- factor(Wk8$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# Stem Heights
Wk8PlotH <- ggplot(data=Wk8, aes(x=Herbivore, y=StemHgt_cm, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank())
# Stem Number
Wk8PlotS <- ggplot(data=Wk8, aes(x=Herbivore, y=TtlStemNum, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.95, .85),axis.text=element_text(size=20),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))


##################################################################################################
## ABOVEGROUND BIOMASS
##########################################
PMass <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Sp_Biomass/SpBmss_Mass_MesoExpt 2013.csv")
head(PMass) ; names(PMass)

# Select only Live Plant Mass
LiveP <- PMass[PMass$Taxon %in% c("LiveStems","LiveShoots"),] # subset live aboveground
LiveP <- LiveP %>%
         select(-Taxon) %>%
         group_by(Bucket, Treat_Code, Chem, Oil, Corexit, Herbivore) %>%
         summarise_each(funs(sum)) %>%
         ungroup()

# Select only Dead Plant Mass
DeadP <- PMass[PMass$Taxon %in% c("DeadStems","DeadShoots"),] # subset dead aboveground
DeadP <- DeadP %>%
         select(-Taxon) %>%
         group_by(Bucket, Treat_Code, Chem, Oil, Corexit, Herbivore) %>%
         summarise_each(funs(sum)) %>%
         ungroup()

# Aggregate to get total Plant Mass per bucket
TtlP1 <- PMass %>%
         select(-Taxon, -TinMass_g, -Tin_Dry_g, -Tin_Ash_g) %>%
         group_by(Bucket, Treat_Code, Chem, Oil, Corexit, Herbivore) %>%
         summarise_each(funs(sum)) %>%
         ungroup() %>%
         rename(Total_Dry_Wgt=Dry_Wgt) %>%
         arrange(Bucket)

#
# ANOVA Total Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlPMass <- lm(Total_Dry_Wgt ~ Oil*Corexit*Herbivore, data=TtlP1)       
Anova(FnlPMass, type="III") # calculates ANOVA table with Type III SS

# ANOVA Live Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlLvMass <- lm(Dry_Wgt ~ Oil*Corexit*Herbivore, data=LiveP)       
Anova(FnlLvMass, type="III") # calculates ANOVA table with Type III SS

# ANOVA Dead Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlDyMass <- lm(Dry_Wgt ~ Oil*Corexit*Herbivore, data=DeadP)       
Anova(FnlDyMass, type="III") # calculates ANOVA table with Type III SS

# PLOTS
colors <- c("green","red","yellow","orange")
# Total Biomass
TtlP1$Chem1 <- factor(TtlP1$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
TtlPlant <- ggplot(data=TtlP1, aes(x=Herbivore, y=Dry_Wgt, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + ylab("Total Biomass") +
                    theme(panel.grid=element_blank())
# Live Biomass
LiveP$Chem1 <- factor(LiveP$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
LiveStem <- ggplot(data=LiveP, aes(x=Herbivore, y=Dry_Wgt, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + ylab("Live Biomass") +
                    theme(panel.grid=element_blank())
# Dead Biomass
DeadP$Chem1 <- factor(DeadP$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
DeadStem <- ggplot(data=DeadP, aes(x=Herbivore, y=Dry_Wgt, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + ylab("Dead Biomass") +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.75, .90),axis.text=element_text(size=20),
                          axis.title=element_text(vjust=-0.04),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))



#########################################################################################
## BELOWGROUND BIOMASS
################################################
BG <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Sp_Biomass/BelowGroundSpartina_MesoExpt 2013.csv")
head(BG) ; names(BG)

# Select only Live Root Mass
LiveU <- BG[BG$Taxon %in% c("LiveRoots"),] # subset live belowground

# Select only Dead Root Mass
DeadU <- BG[BG$Taxon %in% c("DeadRoots"),] # subset dead belowground

# Select ALL Root Mass
AllU <- BG[BG$Taxon %in% c("LiveRoots","DeadRoots"),] ; head(AllU)

TtlU1 <- AllU %>%
         select(-Taxon) %>%
         group_by(Bucket, Treat_Code, Chem, Oil, Corexit, Herbivore) %>%
         summarise_each(funs(sum)) %>%
         ungroup() %>%
         arrange(Bucket)

#write.csv(TtlU1,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sp Biomass\\TtlU1_SpBmss_Meso2013.csv", row.names=F)

##### ANOVAs 
# ANOVA Total Underground Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlUgMass <- lm(Dry_Wgt_g ~ Oil*Corexit*Herbivore, data=TtlU1)       
Anova(FnlUgMass, type="III") # calculates ANOVA table with Type III SS

# ANOVA Live Root Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlLvRootMass <- lm(Dry_Wgt_g ~ Oil*Corexit*Herbivore, data=LiveU)       
Anova(FnlLvRootMass, type="III") # calculates ANOVA table with Type III SS

# ANOVA Dead Root Biomass
options(contrasts=c("contr.sum","contr.poly"))
FnlDdRootMass <- lm(Dry_Wgt_g ~ Oil*Corexit*Herbivore, data=DeadU)       
Anova(FnlDdRootMass, type="III") # calculates ANOVA table with Type III SS


##### PLOTS
colors <- c("green","red","yellow","orange")

TtlU1$Chem1 <- factor(TtlU1$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
TtlRoot <- ggplot(data=TtlU1, aes(x=Herbivore, y=Dry_Wgt_g, fill=Chem1)) + 
                  geom_boxplot() + theme_bw() + ylab("Total Root Mass (g)") + xlab("") +
                  theme(panel.grid=element_blank())

LiveU$Chem1 <- factor(LiveU$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
LiveRoot <- ggplot(data=LiveU, aes(x=Herbivore, y=Dry_Wgt_g, fill=Chem1)) + 
                   geom_boxplot() + theme_bw() + ylab("Live Root Mass (g)") + xlab("") +
                   theme(panel.grid=element_blank())

DeadU$Chem1 <- factor(DeadU$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
DeadRoot <- ggplot(data=DeadU, aes(x=Herbivore, y=Dry_Wgt_g, fill=Chem1)) + 
                    geom_boxplot() + theme_bw() + ylab("Dead Root Mass (g)") + xlab("") +
                    theme(panel.grid=element_blank(),legend.key=element_blank(),
                          legend.background=element_blank(),legend.text=element_text(size=18),
                          legend.position=c(.75, .90),axis.text=element_text(size=20),
                          axis.title=element_text(vjust=0.6, size=14),
                          panel.border=element_blank(),axis.line=element_line(color='black'),
                          panel.background=element_blank(),plot.background=element_blank(),
                          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                     scale_fill_manual(values=colors, guide=guide_legend(title = NULL))


################################
# Above- and Belowground Biomass comparisons and relationships
# Ratio of live to dead


# Above
Livea <- LiveP$Dry_Wgt
Deada <- DeadP[,c(1,10)]

TtlP2 <- TtlP1 %>%
          mutate(Live_AG = Livea) %>%
          full_join(Deada, by="Bucket") %>%
          dplyr::rename(Dead_AG=Dry_Wgt) %>%
          mutate(Ratio_AG = Dead_AG/Live_AG)

TtlP2$Chem1 <- factor(TtlP2$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
DeadShootRatio <- ggplot(data=TtlP2, aes(x=Herbivore, y=Ratio_AG, fill=Chem1)) + 
                         geom_boxplot() + theme_bw() + ylab("Proportion Dead Shoots (g)") + xlab("") +
                         theme(panel.grid=element_blank(),legend.key=element_blank(),
                               legend.background=element_blank(),legend.text=element_text(size=18),
                               legend.position=c(.85, .90),axis.text=element_text(size=20),
                               axis.title=element_text(vjust=0.6, size=15),
                               panel.border=element_blank(),axis.line=element_line(color='black'),
                               panel.background=element_blank(),plot.background=element_blank(),
                               axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                               axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                         scale_fill_manual(values=colors, guide=guide_legend(title = NULL))



# Below
Live <- LiveU$Dry_Wgt_g
TtlU1$Live_UG <- Live

Dead <- DeadU$Dry_Wgt_g
TtlU1$Dead_UG <- Dead

TtlU2 <- mutate(TtlU1, Ratio_UG=Dead_UG/Live_UG)

TtlU2$Chem1 <- factor(TtlU2$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
DeadRootRatio <- ggplot(data=TtlU2, aes(x=Herbivore, y=Ratio_UG, fill=Chem1)) + 
                        geom_boxplot() + theme_bw() + ylab("Proportion Dead Roots (g)") + xlab("") +
                        theme(panel.grid=element_blank(),legend.key=element_blank(),
                              legend.background=element_blank(),legend.text=element_text(size=18),
                              legend.position=c(.85, .90),axis.text=element_text(size=20),
                              axis.title=element_text(vjust=0.6, size=15),
                              panel.border=element_blank(),axis.line=element_line(color='black'),
                              panel.background=element_blank(),plot.background=element_blank(),
                              axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
                              axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid')) +
                        scale_fill_manual(values=colors, guide=guide_legend(title = NULL))



# Dead Roots vs. Dead Stems
ALLDATA <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/ALL_DATA_SEM_MesoExpt2013.csv")

# Below is from: https://stat.ethz.ch/pipermail/r-help/2011-November/295230.html

# Fit a linear model to the data and save the model object:
modD <- lm(LogDeadStemDryWgt~LogDdRootDryWgt, data=ALLDATA)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
routD <- list(paste('Fitted model:',round(coef(modD)[1],3),' + ',
                   round(coef(modD)[2],3), 'x',sep = ''),
             paste('R^2 == ',round(summary(modD)[['r.squared']],3),
                   sep=''))
Dd <- ggplot(ALLDATA, aes(LogDdRootDryWgt,LogDeadStemDryWgt)) + geom_point() +
             geom_smooth(method=lm) + #theme_boxplot() + 
             geom_text(aes(x=.5, y=.8, label=routD[[2]]), 
                       hjust=0, parse=TRUE)
Dd

# Live Roots vs. Live Stems
# Fit a linear model to the data and save the model object:
modL <- lm(LvRootDryWgt~LiveStemDryWgt_g, data=ALLDATA)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
routL <- list(paste('Fitted model:',round(coef(modL)[1],3),' + ',
                   round(coef(modL)[2],3), 'x',sep = ''),
             paste('R^2 == ',round(summary(modL)[['r.squared']],3),
                   sep=''))
Lv <- ggplot(ALLDATA, aes(LiveStemDryWgt_g,LvRootDryWgt)) + geom_point() +
             geom_smooth(method=lm) + #theme_boxplot() + 
             geom_text(aes(x=10, y=50, label=routL[[2]]), 
                       hjust=0, parse=TRUE)
Lv

# Dead Shoots vs. Live Roots
# Fit a linear model to the data and save the model object:
modLD <- lm(LogDeadStemDryWgt~LvRootDryWgt, data=ALLDATA)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
routLD <- list(paste('Fitted model:',round(coef(modLD)[1],3),' + ',
                   round(coef(modLD)[2],3), 'x',sep = ''),
             paste('R^2 == ',round(summary(modLD)[['r.squared']],3),
                   sep=''))
LvD <- ggplot(ALLDATA, aes(LvRootDryWgt,LogDeadStemDryWgt)) + geom_point() +
              geom_smooth(method=lm) + #theme_boxplot() + 
              geom_text(aes(x=10, y=1, label=routLD[[2]]), 
                        hjust=0, parse=TRUE)
LvD 


# Live Shoots vs. Dead Roots
# Fit a linear model to the data and save the model object:
modDL <- lm(LogDdRootDryWgt~LiveStemDryWgt_g, data=ALLDATA)
# Create a list of character strings - the first component
# produces the fitted model, the second produces a
# string to compute R^2, but in plotmath syntax.
routDL <- list(paste('Fitted model:',round(coef(modDL)[1],3),' + ',
                   round(coef(modDL)[2],3), 'x',sep = ''),
             paste('R^2 == ',round(summary(modDL)[['r.squared']],3),
                   sep=''))
DLv <- ggplot(ALLDATA, aes(LiveStemDryWgt_g,LogDdRootDryWgt)) + geom_point() +
              geom_smooth(method=lm) + #theme_boxplot() + 
              geom_text(aes(x=10, y=1.25, label=routDL[[2]]), 
                        hjust=0, parse=TRUE)
DLv 

################# 



