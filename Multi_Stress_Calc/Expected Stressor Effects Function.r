#################################################
### Expectged Stressors Effects Function      ###
### Modified from that used for 2006 expt     ###
### Rachael E. Blake    March 2014            ###
#################################################

library(tidyr) ; library(plyr) ; library(dplyr) ; library(ggplot2) ; library(scales)

# All data
AllD <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/ALL_DATA_SEM_MesoExpt2013.csv")
AllD$Chem1 <- factor(AllD$Chem, levels=c('NC', 'Oil', 'Core', 'OilCore'))

# LIVE2 <- AllD %>%
#          select(Bucket, Chem, LiveStemDryWgt_g) %>%
#          spread(key=Chem, value=LiveStemDryWgt_g) %>%
#          select(-Bucket) %>%
#          filter(!is.na(Core),
#                 !is.na(NC),
#                 !is.na(Oil),
#                 !is.na(OilCore))


# Live Stem Biomass
LIVE <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_LiveBmss_MesoExpt2013.csv")
boxplot(LIVE)
ggplot(AllD, aes(x=Chem1, y=LiveStemDryWgt_g)) + stat_summary(fun.y="mean", geom="bar")
LIVEmn <- summarise_each(LIVE, funs(mean))

# Dead Stem Biomass
DEAD <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_DeadBmss_MesoExpt2013.csv")
boxplot(DEAD)
ggplot(AllD, aes(x=Chem1, y=DeadStemDryWgt_g)) + stat_summary(fun.y="mean", geom="bar")
DEADmn <- summarise_each(DEAD, funs(mean))

# Stem Number
NUM <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_StemNum_MesoExpt2013.csv")
boxplot(NUM)
ggplot(AllD, aes(x=Chem1, y=TtlStemNum)) + stat_summary(fun.y="mean", geom="bar")
NUMmn <- summarise_each(NUM, funs(mean))

# Prokelisia
PROK <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_Insects_MesoExpt2013.csv")
boxplot(PROK)
ggplot(AllD, aes(x=Chem1, y=ProkAbunScaled)) + stat_summary(fun.y="mean", geom="bar")
PROKmn <- summarise_each(PROK, funs(mean(., na.rm=TRUE)))

# Snails
SNAL <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_Snails_MesoExpt2013.csv")
boxplot(SNAL)
ggplot(AllD, aes(x=Chem1, y=SnailWgtScaled)) + stat_summary(fun.y="mean", geom="bar")
SNALmn <- summarise_each(SNAL, funs(mean(., na.rm=TRUE)))

# make a list of the data frames
expstrlist <- list(LIVEmn, DEADmn, NUMmn, PROKmn, SNALmn)

###################################################
# Expected Stressor Effects Function Code: (Additive)

ExpectedStress <- function(dafr) {
       if (dafr$control<dafr$corexit & dafr$control<dafr$oil){ExpSt <- dafr$control+(dafr$oil-dafr$control)+(dafr$corexit-dafr$control)} else     #positive
       if (dafr$control>dafr$corexit & dafr$control>dafr$oil){ExpSt <- dafr$control-(dafr$control-dafr$oil)-(dafr$control-dafr$corexit)} else    #negative
       if (dafr$control>dafr$corexit & dafr$control<dafr$oil){ExpSt <- dafr$control+(dafr$oil-dafr$control)-(dafr$control-dafr$corexit)} else    #oil>control
       if (dafr$control<dafr$corexit & dafr$control>dafr$oil){ExpSt <- dafr$control+(dafr$corexit-dafr$control)-(dafr$control-dafr$oil)}         #oil<control
       return (ExpSt)
       }

#specify which equation to use
#    1 = positive
#    2 = negative
#    3 = mixed1 (oil>control)
#    4 = mixed2 (oil<control)      
       
ExpectedStress(LIVEmn)

####################################################

# use lapply to run my fuction for each of the datasets












