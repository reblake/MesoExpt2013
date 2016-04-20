#################################################
### Expected Stressors Effects Function      ###
### Modified from that used for 2006 expt     ###
### Rachael E. Blake    March 2014            ###
#################################################

library(tidyr) ; library(plyr) ; library(dplyr) ; library(ggplot2) ; library(scales) 
library(lazyeval) ; library(reshape2)

# All data
AllD <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/ALL_DATA_SEM_MesoExpt2013.csv")
AllD$Chem1 <- factor(AllD$Chem, levels=c('NC', 'Oil', 'Core', 'OilCore'))


# function to subset the data by full treatment
treatment_subset <- function(Herb_level, resp_var) {
                    sub_df <- AllD %>%
                             # filter_(.dots = list(~Chem == Chem_level)) %>%
                              filter_(.dots = list(~Herbivore == Herb_level)) %>%
                              select_("Chem", resp_var) %>%
                             # spread_(key="Chem", value=resp_var)
                              dcast( ~ "Chem", value.var=resp_var)
}




dcast(name ~ numbers)

# Live Stem Biomass
LIVE <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_LiveBmss_MesoExpt2013.csv")
boxplot(LIVE)
LIVEmn <- summarise_each(LIVE, funs(mean))

# Dead Stem Biomass
DEAD <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_DeadBmss_MesoExpt2013.csv")
boxplot(DEAD)
DEADmn <- summarise_each(DEAD, funs(mean))

# Stem Number
NUM <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_StemNum_MesoExpt2013.csv")
boxplot(NUM)
NUMmn <- summarise_each(NUM, funs(mean))

# Prokelisia
PROK <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_Insects_MesoExpt2013.csv")
boxplot(PROK)
PROKmn <- summarise_each(PROK, funs(mean(., na.rm=TRUE)))

# Snails
SNAL <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Exp_Stress_Snails_MesoExpt2013.csv")
boxplot(SNAL)
SNALmn <- summarise_each(SNAL, funs(mean(., na.rm=TRUE)))

# make a list of the data frames
expstrlist <- list(LIVEmn, DEADmn, NUMmn, PROKmn, SNALmn)
names(expstrlist) <- c("LIVEmn","DEADmn","NUMmn","PROKmn","SNALmn")

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
       
#ExpectedStress(LIVEmn)

####################################################

# use lapply to run my fuction for each of the datasets

ExpStrsCalc <- lapply(expstrlist, function(i){ExpectedStress(i)})






