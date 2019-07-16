#################################################
### Expected Stressors Effects Function      ###
### Modified from that used for 2006 expt     ###
### Rachael E. Blake    March 2014            ###
#################################################

library(tidyr) ; library(plyr) ; library(dplyr) ; library(ggplot2) ; library(scales) 
library(lazyeval) ; library(reshape2)

# All data
AllD <- read.csv(here::here("ALL_DATA_SEM_MesoExpt2013.csv"))
AllD$Chem1 <- factor(AllD$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))


# function to subset the data by full treatment
treatment_subset <- function(Herb_level, resp_var) {
                    sub_df <- AllD %>%
                              filter(Herbivore == Herb_level) %>%
                              select("Bucket", "Chem", resp_var) %>%
                              spread(key = "Chem", value = resp_var)

                    sub_2 <- sub_df %>%
                             select(-Bucket) %>%
                             summarise_each(funs(mean(., na.rm = TRUE))) %>%
                             rename(corexit=Core, control=NC, oil=Oil, oilcore=OilCore)

                    return(sub_2)
}


# Live Stem Biomass
LIVE_NG <- treatment_subset("NG", "LiveStemDryWgt_g")
LIVE_S <- treatment_subset("S", "LiveStemDryWgt_g")
LIVE_P <- treatment_subset("P", "LiveStemDryWgt_g")
LIVE_SP <- treatment_subset("SP", "LiveStemDryWgt_g")

# Dead Stem Biomass
DEAD_NG <- treatment_subset("NG", "DeadStemDryWgt_g")
DEAD_S <- treatment_subset("S", "DeadStemDryWgt_g")
DEAD_P <- treatment_subset("P", "DeadStemDryWgt_g")   
DEAD_SP <- treatment_subset("SP", "DeadStemDryWgt_g")
  
# Stem Number
NUM_NG <- treatment_subset("NG", "TtlStemNum")
NUM_P <- treatment_subset("P", "TtlStemNum")
NUM_S <- treatment_subset("S", "TtlStemNum")
NUM_SP <- treatment_subset("SP", "TtlStemNum")

# Live Root Biomass
LIVR_NG <- treatment_subset("NG", "LvRootDryWgt_Scaled")
LIVR_S <- treatment_subset("S", "LvRootDryWgt_Scaled")
LIVR_P <- treatment_subset("P", "LvRootDryWgt_Scaled")
LIVR_SP <- treatment_subset("SP", "LvRootDryWgt_Scaled")

# Dead Root Biomass
DEDR_NG <- treatment_subset("NG", "DdRootDryWgt")
DEDR_S <- treatment_subset("S", "DdRootDryWgt")
DEDR_P <- treatment_subset("P", "DdRootDryWgt")
DEDR_SP <- treatment_subset("SP", "DdRootDryWgt")

# Prokelisia
PROK_P <- treatment_subset("P", "ProkAbunScaled")
PROK_SP <- treatment_subset("SP", "ProkAbunScaled")

# Snails
SNAL_S <- treatment_subset("S", "SnailWgtScaled")
SNAL_SP <- treatment_subset("SP", "SnailWgtScaled")


# make a list of the data frames
expstrlist <- list(LIVE_NG, LIVE_S, LIVE_P, LIVE_SP, DEAD_NG, DEAD_S, DEAD_P, DEAD_SP,
                   NUM_NG, NUM_P, NUM_S, NUM_SP, LIVR_NG, LIVR_S, LIVR_P, LIVR_SP, 
                   DEDR_NG, DEDR_S, DEDR_P, DEDR_SP, PROK_P, PROK_SP, SNAL_S, SNAL_SP)
names(expstrlist) <- c("LIVE_NG", "LIVE_S", "LIVE_P", "LIVE_SP", "DEAD_NG", "DEAD_S", 
                       "DEAD_P", "DEAD_SP", "NUM_NG", "NUM_P", "NUM_S", "NUM_SP",
                       "LIVR_NG", "LIVR_S", "LIVR_P", "LIVR_SP", "DEDR_NG", "DEDR_S",
                       "DEDR_P", "DEDR_SP","PROK_P", "PROK_SP", "SNAL_S", "SNAL_SP")

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






