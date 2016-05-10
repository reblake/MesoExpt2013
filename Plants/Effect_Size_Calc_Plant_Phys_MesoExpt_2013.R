##############################################################
##  ##### Effect size calculation #########                 ##
##  Mesocosm Multi-Stressor Experiment 2013                 ##
##  Script by Rachael Blake, September 2013                 ##
##############################################################

# Reference: Schielzeth H (2010) Simple means to improve the interpretability of regression 
# coefficients.  Methods in Ecology & Evolution 1:103-113.
#########################################

# Read in data file of all photosynthesis data
PhotoALL <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/LICOR_Files_Meso_Expt/LICOR_PhotosynMeas_MesoExpt_2013.csv", header=TRUE)
names(PhotoALL) ; head(PhotoALL) ; tail(PhotoALL)

# Taking Mean of the three measurements in each bucket #######
PMean <- PhotoALL %>%
         group_by(Date,MeasType,Bucket.Number,Treatment,Chem,Oil,Corexit,Herbivore) %>%
         summarise_each(funs(mean),-HHMMSS) %>%
         ungroup() %>% 
         filter(Date != "20-May") %>%
         mutate(Week = ifelse((Date %in% c("20-May","21-May","23-May","24-May")),'Initial (Week 1)',
                       ifelse((Date %in% c("27-May","28-May","30-May","31-May")),'Week 2',
                       ifelse((Date %in% c("3-Jun","4-Jun","5-Jun","6-Jun")),'Week 3',
                       ifelse((Date %in% c("2-Jul","3-Jul","5-Jul","6-Jul")),'Final (Week 8)',""))))
                )
         

#########################################
# must read in PMean first, and create Final Light and Final Dark - see above
head(PMean)
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
sc_fl <- scale(FinalLight[,c(11:33)], center=T, scale=T)
head(sc_fl)
FnlLgt_sc <- cbind(FinalLight[,c(1:9)], sc_fl)
head(FnlLgt_sc)

Photo8_effsz <- lm(Photo ~ Oil*Corexit*Herbivore, data=FnlLgt_sc)
summary(Photo8_effsz)


## Initial Week 1 data
# center and scale all data to get effect sizes
sc_in <- scale(InitialLight[,c(11:33)], center=T, scale=T)
head(sc_in)
InLgt_sc <- cbind(InitialLight[,c(1:9)], sc_in)
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





