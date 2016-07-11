##################################################################
###  SEM for Mesocosm Experiment 2013
###  Script by Rachael E. Blake
###  October 2013
##################################################################

# read in data
ALLDATA <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/ALL_DATA_SEM_MesoExpt2013.csv")
names(ALLDATA)
str(ALLDATA)

# Explore data a little
plot(ALLDATA$SnailWgt_per_Day,ALLDATA$TtlStemNum)
plot(ALLDATA$StemHgt_cm,ALLDATA$TtlStemNum)
plot(ALLDATA$StemHgt_cm,ALLDATA$StemDiam_mm)
plot(ALLDATA$StemDiam_mm,ALLDATA$TtlStemNum)

# Look at correlated variables
library(psych)
pairs.panels(ALLDATA[,c(25,24,30,31,35,26,21)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)
# NOTE: Live Stem Bmss and Total Stem Bmss 1:1, so use Live Stem and Dead Stem, not total



######################
##### SEM ############
######################
library(lavaan)

########## Global Model - specify the model structure
mod1 <- 'LiveStemDryScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
         TtlStemNumScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
         LogDeadStemDryWgt ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
         SnailWgtScaled ~ Oil + Corexit              
         LogProkAbunScaled ~ Oil + Corexit '
         #LvRootDryWgt_Scaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
# Fit the model (estimate the parameters)
mod1_fit <- sem(mod1, data=ALLDATA)
# Output a summary of the computed results
summary(mod1_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod1_fit)
mi1 <- modindices(mod1_fit) 
print(mi1[mi1$op == "~",])# extracts mod indicies with "~" operator suggesting adding a path
# plot with standardized coefficients
library(semPlot)
semPaths(mod1_fit, "std", layout="tree3", style="lisrel", curvePivot=TRUE)

##########
mod11 <- 'LiveStemDryScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          Photo_Scaled ~ Oil + Corexit  + SnailWgtScaled + LogProkAbunScaled
          Fv.Fm_Scaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          TtlStemNumScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          LvRootDryWgt_Scaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          SnailWgtScaled ~  Corexit
          LogProkAbunScaled ~ Oil  '
# Fit the model (estimate the parameters)
mod11_fit <- sem(mod11, data=ALLDATA)
# Output a summary of the computed results
summary(mod11_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod11_fit)
mi11 <- modindices(mod11_fit) 
print(mi11[mi11$op == "~",])# extracts mod indicies with "~" operator suggesting adding a path
# plot with standardized coefficients
library(semPlot)
semPaths(mod11_fit, "std", layout="tree3", style="lisrel", curvePivot=TRUE)



########## Spartina Models########################
# Specify the model structure
mod2 <- 'TtlStemNumScaled ~ Oil + Corexit + Snail + Insect
         LiveStemDryScaled ~ Oil + Corexit + Snail + Insect
         LogDeadStemDryWgt ~ Oil + Corexit + Snail + Insect
         LvRootDryWgt_Scaled ~ Oil + Corexit 
         LogDdRootDryWgt ~ Oil + Corexit '
# Fit the model (estimate the parameters)
mod2_fit <- sem(mod2, data=ALLDATA)
# Output a summary of the computed results
summary(mod2_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod2_fit)
modindices(mod2_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod2_fit, "std", layout="tree3", style="lisrel")

########
########
mod22 <- 'TtlStemNumScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          LiveStemDryScaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          LvRootDryWgt_Scaled ~ Oil + Corexit + LogProkAbunScaled 
          Photo_Scaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled
          Fv.Fm_Scaled ~ Oil + Corexit + SnailWgtScaled + LogProkAbunScaled'
# Fit the model (estimate the parameters)
mod22_fit <- sem(mod22, data=ALLDATA)
# Output a summary of the computed results
summary(mod22_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod22_fit)
modindices(mod22_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod22_fit, "std", layout="tree3", style="lisrel")
########
########
mod222 <- 'TtlStemNumScaled ~ Oil + Corexit + Snail + Insect
           LiveStemDryScaled ~ Oil + Corexit + Snail + Insect
           LogDdRootDryWgt ~ Oil + Corexit + LiveStemDryScaled
           Photo_Scaled ~ Oil + Corexit + Snail + Insect
           Fv.Fm_Scaled ~ Oil + Corexit + Snail + Insect
          '
# Fit the model (estimate the parameters)
mod222_fit <- sem(mod222, data=ALLDATA)
# Output a summary of the computed results
summary(mod222_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod222_fit)
modindices(mod222_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod222_fit, "std", layout="tree3", style="lisrel")
########
########
mod2222 <- 'LogDdRootDryWgt ~ Oil + Corexit + LogDeadStemDryWgt
            LvRootDryWgt_Scaled ~ Oil + Corexit + LiveStemDryScaled
           '


LvRootDryWgt_Scaled ~~ LogDdRootDryWgt
# Fit the model (estimate the parameters)
mod2222_fit <- sem(mod2222, data=ALLDATA)
# Output a summary of the computed results
summary(mod2222_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod2222_fit)
modindices(mod2222_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod2222_fit, "std", layout="tree3", style="lisrel")
  

########### Herbivore Model#####################
# specify the model structure
mod3 <- 'SnailWgtScaled ~  Corexit + LiveStemDryScaled + TtlStemNumScaled
         LogProkAbunScaled ~ Oil + LiveStemDryScaled + TtlStemNumScaled'
# Fit the model (estimate the parameters)
mod3_fit <- sem(mod3, data=ALLDATA)
# Output a summary of the computed results
summary(mod3_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod3_fit)
modindices(mod3_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod3_fit, "std", layout="tree2", style="lisrel")
##########
##########
# specify the model structure
mod33 <- 'SnailWgtScaled ~  Corexit + LiveStemDryScaled + TtlStemNumScaled +
                            Photo_Scaled + Fv.Fm_Scaled
          LogProkAbunScaled ~ Oil + LiveStemDryScaled + TtlStemNumScaled + 
                              Photo_Scaled + Fv.Fm_Scaled'
# Fit the model (estimate the parameters)
mod33_fit <- sem(mod33, data=ALLDATA)
# Output a summary of the computed results
summary(mod33_fit, rsq=T, standardized=T)  # rsq=T means output the r-sqr
fitMeasures(mod33_fit)
modindices(mod33_fit)
# plot with standardized coefficients
library(semPlot)
semPaths(mod33_fit, "std", layout="tree2", style="lisrel")
##########
##########


