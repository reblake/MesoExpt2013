###################################################
###  Sulfide Analysis                           ###
###  Meso Expt 2013                             ###
###  July 17, 2013;  Rachael E. Blake           ###
###################################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sulfides\\")

Sraw <- read.csv("Sulfide_MesoExpt2013.csv")
head(Sraw)
tail(Sraw)

#ANOVA TYPE I SS
#SulfAOV <- aov(Conc_ppm ~ Oil*Corexit*Herbivore, Sraw)
#summary(SulfAOV)
#par(mfrow=c(2,2))
#plot(SulfAOV)

#ANOVA TYPE III SS
options(contrasts=c("contr.sum","contr.poly"))
#options(contrasts=c("contr.treatment","contr.poly"))

library(car)  

SulfLM <- lm(Conc_ppm ~ Oil*Corexit*Herbivore, data=Sraw)#,
             #contrasts=list(Oil=contr.sum, Corexit=contr.sum, Herbivore=contr.sum))

summary(SulfLM)#$coeff  # give summary of all coefficients - useful to see what's missing

vif(SulfLM)  # calculates variance inflation factor  to determine multicollinearity

Anova(SulfLM, type="III") # calculates ANOVA table with Type III SS






