######################################################
# May 10, 2013
# Calculating Oiling Height for Mesocosm Experiment
# Rachael E. Blake
######################################################

######################################################
setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sp Biomass\\")

hgt <- read.csv("SpBmssBeg_MesoExpt 2013.csv")
head(hgt)
tail(hgt)

#take the mean of all columns
attach(hgt)
mnhgt <- aggregate(hgt[,-5], by=list(Bucket), FUN=mean, na.rm=TRUE)
mnhgt
detach(hgt)

HalfStemHeight <- mnhgt[,4]/2

OilHeightCalc <- cbind(mnhgt[,-1], HalfStemHeight)

write.csv(OilHeightCalc, file="C://Users//rblake//Documents//LSU//MesoExp_2013//OilHeightCalc")


#################################################################
# June 6, 2013
# Spartina Biomass 

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sp Biomass")

SP <- read.csv("SpBmss_MesoExpt 2013.csv")
head(SP)
tail(SP)
names(SP)

attach(SP)
#Taking means of all values
mean <- aggregate(SP[,-c(1,3:5,9)], by=list(Week,Bucket), FUN=mean, na.rm=TRUE)
mean

#Getting the treatment columns
treats <- SP[,c(1:5)] 
head(treats)  
TreatAll <- treats[order(Bucket),]
TreatSAll <- unique(TreatAll)
TreatSAll[1:25,]

#Binding the treatment columns with the data columns
Hgtmn <- mean[,-c(1:2)]
HgtMean <- cbind(Hgtmn[,-1], TreatSAll)
HgtMean[1:25,]

detach(SP)

#write.csv(HgtMean,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\SpBmss_Meso2013.csv")
Stems <- read.csv("SpBmss_Meso2013.csv")
head(Stems)

## Week 2 ANOVA Type III SS
Wk2 <- subset(Stems, Stems$Week==2)#, select=TtlStemNum:Herbivore)
Wk2

library(car)  

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












