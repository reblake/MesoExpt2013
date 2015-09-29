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

# Stems were oiled half of their height
HalfStemHeight <- mnhgt[,4]/2

OilHeightCalc <- cbind(mnhgt[,-1], HalfStemHeight)

write.csv(OilHeightCalc, file="C://Users//rblake//Documents//LSU//MesoExp_2013//OilHeightCalc")
