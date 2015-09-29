#############################################
###    Mesocosm Experiment 2013           ###
###    Snail Weight Data                  ###
###    Script by Rachael Blake, Dec 2013  ###
#############################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Snails")

SN <- read.csv("Snail data_Final_Meso Expt 2013.csv")

names(SN)
head(SN)

# Exploring data
library(psych)
pairs.panels(SN[,c(18:22)],smooth=F,density=T,ellipses=F,lm=T,digits=3,scale=T)


library(car)
# ANOVA Type III
options(contrasts=c("contr.sum","contr.poly"))
Snl <- lm(Wgt_per_Day ~ Oil*Corexit*Herbivore, data=SN)       
Anova(Snl, type="III") # calculates ANOVA table with Type III SS


library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)
# Boxplot
SN$Chem1 <- factor(SN$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
colors <- c("green","red","yellow","orange")
Wgt_Day <- ggplot(data=SN, aes(x=Herbivore, y=Wgt_per_Day, fill=Chem1)) + 
                  geom_boxplot() + theme_bw() +
                  theme(panel.grid=element_blank(),legend.key=element_blank(),
                        legend.background=element_blank(),legend.text=element_text(size=18),
                        legend.position=c(.95, .2),axis.text=element_text(size=20),
                        panel.border=element_blank(),axis.line=element_line(color='black'),
                        panel.background=element_blank(),plot.background=element_blank(),
                        axis.title.y=element_text(vjust=-0.02)) +
                  scale_fill_manual(values=colors, guide=guide_legend(title = NULL))  
Wgt_Day

########### Getting means per bucket ################
attach(SN)
#Taking means of all values
mean <- aggregate(SN[,-c(2:9)], by=list(Bucket), FUN=mean, na.rm=TRUE)
library(plyr) ; mean1 <- arrange(mean, Group.1) #sorts data by bucket and date
mean1[1:25,]

#Getting the treatment columns
treats <- SN[,c(1:6)] 
head(treats)  
TreatAll <- arrange(treats, Bucket)
TreatSAll <- unique(TreatAll)
TreatSAll[c(25:40),]

#Binding the treatment columns with the data columns
#Hgtmn <- mean[,-c(1:2)]
WgtMean <- cbind(TreatSAll, mean1)
WgtMean[25:40,]

detach(SN)

write.csv(WgtMean,"C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Snails\\SnailWgtMeans_MesoExpt2013.csv")











