###################################################
###  Sulfide Analysis                           ###
###  Meso Expt 2013                             ###
###  July 17, 2013;  Rachael E. Blake           ###
###################################################

Sraw <- read.csv(here("../Sulfides/Sulfide_MesoExpt2013.csv"))
head(Sraw)
tail(Sraw)

mean_sulf <- Sraw %>%
             select(Treat, Chem, Oil, Corexit, Herbivore, Conc_ppm) %>%
             summarize(mean_sulf_conc = mean(Conc_ppm),
                       max_sulf_conc = max(Conc_ppm),
                       min_sulf_conc = min(Conc_ppm))

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

SulfLM2 <- lm(Conc_ppm ~ Chem, data=Sraw)
Anova(SulfLM2, type="III")

# Plot
Sraw$Chem1 <- factor(Sraw$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore')) # for ordering the plot

Sulf_box <- ggplot(data=Sraw, aes(x=Chem, y=Conc_ppm, color= Chem1)) + 
                   geom_boxplot() 


