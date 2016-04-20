#####################################################################
### This will plot the figures for manuscript 1 from MSExpt2013   ###
### Script by Rachael Blake, April 2016                           ###
#####################################################################

library(ggplot2) ; library(scales) ; library(gridExtra)

#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
theme_boxplot <- function(base_size = 12){
  theme_bw(base_size) %+replace%
    theme(legend.key.size=unit(15,"points"),
          legend.text=element_text(size=I(13)),
          legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="none",
          plot.margin=unit(c(0.5,1,0.5,1), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
          panel.border=element_blank(),
          panel.margin=unit(0,"lines"),
          axis.ticks.length=unit(1,"mm"),
          axis.text.x = element_text(margin=margin(5,0,0,0)),
          axis.text.y = element_text(margin=margin(0,5,0,0)),
          axis.text=element_text(size=13),
          axis.title.x=element_text(size=17, margin=margin(15,0,0,0)), 
          axis.title.y=element_text(size=15, angle=90, margin=margin(0,15,0,0)), 
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x=element_text(size=14),
          strip.background=element_rect(colour='black', fill='white'),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
}  
##########################################

#############################
# Expected Multi-Stress 
source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Multi_Stress_Calc/Expected_Stressor_Effects_MesoExp2013.r")
 
ExpStrsCalc  # this displays the expected stressor effects

L <- ggplot(AllD, aes(x=Chem1, y=LiveStemDryWgt_g)) + stat_summary(fun.y="mean", geom="bar") +
            geom_hline(yintercept = 45.52354, color = "red") + theme_boxplot() +
            xlab("") + labs(title= "a") + 
            scale_x_discrete(breaks=c("NC","Oil","Core","OilCore"),
                             labels=c("No\nStress","Oil","Disp.","Oil +\nDisp."))
   
D <- ggplot(AllD, aes(x=Chem1, y=DeadStemDryWgt_g)) + stat_summary(fun.y="mean", geom="bar") +
            geom_hline(yintercept = 2.33355, color = "red") + theme_boxplot() +
            xlab("Chemical Stressor Treatment")

N <- ggplot(AllD, aes(x=Chem1, y=TtlStemNum)) + stat_summary(fun.y="mean", geom="bar") +
            geom_hline(yintercept = 16.08333, color = "red") + theme_boxplot() +
            xlab("Chemical Stressor Treatment")

P <- ggplot(AllD, aes(x=Chem1, y=ProkAbunScaled)) + stat_summary(fun.y="mean", geom="bar") +
            geom_hline(yintercept = -0.1104024, color = "red") + theme_boxplot() +
            xlab("Chemical Stressor Treatment")

S <- ggplot(AllD, aes(x=Chem1, y=SnailWgtScaled)) + stat_summary(fun.y="mean", geom="bar") +
            geom_hline(yintercept = 0.0007776112, color = "red") + theme_boxplot() +
            xlab("Chemical Stressor Treatment")

grid.arrange(L,D,N, ncol=2, nrow=2)
grid.arrange(P,S, ncol=2, nrow=2)
##############################







