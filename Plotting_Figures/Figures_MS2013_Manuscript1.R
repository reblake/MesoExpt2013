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

barcolor <- c("white", "grey70", "grey40", "black")
baroutline <- c("black","black","black","black")
dodge <- position_dodge(width=0.9)

L <- ggplot(AllD, aes(x=Herbivore, y=LiveStemDryWgt_g, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 31.60692, xend = 1.5, yend = 31.60692), size=1.5) +
            geom_segment(aes(x = 1.5, y = 63.46302, xend = 2.5, yend = 63.46302), size=1.5) +
            geom_segment(aes(x = 2.5, y = 48.94692, xend = 3.5, yend = 48.94692), size=1.5) +
            geom_segment(aes(x = 3.5, y = 38.0773, xend = 4.5, yend = 38.0773), size=1.5) +
         #   geom_hline(yintercept = 45.52354, color = "black") + 
            theme_boxplot() + xlab("") + labs(title= "a") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
L   

D <- ggplot(AllD, aes(x=Herbivore, y=DeadStemDryWgt_g, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 2.1167, xend = 1.5, yend = 2.1167), size=1.5) +
            geom_segment(aes(x = 1.5, y = 3.4922, xend = 2.5, yend = 3.4922), size=1.5) +
            geom_segment(aes(x = 2.5, y = 1.469, xend = 3.5, yend = 1.469), size=1.5) +
            geom_segment(aes(x = 3.5, y = 2.2563, xend = 4.5, yend = 2.2563), size=1.5) +
            theme_boxplot() + xlab("") + labs(title= "b") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
D

N <- ggplot(AllD, aes(x=Herbivore, y=TtlStemNum, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 17, xend = 1.5, yend = 17), size=1.5) +
            geom_segment(aes(x = 1.5, y = 17, xend = 2.5, yend = 17), size=1.5) +
            geom_segment(aes(x = 2.5, y = 13.66667, xend = 3.5, yend = 13.66667), size=1.5) +
            geom_segment(aes(x = 3.5, y = 16.66667, xend = 4.5, yend = 16.66667), size=1.5) +
            theme_boxplot() + xlab("") + labs(title= "c") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
N

LR <- ggplot(AllD, aes(x=Herbivore, y=LvRootDryWgt_Scaled, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 2.553967, xend = 1.5, yend = 2.553967), size=1.5) +
            geom_segment(aes(x = 1.5, y = 2.557233, xend = 2.5, yend = 2.557233), size=1.5) +
            geom_segment(aes(x = 2.5, y = 3.010425, xend = 3.5, yend = 3.010425), size=1.5) +
            geom_segment(aes(x = 3.5, y = 2.58818, xend = 4.5, yend = 2.58818), size=1.5) +
            theme_boxplot() + xlab("") + labs(title= "d") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
LR

DR <- ggplot(AllD, aes(x=Herbivore, y=DdRootDryWgt, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 0.1316667, xend = 1.5, yend = 0.1316667), size=1.5) +
            geom_segment(aes(x = 1.5, y = 1.6038, xend = 2.5, yend = 1.6038), size=1.5) +
            geom_segment(aes(x = 2.5, y = 2.04885, xend = 3.5, yend = 2.04885), size=1.5) +
            geom_segment(aes(x = 3.5, y = 1.332867, xend = 4.5, yend = 1.332867), size=1.5) +
            theme_boxplot() + xlab("") + labs(title= "e") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
DR

P <- ggplot(AllD, aes(x=Herbivore, y=ProkAbunScaled, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 1.5, y = 0, xend = 2.5, yend = 0), size=1.5) + #constrained to zero; real value -7.734167
            geom_segment(aes(x = 3.5, y = 0, xend = 4.5, yend = 0), size=1.5) + #constrained to zero; real value -1.713333
            theme_boxplot() + xlab("") + labs(title= "f") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
P

S <- ggplot(AllD, aes(x=Herbivore, y=SnailWgtScaled, fill=Chem1, color="black")) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 2.5, y = 1.234111, xend = 3.5, yend = 1.234111), size=1.5) +
            geom_segment(aes(x = 3.5, y = 0.3211117, xend = 4.5, yend = 0.3211117), size=1.5) +
            theme_boxplot() + xlab("") + labs(title= "g") + 
            scale_fill_manual(values=barcolor) +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
S
 
grid.arrange(L, N, LR, ncol=2, nrow=2)
grid.arrange(P, S, ncol=2, nrow=2)
##############################







