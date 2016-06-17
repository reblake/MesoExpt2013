#####################################################################
### This will plot the figures for manuscript 1 from MSExpt2013   ###
### Script by Rachael Blake, April 2016                           ###
#####################################################################

library(ggplot2) ; library(scales) ; library(gridExtra)  ; library(plyr)  ; library(dplyr)

#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
theme_boxplot <- function(base_size = 12){
  theme_bw(base_size) %+replace%
    theme(legend.key.size=unit(15,"points"),
          legend.text=element_text(size=I(11)),
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
          axis.title.x=element_text(size=15, margin=margin(15,0,0,0)), 
          axis.title.y=element_text(size=15, angle=90, margin=margin(0,15,0,0)), 
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          strip.text.x=element_text(size=13, lineheight=0.5),
          strip.text.y=element_text(size=13),
          strip.background=element_rect(colour='black', fill='white'),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
}  
##########################################

#############################
# Experimental conditions
source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Exp_Conditions/TempLight_HOBO_MesoExpt 2013.r")

Temp_bar <- ggplot(data=Temp_raw, aes(x=Oiled, y=Temp_degC, color=Oiled)) + 
                   geom_boxplot() + theme_boxplot() + xlab("") + labs(title= "a") + 
                   ylab(expression(paste("Temperature (", degree ~ C,")"))) + 
                   scale_x_discrete(breaks=c("N","Y"), labels=c("Not Oiled","Oiled")) + 
                   scale_color_grey(start = 0.6, end = 0) +
                   theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))

Light_bar <- ggplot(data=Temp_raw_Lday, aes(x=Oiled, y=Intensity_Lux, color=Oiled)) + 
             geom_boxplot() + xlab("") + theme_boxplot() + labs(title= "c") + 
             ylab(expression(paste("Light Intensity ( ", Lux," )"))) +
             scale_x_discrete(breaks=c("N","Y"), labels=c("Not Oiled","Oiled")) + 
             scale_color_grey(start = 0.6, end = 0) +
             theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))

source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Exp_Conditions/Sulfides_MesoExpt 2013.r")

Sulf_box <- ggplot(data=Sraw, aes(x=Chem1, y=Conc_ppm, color= Chem1)) + 
                   geom_boxplot() + theme_boxplot() + xlab("") + labs(title= "d") + 
                   ylab(expression(paste("Porewater Sulfide (ppm)"))) + 
                   scale_x_discrete(breaks=c("NC","Core","Oil","OilCore"), 
                                    labels=c("No\nChemicals","Dispersant","Oil","Oil + \nDispersant")) +
                   scale_color_grey(start = 0.8, end = 0) +
                   theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold")) 

source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Oil/Oiling_FINAL Data_MesoExpt2013.r")

TtlHydro <- ggplot(data=OI_tot, aes(x=Herbivore, y=log_Tot_Hydrocarbon, fill=Chem1)) + 
                 geom_boxplot() + theme_boxplot() + xlab("") + labs(title= "b") +  
                 ylab("Total Hydrocarbons (log)") +
                 scale_x_discrete(breaks=c("NG","P","S","SP"),
                                  labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) +
                 scale_fill_grey(start = 1, end = 0) +
                 theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold")) 


grid.arrange(Temp_bar, TtlHydro, Light_bar, Sulf_box, ncol=2, nrow=2)

#############################
# Expected Multi-Stress with biomass and abundances of stems and herbivores
source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Multi_Stress_Calc/Expected_Stressor_Effects_MesoExp2013.r")
 
ExpStrsCalc  # this displays the expected stressor effects

barcolor <- c("white", "grey70", "grey40", "black")
baroutline <- c("black","black","black","black")
dodge <- position_dodge(width=0.9)

L <- ggplot(AllD, aes(x=Herbivore, y=LiveStemDryWgt_g, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 31.60692, xend = 1.5, yend = 31.60692), size=1.5) +
            geom_segment(aes(x = 1.5, y = 63.46302, xend = 2.5, yend = 63.46302), size=1.5) +
            geom_segment(aes(x = 2.5, y = 48.94692, xend = 3.5, yend = 48.94692), size=1.5) +
            geom_segment(aes(x = 3.5, y = 38.0773, xend = 4.5, yend = 38.0773), size=1.5) +
         #   geom_hline(yintercept = 45.52354, color = "black") + 
            theme_boxplot() + xlab("") + ylab("Live Shoots (g dry weight)") +
            scale_fill_manual(values=barcolor) + labs(title= "a") + 
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
L   

D <- ggplot(AllD, aes(x=Herbivore, y=DeadStemDryWgt_g, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 2.1167, xend = 1.5, yend = 2.1167), size=1.5) +
            geom_segment(aes(x = 1.5, y = 3.4922, xend = 2.5, yend = 3.4922), size=1.5) +
            geom_segment(aes(x = 2.5, y = 1.469, xend = 3.5, yend = 1.469), size=1.5) +
            geom_segment(aes(x = 3.5, y = 2.2563, xend = 4.5, yend = 2.2563), size=1.5) +
            theme_boxplot() + xlab("") + ylab("Dead Shoots (g dry weight)") +
            scale_fill_manual(values=barcolor) + #labs(title= "b") + 
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
D

N <- ggplot(AllD, aes(x=Herbivore, y=TtlStemNum, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 17, xend = 1.5, yend = 17), size=1.5) +
            geom_segment(aes(x = 1.5, y = 17, xend = 2.5, yend = 17), size=1.5) +
            geom_segment(aes(x = 2.5, y = 13.66667, xend = 3.5, yend = 13.66667), size=1.5) +
            geom_segment(aes(x = 3.5, y = 16.66667, xend = 4.5, yend = 16.66667), size=1.5) +
            theme_boxplot() + xlab("") + ylab("Number of Shoots") +
            scale_fill_manual(values=barcolor) + labs(title= "b") + 
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
N

LR <- ggplot(AllD, aes(x=Herbivore, y=LvRootDryWgt_Scaled, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 2.553967, xend = 1.5, yend = 2.553967), size=1.5) +
            geom_segment(aes(x = 1.5, y = 2.557233, xend = 2.5, yend = 2.557233), size=1.5) +
            geom_segment(aes(x = 2.5, y = 3.010425, xend = 3.5, yend = 3.010425), size=1.5) +
            geom_segment(aes(x = 3.5, y = 2.58818, xend = 4.5, yend = 2.58818), size=1.5) +
            theme_boxplot() + xlab("") +  ylab("Live Roots (g dry weight)") +
            scale_fill_manual(values=barcolor) + labs(title= "c") + 
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
LR

DR <- ggplot(AllD, aes(x=Herbivore, y=DdRootDryWgt, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 0.5, y = 0.1316667, xend = 1.5, yend = 0.1316667), size=1.5) +
            geom_segment(aes(x = 1.5, y = 1.6038, xend = 2.5, yend = 1.6038), size=1.5) +
            geom_segment(aes(x = 2.5, y = 2.04885, xend = 3.5, yend = 2.04885), size=1.5) +
            geom_segment(aes(x = 3.5, y = 1.332867, xend = 4.5, yend = 1.332867), size=1.5) +
            theme_boxplot() + xlab("") +  
            scale_fill_manual(values=barcolor) + #labs(title= "e") +
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
DR

P <- ggplot(AllD, aes(x=Herbivore, y=ProkAbunScaled, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position=dodge, color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 1.5, y = 0, xend = 2.5, yend = 0), size=1.5) + #constrained to zero; real value -7.734167
            geom_segment(aes(x = 3.5, y = 0, xend = 4.5, yend = 0), size=1.5) + #constrained to zero; real value -1.713333
            theme_boxplot() + xlab("") + ylab("Number of Insects") + labs(title= "d") +
            scale_fill_manual(values=barcolor, breaks=c("NC","Core","Oil","OilCore"),
                              labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) + 
            scale_colour_manual(values=baroutline, breaks=c("NC","Oil","Core","OilCore"),
                              labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"),
                  legend.position=c(.2, .8))
P

S <- ggplot(AllD, aes(x=Herbivore, y=SnailWgtScaled, fill=Chem1)) + 
            stat_summary(fun.y="mean", geom="bar", position="dodge", color="black") +
            stat_summary(fun.data=mean_se, geom="linerange", position=dodge) +
            geom_segment(aes(x = 2.5, y = 1.234111, xend = 3.5, yend = 1.234111), size=1.5) +
            geom_segment(aes(x = 3.5, y = 0.3211117, xend = 4.5, yend = 0.3211117), size=1.5) +
            theme_boxplot() + xlab("") + ylab("Change in Snail Mass (g)") +
            scale_fill_manual(values=barcolor) + labs(title= "e") + 
            scale_colour_manual(values=baroutline) + 
            scale_x_discrete(breaks=c("NG","P","S","SP"),
                             labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) + 
            theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold"))
S
 
grid.arrange(L, N, LR, ncol=2, nrow=2)
grid.arrange(P, S, ncol=2, nrow=2)
##############################
# Plant Physiology
source("C:/Users/rblake/Documents/LSU/MesoExp_2013/Analysis/Plants/Photosyn_MesoExpt 2013.r")
head(PMean)
head(PMean_L_sub2)
head(PMean_long)

# Fv/Fm
FvFmPlot <- ggplot(data=PMean, aes(x=Herbivore, y=as.numeric(Fv.Fm))) + 
                   geom_boxplot(aes(fill=Chem1)) + theme_boxplot() + facet_wrap(~ Week1, ncol=2) +
                   xlab("Herbivore Treatment") + ylab("Fv/Fm") + 
                   theme(legend.position=c(.9, .1),
                         panel.border = element_rect(colour = "black", fill=NA)) +
                   scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                   breaks=c("NC","Core","Oil","OilCore"),
                                   labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                   scale_x_discrete(breaks=c("NG","P","S","SP"),
                                    labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
FvFmPlot

# Photosynthesis
PhotoPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=Photo)) + 
                    geom_boxplot(aes(fill=Chem1)) + theme_boxplot() + facet_wrap(~ Week1, ncol=2) +
                    coord_cartesian(ylim = c(0, 30) + c(-.25, .25)) +
                    xlab("Herbivore Treatment") + ylab("Photosynthesis") + 
                    theme(legend.position=c(.9, .4),
                          panel.border = element_rect(colour = "black", fill=NA)) +
                    scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                    breaks=c("NC","Core","Oil","OilCore"),
                                    labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                    scale_x_discrete(breaks=c("NG","P","S","SP"),
                                     labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
PhotoPlot

# qN 
qNPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=qN)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_boxplot() + facet_wrap(~ Week1, ncol=2) +
                 coord_cartesian(ylim = c(1.5, 3.5) + c(-.25, .25)) +
                 xlab("Herbivore Treatment") + ylab("non-photochemical quenching") + 
                 theme(legend.position=c(.1, .9),
                       panel.border = element_rect(colour = "black", fill=NA)) +
                 scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                 breaks=c("NC","Core","Oil","OilCore"),
                                 labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                 scale_x_discrete(breaks=c("NG","P","S","SP"),
                                  labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
qNPlot

# qP
qPPlot <- ggplot(data=PMean_L_sub2, aes(x=Herbivore, y=qP)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_boxplot() + facet_wrap(~Week1, ncol=2) +
                 coord_cartesian(ylim = c(0.25, 0.75) + c(-.25, .25)) +
                 xlab("Herbivore Treatment") + ylab("photochemical quenching") + 
                 theme(legend.position=c(.1, .9), 
                       panel.border = element_rect(colour = "black", fill=NA)) +
                 scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                 breaks=c("NC","Core","Oil","OilCore"),
                                 labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                 scale_x_discrete(breaks=c("NG","P","S","SP"),
                                  labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
qPPlot

# all plots
whPlot <- ggplot(data=PMean_long, aes(x=Herbivore, y=Value)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_boxplot() +
                 facet_grid(VarType~WeekBb, scales="free", switch="y") +
                 xlab("Herbivore Treatment") + ylab("") + 
                 theme(legend.position = c(.88, .825), 
                       strip.text.y=element_text(size=14),
                       panel.margin = unit(0.25, "cm"),
                       panel.border = element_rect(colour = "black", fill=NA),
                       strip.text.y = element_text(angle=-90, lineheight=0.5)) +
                 scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                 breaks=c("NC","Core","Oil","OilCore"),
                                 labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                 scale_x_discrete(breaks=c("NG","P","S","SP"),
                                  labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
whPlot
  


##############################
# Isotopes









