#####################################################################
### This will plot the figures for manuscript 1 from MSExpt2013   ###
### Script by Rachael Blake, April 2016                           ###
#####################################################################

library(ggplot2) ; library(scales) ; library(gridExtra)  ; library(plyr)  ; library(dplyr) ; library(here)

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
          panel.spacing=unit(0,"lines"),
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
#set_here("D:/Documents/LSU/MesoExp_2013/Analysis")
source(here::here("Exp_Conditions/TempLight_HOBO_MesoExpt_2013.r"))

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

source(here::here("Exp_Conditions/Sulfides_MesoExpt_2013.r"))

Sulf_box <- ggplot(data=Sraw, aes(x=Chem1, y=Conc_ppm, color= Chem1)) + 
                   geom_boxplot() + theme_boxplot() + xlab("") + labs(title= "d") + 
                   ylab(expression(paste("Porewater Sulfide (ppm)"))) + 
                   scale_x_discrete(breaks=c("NC","Core","Oil","OilCore"), 
                                    labels=c("No\nChemicals","Dispersant","Oil","Oil + \nDispersant")) +
                   scale_color_grey(start = 0.8, end = 0) +
                   theme(plot.title=element_text(size=24, hjust=0.04, vjust=0.5, face="bold")) 

source(here::here("Oil/Oiling_FINAL Data_MesoExpt2013.r"))

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
source(here::here("Multi_Stress_Calc/Expected_Stressor_Effects_MesoExp2013.r"))
 
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

#####
# New visuals for stressor effects following 5 editorial journal rejections (7/2/2019)

ms_combi # clean expected stressor info from Expected_Stressor_Effects_MesoExp2013.R script


exp_long <- ms_combi %>% 
            rename(expected_stress = ExpSt,
                   ms_effect = MS_effect) %>% 
            mutate(Herbivore = substring(treat_combo, regexpr("_", treat_combo) + 1),
                   var_resp = substring(treat_combo, 1, regexpr("_", treat_combo)),
                   var_resp = gsub("\\_", "", var_resp)) %>%
            mutate_at(vars(corexit, control, oil, oilcore), list(as.numeric)) 


# exp_df1 <- #AllD %>% 
           # select(Treat, Herbivore, Chem1, LiveStemDryWgt_g, DeadStemDryWgt_g, TtlStemNum, LvRootDryWgt_Scaled,
           #        DdRootDryWgt, ProkAbunScaled, SnailWgtScaled) %>% 
           # group_by(Treat, Herbivore) %>% 
           # summarise_at(c("LiveStemDryWgt_g", "DeadStemDryWgt_g", "TtlStemNum", "LvRootDryWgt_Scaled",
           #                "DdRootDryWgt", "ProkAbunScaled", "SnailWgtScaled"), mean, na.rm = TRUE) %>% 
           # ungroup() %>%
           # gather(key, val, LiveStemDryWgt_g:SnailWgtScaled) %>% 
           # mutate(var_resp = ifelse(key == "LiveStemDryWgt_g", "LIVE",
           #                   ifelse(key == "DeadStemDryWgt_g", "DEAD",
           #                   ifelse(key =="TtlStemNum", "NUM",
           #                   ifelse(key == "LvRootDryWgt_Scaled", "LIVR",
           #                   ifelse(key == "DdRootDryWgt", "DEDR",
           #                   ifelse(key == "ProkAbunScaled", "PROK",
           #                   ifelse(key == "SnailWgtScaled", "SNAL", key))))))),
           #        treat_combo = paste0(var_resp, "_", Herbivore)
           #        ) %>% 
           # full_join(exp_long, by = c("treat_combo", "Herbivore", "var_resp")) %>% 
           # filter(!(Treat %in% c("E", "F", "G", "H", "I", "J", "K", "L"))) %>% 
           # filter(!(var_resp %in% c("DEAD", "DEDR"))) %>% 
           # filter(!is.nan(val)) %>% 
           # mutate_at(vars(corexit, control, oil, oilcore), list(as.numeric))


exp_df2 <- #exp_df1 %>%
           exp_long %>% 
           #filter(Treat %in% c("M", "N", "O", "P")) %>% 
           select(-expected_stress#, -key, -Treat
                  )

exp_df3 <- #exp_df1 %>% 
           exp_long %>% 
           #filter(Treat %in% c("A", "B", "C", "D")) %>% 
           # rename(two_chem = val) %>% 
           full_join(exp_df2) %>% 
           mutate(expected_stress = case_when(var_resp == "PROK" & !(is.na(expected_stress)) ~ 0,
                                              TRUE ~ expected_stress),
                  std_oilcore = oilcore/control,
                  std_exp_stress = expected_stress/control)


exp1 <- ggplot(data = exp_df3, aes(x = std_exp_stress, y = std_oilcore)) + 
               geom_point(data = . %>% filter(!(var_resp %in% c("SNAL"))), size = 4, 
                          aes(fill = ms_effect, shape = var_resp)) + 
               geom_abline(intercept = 0, slope = 1) + xlim(0, 1.5) + ylim(0, 1.5) + labs(title= "b.") +
               theme_classic() + xlab("Expected multi-stress effect (additive)") + 
               ylab("Observed multi-stress effect") + 
               scale_shape_manual(breaks = c("LIVE", "LIVR", "NUM", "PROK"),
                                  labels = c("Live Shoots", "Live Roots", "Stem Number", "Insect Number"),
                                  values = c(21, 22, 23, 24)) +
               scale_fill_manual(breaks = c("additive", "synergistic", "antagonistic"),
                                 values = c("grey40", "black", "grey90")) +
               guides(fill = guide_legend(override.aes=list(shape = 21))) +
               theme(panel.border = element_rect(color = "black", fill = NA), 
                     axis.text = element_text(size=13),
                     axis.title = element_text(size=15),
                     # legend.title = element_blank(),
                     legend.position = "none" #, c(0.9, 0.35),
                     # legend.spacing.y = unit(-0.2, "cm"),
                     # legend.text=element_text(size=11)
                     )




exp2 <- ggplot(data = exp_df3) + 
               geom_point(data = . %>% filter(var_resp %in% c("SNAL")), size = 4, 
                          aes(x = std_exp_stress, y = std_oilcore, shape = var_resp, fill = ms_effect)) + 
               scale_shape_manual(breaks = c("SNAL"),
                                  labels = c("Change in snail \nmass (g)"),
                                  values = 25) +
               scale_fill_manual(breaks = c("synergistic"),
                                 values = c("grey90")) +
               geom_abline(slope = 1) + xlim(0, 8) + ylim(-14, 1.5) + labs(title= "c") +
               theme_classic() + xlab("Expected multi-stress effect (additive)") + 
               ylab("Observed multi-stress effect") +
               guides(fill = guide_legend(override.aes=list(shape = 21))) +
               theme(axis.text = element_text(size=13),
                     axis.title = element_text(size=15),
                     legend.title = element_blank(),
                     legend.position = c(0.85, 0.5),
                     legend.spacing.y = unit(-0.2, "cm"),
                     legend.text=element_text(size=11))

       
exp3 <- ggplot(data = exp_df3) + 
               geom_point(size = 4, aes(x = std_exp_stress, y = std_oilcore, shape = var_resp, fill = ms_effect)) + 
               scale_shape_manual(breaks = c("LIVE", "LIVR", "NUM", "PROK", "SNAL"),
                                  labels = c("Live Shoots", "Live Roots", "Stem Number", "Insect Number", 
                                             "Change in snail \nmass (g)"),
                                  values = c(21, 22, 23, 24, 25)) +
               scale_fill_manual(breaks = c("additive", "synergistic", "antagonistic"),
                                 values = c("grey40", "black", "grey90")) +
               geom_abline(slope = 1) + xlim(0, 8) + ylim(-14, 1.5) + labs(title= "a.") +
               theme_classic() + xlab("Expected multi-stress effect (additive)") + 
               ylab("Observed multi-stress effect") +
               guides(fill = guide_legend(override.aes=list(shape = 21), order = 1),
                      shape = guide_legend(order = 2)) +
               theme(panel.border = element_rect(color = "black", fill = NA), 
                     axis.text = element_text(size=15),
                     axis.title = element_text(size=17),
                     legend.title = element_blank(),
                     legend.position = c(0.75, 0.7),
                     legend.spacing.y = unit(-0.2, "cm"),
                     legend.text=element_text(size=13))
       


grid.arrange(exp3, exp1, ncol=2, nrow=1)



##############


##############################
# Plant Physiology (including isotopes per revision in Sept 2020)
source("./Plants/Photosyn_MesoExpt_2013.r")
source("./Carbon_Isotopes_MS2013.R")
head(PMean)
head(PMean_L_sub2)
head(PMean_long)
head(SIA_if_data2)

SIA_4merge <- SIA_if_data2 %>% 
              rename("WeekBb" = "Week", "Corexit" = "Core") %>% 
              select(-VarType2, -VarType3) %>% 
              mutate(Oil = ifelse(Oil == 1, "Y", "N"),
                     Corexit = ifelse(Corexit == 1, "Y", "N")) %>% 
              mutate(VarType = case_when(VarType == "d13C"~ 'delta^13*C'))

PMean_long_d13C <- PMean_long %>% 
                   mutate(VarType = as.character(VarType),
                          VarType = ifelse(VarType == " \nFv.Fm\n ", " \nFv/Fm\n ", VarType)) %>% 
                   full_join(SIA_4merge)

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

# c13
SIA_if_data2$Chem1 <- factor(SIA_if_data2$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore')) # for ordering the plot

c13Plot <- ggplot(data=SIA_if_data2, aes(x=Herbivore, y=Value)) + 
                  geom_boxplot(aes(fill=Chem1)) + theme_boxplot() +
                  facet_grid(VarType2+VarType3~Week, scales="free", switch="y", 
                             labeller=label_bquote( delta ^ .(VarType2) * .(VarType3) )) +
                  xlab("Herbivore Treatment") + ylab("") + 
                  theme(legend.position = c(.88, .75),
                        panel.spacing = unit(0.25, "cm"),
                        panel.border = element_rect(colour = "black", fill=NA),
                        strip.text.y = element_text(angle=-90, lineheight=0.5, size=14, 
                                                    margin = margin(0,0.5,0,0.5, "cm"))) +
                  scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                  breaks=c("NC","Core","Oil","OilCore"),
                                  labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                  scale_x_discrete(breaks=c("NG","P","S","SP"),
                                   labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
c13Plot
  
# all plots
whPlot <- ggplot(data=PMean_long_d13C, aes(x=Herbivore, y=Value)) + 
                 geom_boxplot(aes(fill=Chem1)) + theme_boxplot() +
                 facet_grid(VarType~WeekBb, scales="free", switch="y", labeller = label_parsed) +
                 xlab("Herbivore Treatment") + ylab("") + 
                 theme(legend.position = c(.88, .88), 
                       #strip.text.y=element_text(size=14),
                       panel.spacing = unit(0.25, "cm"),
                       panel.border = element_rect(colour = "black", fill=NA),
                       strip.text.y = element_text(angle=-90, lineheight=0.5, size=14)) +
                 scale_fill_grey(start = 1, end = 0, guide=guide_legend(title = NULL),
                                 breaks=c("NC","Core","Oil","OilCore"),
                                 labels=c("No Chemicals","Dispersant","Oil","Oil + Dispersant")) +
                 scale_x_discrete(breaks=c("NG","P","S","SP"),
                                  labels=c("No\nGrazers","Insects","Snails","Insects +\nSnails")) 
whPlot
  
png("Figure-3-Plant-Phys.png", width = 8.5, height = 11, units = 'in', res = 300)
# Make plot
dev.off()

##############################










