################################################
###   BOXPLOTS                               ###
###   code by Rachael Blake                  ###
###    March 2014                            ###
################################################

### MY OWN THEME TO SAVE LINES OF CODE BELOW
theme_boxplot <- function(base_size = 12){
 theme_bw(base_size) %+replace%
   theme(legend.key.size=unit(15,"points"),
         legend.text=element_text(size=I(13)),
         legend.key=element_blank(),
         legend.title=element_blank(),
         legend.position="none",
         plot.margin=unit(c(0.25,2,0.25,2), "lines"), # respectively: top, right, bottom, left; refers to margin *outside* labels; default is c(1,1,0.5,0.5)
         panel.border=element_rect(colour='black', fill = NA),
         panel.margin=unit(0,"lines"),
         axis.ticks.length=unit(1,"mm"),
         axis.ticks.margin = unit(0, "lines"),
         axis.text=element_text(size=15),
         axis.title.x=element_text(hjust=.55, vjust=-.01, size=17),
         axis.title.y=element_text(size=17, angle=90, hjust=.56, vjust=-.001),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         strip.text.x=element_text(size=14),
         strip.background=element_rect(colour='black', fill='white'))
}  
#########################################

# Load packages 
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

# Define colors for the boxplot
colors <- c("green","red","yellow","orange")

# Define order of the boxes (use this as "fill=" below)
TtlP1$Chem1 <- factor(TtlP1$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
# "TtlP1 is the dataset, Chem is the column with chemical treatments, Chem1 is an additional
#  column added by this line of code

# Plot
TtlPlant <- ggplot(data=TtlP1, aes(x=Herbivore, y=Dry_Wgt, fill=Chem1)) + 
                   geom_boxplot() + theme_bw() + ylab("Total Biomass") +
                   theme(panel.grid=element_blank(),legend.key=element_blank(),
                         legend.background=element_blank(),legend.text=element_text(size=18),
                         legend.position=c(.95, .85),axis.text=element_text(size=20),
                         axis.title=element_text(vjust=-0.04),
                         panel.border=element_blank(),axis.line=element_line(color='black'),
                         panel.background=element_blank(),plot.background=element_blank()) +
                   scale_fill_manual(values=colors, guide=guide_legend(title = NULL))
# For your data, replace the name of the plot, "data=", "x=", "y=", "fill=", "ylab="








