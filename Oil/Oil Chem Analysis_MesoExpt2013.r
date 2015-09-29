##################################################
###  MesoExpt 2013                             ###  
###  Oil Data                                  ###
###  script by Rachael E. Blake  June 2013     ###
##################################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Oiling Stuff")

OS <- read.csv("2014021_Sediment Samples_FINAL.csv")
head(OS)
tail(OS)
names(OS)

# Convert the date column so it can be used in R
OS$Date1 <- as.Date(OS$Date, format="%d-%b") ; OS$Date1

# Subsetting data by date
May20 <- OS[OS$Date %in% "20-May-2013",]
head(May20) ; tail(May20)

May29 <- OS[OS$Date %in% "29-May-2013",]
head(May29) ; tail(May29)

June12 <- OS[OS$Date %in% "12-Jun-2013",]
head(June12) ; tail(June12)

June20 <- OS[OS$Date %in% "20-Jun-2013",]
head(June20) ; tail(June20)

July5 <- OS[OS$Date %in% "5-Jul-2013",]
head(July5) ; tail(July5)

# Subsetting by Chem treatment
Core <- OS[OS$Chem %in% "Core",]
head(Core) ; tail(Core)

Oil <- OS[OS$Chem %in% "Oil",]
head(Oil) ; tail(Oil)

NC <- OS[OS$Chem %in% "NC",]
head(NC) ; tail(NC)

OilCore <- OS[OS$Chem %in% "OilCore",]
head(OilCore) ; tail(OilCore)

# Plots
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

# some basic plots
OS$Chem1 <- factor(OS$Chem, levels=c('NC', 'Core', 'Oil', 'OilCore'))
qplot(x=Chem1, y=TtlAlkanes, fill=Herbivore, data=OS, geom="boxplot")
      qplot(x=Chem1, y=TtlAlkanes, data=May20, geom="boxplot")
      qplot(x=Chem1, y=TtlAlkanes, data=May29, geom="boxplot")
      qplot(x=Chem1, y=TtlAlkanes, data=June12, geom="boxplot")
      qplot(x=Chem1, y=TtlAlkanes, data=June20, geom="boxplot")
      qplot(x=Chem1, y=TtlAlkanes, data=July5, geom="boxplot")
qplot(x=Date, y=TtlAlkanes, fill=Chem1, data=OS, geom="boxplot")
      qplot(x=Date, y=TtlAlkanes, data=NC, geom="boxplot")
      qplot(x=Date, y=TtlAlkanes, data=Core, geom="boxplot")
      qplot(x=Date, y=TtlAlkanes, data=Oil, geom="boxplot")
      qplot(x=Date, y=TtlAlkanes, data=OilCore, geom="boxplot")


qplot(x=Chem1, y=TtlAromatics, fill=Herbivore, data=OS, geom="boxplot")
      qplot(x=Chem1, y=TtlAromatics, data=May20, geom="boxplot")
      qplot(x=Chem1, y=TtlAromatics, data=May29, geom="boxplot")
      qplot(x=Chem1, y=TtlAromatics, data=June12, geom="boxplot")
      qplot(x=Chem1, y=TtlAromatics, data=June20, geom="boxplot")
      qplot(x=Chem1, y=TtlAromatics, data=July5, geom="boxplot")
qplot(x=Date, y=TtlAromatics, fill=Chem1, data=OS, geom="boxplot")
      qplot(x=Date, y=TtlAromatics, data=NC, geom="boxplot")
      qplot(x=Date, y=TtlAromatics, data=Core, geom="boxplot")
      qplot(x=Date, y=TtlAromatics, data=Oil, geom="boxplot")
      qplot(x=Date, y=TtlAromatics, data=OilCore, geom="boxplot")
#################################

#######################################
### MAKE MY OWN THEME TO SAVE LINES OF CODE
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
         axis.text=element_text(size=12),
         axis.title.x=element_text(hjust=.55, vjust=-.01, size=17),
         axis.title.y=element_text(size=17, angle=90, hjust=.56, vjust=-.001),
         panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         strip.text.x=element_text(size=14),
         strip.background=element_rect(colour='black', fill='white'))
}  
##########################################

################### MAY 20TH ##############################
OilCoreMay20 <- OilCore[OilCore$Date %in% "20-May-2013",] ; OilCoreMay20
CoreMay20 <- Core[Core$Date %in% "20-May-2013",] ; CoreMay20
OilMay20 <- Oil[Oil$Date %in% "20-May-2013",] ; OilMay20
NCMay20 <- NC[NC$Date %in% "20-May-2013",] ; NCMay20

library(reshape2)

melt1 <- melt(OilCoreMay20, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
OCmelt <- ddply(melt1, "variable", summarise, mvalue = mean(value))
OCp <- ggplot(data=OCmelt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5, stat="identity") + 
             labs(title="Oil & Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1))   
OCp
#
melt2 <- melt(CoreMay20, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
Cmelt <- ddply(melt2, "variable", summarise, mvalue = mean(value))
Cp <- ggplot(data=Cmelt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
Cp
#
melt3 <- melt(OilMay20, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
Omelt <- ddply(melt3, "variable", summarise, mvalue = mean(value))
Op <- ggplot(data=Omelt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Oil") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
Op
#
melt4 <- melt(NCMay20, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
NCmelt <- ddply(melt4, "variable", summarise, mvalue = mean(value))
NCp <- ggplot(data=NCmelt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
             labs(title="No Chemicals") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
NCp

#####
multiplot(NCp, Cp, Op, OCp, cols=2)

########################################################################################
# Multiple plot function - from Winston Chang
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

##############################################################################


################### MAY 29TH ##############################
OilCoreMay29 <- OilCore[OilCore$Date %in% "29-May-2013",] ; OilCoreMay29
CoreMay29 <- Core[Core$Date %in% "29-May-2013",] ; CoreMay29
OilMay29 <- Oil[Oil$Date %in% "29-May-2013",] ; OilMay29
NCMay29 <- NC[NC$Date %in% "29-May-2013",] ; NCMay29

library(reshape2)

melt11 <- melt(OilCoreMay29, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
OC1melt <- ddply(melt11, "variable", summarise, mvalue = mean(value))
OC1 <- ggplot(data=OC1melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5, stat="identity") + 
             labs(title="Oil & Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1))   
OC1
#
melt22 <- melt(CoreMay29, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
C1melt <- ddply(melt22, "variable", summarise, mvalue = mean(value))
C1 <- ggplot(data=C1melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
C1
#
melt33 <- melt(OilMay29, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
O1melt <- ddply(melt33, "variable", summarise, mvalue = mean(value))
O1 <- ggplot(data=O1melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Oil") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
O1
#
melt44 <- melt(NCMay29, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
NC1melt <- ddply(melt44, "variable", summarise, mvalue = mean(value))
NC1 <- ggplot(data=NC1melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
             labs(title="No Chemicals") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
NC1

#####
multiplot(NC1, C1, O1, OC1, cols=2)
###############################################################################


################### JULY 5th ##############################
OilCoreJuly5 <- OilCore[OilCore$Date %in% "5-Jul-2013",] ; OilCoreJuly5
CoreJuly5 <- Core[Core$Date %in% "5-Jul-2013",] ; CoreJuly5
OilJuly5 <- Oil[Oil$Date %in% "5-Jul-2013",] ; OilJuly5
NCJuly5 <- NC[NC$Date %in% "5-Jul-2013",] ; NCJuly5

library(reshape2)

melt111 <- melt(OilCoreJuly5, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
OC11melt <- ddply(melt111, "variable", summarise, mvalue = mean(value))
OC11 <- ggplot(data=OC11melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5, stat="identity") + 
             labs(title="Oil & Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1))   
OC11
#
melt222 <- melt(CoreJuly5, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
C11melt <- ddply(melt222, "variable", summarise, mvalue = mean(value))
C11 <- ggplot(data=C11melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Corexit") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
C11
#
melt333 <- melt(OilJuly5, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
O11melt <- ddply(melt333, "variable", summarise, mvalue = mean(value))
O11 <- ggplot(data=O11melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
            labs(title="Oil") + xlab("Aromatic Analyte") + ylab("mean value") +
            scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
            theme(text=element_text(size=12),
                  axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
O11
#
melt444 <- melt(NCJuly5, id=c('Date','Bucket','Treat','Chem','Oil','Corexit',
                                  'Herbivore','TtlAlkanes','TtlAromatics','Date1'),
               measure.vars=c('Naphthalene','Fluorene','Dibenzothiophene','Phenanthrene',
                              'Anthracene','Fluoranthene','Pyrene','Naphthobenzothiophene',
                              'Benzo_.a._Anthracene','Chrysene','Perylene'))
NC11melt <- ddply(melt444, "variable", summarise, mvalue = mean(value))
NC11 <- ggplot(data=NC11melt, aes(x=variable,y=mvalue)) + geom_bar(width=0.5,stat="identity") +
             labs(title="No Chemicals") + xlab("Aromatic Analyte") + ylab("mean value") +
             scale_y_continuous(limits=c(0,6000)) + theme_boxplot() +
             theme(text=element_text(size=12),
                   axis.text.x=element_text(angle=45, vjust=1, hjust=1)) 
NC11

#####
multiplot(NC11, C11, O11, OC11, cols=2)
###############################################################################
multiplot(Op, OCp, O11, OC11, cols=2)
################################################################################


