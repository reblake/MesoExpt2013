#################################################################
###  Plotting Temperatures from Meso Expt 2013                ###
###  Data from HOBO loggers (4 in each of 2 greenhouse areas) ###
###  Script created by Rachael Blake    Sept 2013             ###
#################################################################

setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\HOBO Data\\")

Temp_raw <- read.csv("Temp Data_MASTER_MesoExpt2013.csv")
head(Temp_raw)
names(Temp_raw)

oiled <- Temp_raw[Temp_raw$Oiled=="Y",]  ; head(oiled)
unoiled <- Temp_raw[Temp_raw$Oiled=="N",]  ; head(unoiled)

mean(oiled$Temp_degC)  ;  sd(oiled$Temp_degC)
mean(unoiled$Temp_degC)  ;  sd(unoiled$Temp_degC)

######## TEMPERATURE ############################################
##  T-test for difference between the two greenhouse sections
t.test(oiled$Temp_degC,unoiled$Temp_degC)


## Plotting the data
library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

Temp_raw$Date1 <- as.Date(Temp_raw$Date, format="%m/%d/%Y") 

T1 <- ggplot(data=Temp_raw, aes(x=Date1, y=Temp_degC, group=Logger, 
                                colour=Oiled)) + 
             geom_line() + xlab("Date") + theme_bw() +
             ylab(expression(paste("Temperature (", degree ~ C,")"))) + 
             scale_x_date(breaks="week", labels=date_format("%b-%d")) +
             scale_colour_manual(values=c("black","red"), name="Greenhouse", 
                                 labels=c("No oil","Oiled")) +
             theme(panel.grid=element_blank()) 


######## LIGHT ##################################################
##  T-test for difference between the two greenhouse sections
# using "oiled" and "unoiled" from above
t.test(oiled$Intensity_Lux,unoiled$Intensity_Lux)

## Plotting the data
#library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

#Temp_raw$Date1 <- as.Date(Temp_raw$Date, format="%m/%d/%Y") 

L1 <- ggplot(data=Temp_raw, aes(x=Date1, y=Intensity_Lux, group=Logger, 
                                colour=Oiled)) + 
             geom_line() + xlab("Date") + theme_bw() +
             ylab(expression(paste("Light Intensity (",Lux,")"))) + 
             scale_x_date(breaks="week", labels=date_format("%b-%d")) +
             scale_colour_manual(values=c("black","green"), name="Greenhouse", 
                                 labels=c("No oil","Oiled")) +
             theme(panel.grid=element_blank()) 




