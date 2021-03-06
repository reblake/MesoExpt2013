#################################################################
###  Plotting Temperatures from Meso Expt 2013                ###
###  Data from HOBO loggers (4 in each of 2 greenhouse areas) ###
###  Script created by Rachael Blake    Sept 2013             ###
#################################################################

library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

Temp_raw <- read.csv("D:/Documents/LSU/MesoExp_2013/HOBO Data/Temp_Data_MASTER_MesoExpt2013.csv")
head(Temp_raw)
names(Temp_raw)

Temp_raw$Date1 <- as.Date(Temp_raw$Date, format="%m/%d/%Y")

oiled <- Temp_raw[Temp_raw$Oiled=="Y",]  ; head(oiled)
unoiled <- Temp_raw[Temp_raw$Oiled=="N",]  ; head(unoiled)

# mean oiled and un-oiled temps overall
mean(oiled$Temp_degC)  ;  sd(oiled$Temp_degC)
mean(unoiled$Temp_degC)  ;  sd(unoiled$Temp_degC)
# mean daily temps calc
daily_mean_temp <- Temp_raw %>%
                   select(-Logger, -Intensity_Lux, -Time, -Oiled) %>%
                   group_by(Date, Date1) %>%
                   summarize(DayMn_Temp_degC=mean(Temp_degC))
# min and max daily temps
min(daily_mean_temp$DayMn_Temp_degC) ; max(daily_mean_temp$DayMn_Temp_degC)

# mean oiled and un-oiled light overall
mean(oiled$Intensity_Lux)  ;  sd(oiled$Intensity_Lux)
mean(unoiled$Intensity_Lux)  ;  sd(unoiled$Intensity_Lux)
# mean light overall
mean(Temp_raw$Intensity_Lux)
# mean daily light calc
daily_mean_light <- Temp_raw %>%
                    select(-Logger, -Temp_degC, -Time, -Oiled) %>%
                    filter(!(Intensity_Lux==0)) %>%
                    group_by(Date, Date1) %>%
                    summarize(DayMn_Light_Lux=mean(Intensity_Lux))
# max daily light
max(daily_mean_light$DayMn_Light_Lux) 



######## TEMPERATURE ############################################

## Plotting the data

Temp_raw$Date1 <- as.Date(Temp_raw$Date, format="%m/%d/%Y")

Temp_raw_T <- Temp_raw %>%
              group_by(Date, Oiled, Date1) %>%
              summarise(Day_Mn_Temp = mean(Temp_degC))


T1 <- ggplot(data=Temp_raw_T, aes(x=Date1, y=Day_Mn_Temp, colour=Oiled)) +
             geom_line() + xlab("Date") + theme_bw() +
             ylab(expression(paste("Temperature (", degree ~ C,")"))) +
             scale_x_date(breaks=date_breaks("week"), labels=date_format("%b-%d")) +
             scale_colour_manual(values=c("black","gold"), name="Greenhouse",
                                 labels=c("No oil","Oiled")) +
             theme(panel.grid=element_blank())

T2 <- ggplot(data=Temp_raw, aes(x=Oiled, y=Temp_degC, color=Oiled)) + 
             geom_boxplot() + xlab("Oil Treatment") + theme_bw() +
             ylab(expression(paste("Temperature (", degree ~ C,")"))) 

##  Test for difference between the two greenhouse sections
summary(lm(Day_Mn_Temp~Oiled, data=Temp_raw_T))



######## LIGHT ##################################################

## Plotting the data
#library(ggplot2) ; library(plyr) ; library(grid) ; library(scales)

#Temp_raw$Date1 <- as.Date(Temp_raw$Date, format="%m/%d/%Y") 

 Temp_raw_L <- Temp_raw %>%
               group_by(Date, Oiled, Date1) %>%
               summarise(Day_Mn_Light = mean(Intensity_Lux))

 L1 <- ggplot(data=Temp_raw_L, aes(x=Date1, y=Day_Mn_Light, colour=Oiled)) +
              geom_line() + xlab("Date") + theme_bw() +
              ylab(expression(paste("Light Intensity (", Lux ,")"))) +
              scale_x_date(breaks=date_breaks("week"), labels=date_format("%b-%d")) +
              scale_colour_manual(values=c("black","gold"), name="Greenhouse",
                                  labels=c("No oil","Oiled")) +
              theme(panel.grid=element_blank())


Temp_raw_Lday <- data.frame(filter(Temp_raw, Intensity_Lux!=0))

L2 <- ggplot(data=Temp_raw_Lday, aes(x=Oiled, y=Intensity_Lux, color=Oiled)) + 
             geom_boxplot() + xlab("Oil Treatment") + theme_bw() +
             ylab(expression(paste("Light Intensity (" , Lux ,")"))) 


##  Test for difference between the two greenhouse sections
summary(lm(Day_Mn_Light~Oiled, data=Temp_raw_L))


