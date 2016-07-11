######################################################
###  Light and A-Ci Curves
###  Meso Expt 2013
###  Script by Rachael Blake, May 2016
######################################################

# Load libraries
library(plyr) ; library(ggplot2) ; library(dplyr) ; library(grid) ; library(scales)
library(car) ; library(gridExtra) 


curves <- read.csv("C:/Users/rblake/Documents/LSU/MesoExp_2013/LICOR_Files_Meso_Expt/LICOR_CURVES_MesoExpt 2013.csv", header=TRUE)
head(curves) ; str(curves)

a_ci <- curves %>% 
        filter(Curve_type == "A-Ci",
               Bucket.Number %in% c(67,6,70,38,10,73,84,86,77,37)) 
        
ggplot(data=a_ci, aes(x=Ci, y=Photo, group=Bucket.Number, color=Bucket.Number)) +
       geom_line(size=1)


lite <- curves %>%
        filter(Curve_type == "Light",
               Bucket.Number %in% c(6,74,24,73,21,76))

ggplot(data=lite, aes(x=Ci, y=Photo, group=Bucket.Number, color=Bucket.Number)) +
       geom_line(size=1)



