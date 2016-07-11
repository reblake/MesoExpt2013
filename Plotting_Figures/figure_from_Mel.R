library(ggplot2)
library(dplyr)

data <- source('C:/Users/Melanie/Desktop/plotQuestion/data.txt')
data <- data$value


ggplot(data=data, aes(x=Herbivore, y=Photo, fill=Chem1)) + 
  geom_boxplot() + theme_bw() + facet_wrap(~ Week1, ncol=2) 

table(data$Herbivore, data$Chem1, data$Week1)


data2 <- data %>%
  select(Chem1, Herbivore, Week1, Photo)

complete <- expand.grid(Chem1 = unique(data2$Chem1),
                        Herbivore = unique(data2$Herbivore),
                        Week1 = unique(data2$Week1)) 

data2 <- full_join(complete, data2, by=c("Chem1", "Herbivore", "Week1")) %>%
  mutate(Photo = ifelse(is.na(Photo), -10, Photo))
table(data2$Herbivore, data2$Chem1, data2$Week1)

  
ggplot(data=data2, aes(x=Herbivore, y=Photo)) + 
  geom_boxplot(aes(fill=Chem1)) + theme_bw() + facet_wrap(~ Week1, ncol=2) +
  coord_cartesian(ylim = c(0, 30) + c(-.25, .25))
