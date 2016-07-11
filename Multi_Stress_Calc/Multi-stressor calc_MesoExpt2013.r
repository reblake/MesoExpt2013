###############################################################
###  Stressor interaction analysis                          ###
###  Mesocosm Experiment 2013                               ###
###  Script by Rachael Blake                                ###
###  October 2013                                           ###
###############################################################


########################################################################################
# From Blake & Duffy 2010 Oikos
# for cases where responses of both single-stressor treatments were less than control, 
# we used the equation 
# {ExpSt <- control-(control-stressA)-(control-stressB)}

# for cases where respons of both single-stressor treatments were more than control, 
# we used the equation
# {ExpSt <- control+(stressA-control)+(stressB-control)}

# for cases where stressor response was mixed we used one of the following depending on 
# the which is greater...exptst <- C+(Stress>C-C)-(C-Stress<C)
# {ExpSt <- control+(stressA-control)-(control-stressB)}
# {ExpSt <- control+(stressB-control)-(control-stressA)}
#########################################################################################

# Spartina Growth / Biomass
setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\Sp Biomass")

# The mean stem data
Stems <- read.csv("SpBmssForMultiStress_Meso2013.csv")
head(Stems)
names(Stems)

#subsetting for week 6 stem number only
Wk6 <- subset(Stems, Stems$Week==6)#, select=TtlStemNum:Herbivore)
Wk6

# arranging the data
control <- Wk6[(Wk6$Oil==0)&(Wk6$Corexit==1),8]
Oil <- Wk6[(Wk6$Oil==1)&(Wk6$Corexit==0),8]
Corexit <- Wk6[(Wk6$Oil==0)&(Wk6$Corexit==1),8]
Multi <- Wk6[(Wk6$Oil==1)&(Wk6$Corexit==1),8]

Wk6StemNumbyChem <- cbind(control,Oil,Corexit,Multi)
Wk6StemNumbyChem <- as.data.frame(Wk6StemNumbyChem)

### MULTI_STRESSOR CALC FUNCTION #############################

ExpectedStressFunction <- function(equation) {
     if(equation==1){ExpSt <- control+(Oil-control)+(Corexit-control)} else     #positive
     if (equation==2){ExpSt <- control-(control-Oil)-(control-Corexit)} else    #negative
     if (equation==3){ExpSt <- control+(Oil-control)-(control-Corexit)} else    #Oil>control
     if (equation==4){ExpSt <- control+(Corexit-control)-(control-Oil)}         #Oil<control
     return (ExpSt)
     }

#specify which equation to use
#    1 = positive
#    2 = negative
#    3 = mixed1 (Oil>control)
#    4 = mixed2 (Oil<control)      
     
############################################################

eq <- 1                                                              
with(Wk6StemNumbyChem, ExpectedStressFunction(eq))
eq2 <- 2                                                              
with(Wk6StemNumbyChem, ExpectedStressFunction(eq2))
eq3 <- 3                                                              
with(Wk6StemNumbyChem, ExpectedStressFunction(eq3))
eq4 <- 4
with(Wk6StemNumbyChem, ExpectedStressFunction(eq4))







