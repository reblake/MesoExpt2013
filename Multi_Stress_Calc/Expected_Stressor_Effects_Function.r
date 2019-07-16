#################################################
### Expectged Stressors Effects Function      ###
### Modified from that used for 2006 expt     ###
### Rachael E. Blake    March 2014            ###
#################################################

# Need to write a function for choosing and applying the correct "expected" equation.

# Expected Stressor Effects Function Code:
       
# In this case, might be useful to attach and detach the dataset, so that I don't have to go in and
# re-type all those dataset names each time.

###################################################


setwd("C:\\Users\\rblake\\Documents\\LSU\\MesoExp_2013\\")

LIVE <- read.csv("Exp Stress_LiveBmss_MesoExpt2013.csv")
names(LIVE)

DEAD <- read.csv("Exp Stress_DeadBmss_MesoExpt2013.csv")
names(DEAD)

NUM <- read.csv("Exp Stress_StemNum_MesoExpt2013.csv")
names(NUM)


###################################################

attach(LIVE)

ExpectedStressFunction <- function(equation) {
       if(equation==1){ExpSt <- control+(oil-control)+(corexit-control)} else     #positive
       if (equation==2){ExpSt <- control-(control-oil)-(control-corexit)} else    #negative
       if (equation==3){ExpSt <- control+(oil-control)-(control-corexit)} else    #oil>control
       if (equation==4){ExpSt <- control+(corexit-control)-(control-oil)}         #oil<control
       return (ExpSt)
       }

#specify which equation to use
#    1 = positive
#    2 = negative
#    3 = mixed1 (oil>control)
#    4 = mixed2 (oil<control)      
       
equation <- 1                                                              
ExpectedStressFunction(equation)

#This is for use on my laptop:
write.table(ExpectedStressFunction(equation),"C:/Users/reblake/Desktop/ExpSt_Zost-Elas.csv", sep=",") 


detach(LIVE)

####################################################

