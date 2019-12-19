# Oiling Height calc_MesoExpt2013.r
# TempLight_HOBO_MesoExpt_2013.r
# Sulfides_MesoExpt_2013.r
#
# Snails_MesoExpt_2013.r
# Insects_MesoExpt_2013
# 
# Expected_Stressor_Effects_Function.r
# Expected_Stressor_Effects_MesoExp2013.r
# Multi-stressor calc_MesoExpt2013.r
# 
# Oil Chem Analysis_MesoExpt2013.r
# Oiling_FINAL_Data_MesoExpt2013.r
# 
# Effect_Size_Calc_Plant_Phys_MesoExpt_2013.R
# Initial Sp Bmss_MesoExpt 2013.R
# Photosyn_MesoExpt_2013.r
# Spartina_Bmss_MesoExpt_2013.R
# Curves_Light_ACi_MesoExpt2013.R
# 
# Figures_MS2013_Manuscript1.R
# 
# SEM_All_MesoExpt 2013.r
# SEM_All_MesoExpt 2013_Mar2014.r
# 
# test_plot.R
# 
# 

library(DiagrammeR) ; library(DiagrammeRsvg) ; library(rsvg)



flowchar <- grViz("digraph {
                    
                  graph[overlap = true, fontsize = 16, rankdir = LR,
                        outputorder = edgesfirst]
                  
                  node[shape = rectangle, style = filled, fontname = Helvetica, color = black]

                  node[fillcolor = Plum2, margin = 0.2]
                  A[label = 'Table 1: ANOVA']
                  B[label = 'Figure 1: Final Biomass']
                  C[label = 'Figure 2: SEM']
                  D[label = 'Figure 3: Ecophysiology']
                  E[label = 'Figure 4: Isotopes']
                  F[label = 'Supp Fig 1: Expt. Design']
                  G[label = 'Supp Fig 2: Expt. Sampling']
                  H[label = 'Supp Fig 3: Env. Conditions']
                  I[label = 'Supp Table 1: ANOVA Isotopes']
                  J[label = 'Supp Fig 4: Isotopes']
                  K[label = 'Supp Fig 5: Stressor Effects']
                  
                  node[fillcolor = PaleTurquoise1, margin = 0.2]
                  L[label = 'Figures_MS2013_Manuscript1.R']
                  M[label = 'test_plot.R']
                  
                  

                  node[fillcolor = DarkSeaGreen1, margin = 0.2]
                  
                  
                  N[label = '']
                  O[label = '']
                  P[label = '']
                  Q[label = '']
                  R[label = '']
                  S[label = '']

                  edge[color = black, arrowhead = vee, arrowsize = 1.25]
                  H -> {A B C} 
                  I -> {D E}
                  J -> D 
                  
                  {rank = same; N -> GG -> MM -> FF [color=invis]}
    
            }")
                  
                  