# Oiling Height calc_MesoExpt2013.r
### TempLight_HOBO_MesoExpt_2013.r
### Sulfides_MesoExpt_2013.r
#
# Snails_MesoExpt_2013.r
# Insects_MesoExpt_2013.r
# 
# Expected_Stressor_Effects_Function.r
### Expected_Stressor_Effects_MesoExp2013.r
# Multi-stressor calc_MesoExpt2013.r
# 
# Oil Chem Analysis_MesoExpt2013.r
### Oiling_FINAL_Data_MesoExpt2013.r
# 
# Effect_Size_Calc_Plant_Phys_MesoExpt_2013.R
# Initial Sp Bmss_MesoExpt 2013.R
### Photosyn_MesoExpt_2013.r
# Spartina_Bmss_MesoExpt_2013.R
# Curves_Light_ACi_MesoExpt2013.R
# 
### Figures_MS2013_Manuscript1.R
# 
### SEM_All_MesoExpt 2013.r
###      ALL_DATA_SEM_MesoExpt2013.csv
# 
### test_plot.R
# 
### Mesocosm_Expt_2013_SIA Figs.Rmd
#    Isotope_figures.txt
# 

library(DiagrammeR) ; library(DiagrammeRsvg) ; library(rsvg)



flowchar <- grViz("digraph {
                    
                  graph[overlap = true, fontsize = 16, rankdir = LR,
                        outputorder = edgesfirst]
                  
                  node[shape = rectangle, style = filled, fontname = Helvetica, color = black]

                  node[fillcolor = Plum2, margin = 0.2]
                  A1[label = 'Table 1: ANOVA Biomass']
                  A[label = 'Figure 1: Final Biomass']
                  B[label = 'Figure 2: SEM']
                  C[label = 'Figure 3: Ecophysiology']
                  D[label = 'Figure 4: Isotopes']
                  E[label = 'Supp Fig 1: Expt. Design']
                  F[label = 'Supp Fig 2: Expt. Sampling']
                  G[label = 'Supp Fig 3: Env. Conditions']
                  H[label = 'Supp Table 1: ANOVA Isotopes']
                  I[label = 'Supp Fig 4: Isotopes']
                  J[label = 'Supp Fig 5: Stressor Effects']
                  
                  node[fillcolor = PaleTurquoise1, margin = 0.2]
                  K[label = 'Figures_MS2013_Manuscript1.R']
                  L[label = 'test_plot.R']
                  M[label = 'SEM_All_MesoExpt 2013.r']
                  N[label = 'SEM_Figs.pptx']
                  O[label = 'Mesocosm_Expt_2013_SIA Figs.Rmd']
                  P[label = 'Experimental Design_MesoExpt2013.pptx']
                  Q[label = 'Google Calendar']
                  R[label = '']
                  S[label = '']
                  

                  node[fillcolor = DarkSeaGreen1, margin = 0.2]
                  AA[label = 'Photosyn_MesoExpt 2013.r']
                  BB[label = 'TempLight_HOBO_MesoExpt_2013.r']
                  CC[label = 'Sulfides_MesoExpt_2013.r']
                  DD[label = 'Oiling_FINAL Data_MesoExpt2013.r']
                  EE[label = 'Expected_Stressor_Effects_MesoExp2013.r']
                  FF[label = '']
                  
                  
                  node[fillcolor = YellowGreen, margin = 0.2]
                  LL[label = 'ALL_DATA_SEM_MesoExpt2013.csv']
                  MM[label = 'Isotope_figures.txt']
                  NN[label = 'Temp_Data_MASTER_MesoExpt2013.csv']
                  OO[label = 'Sulfide_MesoExpt2013.csv']
                  PP[label = 'OilConc_FINAL_MesoExpt 2013.csv']
                  QQ[label = 'LICOR_PhotosynMeas_MesoExpt_2013.csv']
                  

                  edge[color = black, arrowhead = vee, arrowsize = 1.25]
                  K -> L
                  L -> J
                  K -> A
                  K -> C
                  K -> G
                  AA -> K
                  BB -> K
                  CC -> K
                  DD -> K
                  EE -> K
                  M -> B
                  N -> B
                  O -> D
                  O -> H
                  O -> I
                  P -> E
                  Q -> F
                  LL -> M
                  LL -> PP
                  LL -> EE
                  MM -> O
                  NN -> BB
                  OO -> CC
                  PP -> DD
                  
                  
                  {rank = same; A1 -> A -> B -> C -> D -> E -> F -> G -> H -> I -> J [color=invis]}
                  {rank = same; L  [color=invis]}
                  {rank = same; N -> M -> K -> O [color=invis]}
                  {rank = same; AA -> EE -> DD -> CC -> BB [color=invis]}
                  {rank = same; PP -> OO -> MM -> NN  [color=invis]}
                  {rank = same; LL [color=invis]}
    
            }")
                  
                  