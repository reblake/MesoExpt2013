library(purrr) ; library(tidyverse) ; library(ggforce) ; library(gridExtra)


ExpStrsCalc  # list of values

exp_ul <- unlist(ExpStrsCalc)
  
  
exp_df <- as_tibble(as.list(exp_ul))


exp_long <- exp_df %>% 
            gather() %>% 
            dplyr::rename(response = key,
                   expected_stress = value) %>% 
            mutate(Herbivore = substring(response, regexpr("_", response) + 1))


ggplot(data = exp_long, aes(x = response, y = expected_stress)) + geom_point()




##############
# Dotplots

head(AllD)



dplot <- function(varib){
                  ggplot(data = AllD, aes_string(x = "Chem1", y = "Herbivore")) + 
                  geom_point(aes_string(size = varib))
                  }

dplot("LiveStemDryWgt_g")

dplot("DeadStemDryWgt_g")

dplot("TtlStemNum")



dplot2 <- function(varib){
                   ggplot(data = AllD, aes_string(x = "Chem1", y = "Herbivore")) + 
                   geom_point(aes_string(size = varib))
                   }

dplot2("LiveStemDryWgt_g")


dot_df <- AllD %>% 
          select(Treat, Herbivore, Chem1, LiveStemDryWgt_g, DeadStemDryWgt_g, TtlStemNum, LvRootDryWgt_Scaled,
                 DdRootDryWgt, ProkAbunScaled, SnailWgtScaled) %>% 
          group_by(Treat, Herbivore, Chem1) %>% 
          summarise_at(c("LiveStemDryWgt_g", "DeadStemDryWgt_g", "TtlStemNum", "LvRootDryWgt_Scaled",
                         "DdRootDryWgt", "ProkAbunScaled", "SnailWgtScaled"), mean, na.rm = TRUE) %>% 
          ungroup() %>%
          gather(key, val, LiveStemDryWgt_g:SnailWgtScaled) %>% 
          mutate(var_resp = ifelse(key == "LiveStemDryWgt_g", "LIVE",
                            ifelse(key == "DeadStemDryWgt_g", "DEAD",
                            ifelse(key =="TtlStemNum", "NUM",
                            ifelse(key == "LvRootDryWgt_Scaled", "LIVR",
                            ifelse(key == "DdRootDryWgt", "DEDR",
                            ifelse(key == "ProkAbunScaled", "PROK",
                            ifelse(key == "SnailWgtScaled", "SNAL", key))))))),
                 response = paste0(var_resp, "_", Herbivore)
                 ) %>% 
          full_join(exp_long, by = c("response", "Herbivore")) %>% 
          full_join(exp_df2) %>%  # defined below
          dplyr::rename(two_chem = val) %>%
          mutate(expected_stress = case_when(var_resp == "PROK" & !(is.na(expected_stress)) ~ 0,
                                             TRUE ~ expected_stress),
                 std_two_chem = two_chem/control,
                 std_exp_stress = expected_stress/control)



dplot3 <- ggplot(data = dot_df %>% filter(!var_resp %in% c("DEAD", "DEDR")), 
                 aes(x = var_resp, y = Herbivore)) +
          geom_point(data = . %>% filter(!is.na(.$std_exp_stress)), shape = 1,
                     aes(size = factor(std_exp_stress))) + 
          geom_point(data = . %>% filter(Treat %in% c("M", "N", "O", "P"),
                                         !is.na(.$std_exp_stress)), 
                     shape = 1,
                     aes(size = factor(std_two_chem),
                         color = factor(ms_effect))) +
          scale_size_discrete(range = c(10, 18))
  


##############
# 1-1 line plots

exp_df <- AllD %>% 
          select(Treat, Herbivore, Chem1, LiveStemDryWgt_g, DeadStemDryWgt_g, TtlStemNum, LvRootDryWgt_Scaled,
                 DdRootDryWgt, ProkAbunScaled, SnailWgtScaled) %>% 
          group_by(Treat, Herbivore) %>% 
          summarise_at(c("LiveStemDryWgt_g", "DeadStemDryWgt_g", "TtlStemNum", "LvRootDryWgt_Scaled",
                 "DdRootDryWgt", "ProkAbunScaled", "SnailWgtScaled"), mean, na.rm = TRUE) %>% 
          ungroup() %>%
          gather(key, val, LiveStemDryWgt_g:SnailWgtScaled) %>% 
          mutate(var_resp = ifelse(key == "LiveStemDryWgt_g", "LIVE",
                            ifelse(key == "DeadStemDryWgt_g", "DEAD",
                            ifelse(key =="TtlStemNum", "NUM",
                            ifelse(key == "LvRootDryWgt_Scaled", "LIVR",
                            ifelse(key == "DdRootDryWgt", "DEDR",
                            ifelse(key == "ProkAbunScaled", "PROK",
                            ifelse(key == "SnailWgtScaled", "SNAL", key))))))),
                 response = paste0(var_resp, "_", Herbivore)
                 ) %>% 
          full_join(exp_long, by = c("response", "Herbivore")) %>% 
          filter(!(Treat %in% c("E", "F", "G", "H", "I", "J", "K", "L"))) %>% 
          mutate(ms_effect = ifelse(response %in% c("LIVE_P", "LIVR_P"), "additive", 
                             ifelse(response %in% c("LIVE_SP", "LIVE_S", "LIVE_NG", "LIVR_NG",
                                                    "LIVR_S", "NUM_S", "PROK_P", "PROK_SP"), "antagonistic",
                             ifelse(response %in% c("NUM_SP", "LIVR_SP", "NUM_P", "NUM_NG",
                                                    "SNAL_SP", "SNAL_S"), "synergistic", NA_character_)))
                 )


exp_df2 <- exp_df %>% 
           filter(Treat %in% c("M", "N", "O", "P")) %>% 
           select(-key, -expected_stress, -Treat) %>% 
           dplyr::rename(control = val)

exp_df3 <- exp_df %>% 
           filter(Treat %in% c("A", "B", "C", "D")) %>% 
           dplyr::rename(two_chem = val) %>% 
           full_join(exp_df2) %>% 
           mutate(expected_stress = case_when(var_resp == "PROK" & !(is.na(expected_stress)) ~ 0,
                                              TRUE ~ expected_stress),
                  std_two_chem = two_chem/control,
                  std_exp_stress = expected_stress/control,
                  mask = ifelse(response %in% c("SNAL_SP", "SNAL_S"), 1, 0)) %>% 
           filter(!(var_resp %in% c("DEAD", "DEDR")))


##################

exp1 <- ggplot(data = exp_df3, aes(x = std_exp_stress, y = std_two_chem)) + 
               geom_point(data = . %>% filter(!(var_resp %in% c("SNAL"))), size = 4, 
                          aes(fill = ms_effect, shape = var_resp)) + 
               geom_abline(intercept = 0, slope = 1) + xlim(0, 1.5) + ylim(0, 1.5) + 
               theme_bw() + xlab("") + labs(title= "b.") +
               xlab("Expected multi-stress effect (additive)") + 
               ylab("") + 
               scale_shape_manual(breaks = c("LIVE", "LIVR", "NUM", "PROK", "SNAL"),
                                  labels = c("Live Shoots", "Live Roots", "Stem Number", 
                                             "Insect Number", "Change in snail mass (g)"),
                                  values = c(21, 22, 23, 24, 25), guide = FALSE) +
               scale_fill_manual(breaks = c("additive", "synergistic", "antagonistic"),
                                 values = c("grey40", "black", "grey90"), guide = FALSE) +
             #  guides(fill = guide_legend(override.aes=list(shape = 21)), guide = FALSE) +
               theme(axis.text = element_text(size=13),
                     axis.title = element_text(size=15),
                     legend.title = element_blank(),
                     legend.position = c(0.9, 0.35),
                     legend.spacing.y = unit(-0.2, "cm"),
                     legend.text=element_text(size=11),
                     panel.grid = element_blank())




exp2 <- ggplot(data = exp_df3) + 
               geom_point(data = . %>% filter(var_resp %in% c("SNAL")), size = 4, 
                          aes(x = std_exp_stress, y = std_two_chem, shape = var_resp, fill = ms_effect)) + 
               scale_shape_manual(breaks = c("SNAL"),
                                  labels = c("Change in snail mass (g)"),
                                  values = 25, guide = FALSE) +
                scale_fill_manual(breaks = c("synergistic"),
                                  values = c("grey90"), guide = FALSE) +
               geom_abline(slope = 1) + xlim(0, 8) + ylim(-14, 1.5) + labs(title= "b.") +
               theme_bw() + xlab("Expected multi-stress effect (additive)") + 
               ylab("Observed multi-stress effect") +
               # guides(fill = guide_legend(override.aes=list(shape = 21))) +
               theme(axis.text = element_text(size=13),
                     axis.title = element_text(size=15),
                     legend.position = "none"#,
                     # legend.title = element_blank(),
                     # legend.position = c(0.85, 0.5),
                     # legend.spacing.y = unit(-0.2, "cm"),
                     # legend.text=element_text(size=11)
                     )
       

exp3 <- ggplot(data = exp_df3) + 
               geom_point(size = 4, 
                          aes(x = std_exp_stress, y = std_two_chem, shape = var_resp, fill = ms_effect)) + 
               xlab("Expected multi-stress effect (additive)") + 
               ylab("Observed multi-stress effect") + theme_bw() +
               scale_shape_manual(breaks = c("LIVE", "LIVR", "NUM", "PROK", "SNAL"),
                                  labels = c("Live Shoots", "Live Roots", "Stem Number", 
                                             "Insect Number", "Change in snail\nmass (g)"),
                                  values = c(21, 22, 23, 24, 25)) +
               scale_fill_manual(breaks = c("additive", "synergistic", "antagonistic"),
                                 values = c("grey40", "black", "grey90")) +
               geom_abline(slope = 1) + xlim(-1, 8) + ylim(-14, 2) + labs(title= "a.") +
               annotate("segment", x=-0.7, y=-0.7, xend=2, yend=-0.7, color="black", 
                        linetype="dashed", size=0.5) +
               annotate("segment", x=2, y=-0.7, xend=2, yend=2, color="black", 
                        linetype="dashed", size=0.5) +
               annotate("segment", x=2, y=2, xend=-0.7, yend=2, color="black", 
                        linetype="dashed", size=0.5) +
               annotate("segment", x=-0.7, y=2, xend=-0.7, yend=-0.7, color="black", 
                        linetype="dashed", size=0.5) +
               guides(fill = guide_legend(override.aes=list(shape = 21))) +
               theme(axis.text = element_text(size=13),
                     axis.title = element_text(size=15),
                     legend.title = element_blank(),
                     legend.position = c(0.7, 0.65),
                     legend.spacing.y = unit(-0.2, "cm"),
                     legend.text=element_text(size=11),
                     panel.grid = element_blank()
                     )


grid.arrange(exp3, exp1, ncol=2, nrow=1)


#########






