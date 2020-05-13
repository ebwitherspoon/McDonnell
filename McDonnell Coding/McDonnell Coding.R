# Install Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("readxl", "irr", "sjmisc", "tidyverse", "data.table") 

# Import ELA and Math Data
df_ELA <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_ELA_MASTER.xlsx")
df_MATH <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_MATH_MASTER.xlsx")
# Stack and Clean ELA and Math 
df_full <- rbind(df_ELA, df_MATH) %>%
  drop_na(Content) %>%
  mutate(odd = ifelse((Pair == "Radish" & Cycle == "B" & `Pre/Post` == "Post") |
           (Pair == "Rabbit" & Cycle == "3" & `Pre/Post`  == "Pre"), T, F)) %>%
  filter(odd == F) %>%
  rename(HiLo = `Hi/Lo`, PrePost = `Pre/Post`) %>%
  mutate(PrePost = fct_rev(PrePost)) %>%
  mutate(Cycle = recode(Cycle, "1" = "A", "2" = "B", "3" = "C")) %>%
  filter(Cycle != "A") %>%
  mutate(Actor = recode(Actor, `Student:` = "")) %>%
  mutate(Simulation1 = replace_na(Simulation1, 0)) %>%
  mutate(Simulation2 = replace_na(Simulation2, 0)) %>%
  mutate(Sim_Any = ifelse(Simulation1 == 1 | Simulation2 == 1, 1, 0)) %>%
  mutate(SimWords = ifelse(Sim_Any == 1, Words, 0)) %>%
  mutate(Subcode1 = (case_when(
    str_detect(Subcode1,"(?i)Eval*") ~ "Evaluating",
    str_detect(Subcode1, "(?i)Ambig*")  ~ "Ambiguity",
    str_detect(Subcode1, "(?i)Alte*")  ~ "Alternative",
    str_detect(Subcode1, "(?i)Relau*")  ~ "Relaunch",
    TRUE ~ Subcode1))) %>%
  mutate(Subcode1 = recode(Subcode1, `Frame` = "")) %>%
  mutate(Subcode2 = case_when(
    str_detect(Subcode2,"(?i)Eval*") ~ "Evaluating",
    str_detect(Subcode2, "(?i)Ambig*")  ~ "Ambiguity",
    str_detect(Subcode2, "(?i)Alte*")  ~ "Alternative",
    str_detect(Subcode2, "(?i)Relau*")  ~ "Relaunch",
    TRUE ~ Subcode2)) %>%
  mutate(Subcode2 = recode(Subcode2, `?` = "", `?*` = "", `*` = "", `END` = "", `NC` = "")) %>%
  mutate_all(na_if,"") %>%
  # Create Indicator Variables
  mutate(Eval1 = ifelse(Subcode1 != "Evaluating" | is.na(Subcode1),0,1)) %>%
  mutate(Ambig1 = ifelse(Subcode1 != "Ambiguity" | is.na(Subcode1),0,1)) %>%
  mutate(Alt1 = ifelse(Subcode1 != "Alternative" | is.na(Subcode1),0,1)) %>%
  mutate(Rel1 = ifelse(Subcode1 != "Relaunch" | is.na(Subcode1),0,1)) %>%
  mutate(Eval2 = ifelse(Subcode2 != "Evaluating" | is.na(Subcode2),0,1)) %>%
  mutate(Ambig2 = ifelse(Subcode2 != "Ambiguity" | is.na(Subcode2),0,1)) %>%
  mutate(Alt2 = ifelse(Subcode2 != "Alternative" | is.na(Subcode2),0,1)) %>%
  mutate(Rel2 = ifelse(Subcode2 != "Relaunch" | is.na(Subcode2),0,1)) %>%
  # Across Raters
  mutate(Eval_Any = ifelse(Eval1 == 1 | Eval1 == 1, 1, 0)) %>%
  mutate(Ambig_Any = ifelse(Ambig1 == 1 | Ambig2 == 1, 1, 0)) %>%
  mutate(Alt_Any = ifelse(Alt1 == 1 | Alt2 == 1, 1, 0)) %>%
  mutate(Rel_Any = ifelse(Rel1 == 1 | Rel2 == 1, 1, 0)) %>%
  mutate(EvalWords = ifelse(Eval_Any == 1, Words, 0)) %>%
  mutate(AmbigWords = ifelse(Ambig_Any == 1, Words, 0)) %>%
  mutate(AltWords = ifelse(Alt_Any == 1, Words, 0)) %>%
  mutate(RelWords = ifelse(Rel_Any == 1, Words, 0)) %>%
  # Generate Cumulative Count Variables
  mutate(Sim_Cum = cumsum(ifelse(Sim_Any == 0, 0, 
                                 ifelse(Sim_Any != lag(Sim_Any) | is.na(lag(Sim_Any)) , 1, 0)))) %>%
  mutate(Sim_Cum = ifelse(Sim_Any == 1, Sim_Cum, NA)) %>%
  mutate(Alt_Cum = cumsum(ifelse(Alt_Any == 0, 0, 
                                 ifelse(Alt_Any != lag(Alt_Any) | is.na(lag(Alt_Any)) , 1, 0)))) %>%
  # By Pair
  group_by(Pair, Cycle, PrePost) %>%
  mutate(Sim_Cum_Pair = cumsum(ifelse(Sim_Any == 0, 0, 
                                      ifelse(Sim_Any != lag(Sim_Any) | is.na(lag(Sim_Any)) , 1, 0)))) %>%
  mutate(Sim_Cum_Pair = ifelse(Sim_Any == 1, Sim_Cum_Pair, NA)) %>%
  mutate(Ambig_Cum_Pair = cumsum(ifelse(Ambig_Any == 0, 0, 
                                   ifelse(Ambig_Any != lag(Ambig_Any) | is.na(lag(Ambig_Any)) , 1, 0)))) %>%
  mutate(Ambig_Cum_Pair = ifelse(Ambig_Any == 1, Ambig_Cum_Pair, NA)) %>%
  ungroup() %>%
  # Per Simulation
  group_by(Pair, Cycle, PrePost, Sim_Cum) %>%
  mutate(AltCount = cumsum(ifelse(Alt_Any == 0, 0, 1))) %>%
  mutate(AltCount = ifelse(Alt_Any == 1, AltCount, NA)) %>%
  mutate(RelCount = cumsum(ifelse(Rel_Any == 0, 0, 1))) %>%
  mutate(RelCount = ifelse(Rel_Any == 1, RelCount, NA)) %>%
  ungroup() %>%
  # Per Alternative
  group_by(Pair, Cycle, PrePost, Sim_Cum, Alt_Cum) %>%
  mutate(EvalCount = cumsum(ifelse(Eval_Any == 0, 0, 1))) %>%
  mutate(EvalCount = ifelse(Eval_Any == 1, EvalCount, NA)) %>%
  ungroup() %>%
  mutate(Obs = row_number()) %>%
  # Select variables for analysis
  dplyr::select(Obs, Content:Actor,                                                # Identifiers
                Simulation1:Words,odd,                                             # Raw Codes
                Eval1:Rel2, Sim_Any, Ambig_Any, Alt_Any, Eval_Any, Rel_Any,        # Generated Vars: Indicators
                SimWords, AmbigWords, AltWords, EvalWords, RelWords,               # Generated Vars: Words
                Sim_Cum:EvalCount)                                                  # Generated Vars: Counts

write_csv(df_full, "~/Dropbox/GitHub/McDonnell/McDonnell Coding/McDonnell_CLEAN.csv")

