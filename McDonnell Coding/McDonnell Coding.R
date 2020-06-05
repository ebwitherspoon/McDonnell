# Install Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "readxl", "irr") 

# Import ELA and Math Data
df_ELA <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_ELA_MASTER_FINAL.xlsx")
df_MATH <- read_excel("~/Box Sync/McDonnell Teacher Learning Shared/Phase1_Coding_2020/Mechanisms Coding/Mechanisms_MATH_MASTER_FINAL.xlsx")
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
  mutate(Sim_Final = replace_na(Sim_Final, 0)) %>%
  mutate(SimWords = ifelse(Sim_Final == 1, Words, 0)) %>%
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
  mutate(Sub_Final = case_when(
    str_detect(Sub_Final,"(?i)Eval*") ~ "Evaluating",
    str_detect(Sub_Final, "(?i)Ambig*")  ~ "Ambiguity",
    str_detect(Sub_Final, "(?i)Alte*")  ~ "Alternative",
    str_detect(Sub_Final, "(?i)Relau*")  ~ "Relaunch",
    TRUE ~ Sub_Final)) %>%
  # Create Indicator Variables
  mutate(Eval_Final = ifelse(Sub_Final == "Evaluating" & !is.na(Sub_Final), 1, 0)) %>%
  mutate(Ambig_Final = ifelse(Sub_Final == "Ambiguity" & !is.na(Sub_Final), 1, 0)) %>%
  mutate(Alt_Final = ifelse(Sub_Final == "Alternative" & !is.na(Sub_Final) |
                              `New_alt(eval)` & !is.na(`New_alt(eval)`) == 1, 1, 0)) %>%
  mutate(Rel_Final = ifelse(Sub_Final != "Relaunch" | is.na(Sub_Final), 0, 1)) %>%
  # Word Counts
  mutate(EvalWords = ifelse(Eval_Final == 1, Words, 0)) %>%
  mutate(AmbigWords = ifelse(Ambig_Final == 1, Words, 0)) %>%
  mutate(AltWords = ifelse(Alt_Final == 1, Words, 0)) %>%
  mutate(RelWords = ifelse(Rel_Final == 1, Words, 0)) %>%
  # Generate Cumulative Count Variables
  mutate(Sim_Cum = cumsum(ifelse(Sim_Final == 0, 0, 
                                 ifelse(Sim_Final == 1 & Ambig_Final == 1, 1, 0)))) %>%
  mutate(Sim_Cum = ifelse(Sim_Final == 1, Sim_Cum, NA)) %>%
  mutate(Alt_Cum = cumsum(ifelse(Alt_Final == 0, 0, 
                                 ifelse(Alt_Final != lag(Alt_Final) | is.na(lag(Alt_Final)) , 1, 0)))) %>%
  # By Pair
  group_by(Pair, Cycle, PrePost) %>%
  mutate(Sim_Cum_Pair = cumsum(ifelse(Sim_Final == 0, 0, 
                                      ifelse(Sim_Final != lag(Sim_Final) | is.na(lag(Sim_Final)) , 1, 0)))) %>%
  mutate(Sim_Cum_Pair = ifelse(Sim_Final == 1, Sim_Cum_Pair, NA)) %>%
  mutate(Ambig_Cum_Pair = cumsum(ifelse(Ambig_Final == 0, 0, 
                                   ifelse(Ambig_Final != lag(Ambig_Final) | is.na(lag(Ambig_Final)) , 1, 0)))) %>%
  mutate(Ambig_Cum_Pair = ifelse(Ambig_Final == 1, Ambig_Cum_Pair, NA))  %>%
  ungroup() %>%
  # Per Simulation
  group_by(Pair, Cycle, PrePost, Sim_Cum) %>%
  mutate(AltCount = cumsum(ifelse(Alt_Final == 0, 0, 1))) %>%
  mutate(AltCount = ifelse(Alt_Final == 1, AltCount, NA)) %>%
  mutate(RelCount = cumsum(ifelse(Rel_Final == 0, 0, 1))) %>%
  mutate(RelCount = ifelse(Rel_Final == 1, RelCount, NA)) %>%
  ungroup() %>%
  # Per Alternative
  group_by(Pair, Cycle, PrePost, Sim_Cum, Alt_Cum) %>%
  mutate(EvalCount = cumsum(ifelse(Eval_Final == 0, 0, 1))) %>%
  mutate(EvalCount = ifelse(Eval_Final == 1, EvalCount, NA)) %>%
  ungroup() %>%
  mutate(Obs = row_number()) %>%
  mutate(PairsCyclePrePost = paste(Pair,Cycle, PrePost)) %>%
  mutate(PairsCyclePrePost = as_factor(PairsCyclePrePost)) %>%
  group_by(PairsCyclePrePost) %>%
  mutate(ObsPair = row_number()) %>%
  mutate(maxObs = max(ObsPair)) %>%
  mutate(SelectAll = 1) %>%
  ungroup() %>%
  mutate(plotAmbig = ifelse(Ambig_Final == 0, -10, 1.025)) %>%
  mutate(plotAlt = ifelse(Alt_Final == 0, -10, 1)) %>%
  mutate(plotEval = ifelse(Eval_Final == 0, -10, .975)) %>%
  # Select variables for analysis
  dplyr::select(Obs, Content:Actor, PairsCyclePrePost,                      # Identifiers
                Sim_Final:Words,odd, 
                Ambig_Final, Alt_Final, Eval_Final, Rel_Final,              # Raw Codes
                SimWords, AmbigWords, AltWords, EvalWords, RelWords,        # Generated Vars: Indicators
                Sim_Cum, Sim_Cum_Pair, Ambig_Cum_Pair, Alt_Cum,
                AltCount, EvalCount, RelCount,                              # Generated Vars: Words
                ObsPair:plotEval)                                           # Generated Vars: Counts

write_csv(df_full, "~/Dropbox/GitHub/McDonnell/McDonnell Coding/McDonnell_CLEAN.csv")

df_full <- read_csv("~/McDonnell_CLEAN.csv")

df_test <- df_full %>%
  group_by(Content, HiLo, PairsCyclePrePost, Sim_Cum, Pair, PrePost) %>%
  mutate(AltTally = cumsum(ifelse(Rel_Final == 0, 0, 1)), 
         AltTally = ifelse(Rel_Final == 1, AltTally, NA), 
         AltTally = max(AltTally, na.rm = T)) 
  slice(1) %>%
  mutate(AltTally = ifelse(AltTally == -Inf, NA, AltTally)) %>%
  ungroup() %>%
  group_by(Pair, PrePost) %>%
  mutate(Alt_Med = median(AltTally, na.rm = T),
         Alt_Med = ifelse(is.nan(Alt_Med), NA, Alt_Med))

p1 <- df_test %>% 
  subset(Content == "ELA") %>%
  ggplot(aes_string(x = "Pair", y = "Alt_Med", fill = "PrePost")) + 
    ggtitle("ELA") +
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") +
    geom_text(aes(label=round(Alt_Med, digits = 2)), position = position_dodge(width = 1)) +
    coord_cartesian(ylim = c(0,50))
  plot(p1)

