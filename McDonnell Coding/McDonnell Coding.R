# Install Packages
if (!require("pacman")) install.packages("pacman")
library(pacman)
pacman::p_load("tidyverse", "readxl", "irr", "sjmisc") 

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
  mutate(Actor = recode(Actor, `Student:` = "")) %>%
  mutate(Simulation1 = replace_na(Simulation1, value = 0)) %>%
  mutate(Simulation2 = replace_na(Simulation2, value = 0)) %>%
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
  mutate(Eval1 = ifelse(Subcode1 != "Evaluating" | is.na(Subcode1),0,1)) %>%
  mutate(Ambig1 = ifelse(Subcode1 != "Ambiguity" | is.na(Subcode1),0,1)) %>%
  mutate(Alt1 = ifelse(Subcode1 != "Alternative" | is.na(Subcode1),0,1)) %>%
  mutate(Rel1 = ifelse(Subcode1 != "Relaunch" | is.na(Subcode1),0,1)) %>%
  mutate(Eval2 = ifelse(Subcode2 != "Evaluating" | is.na(Subcode2),0,1)) %>%
  mutate(Ambig2 = ifelse(Subcode2 != "Ambiguity" | is.na(Subcode2),0,1)) %>%
  mutate(Alt2 = ifelse(Subcode2 != "Alternative" | is.na(Subcode2),0,1)) %>%
  mutate(Rel2 = ifelse(Subcode2 != "Relaunch" | is.na(Subcode2),0,1)) %>%
  mutate(Eval_Any = ifelse(Eval1 == 1 | Eval1 == 1, 1, 0)) %>%
  mutate(Ambig_Any = ifelse(Ambig1 == 1 | Ambig2 == 1, 1, 0)) %>%
  mutate(Alt_Any = ifelse(Alt1 == 1 | Alt2 == 1, 1, 0)) %>%
  mutate(Rel_Any = ifelse(Rel1 == 1 | Rel2 == 1, 1, 0)) %>%
  mutate(EvalWords = ifelse(Eval_Any == 1, Words, 0)) %>%
  mutate(AmbigWords = ifelse(Ambig_Any == 1, Words, 0)) %>%
  mutate(AltWords = ifelse(Alt_Any == 1, Words, 0)) %>%
  mutate(RelWords = ifelse(Rel_Any == 1, Words, 0)) %>%
  select(Content:Actor, Simulation1:RelWords)

write_csv(df_full, "~/Dropbox/GitHub/McDonnell/McDonnell Coding/McDonnell_CLEAN.csv")
  
##### Kappas #####
# ELA - Overall
df_ELA <- df_full %>%
  subset(Content == "ELA") %>%
  select(Simulation1, Simulation2) 
kappa2(df_ELA)

#### Descriptives and Graphs ####

# ELA Simulation by Hi/Low

df_plot1 <- df_full %>%
  filter(Content == "Math") %>%
  group_by_at(vars(HiLo)) %>%
  mutate(Sim_Pct = mean(Simulation1*100))

plot1 <- df_plot1 %>%
  ggplot( 
         aes_string(x = "HiLo", y = "Sim_Pct", fill = "HiLo")) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") +
    geom_text(aes(label=round(Sim_Pct, digits = 2)), position = position_dodge(width = 1)) +
    coord_cartesian(ylim = c(0,50))
plot1
  
df_plot2 <- df_full %>%
  filter(Content == "ELA", is.na(Actor) == FALSE) %>%
  group_by(Cycle, Actor) %>%
  mutate(Pct_SimWords = mean(SimWords)/mean(Words)*100)
  
plot2 <- df_plot2 %>%
  ggplot(
    aes(factor(Cycle), Pct_SimWords, fill = Actor)) + 
    geom_bar(stat="identity", position = "dodge") + 
    scale_fill_brewer(palette = "Set1") +
    geom_text(aes(label=round(Pct_SimWords, digits = 2)), position = position_dodge(width = 1)) +
    coord_cartesian(ylim = c(0, 50)) 
plot2

df_plot3 <- df_full %>%
  filter(Sim_Any == 1, Content == "ELA", is.na(Actor) == FALSE) %>%
  group_by(Cycle, Actor) %>%
  mutate(Pct_SimWords = mean(Words))

plot3 <- df_plot3 %>%
  ggplot(
    aes(factor(Cycle), Pct_SimWords, fill = Actor)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  geom_text(aes(label=round(Pct_SimWords, digits = 2)), position = position_dodge(width = 1)) +
  coord_cartesian(ylim = c(0, 50)) 
plot3



