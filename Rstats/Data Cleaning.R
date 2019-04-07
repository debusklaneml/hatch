#WMQ Clean UP
#Load Libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggplot2)
library(tidylog)
library(vtable)
library(MplusAutomation)
setwd("~/Dropbox/Hatch/Data/Cleaning_WMQ")



#Bring in WMQ----
wmq.clean.1.1 <- read_csv("wmq.clean.1.1.csv") %>%
  clean_names() %>%
  mutate(student_id = as.numeric(student_id)) %>%
  distinct()# %>%
  #drop_na()

#Bring in demographics
demo <- read_csv("demo.csv") %>%
  clean_names() %>%
  mutate(student_id = as.numeric(student_id)) %>%
  select(-state_id, - suffix, -mid_init, -school, -state_school) %>%
  distinct()

#Merge in demo
raw <- left_join(wmq.clean.1.1, demo) 
#Overview of raw data
# stats <- c('mean(x)', 'countNA(x)', 'min(x)', 'max(x)', 'sd(x)', 'var(x)')
# vtable(raw, missing = TRUE, out = 'browser', summ = stats)

#Clean it----
raw.1 <- raw %>%
  select(-student_id, -stu_name, -t_last, -mast1:-perfav4, -fbp1:-fbp2, -sr1:-sr11) %>%
  mutate(id = rownames(raw.1)) %>%
  select(id, school_year:ccps_gate, everything(), -last_name, -first_name, -grade, -school_year) %>%
  mutate(ethnic.1 = case_when(
    hispanic_latino == "Y" ~ 4,
    ethnic == "01" ~ 1,
    ethnic == "02" ~ 2,
    ethnic == "03" ~ 3,
    ethnic == "05" ~ 5,
    ethnic == "06" ~ 6,
    ethnic > "06" ~ 7,
    TRUE ~ 9999)) %>% #There is nothing else, so 9999 doesnt matter.
  select(id, sex, ethnic.1, everything(), -ethnic, -hispanic_latino) %>%
  rename(ethnic = ethnic.1) %>%
  mutate(ell = case_when(
    ell_status == "Y" ~ 1,
    TRUE ~ 0)) %>%
  select(id, sex, ethnic, ell, everything(), -ell_status) %>%
  mutate(disability = case_when(
    disability_code > 0 ~ 1,
    TRUE ~ 0)) %>%
  select(id, sex, ethnic, ell, disability, t_id, everything(), -disability_code) %>%
  mutate(gifted = case_when(
    is.na(ccps_gate) ~ 0,
    TRUE ~ 1)) %>%
  select(id, sex, ethnic, ell, disability, t_id, gifted, everything(), -ccps_gate) %>%
  mutate(female = case_when(
    sex == "F" ~ 1, 
    TRUE ~ 0)) %>%
  select(id, female, everything(), -sex)
  
raw.8.9.10 <- raw.1 %>%
  filter(grade_level > 7) %>%
  mutate(id = as.numeric(id))

# #Descriptive Statistics
# #Variables
# vars <- raw.8.9.10 %>%
#   skim_to_wide() 
# #Demo descriptives
# raw.8.9.10 %>%
#   tabyl(grade_level, ethnic)
# raw.8.9.10 %>%
#   tabyl(female, ethnic)
# raw.8.9.10 %>%
#   tabyl(grade_level, female)
# raw.8.9.10 %>%
#   tabyl(female)
# raw.8.9.10 %>%
#   tabyl(ethnic)
# raw.8.9.10 %>%
#   tabyl(grade_level)
# raw.8.9.10 %>%
#   tabyl(ell)
# raw.8.9.10 %>%
#   tabyl(grade_level, ell)
# raw.8.9.10 %>%
#   tabyl(gifted)
# raw.8.9.10 %>%
#   tabyl(disability)

prepareMplusData(raw.8.9.10, "raw.8.9.10.dat", inpfile = TRUE)
saveRDS(raw.8.9.10, "raw.8.9.10.rds")

#Proportionalize----
#Import each dataset
fscores.cfa <- read_csv("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/2.CFA/fscores.cfa.csv")
# fscores.esem <- read_csv("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM/fscores.esem.csv")
fscores.besem <- read_csv("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM/fscores.besem.csv")

#Create Training and Test datasets for CFA
fscores.cfa <- fscores.cfa %>%
  mutate(n = row_number()) %>%
  select(n, everything())

calib.cfa <- fscores.cfa %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  ungroup()

valid.cfa <- anti_join(fscores.cfa, calib.cfa, by = "n") %>%
  arrange(n)

prepareMplusData(calib.cfa, "calib.cfa.dat", inpfile = T)
prepareMplusData(valid.cfa, "valid.cfa.dat", inpfile = T)
prepareMplusData(fscores.cfa, "fscores.cfa.dat", inpfile = T)

# #Create Training and Test datasets for ESEM
# fscores.esem <- fscores.esem %>%
#   mutate(n = row_number()) %>%
#   select(n, everything())
# train.esem <- fscores.esem %>%
#   group_by(female, ethnic, ell, disability, gifted, grade_level, t_id) %>%
#   sample_frac(.55) %>%
#   arrange(n)
# test.esem <- anti_join(fscores.esem, train.esem) %>%
#   arrange(n)
# prepareMplusData(train.esem, "train.esem.dat", inpfile = T)
# prepareMplusData(test.esem, "test.esem.dat", inpfile = T)

#Create Training and Test datasets for bESEM
fscores.besem <- fscores.besem %>%
  mutate(n = row_number()) %>%
  select(n, everything()) %>%
  filter(!glob == 999)
calib.besem <- fscores.besem %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  arrange(n)
valid.besem <- anti_join(fscores.besem, calib.besem, by = "n") %>%
  arrange(n)
prepareMplusData(calib.besem, "calib.besem.dat", inpfile = T)
prepareMplusData(valid.besem, "valid.besem.dat", inpfile = T)
prepareMplusData(fscores.besem, "fscores.besem.dat", inpfile = T )


#Random split by particular variables. 
# hatch.8.9.10 <- raw.8.9.10 %>%
#   mutate(n = row_number()) %>%
#   select(n, everything())
# train <- hatch.8.9.10 %>%
#   group_by(female, ethnic, ell, disability, t_id, gifted, grade_level) %>%
#   sample_frac(.55) %>%
#   arrange(n)
# 
# #Mplus Training dataset generation
# prepareMplusData(train, "train.dat", inpfile = TRUE)
# 
# #ensure proportions mimic full sample
# hatch.8.9.10 %>%
#   tabyl(female)
# train %>%
#   tabyl(female)
# hatch.8.9.10 %>%
#   tabyl(gifted) 
# train %>%
#   tabyl(gifted)
# 
# #Recover other proportion apart from the 'training' dataset
# test <- anti_join(hatch.8.9.10, train) %>%
#   arrange(n)
# 
# #Mplus Test dataset generation
# prepareMplusData(test, "test.dat", inpfile = TRUE)



















