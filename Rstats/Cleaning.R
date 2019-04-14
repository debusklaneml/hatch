#WMQ Clean UP
#Load Libraries
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggplot2)
library(tidylog)
library(vtable)
library(MplusAutomation)
library(readxl)
library(skimr)
setwd("~/Dropbox/Hatch/Data/Cleaning_WMQ")

#Bring in WMQ----
wmq.clean.1.1 <- read_excel("wmq.clean.1.1.xlsx")

wmq.clean.1.1 <- wmq.clean.1.1 %>%
  clean_names() %>%
  distinct(student_id, .keep_all = T) %>%
  mutate(student_id = as.numeric(student_id)) %>%
  mutate(id = row_number()) %>%
  filter(!is.na(student_id))

#Bring in demographics
demo <- read_csv("demo.csv") %>%
  clean_names() %>%
  mutate(student_id = as.numeric(student_id)) %>%
  select(-state_id, - suffix, -mid_init, -school, -state_school) %>%
  distinct()

#Bring in grades
sol.mp1 <- read_excel("wmp.sol.mp1.xlsx") %>%
  clean_names() %>%
  select(student_id, test_score, mark, course_description)

#Merge in demo
raw <- left_join(wmq.clean.1.1, demo) %>%
  distinct(id, .keep_all = T)
raw <- left_join(raw, sol.mp1) %>%
  distinct(id, .keep_all = T)

#Overview of raw data
# stats <- c('mean(x)', 'countNA(x)', 'min(x)', 'max(x)', 'sd(x)', 'var(x)')
# vtable(raw, missing = TRUE, out = 'browser', summ = stats)


#Clean it----
raw.1 <- raw %>%
  select(-student_id, -stu_name, -t_last, -mast1:-perfav4, -fbp1:-fbp2, -sr1:-sr11) %>%
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
  mutate(was10 = case_when(
    was10 == 1 ~ 5,
    was10 == 2 ~ 4,
    was10 == 3 ~ 3,
    was10 == 4 ~ 2,
    was10 == 5 ~ 1,
    TRUE ~ was10)) %>%
  select(-course_description)

prepareMplusData(raw.8.9.10, "raw.8.9.10.dat", inpfile = TRUE)
saveRDS(raw.8.9.10, "raw.8.9.10.rds")


#Descriptive Statistics----
# #Variables
vars <- fscores.cfa %>%
  skim_to_wide()
#Demo descriptives
fscores.cfa %>%
  tabyl(grade_level, minority)
fscores.cfa %>%
  tabyl(female, ethnic)
fscores.cfa %>%
  tabyl(grade_level, female)
fscores.cfa %>%
  tabyl(female)
fscores.cfa %>%
  tabyl(ethnic)
fscores.cfa %>%
  tabyl(grade_level)
fscores.cfa %>%
  tabyl(ell)
fscores.cfa %>%
  tabyl(grade_level, ell)
fscores.cfa %>%
  tabyl(gifted)
fscores.cfa %>%
  tabyl(disability)

raw.8.9.10 %>%
  tabyl(grade_level)



#Proportionalize----
#Import each dataset
fscores.cfa <- read_excel("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/2.CFA/fscores.cfa.xlsx")
fscores.esem <- read_excel("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM/fscores.esem.xlsx")
fscores.besem <- read_excel("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM/fscores.besem.xlsx")
fscores.besem.alt.1 <- read_excel("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM.alt/fscores.besem.alt.xlsx")
wses <- read_xlsx("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/8.WSES/fscores.wses.xlsx")
was12 <- read_xlsx("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/9.WAS12/fscores.was12.xlsx")

#Create Training and Test datasets for CFA
fscores.cfa <- fscores.cfa %>%
  select(id, everything()) %>%
  mutate(test_score = as.numeric(test_score)) %>%
  mutate(mark = as.numeric(mark)) %>%
  mutate(minority = case_when(
    ethnic == 2 | ethnic == 5 ~ 0,
    TRUE ~ 1)) %>%
  left_join(wses, by = "id") %>%
  left_join(was12, by = "id") %>%
  left_join(writing.sol, by = "id") %>%
  distinct()

#CFA Calibration
calib.cfa <- fscores.cfa %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  ungroup()
#CFA Validation
valid.cfa <- anti_join(fscores.cfa, calib.cfa, by = "id") %>%
  arrange(id)

prepareMplusData(calib.cfa, "calib.cfa.dat", inpfile = T)
prepareMplusData(valid.cfa, "valid.cfa.dat", inpfile = T)
prepareMplusData(fscores.cfa, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/CFA/fscores.cfa.dat", inpfile = T)

#Create Training and Test datasets for ESEM
fscores.esem <- fscores.esem %>%
  select(id, everything()) %>%
  mutate(test_score = as.numeric(test_score)) %>%
  mutate(mark = as.numeric(mark)) %>%
  mutate(minority = case_when(
    ethnic == 2 | ethnic == 5 ~ 0,
    TRUE ~ 1)) %>%
  left_join(wses, by = "id") %>%
  left_join(was12, by = "id") %>%
  left_join(writing.sol, by = "id") %>%
  distinct()
#ESEM Calibraton
calib.esem <- fscores.esem %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  ungroup()
#ESEM Validation
valid.esem <- anti_join(fscores.esem, calib.esem, by = "id") %>%
  arrange(id)

prepareMplusData(calib.esem, "calib.esem.dat", inpfile = T)
prepareMplusData(valid.esem, "valid.esem.dat", inpfile = T)
prepareMplusData(fscores.esem, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/ESEM/fscores.esem.dat", inpfile = T)

#Create Training and Test datasets for bESEM
fscores.besem <- fscores.besem %>%
  select(id, everything()) %>%
  mutate(test_score = as.numeric(test_score)) %>%
  mutate(mark = as.numeric(mark)) %>%
  filter(!glob == 999) %>%
  mutate(minority = case_when(
    ethnic == 2 | ethnic == 5 ~ 0,
    TRUE ~ 1)) %>%
  left_join(wses, by = "id") %>%
  left_join(was12, by = "id") %>%
  left_join(writing.sol, by = "id") %>%
  distinct()
#bESEM Calibration
calib.besem <- fscores.besem %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  arrange(id) %>%
  ungroup()
#bESEM Validation
valid.besem <- anti_join(fscores.besem, calib.besem, by = "id") %>%
  arrange(id)
prepareMplusData(calib.besem, "calib.besem.dat", inpfile = T)
prepareMplusData(valid.besem, "valid.besem.dat", inpfile = T)
prepareMplusData(fscores.besem, "fscores.besem.dat", inpfile = T )

#Create Datasets for bESEM.alt
fscores.besem.alt <- fscores.besem.alt.1 %>%
  select(id, everything()) %>%
  mutate(test_score = as.numeric(test_score)) %>%
  mutate(mark = as.numeric(mark)) %>%
  filter(!glob == 999) %>%
  mutate(minority = case_when(
    ethnic == 2 | ethnic == 5 ~ 0,
    TRUE ~ 1)) %>%
  left_join(wses, by = "id") %>%
  left_join(was12, by = "id") %>%
  left_join(writing.sol, by = "id") %>%
  # rename(perf_level = performance_level_total_mc_tei_sp,
  #        writing_tot = scaled_score_writing_total,
  #        writing_cat1 = scaled_score_reporting_category_1,
  #        writing_cat2 = scaled_score_reporting_category_2) %>%
  distinct()
#bESEM.alt Calibration
calib.besem.alt <- fscores.besem.alt %>%
  group_by(female, ell, grade_level) %>%
  sample_frac(.5) %>%
  arrange(id) %>%
  ungroup()
#bESEM Validation
valid.besem.alt <- anti_join(fscores.besem.alt, calib.besem.alt, by = "id") %>%
  arrange(id)

prepareMplusData(calib.besem.alt, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Calibrate/calib.besem.alt.dat", inpfile = T)
prepareMplusData(valid.besem.alt, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Validate/valid.besem.alt.dat", inpfile = T)
prepareMplusData(fscores.besem.alt, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM.alt/fscores.besem.alt.dat", inpfile = T )




#Morin's Similarity between Calibration and Validation datasets for bESEM----
#Bring together and identify Calib and Validation for multi-group LPA
besem.alt.mg.lpa <- bind_rows(calib.besem.alt, valid.besem.alt, .id = "i") %>%
  mutate(i = as.numeric(i))
prepareMplusData(besem.alt.mg.lpa, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Morin.Similarity/besem.alt.mg.lpa.dat", inpfile = T)
prepareMplusData(fscores.cfa, "fscores.cfa.dat", inpfile = T)

#Add in outcomes----
writing.sol <- read_excel("WRTG1819.xlsx")
writing.sol <- writing.sol %>%
  clean_names() %>%
  select(student_number:scaled_score_reporting_category_2, test_name) %>%
  rename(student_id = student_number)
id <- wmq.clean.1.1 %>%
  select(student_id, id)
writing.sol <- left_join(writing.sol, id) 
writing.sol <- writing.sol %>%
  filter(!is.na(id)) %>%
  filter(!scaled_score_writing_total > 900) %>%
  filter(!scaled_score_writing_total < 10) %>%
  select(-student_id) 
writing.sol <- writing.sol %>%
  mutate(total_8th = case_when(
    test_name == "Gr 8 Writing" ~ scaled_score_writing_total,
    TRUE ~ NA_real_),
    total_10th = case_when(
      test_name == "EOC Writing (10)" ~ scaled_score_writing_total,
      TRUE ~ NA_real_),
    cat1_8th = case_when(
      test_name == "Gr 8 Writing" ~ scaled_score_reporting_category_1,
      TRUE ~ NA_real_),
    cat1_10th = case_when(
      test_name == "EOC Writing (10)" ~scaled_score_reporting_category_1,
      TRUE ~ NA_real_),
    cat2_8th = case_when(
      test_name == "Gr 8 Writing" ~ scaled_score_reporting_category_2,
      TRUE ~ NA_real_),
    cat2_10th = case_when(
      test_name == "EOC Writing (10)" ~scaled_score_reporting_category_2,
      TRUE ~ NA_real_)) %>%
  select(-performance_level_total_mc_tei_sp, -scaled_score_writing_total, -scaled_score_reporting_category_1, 
         -scaled_score_reporting_category_2, -test_name)
    
writing.sol.raw <- read_excel("WRTG1819.xlsx")   
    
    
    

    
    
    
    
    
    
    
    
    