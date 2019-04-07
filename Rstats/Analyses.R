#Hatch - Analyses
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggplot2)
library(tidylog)
library(vtable)
library(MplusAutomation)

setwd("~/Dropbox/Hatch/Analyses")

#Reliability----
#Brind the rds files in
raw.8.9.10 <- readRDS("raw.8.9.10.rds")
#subsets
conventions <- raw.8.9.10 %>%
  select(se1, se3, se5)
idea <- raw.8.9.10 %>%
  select(se2, se6, se7) 
sr <- raw.8.9.10 %>%
  select(se4, se8, se9)


coefficientalpha::bootstrap(conventions, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(idea, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(sr, type = "omega", alpha = .95, nboot = 1000, ci = "bc")


#EFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/1.EFA", showOutput = TRUE)
EFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/1.EFA")
EFAsummary <- EFAoutput$summaries 
EFAsummary <- EFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi:num_factors) %>%
  unite(col = title, title, num_factors, sep = " ")
write_csv(EFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/1.EFA/EFAsummary.csv") 
#Need to combin num_factors + title

#CFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/2.CFA", showOutput = TRUE)
CFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/2.CFA")
CFAsummary <- CFAoutput$summaries
CFAsummary <- CFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(CFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/2.CFA/CFAsummary.csv")

#hCFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA", showOutput = TRUE)
hCFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA")
hCFAsummary <- hCFAoutput$summaries
hCFAsummary <- hCFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(hCFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA/hCFAsummary.csv")

#ESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM", showOutput = TRUE)
ESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM")
ESEMsummary <- ESEMoutput$summaries
ESEMsummary <- ESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(ESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM/ESEMsummary.csv")

#hESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM", showOutput = TRUE)
hESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM")
hESEMsummary <- hESEMoutput$summaries
hESEMsummary <- hESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(hESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM/hESEMsummary.csv")

#bCFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA", showOutput =TRUE)
bCFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA")
bCFAsummary <- bCFAoutput$summaries
bCFAsummary <- bCFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(bCFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA/bCFAsummary.csv")

#bESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM", showOutput = TRUE)
bESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM")
bESEMsummary <- bESEMoutput$summaries
bESEMsummary <- bESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(bESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM/bESEMsummary.csv")

#Compare Models----
Model.Comparisions.all <- bind_rows(EFAsummary, CFAsummary, hCFAsummary, bCFAsummary, ESEMsummary, hESEMsummary, bESEMsummary) %>%
  select(-aic, -bic, -a_bic)
write_csv(Model.Comparisions.all, "/Users/morganldebusk-lane/Dropbox/Hatch/Chapter 4/Model.Comparisons.all.csv")

#RQ1 & RQ2 Parameter Estimates----
CFApe <- CFAoutput$parameters$stdyx.standardized
write_csv(CFApe, "/Users/morganldebusk-lane/Dropbox/Hatch/Chapter 4/CFApe.csv")
bESEMpe <- bESEMoutput$parameters$stdyx.standardized
write_csv(bESEMpe, "/Users/morganldebusk-lane/Dropbox/Hatch/Chapter 4/bESEMpe.csv")

#CFA fs Enum----
createModels("iterate.calibrate.cfa.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Calibrate", showOutput = T)
calibrate.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Calibrate")
HTMLSummaryTable(calibrate.cfa.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                      "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Calibrate/calibrate.cfa.html")

# createModels("iterate.test.cfa.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Validate", showOutput = T)
# test.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Test")
# HTMLSummaryTable(test.cfa.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
#                                                 "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
#                  filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Test/test.cfa.html")
#ESEM fs Enum----
createModels("iterate.train.esem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Train", showOutput = T)
train.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Train")
HTMLSummaryTable(train.esem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                               "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Train/train.esem.html")




createModels("iterate.test.esem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Test", showOutput = T)
test.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Test")
HTMLSummaryTable(test.esem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                 "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Test/test.esem.html")





#bESEM fs Enum----
createModels("iterate.calibrate.besem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate", showOutput = T)
calib.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate")
HTMLSummaryTable(calib.besem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate/calib.besem.html")

# createModels("iterate.test.besem.txt")
# runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test", showOutput = T)
# test.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test")
# HTMLSummaryTable(test.besem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
#                                                   "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
#                  filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test/test.besem.html")



#Build dataframe with all fit----
calibrate.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Calibrate")
calib.cfa <- calibrate.cfa.output %>%
  map_df('summaries')
validate.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Validate")
valid.cfa <- validate.cfa.output %>%
  map_df('summaries')
# valid.cfa <- validate.cfa.output$summaries
# test.cfa <- test.cfa.output %>%
#   map_df('summaries') 
# cfa <- bind_rows(train.cfa, test.cfa)

# train.esem <- train.esem.output %>%
#   map_df('summaries')
# test.esem <- test.esem.output %>%
#   map_df('summaries')
# esem <- bind_rows(train.esem, test.esem)
calib.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate")
calib.besem <- calib.besem.output %>%
  map_df('summaries')
validate.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Validate")
valid.besem <- validate.besem.output %>%
  map_df('summaries')
# test.besem <- test.besem.output %>%
#   map_df('summaries')
# bsesm <- bind_rows(train.besem, test.besem)

#Full Models
cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/CFA")
besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/bESEM")
cfa.lpa <- cfa.output$summaries
besem.lpa <- besem.output$summaries
#bring all together
all_enum <- bind_rows(calib.cfa, valid.cfa, calib.besem, valid.besem) %>%
  clean_names() %>%
  select(-mplus_version, -analysis_type:-estimator, -n_groups:-n_categorical_latent_vars, -filename, -t11_km1starts:-t11_vlmr_param_diff,
         -t11_vlmr_sd, -t11_vlmr_mean:-t11_vlmr_p_value, -blrt_requested_draws:-blrt_successful_draws) %>%
  select(title, observations, parameters, ll, ll_correction_factor, aic, aicc, bic, a_bic, everything())
write_csv(all_enum, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/all_fit.csv")










