#Hatch - Analyses
library(tidyverse)
library(janitor)
library(ggthemes)
library(ggplot2)
library(tidylog)
library(vtable)
library(MplusAutomation)
library(readxl)

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
wses.bs <- raw.8.9.10 %>%
  select(wses1, wses2, wses3, wses4, wses5)
wses.as <- raw.8.9.10 %>%
  select(wses6, wses7, wses8, wses9, wses10)
was12.affect <- raw.8.9.10 %>%
  select(was1, was2, was3, was4, was5, was6)
was12.concern <- raw.8.9.10 %>%
  select(was7, was8, was9, was10, was11, was12)

         
         
coefficientalpha::bootstrap(conventions, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(idea, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(sr, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(wses.bs, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(wses.as, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(was12.affect, type = "omega", alpha = .95, nboot = 1000, ci = "bc")
coefficientalpha::bootstrap(was12.concern, type = "omega", alpha = .95, nboot = 1000, ci = "bc")




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

#ESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM", showOutput = TRUE)
ESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM")
ESEMsummary <- ESEMoutput$summaries
ESEMsummary <- ESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(ESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/3.ESEM/ESEMsummary.csv")


#hCFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA", showOutput = TRUE)
hCFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA")
hCFAsummary <- hCFAoutput$summaries
hCFAsummary <- hCFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(hCFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/4.hCFA/hCFAsummary.csv")
  





#bCFA Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA", showOutput =TRUE)
bCFAoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA")
bCFAsummary <- bCFAoutput$summaries
bCFAsummary <- bCFAsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(bCFAsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/5.bCFA/bCFAsummary.csv")


#hESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM", showOutput = TRUE)
hESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM")
hESEMsummary <- hESEMoutput$summaries
hESEMsummary <- hESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(hESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/6.hESEM/hESEMsummary.csv")








#bESEM Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM", showOutput = TRUE)
bESEMoutput <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM")
bESEMsummary <- bESEMoutput$summaries
bESEMsummary <- bESEMsummary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(bESEMsummary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM/bESEMsummary.csv")
#bESEM.alt Model----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM.alt", showOutput = TRUE)
bESEM.alt.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM.alt")
bESEM.alt.summary <- bESEM.alt.output$summaries
bESEM.alt.summary <- bESEM.alt.summary %>%
  clean_names() %>%
  select(title, parameters, chi_sq_m_value, chi_sq_m_df, cfi, tli, rmsea_estimate, rmsea_90ci_lb, rmsea_90ci_ub, rmsea_p_lt05, srmr)
write_csv(bESEM.alt.summary, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/7.bESEM.alt/bESEM.alt.summary.csv")



#Compare Models----
Model.Comparisions.all <- bind_rows(EFAsummary, CFAsummary, hCFAsummary, bCFAsummary, ESEMsummary, hESEMsummary, bESEM.alt.summary) %>%
  select(-aic, -bic, -a_bic)
write_csv(Model.Comparisions.all, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/Models/Model.Comparisons.csv")

#RQ1 & RQ2 Parameter Estimates----
CFApe <- CFAoutput$parameters$stdyx.standardized
write_csv(CFApe, "/Users/morganldebusk-lane/Dropbox/Hatch/Chapter 4/CFApe.csv")
ESEMpe <- ESEMoutput$parameters$stdyx.standardized
write_csv(ESEMpe, "/Users/morganldebusk-lane/Dropbox/Hatch/Chapter 4/ESEMpe.csv")
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
createModels("iterate.calibrate.esem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate", showOutput = T)
calibrate.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate")
HTMLSummaryTable(calibrate.esem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                               "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate/calib.esem.html")



# 
# createModels("iterate.calibrate.esem.txt")
# runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate", showOutput = T)
# test.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate")
# HTMLSummaryTable(test.esem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
#                                                  "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
#                  filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate/test.esem.html")





#bESEM fs Enum----
createModels("iterate.calibrate.besem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate", showOutput = T)
calibrate.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate")
HTMLSummaryTable(calibrate.besem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate/calib.besem.html")

# createModels("iterate.test.besem.txt")
# runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test", showOutput = T)
# test.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test")
# HTMLSummaryTable(test.besem.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
#                                                   "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
#                  filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Test/test.besem.html")





#bESEM.alt fs Enum----
createModels("iterate.calibrate.besem.alt.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Calibrate", showOutput = T)
calibrate.besem.alt.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Calibrate")
HTMLSummaryTable(calibrate.besem.alt.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                      "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Calibrate/calib.besem.alt.html")
#Build dataframe with all fit----
calibrate.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Calibrate")
calib.cfa <- calibrate.cfa.output %>%
  map_df('summaries')
validate.cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/CFA/Validate")
valid.cfa <- validate.cfa.output %>%
  map_df('summaries')
#valid.cfa <- validate.cfa.output$summaries

calibrate.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Calibrate")
calib.esem <- calibrate.esem.output %>%
  map_df('summaries')
validate.esem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/ESEM/Validate")
valid.esem <- validate.esem.output %>%
  map_df('summaries')
#bESEM
calibrate.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Calibrate")
calib.besem <- calibrate.besem.output %>%
  map_df('summaries')
validate.besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Validate")
valid.besem <- validate.besem.output %>%
  map_df('summaries')
#bESEM.alt
calibrate.besem.alt.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Calibrate")
calib.besem.alt <- calibrate.besem.alt.output %>%
  map_df('summaries')
validate.besem.alt.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Validate")
valid.besem.alt <- validate.besem.alt.output %>%
  map_df('summaries')





besem.full <- besem.full.output %>%
  map_df('summaries')

#Full Models
cfa.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/CFA")
besem.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/bESEM")
cfa.lpa <- cfa.output$summaries
besem.lpa <- besem.output$summaries

#bring all together
all_enum <- bind_rows(calib.cfa, valid.cfa, calib.esem, valid.esem, calib.besem.alt, valid.besem.alt ) %>%
  clean_names() %>%
  select(-mplus_version, -analysis_type:-estimator, -n_groups:-n_categorical_latent_vars, -filename, -t11_km1starts:-t11_vlmr_param_diff,
         -t11_vlmr_sd, -t11_vlmr_mean:-t11_vlmr_p_value, -blrt_requested_draws:-blrt_successful_draws) %>%
  select(title, observations, parameters, ll, ll_correction_factor, aic, aicc, bic, a_bic, everything())
write_csv(all_enum, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/all_fit.csv")













#Full bESEM LPA Enum----
createModels("iterate.besem.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM", showOutput = T)
besem.full.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM")
HTMLSummaryTable(besem.full.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                     "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM/besem.full.html")

#Full bESEM Alt Enum----
createModels("iterate.besem.alt.txt")
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM.alt", showOutput = T)
besem.alt.full.output <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM.alt")
HTMLSummaryTable(besem.alt.full.output, keepCols = c("Title", "LL", "Observations", "Parameters", "AIC", "BIC", "aBIC", "BLRT_2xLLDiff", "BLRT_PValue",
                                                 "T11_VLMR_2xLLDiff", "T11_VLMR_PValue", "Entropy"), sortBy = "Parameters", 
                 filename = "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM.alt/besem.alt.full.html")
#bESEM Similarity----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Morin.Similarity")
besem.similarity <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Morin.Similarity")
besem.sim <- besem.similarity %>%
  map_df('summaries') %>%
  clean_names() %>%
  select(title, observations, parameters, ll, ll_correction_factor, aic, aicc, bic, a_bic, entropy)
write_csv(besem.sim, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM/Morin.Similarity/besem.sim.csv")

          


#bESEM.alt Similarity profile----
runModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Morin.Similarity")
besem.alt.similarity <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Morin.Similarity")
besem.alt.sim <- besem.alt.similarity %>%
  map_df('summaries') %>%
  clean_names() %>%
  select(title, observations, parameters, ll, ll_correction_factor, aic, aicc, bic, a_bic, entropy)
write_csv(besem.alt.sim, "/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Enumeration/bESEM.alt/Morin.Similarity/besem.alt.sim.csv")

#bESEM.alt Predictors----
#bring in data
cprobs <- read_excel("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Predictors/cprobs.besem.alt.xlsx") %>% 
  na_if("*")
#Determine how many are of each modal class assignement
# c = 1 
cprobs.2 <- cprobs %>% 
  filter(c == 3)
skimr::skim(cprobs.2)  

cprobs %>%
  tabyl(grade_level, c)
  
  
  
  
  
  
  








#bESEM.alt 3 Profile - Figure----
bESEM.full.LPA.3p <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Predictors")
bESEM.c3 <- bESEM.full.LPA.3p$parameters$unstandardized
bESEM.c3.figure <- bESEM.c3 %>% 
  filter(paramHeader == "Means") %>% 
  filter(LatentClass != "Categorical.Latent.Variables") %>% 
  ggplot(aes(LatentClass, est, fill = param)) +
  geom_col(position = "dodge") +
  labs(y = "Scores",
       x = "Latent Class") +
  expand_limits(y = c(-1, 1))
bESEM.c3.figure + theme_apa()

test.figure <- bESEM.c3 %>% 
  clean_names() %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") 
test.figure <- test.figure %>%
  ggplot() + geom_col(aes(latent_class, est, fill = param), position = "dodge") +
  #scale_fill_manual(values = c(rep(c("gray45", "gray55", "gray65", "gray75"))))
  scale_fill_grey() + 
  labs(y = "Average Factor Score",
       x = "Profile") + 
  expand_limits(y = c(-1, 1))
test.figure + theme_apa()

bESEM.c3.tab <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/bESEM.alt")
bESEM.c3.tab.1 <- bESEM.c3.tab$X3.besem.alt.LPA.out$parameters$unstandardized
bESEM.c3.table11 <- bESEM.c3.tab.1 %>% 
  clean_names() %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") %>%
  select(-param_header, -pval, -est_se) 
write_csv(bESEM.c3.table11, "bESEM.c3.csv")




#cfa 4 Profile - Figure----
cfa.full <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/CFA")
cfa.full.c4 <- cfa.full$parameters$unstandardized
cfa.figure <- cfa.full.c4 %>%
  clean_names() %>%
  mutate(latent_class = fct_reorder(latent_class, est)) %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") %>%
  ggplot() + geom_col(aes(latent_class, est, fill = param), position = "dodge") +
  #scale_fill_manual(values = c(rep(c("gray45", "gray55", "gray65", "gray75"))))
  scale_fill_grey() + 
  labs(y = "Average Factor Score",
       x = "Profile") + 
  expand_limits(y = c(-1, 1))
cfa.figure + theme_apa()

cfa.tab <- cfa.full.c4 %>%
  clean_names() %>%
  mutate(latent_class = fct_reorder(latent_class, est)) %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") %>%
  select(-param_header, -pval, -est_se) 
write_csv(cfa.tab, "cfa.table.csv")

#ESEM 4 Profile - Figure----
esem.full <- readModels("/Users/morganldebusk-lane/Dropbox/Hatch/Analyses/LPA/Full/ESEM")
esem.full.c4 <- esem.full$parameters$unstandardized
esem.figure <- esem.full.c4 %>%
  clean_names() %>%
  mutate(latent_class = fct_reorder(latent_class, est)) %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") %>%
  ggplot() + geom_col(aes(latent_class, est, fill = param), position = "dodge") +
  #scale_fill_manual(values = c(rep(c("gray45", "gray55", "gray65", "gray75"))))
  scale_fill_grey() + 
  labs(y = "Average Factor Score",
       x = "Profile") + 
  expand_limits(y = c(-1, 1))
esem.figure + theme_apa()

esem.tab <- esem.full.c4 %>%
  clean_names() %>%
  mutate(latent_class = fct_reorder(latent_class, est)) %>%
  filter(param_header == "Means") %>% 
  filter(latent_class != "Categorical.Latent.Variables") %>%
  select(-param_header, -pval, -est_se) 
write_csv(esem.tab, "esem.table.csv")





