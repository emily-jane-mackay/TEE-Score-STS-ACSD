## ---------------------------
##
## Match & analyse test data
##
## ---------------------------
## Notes: This file performs matching of the 
## testing data as well as McNemar tests for 
## differences by TEE status. The same code 
## may be used for low, medium and high surgical 
## volume hosoitals.
## ---------------------------

library(dplyr)
library(match2C)
setwd('./medium_hospitals')

test = read.csv('./data/test.csv')

test$numdisv_i <- test$numdisv_i - 1 # recode

test = test %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55,
         chf = ifelse(nyha_1 == 0, 0, 1),
         numdisv_i = ifelse(numdisv_i == 3, 1, 0))


test$predmort_i[is.na(test$predmort_i)] = median(test$predmort_i, na.rm = TRUE)
cut_predmort <- quantile(test$predmort_i)
test$predmort_cat <- as.numeric(cut(test$predmort_i, cut_predmort))

test <- test %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         mv_insuff_1_num = as.numeric(as.factor(mv_insuff_1)),
         tv_insuff_1_num = as.numeric(as.factor(tv_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

# Assign scores to testing data.
fit_treated <- readRDS(file = "./code/or_treat_medium_hosp.rds",.GlobalEnv)
fit_control <- readRDS(file = "./code/or_control_medium_hosp.rds",.GlobalEnv)
cate_glm_train = readRDS(file = './code/cate_distribution_medium_hosp.rds', .GlobalEnv)

cate_function <- function(newdata) {
  # E[Y(1) | X] = E[Y | A = 1, X]
  pred_mu_treat <- predict(fit_treated, newdata = newdata, type = "response") 
  # E[Y(0) | X] = E[Y | A = 0, X]
  pred_mu_control <- predict(fit_control, newdata = newdata, type = "response") 
  # E[Y(1) - Y(0) | X] =  E[Y | A = 1, X] - E[Y | A = 0, X]
  pred_cate <- pred_mu_treat - pred_mu_control
  return(pred_cate)
}

quantiles_train_glm <- quantile(cate_glm_train, c(0, 0.2, 0.4, 0.6, 0.8, 1))

scoring_function_glm <- function(newdata) {
  preds_cate <-  cate_function(newdata = newdata)
  scores <- findInterval(preds_cate, quantiles_train_glm, all.inside = TRUE)
  return(scores)
}

test$cate_score_glm =  scoring_function_glm(test)

test <- test %>% 
  mutate(exposure = ifelse(tee == 0, 1, 0))

check_all_col <- function(X){
  drop_id = which(apply(X, 2, function(col)  all(col == col[1])) == TRUE)
  if (length(drop_id) == 0)
    return(X)
  return(X[,-drop_id])
}

match_on_test <- function(dataset_to_be_matched){
  
  cat(nrow(dataset_to_be_matched), '\n')
  X <- as.matrix(dataset_to_be_matched[,covars_X])
  X = check_all_col(X)
  
  
  X_all = as.matrix(dataset_to_be_matched[,covars_m])
  
  # ****Exposure is no TEE****
  Z <- dataset_to_be_matched$exposure
  
  # Fit a propensity score model
  propensity = glm(exposure ~ age + gender_i + white + bmi_i +
                     admitsrc_i + arrhythmia_p + afib_parox + afib_persist + 
                     chrlungd_i + nyha_1 + cvd_p + cva_p +  
                     liver_p + osa_p + pasys_i + pvd_p + prev_pci + 
                     pci_6_hrs_less + pci_stent + pci_ind_surgery_1_num + 
                     cathtosurg_i + prev_cabg + 
                     rfhemoglobin_i + platelets_i +
                     totalbumin_i + creatlst_i + inr_i + 
                     left_main_50 + stemi_nstemi + cardio_shock + 
                     op_status_i_num + numdisv_i + ace_i_48 + inotrop_48 + 
                     hdef_i + av_insuff_1_num,
                   family = binomial,
                   data = dataset_to_be_matched)$fitted.values
  
  dataset_to_be_matched$propensity <- propensity
  
  
  # Create use-specified distance lists for left and right networks
  
  dist_list_left <- create_list_from_scratch(Z, X, p = propensity, 
                                             caliper_low = .5, k = 500,
                                             penalty = 100)
  
  dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                             p = propensity,
                                             caliper_low = 0.5,
                                             k = 500,
                                             penalty = 100,
                                             method = 'L1')
  
  dist_list_right_fb = create_list_from_scratch(Z = Z,
                                                X = X_all[, c('nyha_1',
                                                              "inotrop_48",
                                                              "left_main_50",
                                                              "numdisv_i")],
                                                p = propensity, 
                                                caliper_low = 0.5,
                                                k = 500,
                                                penalty = 100,
                                                method = 'L1') #L1 is better for ordinal data
  
  dist_list_right$d = 100*(1e2*dist_list_right$d) + 1e2*dist_list_right_fb$d
  
  # Perform match
  
  start_time <- Sys.time()
  matching_output_manual = match_2C_list(Z = Z, dataset = dataset_to_be_matched,
                                         dist_list_1 = dist_list_left,
                                         dist_list_2 = dist_list_right,
                                         lambda = 10,
                                         controls = 1)
  end_time <- Sys.time()
  cat(end_time - start_time, '\n')
  
  
  dt_matched <- matching_output_manual$matched_data_in_order %>% filter(!is.na(matched_set))
  dt_tee <- dt_matched %>% filter(tee == 1)
  dt_notee <- dt_matched %>% filter(tee == 0)
  
  return(list(tee = dt_tee,
              no_tee = dt_notee))
}


covars_m <- c("age", "gender_i", "white", "bmi_i",
              "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
              "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
              "liver_p", "osa_p", "pasys_i", "pvd_p", 
              "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
              "cathtosurg_i", "rfhemoglobin_i", 
              "platelets_i", "totalbumin_i", "creatlst_i", "inr_i", 
              "left_main_50", "stemi_nstemi", "cardio_shock", "numdisv_i",
              "ace_i_48", "inotrop_48","hdef_i","av_insuff_1_num", 
              "op_status_i_num", 'dialysis_pre', 'redo_srg', 
              "stemi_nstemi", "cardio_shock", "prev_cabg", 'predmort_cat')

covars_X <- c("age", "gender_i", "white", "bmi_i",
              "admitsrc_i", "arrhythmia_p", "afib_parox", 
              "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
              "liver_p", "osa_p", "pasys_i", "pvd_p", 
              "prev_pci", "pci_stent", "pci_ind_surgery_1_num", 
              "cathtosurg_i", "rfhemoglobin_i", 
              "platelets_i", "totalbumin_i", "creatlst_i", "inr_i", 
              
              "ace_i_48", "hdef_i","av_insuff_1_num", 
              "op_status_i_num", "pci_6_hrs_less",
              'redo_srg',
              "stemi_nstemi",
              "cardio_shock",
              "prev_cabg",
              "afib_persist", 'dialysis_pre')

test <- test[complete.cases(test[,covars_m]),]


for (i in 2015:2022){
  for (j in 1:4){
    cat(i, '\n')
    test_samp <- test %>% filter(surgyear == i,
                                 predmort_cat == j)
    test_treated = NULL
    test_control = NULL
    
    # Match w/in recommendation score
    cat('1', '\n')
    res_sub_test_1 = test_samp %>% filter(cate_score_glm == 1) %>% match_on_test
    cat('2', '\n')
    res_sub_test_2 = test_samp %>% filter(cate_score_glm == 2) %>% match_on_test
    cat('3', '\n')
    res_sub_test_3 = test_samp %>% filter(cate_score_glm == 3) %>% match_on_test
    cat('4', '\n')
    res_sub_test_4 = test_samp %>% filter(cate_score_glm == 4) %>% match_on_test
    cat('5', '\n')
    res_sub_test_5 = test_samp %>% filter(cate_score_glm == 5) %>% match_on_test
    
    test_treated = rbind(test_treated, res_sub_test_1$tee, res_sub_test_2$tee, 
                         res_sub_test_3$tee, res_sub_test_4$tee,
                         res_sub_test_5$tee)
    test_control = rbind(test_control, res_sub_test_1$no_tee, res_sub_test_2$no_tee,
                         res_sub_test_3$no_tee, res_sub_test_4$no_tee,
                         res_sub_test_5$no_tee)
    
    write.csv(test_treated, paste0('./testing/test_trt_', i, "_", j, '.csv'))
    write.csv(test_control, paste0('./testing/test_cntrl_', i, "_", j, '.csv'))
  }
}



# Analyze matched test data -----------------------------------------------

setwd("./medium_hospitals")

# Load matched testing data
test_treated = read.csv('./testing/test_trt_2014.csv')
test_control = read.csv('./testing/test_cntrl_2014.csv')
for (yr in seq(2014, 2022, 1)){
  for (j in 1:4){
    test_treated = rbind(test_treated, 
                         read.csv(paste0('./testing/test_trt_', yr, "_", j, '.csv')))
    test_control = rbind(test_control, 
                         read.csv(paste0('./testing/test_cntrl_', yr, "_", j, '.csv')))
  }
  
}

# Summarize results
sts_test_m = rbind(test_treated, test_control)
sts_test_m %>% 
  group_by(cate_score_glm, tee) %>%
  summarise(count = n(),
            death_count = sum(death_opmort),
            mean_death = mean(death_opmort)) %>% round(digits = 4) 

# Report test results
for (s in 1:5){
  cat(s, '\n')
  sts_test_t = sts_test_m %>%
    filter(cate_score_glm == s,
           tee == 1)
  
  sts_test_c = sts_test_m %>%
    filter(cate_score_glm == s,
           tee == 0)
  
  t_test_res = t.test(sts_test_t$death_opmort - sts_test_c$death_opmort)
  cat(c('Risk Difference', round(100*t_test_res$estimate, 3), 
        round(100*t_test_res$conf.int, 3), 
        round(t_test_res$p.value, 3), '\n'))
  
  mcnemar_res = mcnemar_test(sts_test_t, sts_test_c)
  cat(c('Odds Ratio', round(mcnemar_res$estimate, 3), 
        round(mcnemar_res$conf.int, 3),
        round(mcnemar_res$p.value, 3), '\n'))
  
}

sts_test_m = sts_test_m %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55)

# covars <- c('chf',  "hdef_i", 'HF_below_55', 'HF_above_70', 'HF_normal',
#             "creatlst_i", "left_main_50", "numdisv_i", "inotrop_48",
#             "age", "gender_i", "white", "bmi_i",
#             "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
#             "chrlungd_i", "nyha_1", "cvd_p", "cva_p", "dialysis_pre",
#             "liver_p", "osa_p", "pasys_i", "pvd_p", "prev_pci", "pci_6_hrs_less",
#             "pci_stent", "pci_ind_surgery_1_num", "cathtosurg_i",
#             "prev_cabg", "redo_srg", "rfhemoglobin_i", "platelets_i",
#             "totalbumin_i", "inr_i", 
#             "stemi_nstemi", "cardio_shock", 
#             "ace_i_48", "predmort_cat",
#             "op_status_i_num")
# 
# catVars <- c('chf', 'HF_below_55', 'HF_above_70', 'HF_normal', "gender_i", "white",
#              "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
#              "chrlungd_i", "nyha_1", "cvd_p", "cva_p", "dialysis_pre",
#              "liver_p", "osa_p", "pvd_p", "prev_pci", "pci_6_hrs_less",
#              "pci_stent", "pci_ind_surgery_1_num",
#              "prev_cabg", "redo_srg", "left_main_50",
#              "stemi_nstemi", "cardio_shock", "numdisv_i",
#              "ace_i_48", "inotrop_48",
#              "op_status_i_num")

covars <- c('chf',  "hdef_i", 'HF_below_55',
            "creatlst_i", "left_main_50", "numdisv_i", "inotrop_48",
            "predmort_i")

catVars <- c('chf', 'HF_below_55', "left_main_50", "numdisv_i", "inotrop_48")

# Table 1 for each subgroup match
sts_test_m1 = sts_test_m %>%
  filter(cate_score_glm == 2)

tb1_test1 <- tableone::CreateTableOne(vars = covars,
                                      factorVars = catVars,
                                      strata = 'tee',
                                      data = sts_test_m1,
                                      test = F,
                                      addOverall = TRUE,
                                      includeNA = TRUE)

# By subgroup
tableone::CreateTableOne(vars = covars,
                         factorVars = catVars,
                         strata = 'cate_score_glm',
                         data = sts_test_m,
                         test = F,
                         addOverall = TRUE,
                         includeNA = TRUE)


###############################################################
# McNemar Test
###############################################################
library(exact2x2)
mcnemar_test <- function(dt_treated, dt_control){
  n = dim(dt_treated)[1]
  pp=0
  pn=0
  np=0
  nn=0
  for(i in seq(1,n,1))
  {
    if(dt_treated$death_opmort[i]==1 & dt_control$death_opmort[i]==1){pp = pp+1}
    else if(dt_treated$death_opmort[i]==1 & dt_control$death_opmort[i]==0){pn = pn+1}
    else if(dt_treated$death_opmort[i]==0 & dt_control$death_opmort[i]==1){np = np+1}
    else if(dt_treated$death_opmort[i]==0 & dt_control$death_opmort[i]==0){nn = nn+1}
  }
  contingency_mtx <- matrix(c(pp,pn,np,nn),ncol = 2,byrow = T, dimnames = list(c("t_1","t_0"),c("c_1","c_0")))
  return(mcnemar.exact(contingency_mtx))
}

# Get overall p-values for revisions 01/26/25
all_notee = all %>% filter(tee == 0)
all_tee = all %>% filter(tee == 1)

mcnemar_test(all_tee, all_notee)

