## ---------------------------
##
## Match training data
##
## ---------------------------
## Notes: This file includes code to perform matching 
## on the training set. Due to large sample size, matching 
## is performed within strata defined by year and STS 
## predicted mortality (pred_mort) quantile. The same code 
## may be used for low, medium and high surgical volume hospitals.
## ---------------------------

library(tidyverse)
library(match2C)

# Match training data -----------------------------------------------------
train <- read_csv("./large_hospitals/data/train.csv")

train$numdisv_i <- train$numdisv_i - 1 # recode

cut_predmort <- quantile(train$predmort_i)
train$predmort_cat <- as.numeric(cut(train$predmort_i, cut_predmort))

train <- train %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         mv_insuff_1_num = as.numeric(as.factor(mv_insuff_1)),
         tv_insuff_1_num = as.numeric(as.factor(tv_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

train <- train %>% 
  mutate(exposure = ifelse(tee == 0, 1, 0)) # Exposure is no TEE

# all covariates
covars_m <- c("age", "gender_i", "white", "bmi_i",
              "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
              "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
              "liver_p", "osa_p", "pasys_i", "pvd_p", 
              "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
              "cathtosurg_i", "rfhemoglobin_i", 
              "platelets_i", "totalbumin_i", "creatlst_i", "inr_i", 
              "left_main_50", "numdisv_i", "op_status_i_num", 
              "ace_i_48", "inotrop_48","hdef_i","av_insuff_1_num", 
              'dialysis_pre', 'redo_srg', "prev_cabg",
              "stemi_nstemi", "cardio_shock")

# selected covariates
covars_X <- c("age", "gender_i", "white", "bmi_i",
              "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
              "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
              "liver_p", "osa_p", "pasys_i", "pvd_p", 
              "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
              "cathtosurg_i", "rfhemoglobin_i", 
              "platelets_i", "totalbumin_i", "creatlst_i", "inr_i", 
              "left_main_50", "numdisv_i",
              "ace_i_48", "inotrop_48","hdef_i","av_insuff_1_num", 
              "op_status_i_num")

train <- train[complete.cases(train[,covars_m]),]

match_one_batch <- function(dataset_to_be_matched){
  
  X <- as.matrix(dataset_to_be_matched[,covars_X])
  
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
                                             caliper_low = .3, k = 300,
                                             penalty = 100)
  
  dist_list_right = create_list_from_scratch(Z = Z, X = propensity,
                                             p = propensity,
                                             caliper_low = 0.3,
                                             k = 300,
                                             penalty = 100,
                                             method = 'L1')
  
  dist_list_right_fb = create_list_from_scratch(Z = Z,
                                                X = X_all[, c('dialysis_pre', 
                                                          'redo_srg',
                                                          'nyha_1',
                                                          "stemi_nstemi",
                                                          "cardio_shock",
                                                          "prev_cabg")],
                                                p = propensity, 
                                                caliper_low = 0.3,
                                                k = 300,
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

# Match in batches
# Matches exactly on surgery year and predicted mortality quantile.
for (i in 2014:2014){
  for (j in 4:4){
    cat('Year', i, '\n')
    cat('Quantile', j, '\n')
    sts_train_sub = train %>%
      filter(surgyear == i,
             predmort_cat == j)
    cat('Total obs.', dim(sts_train_sub)[1], '\n')
    cat('Exposed', sum(sts_train_sub$exposure), '\n')
    res_sub = match_one_batch(sts_train_sub)
    train_treated = res_sub$tee
    train_control = res_sub$no_tee
    
    write.csv(train_treated, paste0('./large_hospitals/training/train_trt_', i, "_", j, '.csv'))
    write.csv(train_control, paste0('./large_hospitals/training/train_cntrl_', i, "_", j, '.csv')) 
  }
}

# Combine results
train_treated = NULL
train_control = NULL
for (i in 2014:2022){
  for (j in 1:4){
    train_treated = rbind(train_treated, 
                          read.csv(paste0('./large_hospitals/training/train_trt_', i, "_", j, '.csv')))
    train_control = rbind(train_control, 
                          read.csv(paste0('./large_hospitals/training/train_cntrl_', i, "_", j, '.csv')))
  }
}

write_csv(train_treated, './large_hospitals/training/large_hosp_train_trt_by_year.csv')
write_csv(train_control, './large_hospitals/training/large_hosp_train_cntrl_by_year.csv')

