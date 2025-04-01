## ---------------------------
##
## Calculate CATE & TEE score
##
## ---------------------------
## Notes: This file is used to calculate the CATE 
## and TEE scores using matched training data. 
## The same code may be used for low, medium and 
## high surgical volume hospitals.
## ---------------------------

setwd('./medium_hospitals')
train_treated = read_csv("./training/medium_hosp_train_trt_by_year.csv")
train_control = read_csv("./training/medium_hosp_train_cntrl_by_year.csv")

train_treated = train_treated %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55,
         chf = ifelse(nyha_1 == 0, 0, 1),
         numdisv_i = ifelse(numdisv_i == 3, 1, 0))

train_control = train_control %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55,
         chf = ifelse(nyha_1 == 0, 0, 1),
         numdisv_i = ifelse(numdisv_i == 3, 1, 0))

# Check balance
train = rbind(train_treated, train_control)

# START HERE LOAD MATCHED TRAINING DATA
train_treated$chf <- ifelse(train_treated$nyha_1==0, 0, 1)
train_control$chf <- ifelse(train_control$nyha_1==0, 0, 1)

# Outcome regression model
formula_mu_2 <- death_opmort ~ chf*numdisv_i + HF_below_55 +
  creatlst_i + left_main_50 + inotrop_48

# Fit GLM among treated units
or_treat <- glm(formula_mu_2, data = train_treated, 
                family = binomial(link = "logit"))
saveRDS(or_treat, file = "./code/or_treat_medium_hosp.rds")

# Fit GLM among control units
or_control <- glm(formula_mu_2, data = train_control, 
                  family = binomial(link = "logit"))
saveRDS(or_control, file = "./code/or_control_medium_hosp.rds")

# Write function to predict CATE based on glms fit on training data
cate_function <- function(newdata) {
  # E[Y(1) | X] = E[Y | A = 1, X]
  pred_mu_treat <- predict(or_treat, newdata = newdata, type = "response") 
  # E[Y(0) | X] = E[Y | A = 0, X]
  pred_mu_control <- predict(or_control, newdata = newdata, type = "response") 
  # E[Y(1) - Y(0) | X] =  E[Y | A = 1, X] - E[Y | A = 0, X]
  pred_cate <- pred_mu_treat - pred_mu_control
  return(pred_cate)
}

# Predict CATE on training data
cate_glm_train <- cate_function(rbind(train_treated, train_control))
saveRDS(cate_glm_train, file = "./code/cate_distribution_medium_hosp.rds")

# Make quantiles based on CATEs in the training set
quantiles_train_glm <- quantile(cate_glm_train, c(0, 0.2, 0.4, 0.6, 0.8, 1))

# Function to assign a score to CATE based on training data quantiles.
scoring_function_glm <- function(newdata) {
  preds_cate <-  cate_function(newdata = newdata)
  scores <- findInterval(preds_cate, quantiles_train_glm, all.inside = TRUE)
  return(scores)
}

# Assign scores to training data.
sts_train_m = rbind(train_treated, train_control)
sts_train_m$cate_score_glm =  scoring_function_glm(sts_train_m)
sts_train_m$raw_score = cate_function(sts_train_m)
sts_train_m = sts_train_m %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55)

# Look at mean death by TEE score and TEE receipt in training set.
sts_train_m %>% 
  group_by(cate_score_glm, tee) %>%
  summarise(count = n(),
            death_count = sum(death_opmort),
            mean_death = mean(death_opmort)) %>% 
  round(digits = 4) 

# Table 1 of training data by score

covars <- c('surgyear', 'chf',  "hdef_i", 'HF_below_55', 'HF_above_70', 'HF_normal',
            "creatlst_i", "left_main_50", "numdisv_i", "inotrop_48",
            "age", "gender_i", "white", "bmi_i",
            "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
            "chrlungd_i", "nyha_1", "cvd_p", "cva_p", "dialysis_pre",
            "liver_p", "osa_p", "pasys_i", "pvd_p", "prev_pci", "pci_6_hrs_less",
            "pci_stent", "pci_ind_surgery_1_num", "cathtosurg_i",
            "prev_cabg", "redo_srg", "rfhemoglobin_i", "platelets_i",
            "totalbumin_i", "inr_i", 
            "stemi_nstemi", "cardio_shock", 
            "ace_i_48", "predmort_cat",
            "op_status_i_num")

catVars <- c('surgyear', 'chf', 'HF_below_55', 'HF_above_70', 'HF_normal', "gender_i", "white",
             "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
             "chrlungd_i", "nyha_1", "cvd_p", "cva_p", "dialysis_pre",
             "liver_p", "osa_p", "pvd_p", "prev_pci", "pci_6_hrs_less",
             "pci_stent", "pci_ind_surgery_1_num",
             "prev_cabg", "redo_srg", "left_main_50",
             "stemi_nstemi", "cardio_shock", "numdisv_i",
             "ace_i_48", "inotrop_48",
             "op_status_i_num")

tb1_train_by_score <- tableone::CreateTableOne(vars = covars,
                                      factorVars = catVars,
                                      strata = 'cate_score_glm',
                                      data = sts_train_m,
                                      addOverall = TRUE,
                                      includeNA = TRUE)
