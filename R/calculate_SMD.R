# Calculate SMD

setwd('./small_hospitals')
test <- read_csv('./sts_small_train_prematch.csv')
matched_treated <- read_csv('./training/small_hosp_train_trt_by_year.csv')
matched_control <- read_csv('./training/small_hosp_train_cntrl_by_year.csv')

########### Make binary vars for categorical variables
matched_treated <- matched_treated %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

matched_treated <- matched_treated %>% 
  mutate(admit_elective = ifelse(admitsrc_i == 1, 1, 0),
         admit_emergency = ifelse(admitsrc_i == 2, 1, 0),
         admit_transfer = ifelse(admitsrc_i == 3, 1, 0),
         admit_other = ifelse(admitsrc_i == 4, 1, 0),
         lung_none = ifelse(chrlungd_i == 1, 1, 0),
         lung_mild = ifelse(chrlungd_i == 2, 1, 0),
         lung_moderate = ifelse(chrlungd_i == 3, 1, 0),
         lung_severe = ifelse(chrlungd_i == 4, 1, 0),
         lung_unknown = ifelse(chrlungd_i == 5, 1, 0),
         pci_none = ifelse(pci_ind_surgery_1_num == 3, 1, 0),
         pci_deter = ifelse(pci_ind_surgery_1_num == 1, 1, 0),
         pci_nodeter = ifelse(pci_ind_surgery_1_num == 2, 1, 0),
         pci_wostemi = ifelse(pci_ind_surgery_1_num == 5, 1, 0),
         pci_stemi = ifelse(pci_ind_surgery_1_num == 6, 1, 0),
         pci_other = ifelse(pci_ind_surgery_1_num == 4, 1, 0),
         av_none = ifelse(av_insuff_1_num == 1, 1, 0),
         av_trace = ifelse(av_insuff_1_num == 5, 1, 0),
         av_mild = ifelse(av_insuff_1_num == 2, 1, 0),
         av_moderate = ifelse(av_insuff_1_num == 3, 1, 0),
         av_severe = ifelse(av_insuff_1_num == 4, 1, 0),
         op_elective = ifelse(op_status_i_num == 1, 1, 0),
         op_emergent = ifelse(op_status_i_num == 2, 1, 0),
         op_urgent = ifelse(op_status_i_num == 3, 1, 0))

matched_control <- matched_control %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

matched_control <- matched_control %>% 
  mutate(admit_elective = ifelse(admitsrc_i == 1, 1, 0),
         admit_emergency = ifelse(admitsrc_i == 2, 1, 0),
         admit_transfer = ifelse(admitsrc_i == 3, 1, 0),
         admit_other = ifelse(admitsrc_i == 4, 1, 0),
         lung_none = ifelse(chrlungd_i == 1, 1, 0),
         lung_mild = ifelse(chrlungd_i == 2, 1, 0),
         lung_moderate = ifelse(chrlungd_i == 3, 1, 0),
         lung_severe = ifelse(chrlungd_i == 4, 1, 0),
         lung_unknown = ifelse(chrlungd_i == 5, 1, 0),
         pci_none = ifelse(pci_ind_surgery_1_num == 3, 1, 0),
         pci_deter = ifelse(pci_ind_surgery_1_num == 1, 1, 0),
         pci_nodeter = ifelse(pci_ind_surgery_1_num == 2, 1, 0),
         pci_wostemi = ifelse(pci_ind_surgery_1_num == 5, 1, 0),
         pci_stemi = ifelse(pci_ind_surgery_1_num == 6, 1, 0),
         pci_other = ifelse(pci_ind_surgery_1_num == 4, 1, 0),
         av_none = ifelse(av_insuff_1_num == 1, 1, 0),
         av_trace = ifelse(av_insuff_1_num == 5, 1, 0),
         av_mild = ifelse(av_insuff_1_num == 2, 1, 0),
         av_moderate = ifelse(av_insuff_1_num == 3, 1, 0),
         av_severe = ifelse(av_insuff_1_num == 4, 1, 0),
         op_elective = ifelse(op_status_i_num == 1, 1, 0),
         op_emergent = ifelse(op_status_i_num == 2, 1, 0),
         op_urgent = ifelse(op_status_i_num == 3, 1, 0))

pre_match <- pre_match %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

pre_match <- pre_match %>% 
  mutate(admit_elective = ifelse(admitsrc_i == 1, 1, 0),
         admit_emergency = ifelse(admitsrc_i == 2, 1, 0),
         admit_transfer = ifelse(admitsrc_i == 3, 1, 0),
         admit_other = ifelse(admitsrc_i == 4, 1, 0),
         lung_none = ifelse(chrlungd_i == 1, 1, 0),
         lung_mild = ifelse(chrlungd_i == 2, 1, 0),
         lung_moderate = ifelse(chrlungd_i == 3, 1, 0),
         lung_severe = ifelse(chrlungd_i == 4, 1, 0),
         lung_unknown = ifelse(chrlungd_i == 5, 1, 0),
         pci_none = ifelse(pci_ind_surgery_1_num == 3, 1, 0),
         pci_deter = ifelse(pci_ind_surgery_1_num == 1, 1, 0),
         pci_nodeter = ifelse(pci_ind_surgery_1_num == 2, 1, 0),
         pci_wostemi = ifelse(pci_ind_surgery_1_num == 5, 1, 0),
         pci_stemi = ifelse(pci_ind_surgery_1_num == 6, 1, 0),
         pci_other = ifelse(pci_ind_surgery_1_num == 4, 1, 0),
         av_none = ifelse(av_insuff_1_num == 1, 1, 0),
         av_trace = ifelse(av_insuff_1_num == 5, 1, 0),
         av_mild = ifelse(av_insuff_1_num == 2, 1, 0),
         av_moderate = ifelse(av_insuff_1_num == 3, 1, 0),
         av_severe = ifelse(av_insuff_1_num == 4, 1, 0),
         op_elective = ifelse(op_status_i_num == 1, 1, 0),
         op_emergent = ifelse(op_status_i_num == 2, 1, 0),
         op_urgent = ifelse(op_status_i_num == 3, 1, 0))


###########

covars <-  c("age", "gender_i", "white", "bmi_i", "chf",
               "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
               "chrlungd_i", "cvd_p", "cva_p",
               "liver_p", "osa_p", "pasys_i", "pvd_p", 
               "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
               "cathtosurg_i", "rfhemoglobin_i", 
               "platelets_i", "totalbumin_i", "creatlst_i", "inr_i", 
               "left_main_50", "numdisv_i", "op_status_i_num", 
               "ace_i_48", "inotrop_48","hdef_i","av_insuff_1_num", 
               'dialysis_pre', 'redo_srg', "prev_cabg",
               "stemi_nstemi", "cardio_shock", "predmort_i")

raceVars <- c("white", "black", "asian", "nativeam", "other")

binVars <- c("admit_elective", "admit_emergency", "admit_transfer", 
             "admit_other", "lung_none", "lung_mild", "lung_moderate",
             "lung_severe", "lung_unknown", "pci_none", "pci_deter",
             "pci_nodeter", "pci_wostemi", "pci_stemi", "pci_other",
             "av_none", "av_trace", "av_mild", "av_moderate", "av_severe",
             "op_elective", "op_emergent", "op_urgent")

matched_control = matched_control %>%
  mutate(HF_below_55 = hdef_i < 55 + 0,
         HF_above_70 = hdef_i > 70 + 0,
         HF_normal = 1 - HF_above_70 - HF_below_55,
         chf = ifelse(nyha_1 == 0, 0, 1),
         numdisv_i = ifelse(numdisv_i == 3, 1, 0))

dt_treated_before <- pre_match %>% filter(tee == 1) %>% select(all_of(covars))
dt_control_before <- pre_match %>% filter(tee == 0) %>% select(all_of(covars))
dt_treated_after <- matched_treated %>% select(all_of(covars))
dt_control_after <- matched_control %>% select(all_of(covars))
####
mean_treated_before = apply(dt_treated_before, 2, mean)
mean_control_before = apply(dt_control_before, 2, mean)
mean_diff_before = mean_treated_before - mean_control_before

sd_treated_before = apply(dt_treated_before, 2, stats::sd)
sd_control_before = apply(dt_control_before, 2, stats::sd)

pooled_sd = sqrt(sd_treated_before^2 + sd_control_before^2)

std_before = mean_diff_before/pooled_sd

mean_treated_after = apply(dt_treated_after, 2, mean)
mean_control_after = apply(dt_control_after, 2, mean)
mean_diff_after = mean_treated_after - mean_control_after

std_after = mean_diff_after/pooled_sd

std_after %>% as.data.frame() %>% View

# Get smd for midcab and opcab variables for revisions 01/26/25
sm_train_trt = sts_small %>% filter(tee == 1)
sm_train_cntrl = sts_small %>% filter(tee == 0)
mean_treated_before = mean(sm_train_trt$op_cab)
mean_control_before = mean(sm_train_cntrl$op_cab)
mean_diff_before = mean_treated_before - mean_control_before

pooled_sd = sqrt(sd(sm_train_trt$op_cab)^2 + sd(sm_train_cntrl$op_cab)^2)

mean_treated_after = mean(small_update_trt$op_cab)
mean_control_after = mean(small_update_cntrl$op_cab)
mean_diff_after = mean_treated_after - mean_control_after

std_after = mean_diff_after/pooled_sd

