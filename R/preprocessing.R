## ---------------------------
##
##
## Preparing STS data for analysis.
##
## Author: Charlotte Talham
##
## Date Created: 2023-07-18
##
##
## ---------------------------
##
## Notes: Last updated with 4.2.24 processed data
##   
## ---------------------------
library(tidyverse)
library(tableone)
library(ggplot2)
library(diptest)

# Load STS data
# Starting sample size N = 1,321,030
sts1 <- read_csv("./STS_ACSD_data_processed_2024_04_02.csv")

# Exclusion criteria
sts <- sts1 %>% filter(exclude != 1)
rm(sts1)

# Impute missing data -----------------------------------------------------

# Median imputation of continuous variables
sts$rfhemoglobin_i <- ifelse(is.na(sts$rfhemoglobin), 
                             median(sts$rfhemoglobin, na.rm = TRUE), 
                             sts$rfhemoglobin)
sts$platelets_i <- ifelse(is.na(sts$platelets), 
                        median(sts$platelets, na.rm = TRUE), sts$platelets)
sts$totalbumin_i <- ifelse(is.na(sts$totalbumin), 
                           median(sts$totalbumin, na.rm = TRUE), 
                           sts$totalbumin)
sts$creatlst_i <- ifelse(is.na(sts$creatlst), 
                         median(sts$creatlst, na.rm = TRUE), 
                         sts$creatlst)
sts$inr_i <- ifelse(is.na(sts$inr), median(sts$inr, na.rm = TRUE), sts$inr)
sts$hdef_i <- ifelse(is.na(sts$hdef), median(sts$hdef, na.rm = TRUE), sts$hdef)
sts$cathtosurg_i <- ifelse(is.na(sts$cathtosurg), 
                           median(sts$cathtosurg, na.rm = TRUE), 
                           sts$cathtosurg)
sts$predmort_i <- ifelse(is.na(sts$predmort), 
                         median(sts$predmort, na.rm = TRUE), 
                         sts$predmort)
sts$predreop_i <- ifelse(is.na(sts$predreop), 
                         median(sts$predreop, na.rm = TRUE), 
                         sts$predreop)
sts$predstro_i <- ifelse(is.na(sts$predstro), 
                         median(sts$predstro, na.rm = TRUE), 
                         sts$predstro)
sts$predvent_i <- ifelse(is.na(sts$predmort), 
                         median(sts$predmort, na.rm = TRUE), 
                         sts$predmort)
sts$predmm_i <- ifelse(is.na(sts$predvent), median(sts$predvent, na.rm = TRUE), 
                         sts$predvent)
sts$pasys_i <- ifelse(is.na(sts$pasys), median(sts$pasys, na.rm = TRUE),
                      sts$pasys)
sts$venthrstot_i <- ifelse(is.na(sts$venthrstot), 
                           median(sts$venthrstot, na.rm = TRUE), 
                           sts$venthrstot) # Secondary outcome

# BMI
sts$bmi_fix <- ifelse(sts$bmi < 12 | sts$bmi > 100, NA, sts$bmi)
sts$bmi_i <- ifelse(is.na(sts$bmi_fix), median(sts$bmi_fix, na.rm = TRUE), 
                    sts$bmi_fix)


# Mode imputation of categorical variables

# Write mode function from Ken Williams on stackoverflow
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


sts$gender_i <- ifelse(is.na(sts$gender), mode(sts$gender),
                       sts$gender) %>% as.factor()
sts$admitsrc_i <- ifelse(is.na(sts$admitsrc), mode(sts$admitsrc),
                         sts$admitsrc) %>% as.factor()
sts$chrlungd_i <- ifelse(is.na(sts$chrlungd) | sts$chrlungd == 6, 
                         mode(sts$chrlungd), sts$chrlungd) %>% as.factor()
sts$cardsymptimeofadm_i <- ifelse(is.na(sts$cardsymptimeofadm),
                                  mode(sts$cardsymptimeofadm),
                                  sts$cardsymptimeofadm) %>% as.factor()
sts$op_status_i <- ifelse(sts$op_status == "unknown",
                          mode(sts$op_status), sts$op_status) %>% as.factor()
sts$numdisv_i <- ifelse(is.na(sts$numdisv), mode(sts$numdisv), sts$numdisv)
sts$white <- ifelse(is.na(sts$racecaucasian), 1, sts$racecaucasian)

sts$asian <- ifelse(is.na(sts$raceasian), 2, sts$raceasian)
sts$black <- ifelse(is.na(sts$raceblack), 2, sts$raceblack)
sts$nativeam <- ifelse(is.na(sts$racenativeam), 2, sts$racenativeam)
sts$other <- ifelse(is.na(sts$raceother), 2, sts$raceother)

rm(mode)


# Recode vars -------------------------------------------------------------

# Create surgical volume variable
sts <- sts %>% group_by(sts_hospid) %>% mutate(surg_vol = n()/8.5) %>% ungroup()

sts$surg_vol_cat <- ifelse(sts$surg_vol <= 100, 1, NA)
sts$surg_vol_cat <- ifelse(sts$surg_vol > 100 & sts$surg_vol <= 250,
                           2, sts$surg_vol_cat)
sts$surg_vol_cat <- ifelse(sts$surg_vol > 250,
                           3, sts$surg_vol_cat)
sts <- sts %>% mutate(small_hosp = ifelse(surg_vol_cat == 3, 0, 1))
sts <- sts %>% mutate(large_hosp = ifelse(surg_vol_cat == 3, 1, 0))

# Recode vars with NA to another level of factor vars
sts$nyha_1 <- ifelse(is.na(sts$nyha), 0, sts$nyha)
sts$av_insuff_1 <- ifelse(is.na(sts$av_insuff), "healthy", sts$av_insuff)
sts$mv_insuff_1 <- ifelse(is.na(sts$mv_insuff), "healthy", sts$mv_insuff)
sts$tv_insuff_1 <- ifelse(is.na(sts$tv_insuff), "healthy", sts$tv_insuff)
sts$pci_ind_surgery_1 <- ifelse(is.na(sts$pci_ind_surgery), "No PCI",
                                sts$pci_ind_surgery)

# Reorder factor
sts$pci_ind_surgery_1 <- factor(sts$pci_ind_surgery_1, 
                                levels = c("No PCI", "complic-deterioration",
                                           "complic-no-deterioration",
                                           "planned-staged-STEMI",
                                           "planned-staged-no-STEMI",
                                           "other-reason"))

sts$av_insuff_1 <- as.factor(sts$av_insuff_1)
sts$mv_insuff_1 <- as.factor(sts$av_insuff_1)
sts$tv_insuff_1 <- as.factor(sts$av_insuff_1)

# Add MIDCAB and OPCAB variables
# create <mid_cab> == 1 (using the following OR logic): robotic == 1 | 
# opapp == 2 | oppap == 11 | opapp == 12
sts$mid_cab <- ifelse(sts$robotic == 1 | sts$opapp == 2 | sts$opapp == 11 | 
                        sts$opapp == 12, 1, 0)
sts$mid_cab <- ifelse(is.na(sts$mid_cab), 0, sts$mid_cab) # 18,841
sts$op_cab <- ifelse(sts$cpbutil == 1, 1, 0)
sts$op_cab <- ifelse(is.na(sts$op_cab), 0, sts$op_cab) # 149,037

# Select variables for export
vars <- c("age", "gender_i", "white", "bmi_i",
          "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
          "chrlungd_i", "nyha_1", "cvd_p", "cva_p", "dialysis_pre",
          "liver_p", "osa_p", "pasys_i", "pvd_p", "prev_pci", "pci_6_hrs_less",
          "pci_stent", "pci_ind_surgery_1", "cathtosurg_i",
          "prev_cabg", "redo_srg", "rfhemoglobin_i", "platelets_i",
          "totalbumin_i", "creatlst_i", "inr_i", "left_main_50",
          "cardsymptimeofadm_i", "cardio_shock", "numdisv_i",
          "ace_i_48", "inotrop_48", "hdef_i", "predmort_i",
          "av_insuff_1", 'mv_insuff_1', 'tv_insuff_1',
          "op_status_i", "surg_vol_cat", "surg_vol", "small_hosp",
          "large_hosp", "sts_hospid", "sts_surgid", "predreop_i",
          "predstro_i", "predvent_i", "predmm_i", "stemi_nstemi", 
          "death_all_30", "death_opmort", "chest_washout", "stroke_complic",
          "venthrstot_i", "renal_fail", "cor_reint", "dvt_complic",
          "afib_new", "surgyear", "tee", "asian", "black", "nativeam", "other",
          "mid_cab", "op_cab", "acsdid")

sts_export <- sts %>% select(vars)
write_csv(sts_export, "./sts_trimmed_012625.csv")

# Subset by hospital size -------------------------------------------------
sts <- read_csv("./sts_trimmed_012625.csv")

# Low surgical volume hospitals
sts_small <- sts %>% filter(surg_vol_cat == 1) %>% mutate(rn=row_number())
set.seed(33)
small_train <- sts_small %>% slice_sample(prop = 0.25)
small_test <- sts_small %>% slice(-pull(small_train,rn))
write_csv(sts_small, "./small_hospitals/sts_small.csv")
write_csv(small_train, "./small_hospitals/train.csv")
write_csv(small_test, "./small_hospitals/test.csv")

# Medium surgical volume hospitals
sts_medium <- sts %>% filter(surg_vol_cat == 2) %>% 
  mutate(rn=row_number())
set.seed(33)
med_train <- sts_medium %>% slice_sample(prop = 0.25)
med_test <- sts_medium %>% slice(-pull(med_train,rn))
write_csv(sts_medium, "./medium_hospitals/sts_medium.csv")
write_csv(med_train, "./medium_hospitals/train.csv")
write_csv(med_test, "./medium_hospitals/test.csv")

# High surgical volume hospitals
sts_large <- sts %>% filter(surg_vol_cat == 3) %>% 
  mutate(rn=row_number())
set.seed(33)
large_train <- sts_large %>% slice_sample(prop = 0.25)
large_test <- sts_large %>% slice(-pull(large_train,rn))

write_csv(sts_large, "./large_hospitals/sts_large.csv")
write_csv(large_train, "./large_hospitals/train.csv")
write_csv(large_test, "./large_hospitals/test.csv")


# Table one ---------------------------------------------------------------

# Recode

sts$numdisv_i <- sts$numdisv_i - 1
sts$disv_3 <- ifelse(sts$numdisv_i == 3, 1, 0)

sts <- sts %>%
  mutate(pci_ind_surgery_1_num =  as.numeric(as.factor(pci_ind_surgery_1)),
         av_insuff_1_num = as.numeric(as.factor(av_insuff_1)),
         mv_insuff_1_num = as.numeric(as.factor(mv_insuff_1)),
         tv_insuff_1_num = as.numeric(as.factor(tv_insuff_1)),
         op_status_i_num = as.numeric(as.factor(op_status_i)))

sts$asian <- ifelse(sts$asian == 0, 1, 0)
sts$black <- ifelse(sts$black == 1, 1, 0)
sts$nativeam <- ifelse(sts$nativeam == 1, 1, 0)
sts$white <- ifelse(sts$white == 1, 1, 0)
sts$other <- ifelse(sts$other == 1, 1, 0)

covars <- c("age", "gender_i", 'asian', 'black', 'nativeam', "white",
            'other', "bmi_i", "admitsrc_i", "arrhythmia_p", "afib_parox", 
            "afib_persist", "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
            'dialysis_pre', "liver_p", "osa_p", "pasys_i", "pvd_p", 
            "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
            "cathtosurg_i", "prev_cabg", 'redo_srg', "rfhemoglobin_i", 
            "platelets_i", "totalbumin_i", "inr_i", "creatlst_i",  
            "left_main_50", "stemi_nstemi", "cardio_shock", 
            "disv_3", "ace_i_48", "inotrop_48", "av_insuff_1_num", 
            "op_status_i_num","hdef_i",'predmort_i', 'surg_vol_cat',
            "mid_cab", "op_cab")

catvars <- c("gender_i", 'asian', 'black', 'nativeam', "white", 'other',
             "admitsrc_i", "arrhythmia_p", "afib_parox", "afib_persist",
             "chrlungd_i", "nyha_1", "cvd_p", "cva_p", 
             'dialysis_pre', "liver_p", "osa_p", "pvd_p", 
             "prev_pci", "pci_6_hrs_less","pci_stent", "pci_ind_surgery_1_num", 
             "prev_cabg", 'redo_srg',  
             "left_main_50", "stemi_nstemi", "cardio_shock", 
             "disv_3", "ace_i_48", "inotrop_48", "av_insuff_1_num", 
             "op_status_i_num", 'surg_vol_cat', "mid_cab", "op_cab")


# Pre match training data table 1
tb1 <- tableone::CreateTableOne(vars = covars,
                                factorVars = catvars,
                                strata = 'tee',
                                data = sts,
                                addOverall = TRUE,
                                includeNA = TRUE,
                                test = FALSE)

# Histogram of TEE surgeries by hospital surgical volume ----------------------

tee_prop_small <- sts_small %>% group_by(sts_hospid) %>% 
  summarize(prop = mean(tee)) %>% 
  mutate(hosp_size = "Low Surgical Volume") %>% 
  select(c(hosp_size, prop))
tee_prop_med <- sts_medium %>% group_by(sts_hospid) %>% 
  summarize(prop = mean(tee)) %>% 
  mutate(hosp_size = "Medium Surgical Volume") %>% 
  select(c(hosp_size, prop))
tee_prop_large <- sts_large %>% group_by(sts_hospid) %>% 
  summarize(prop = mean(tee)) %>% 
  mutate(hosp_size = "High Surgical Volume") %>% 
  select(c(hosp_size, prop))
hist_df <- tee_prop_large %>% rbind(rbind(tee_prop_small, tee_prop_med))
hist_df$hosp_size <- factor(hist_df$hosp_size, 
                            levels = c("Low Surgical Volume",
                                       "Medium Surgical Volume",
                                       "High Surgical Volume"))

# Get mean and median TEE use
get_mean_ci <- function(x, conf.level = 0.95) {
  x <- na.omit(x)  # Remove NAs
  n <- length(x)
  mean_x <- mean(x)
  stderr <- sd(x) / sqrt(n)  # Standard error
  alpha <- 1 - conf.level
  error_margin <- qt(1 - alpha/2, df = n - 1) * stderr  # t-distribution critical value
  
  lower <- mean_x - error_margin
  upper <- mean_x + error_margin
  
  return(c(mean = mean_x, lower_CI = lower, upper_CI = upper))
}
get_median_ci <- function(x, conf.level = 0.95) {
  x <- na.omit(x)  # Remove NAs
  n <- length(x)
  median_x <- median(x)
  k <- qbinom(p = 0.025, size = n, prob = 0.5)  # Find k using binomial quantile
  sorted_x <- sort(x)
  ci_lower <- sorted_x[k + 1]  # Lower bound
  ci_upper <- sorted_x[n - k]  # Upper bound
  
  return(c(median = median_x, lower_CI = ci_lower, upper_CI = ci_upper))
}
get_mean_ci(tee_prop_small$prop)
get_mean_ci(tee_prop_med$prop)
get_mean_ci(tee_prop_large$prop)
get_median_ci(tee_prop_small$prop)
get_median_ci(tee_prop_med$prop)
get_median_ci(tee_prop_large$prop)

# Perform test for multimodality
tee_prop_small %>% pull(prop) %>% dip.test(simulate.p.value = TRUE)
tee_prop_med %>% pull(prop) %>% dip.test(simulate.p.value = TRUE)
tee_prop_large %>% pull(prop) %>% dip.test(simulate.p.value = TRUE)

# p-values for each facet
p_values <- data.frame(
  hosp_size = c("Low Surgical Volume",
            "Medium Surgical Volume",
            "High Surgical Volume"),
  p_value = c("<0.001", "<0.001", " = 0.749")
)
p_values$hosp_size <- factor(p_values$hosp_size, 
                            levels = c("Low Surgical Volume",
                                       "Medium Surgical Volume",
                                       "High Surgical Volume"))

# Histogram
hist <- hist_df %>% ggplot(aes(x = prop)) +
  geom_density(fill = "skyblue", 
                 color = "skyblue", alpha = .9) + 
  facet_wrap(~ hosp_size) +  # Create separate panels for each hospital size
  theme_minimal() +  # Use a clean theme
  labs(
    # title = "Proportion of CABG Surgeries Using TEE by Hospital Surgical Volume",
    x = "Proportion of CABG Surgeries Using TEE"
    # y = "Count"
  ) +
  scale_x_continuous(breaks = c(0, 1/4, 1/2, 3/4, 1),
                     # labels = c("0", "1/4", "1/2", "3/4", "1")
                     labels = c("0", "", "0.5", "", "1")) +
  theme(strip.text = element_text(size = 12),  # Customize facet labels
        axis.text = element_text(size = 10),    # Customize axis labels
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),          # Remove y-axis labels
        axis.title.y = element_blank(),       # Remove y-axis title
        # panel.grid.major.y = element_blank(),   # Remove y-axis grid lines
        panel.grid.minor.y = element_blank(),
        # panel.grid.major.x = element_blank(),  # Remove y-axis grid lines
        panel.grid.minor.x = element_blank()# Remove minor y-axis grid lines
        ) +
  geom_text(data = p_values, aes(x = Inf, y = Inf, 
                                 label = paste0("p", p_value)),
            hjust = 1.1, vjust = 1.1, inherit.aes = FALSE)

ggsave("./manuscript_files/hist.svg", plot = hist, device = "svg",
       width = 6, height = 4)
ggsave("./manuscript_files/hist.pdf", plot = hist, device = "pdf",
       width = 6, height = 4)



