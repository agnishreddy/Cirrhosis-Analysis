library(dplyr)
library(tidyr)
library(haven) 


file_path <- "G:\\Fresh Downloads\\archive\\cirrhosis.csv" 
cirrhosis_data <- read.csv(file_path)


dm <- cirrhosis_data %>%
  transmute(
    STUDYID = "CIRR001",   
    USUBJID = paste0("SUBJ", ID), 
    AGE = Age,
    SEX = Sex,
    ARM = ifelse(is.na(Drug), "Unknown", Drug) 
  )


ae <- cirrhosis_data %>%
  select(ID, Ascites, Hepatomegaly, Spiders, Edema) %>%
  pivot_longer(cols = c(Ascites, Hepatomegaly, Spiders, Edema),
               names_to = "AE_TERM",
               values_to = "AE_SEVERITY") %>%
  filter(!is.na(AE_SEVERITY)) %>%
  mutate(
    STUDYID = "CIRR001",
    USUBJID = paste0("SUBJ", ID)
  )


lb <- cirrhosis_data %>%
  select(ID, Bilirubin, Cholesterol, Albumin, Copper) %>%
  pivot_longer(cols = -ID,
               names_to = "LB_TEST",
               values_to = "LB_RESULT") %>%
  mutate(
    STUDYID = "CIRR001",
    USUBJID = paste0("SUBJ", ID)
  )


sv <- cirrhosis_data %>%
  transmute(
    STUDYID = "CIRR001",
    USUBJID = paste0("SUBJ", ID),
    STATUS = ifelse(Status == "D", "Deceased", "Censored"),
    SURVIVAL_DAYS = N_Days
  )


adlb <- lb %>%
  group_by(USUBJID, LB_TEST) %>%
  mutate(
    BASELINE = first(LB_RESULT, order_by = ID), 
    CFB = LB_RESULT - BASELINE                 
  ) %>%
  ungroup()


adsv <- sv %>%
  mutate(
    EVENT = ifelse(STATUS == "Deceased", 1, 0)
  )


write_xpt(dm, "sdtm_dm.xpt")
write_xpt(ae, "sdtm_ae.xpt")
write_xpt(lb, "sdtm_lb.xpt")
write_xpt(sv, "sdtm_sv.xpt")
write_xpt(adlb, "adam_adlb.xpt")
write_xpt(adsv, "adam_adsv.xpt")


cat("CDISC datasets have been successfully created and exported!")




getwd()




library(survival)
library(survminer)
library(dplyr)

# Load the dataset
file_path <- "G:\\Fresh Downloads\\archive\\cirrhosis.csv" 
cirrhosis_data <- read.csv(file_path)


survival_data <- cirrhosis_data %>%
  mutate(
    EVENT = ifelse(Status == "D", 1, 0), 
    TIME = N_Days                        
  )


km_fit <- survfit(Surv(TIME, EVENT) ~ 1, data = survival_data)

ggsurvplot(
  km_fit,
  data = survival_data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curve",
  risk.table = TRUE,    
  conf.int = TRUE,      
  ggtheme = theme_minimal() 
)



km_fit_group <- survfit(Surv(TIME, EVENT) ~ Drug, data = survival_data)


ggsurvplot(
  km_fit_group,
  data = survival_data,
  xlab = "Days",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curve by Treatment Group",
  risk.table = TRUE,
  conf.int = TRUE,
  ggtheme = theme_minimal(),
  pval = TRUE 
)







