# Objective: Survival analysis for customer churn

# Load the data
train_data_vars <- names(train)
dat_survival <- dat %>% dplyr::select(train_data_vars) # keep only variables existed in train dataset
#dat_survival$type <- as.factor(dat_survival$type)
dat_survival$rim_status_closed <- as.numeric(dat_survival$rim_status_closed) - 1

# Convert the membership duration from day to month level and year level
dat_survival$rim_duration_month <- dat_survival$rim_duration/30
dat_survival$rim_duration_year <- dat_survival$rim_duration/365

# Plot the graph to examine the distribution of membership duration
# and the churn status

plotMemDuration <- dat_survival %>%
  mutate(churn = rim_status_closed %>% factor(labels = c("No","Yes"))) %>%
  ggplot() +
  geom_histogram(aes(x = rim_duration_month, fill = factor(churn))) +
  facet_grid( ~ churn) +
  theme(legend.position = "none")
plotMemDuration

plotMemDuration <- dat_survival %>%
  mutate(churn = rim_status_closed %>% factor(labels = c("No","Yes"))) %>%
  ggplot() +
  geom_histogram(binwidth = 1, aes(x = rim_duration_year, fill = factor(churn))) +
  facet_grid( ~ churn) +
  theme(legend.position = "none")
plotMemDuration

dat_survival %>% group_by(rim_status_closed) %>% summarise(avg_month = mean(rim_duration_month), 
                                                           min_month = min(rim_duration_month), 
                                                           max_month = max(rim_duration_month))
# Create a survival object for Kaplan-Meier Analysis
survObj <- Surv(dat_survival$rim_duration, dat_survival$rim_status_closed)
str(survObj)
fitKM <- survfit(survObj ~ 1)
fitKM
plot(fitKM, xlab = "Membership Duration (by day)", ylab = "Survival function", main = "Survival function")


# Kaplan-Meier with Categorial Covariate
fitKM_type <- survfit(survObj ~ type, data=dat_survival)
print(fitKM_type)

# Cox Proportional Hazards Model
dat_survival$rim_duration_month <- NULL
dat_survival$rim_duration_year <- NULL

dd <- datadist(dat_survival)
options(datadist = "dd")

fitCPH <- coxph(formula = Surv(rim_duration, rim_status_closed) ~ age + type  + moving_time + 
                 total_acct + CK + SV + CD + IL +  ML + VISA +
                 mrm + trm +  orig_rate + acc_duration + 
                 branch_rating + member_rating +
                 contract_rating,
               data = dat_survival)
#relevel(type, ref = "Non Member")
summary(fitCPH)
exp(fitCPH$coefficients) %>% round(2)
survfit(fitCPH)
plot(survfit(fitCPH))
ggforest(fitCPH)

# Save the model for UI
saveRDS(fitCPH, "CPH_model.rds")


# References:
# https://www.emilyzabor.com/tutorials/survival_analysis_in_r_tutorial.html
# https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/
# http://www.sthda.com/english/wiki/cox-proportional-hazards-model