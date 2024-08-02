# Objective: Create different datasets for each observation/ performance window

##################
# mbr data
##################

# Set the observation and performance windows.
Obs_start <- as.Date("2016-10-01")
Obs_end <- as.Date("2017-09-30")
Perf_start <- as.Date("2017-10-01")
Perf_end <- as.Date("2017-12-31")


# Selection criteria for each window:
# we want member active during observation window
# if rim_closed_dt is in the observation window, exclude that row. If the rim_closed_dt is within the performance window, it is coded 1(churned); otherwise, 0
# member duration = member open date up to the end of the observation window

# we want account active during observation window only. 
# Exclude rows that have account outside of observation window
# account duration = if(closed_dt <= end_obs, closed_dt - created_dt, end_obs - created_dt)
  
# online duration = if(Enrollment_date > end_obs, 0, if(Last_login_date <= end_obs, last_login_date - enrollment_date, end_obs - enrollment_date))

# Contract date should be before the end of observation window
#Obs_start %m+% months(0)
datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  dat <- mbr_clean %>%
    filter(rim_open_dt <= Obs_end_new) %>%
    filter(rim_closed_dt > Obs_end_new) %>%
    filter(create_dt <= Obs_end_new) %>%
    mutate(rim_status_closed_original = rim_status_closed) %>%
    mutate(rim_status_closed = if_else(rim_closed_dt >= Perf_start_new & rim_closed_dt <= Perf_end_new, 1,0)) %>%
    mutate(rim_status_closed = if_else(rim_status_closed_original == 0, 0,rim_status_closed)) %>% # for clients who have the end date 2021-10-31, they are actually active and rim_status_closed should be 0
    mutate(rim_duration = as.numeric(difftime(Obs_end_new,rim_open_dt,units=c("days"))),
           acc_duration = if_else(closed_dt <= Obs_end_new, 
                                  as.numeric(difftime(closed_dt,create_dt,units=c("days"))),
                                  as.numeric(difftime(Obs_end_new,create_dt,units=c("days")))),
           online_duration = if_else(Enrollment_Date > Obs_end_new, 0, 
                                     if_else(Last_Login_Date <= Obs_end_new, 
                                     as.numeric(difftime(Last_Login_Date,Enrollment_Date,units=c("days"))),
                                     as.numeric(difftime(Obs_end_new,Enrollment_Date,units=c("days")))))
    ) %>%
    mutate(online_duration = if_else(is.na(online_duration),0, round(online_duration,0))) %>%
    mutate(contract_dt = if_else(contract_dt <= Obs_end_new,contract_dt, as.Date("1900-01-01"))) %>%
    mutate(period = i+1) %>%
    mutate(observation_window = paste(Obs_start_new, ":", Obs_end_new),
           performance_window = paste(Perf_start_new, ":", Perf_end_new))
  
  dat <- data.frame(dat)
  datalist[[i+1]] <- dat
}

mbr_clean2 = do.call(rbind, datalist)
mbr_clean2$contract_dt[mbr_clean2$contract_dt==as.Date("1900-01-01")] <- NA

# Count the number of churn for each period
count_churn <- mbr_clean2 %>% group_by(period, rim_status_closed) %>% count()

write.csv(mbr_clean2, "mbr_clean2.csv", row.names=FALSE)


##################
# bal_hist data
##################
# select balance within the observation window and then aggregate values

datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  dat <- bal_dp %>%
    filter(is.na(cur_bal)==FALSE) %>%
    filter(period_end_dt >= Obs_start_new & period_end_dt <= Obs_end_new) %>%
    arrange(period_end_dt) %>%
    group_by(acct_type, account_id) %>%
    summarise(n = length(cur_bal), beg_bal = first(cur_bal), end_bal = last(cur_bal), 
              min_bal = min(cur_bal), max_bal = max(cur_bal), avg_bal = mean(cur_bal),
              beg_rate = first(avg_rate), end_rate = last(avg_rate), 
              min_rate = min(avg_rate), max_rate = max(avg_rate), avg_rate = mean(avg_rate)) %>%
    mutate(period = i+1)
  dat <- dat %>% dplyr::select(-c("acct_type", "n"))   
  
  dat <- data.frame(dat)
  datalist[[i+1]] <- dat
}

bal_dp_agg = do.call(rbind, datalist)


datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  dat<- bal_loan %>%
    filter(is.na(cur_bal)==FALSE) %>%
    filter(period_end_dt >= Obs_start_new & period_end_dt <= Obs_end_new) %>%
    arrange(period_end_dt) %>%
    group_by(acct_type, account_id) %>%
    summarise(n = length(cur_bal), beg_bal = first(cur_bal), end_bal = last(cur_bal), 
              min_bal = min(cur_bal), max_bal = max(cur_bal), avg_bal = mean(cur_bal),
              beg_rate = first(avg_rate), end_rate = last(avg_rate), 
              min_rate = min(avg_rate), max_rate = max(avg_rate), avg_rate = mean(avg_rate)) %>%
    mutate(period = i+1)
  dat <- dat %>% dplyr::select(-c("acct_type", "n")) 
  
  dat <- data.frame(dat)
  datalist[[i+1]] <- dat
}

bal_loan_agg = do.call(rbind, datalist)

write.csv(bal_dp_agg, "bal_dp_agg.csv", row.names=FALSE)
write.csv(bal_loan_agg, "bal_loan_agg.csv", row.names=FALSE)

##################
# dp_trans and ln_trans data
##################
# select transactions within the observation window and then aggregate values

datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
# Aggregate data
  dat <- dp_trans %>%
    filter(mon_date >= Obs_start_new & mon_date <= Obs_end_new) %>%
    arrange(mon) %>%
    group_by(acct_type, account_id) %>%
    summarise(n = length(amt), total_amt = sum(amt), avg_amt = mean(amt),
            total_ct = sum(ct), avg_ct = mean(ct)) %>%
    mutate(period = i+1)
  dat <- dat %>% dplyr::select(-c("acct_type", "n")) 

  dat <- data.frame(dat)
  datalist[[i+1]] <- dat
}

dp_trans_agg = do.call(rbind, datalist)


datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  # Aggregate data
  dat <- ln_trans %>%
    filter(mon_date >= Obs_start_new & mon_date <= Obs_end_new) %>%
    arrange(mon) %>%
    group_by(acct_type, account_id) %>%
    summarise(n = length(amt), total_amt = sum(amt), avg_amt = mean(amt),
              total_ct = sum(ct), avg_ct = mean(ct)) %>%
    mutate(period = i+1)
  dat <- dat %>% dplyr::select(-c("acct_type", "n")) 
  
  dat <- data.frame(dat)
  datalist[[i+1]] <- dat
}

ln_trans_agg = do.call(rbind, datalist)

write.csv(dp_trans_agg, "dp_trans_agg.csv", row.names=FALSE)
write.csv(ln_trans_agg, "ln_trans_agg.csv", row.names=FALSE)

##################
# Merging mbr_clean2, bal_dp_agg and dp_trans_agg data
##################
#memory.limit(size=1800)
# Load the data
# mbr_clean2 <- read_csv("data/processed/mbr_clean2.csv")
# #mbr_clean <- subset( mbr_clean, select = -1 )
# bal_dp_agg <- read_csv("data/processed/bal_dp_agg.csv")
# dp_trans_agg <- read_csv("data/processed/dp_trans_agg.csv")


# Filter mbr_clean to only CK, SV and CD
dp_acct = c("CD", "CK", "SV")
mbr_dp <- mbr_clean2 %>% filter(acct_type %in% dp_acct)

# Merge the bal_dp_agg and dp_trans_agg to mbr_dp
mbr_dp <- mbr_dp %>% left_join(bal_dp_agg, by=c("period","account_id"))
mbr_dp <- mbr_dp %>% left_join(dp_trans_agg, by=c("period","account_id"))

# Remove rows with missing both bal_dp_agg or dp_trans_agg data
mbr_dp <- mbr_dp %>% filter(!(is.na(avg_bal)==TRUE | is.na(avg_amt)==TRUE))
mbr_dp <- mbr_dp %>% dplyr::select(-c("acct_type.x", "acct_type.y")) 

write.csv(mbr_dp, "mbr_dp.csv", row.names=FALSE)

##################
# Merging mbr_clean, bal_loan_agg and ln_trans_agg data
##################

# # Load the data
# mbr_clean <- read_csv("data/processed/mbr_clean.csv")
# bal_loan_agg <- read_csv("data/processed/bal_loan_agg.csv")
# ln_trans_agg <- read_csv("data/processed/ln_trans_agg.csv")

# Filter mbr_clean to loan type only
dp_acct = c("CD", "CK", "SV")
mbr_ln <- mbr_clean2 %>% filter(!(acct_type %in% dp_acct))

# Merge the bal_dp_agg and dp_trans_agg to mbr_ln
mbr_ln <- mbr_ln %>% left_join(bal_loan_agg, by=c("period","account_id"))
mbr_ln <- mbr_ln %>% left_join(ln_trans_agg, by=c("period","account_id"))

# Remove rows with missing both bal_dp_agg or dp_trans_agg data
mbr_ln <- mbr_ln %>% filter(!(is.na(avg_bal)==TRUE | is.na(avg_amt)==TRUE))
mbr_ln <- mbr_ln %>% dplyr::select(-c("acct_type.x", "acct_type.y")) 

write.csv(mbr_ln, "mbr_ln.csv", row.names=FALSE)


##################
# Visas data
##################
# select visas that are active within the observation window and then aggregate values


datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  # Aggregate data
  dat <- visas %>%
    filter((closed_dt >= Obs_start_new & closed_dt <= Obs_end_new) | (effective_dt >= Obs_start_new & effective_dt <= Obs_end_new)) %>%
    mutate(acc_duration = as.numeric(difftime(closed_dt,Obs_start_new,units=c("days")))) %>% # recalculate the duration date from the observation start date
    mutate(period = i+1)
  dat <- dat %>% dplyr::select(-c("status", "ext_status_code", "closed_dt")) 
  
  dat2 <- mbr_clean2 %>%
    dplyr::select(rim_status_closed, age, type, mrm, zip, moving_time, rim_duration, online_duration, member_id, rim_closed_dt) %>%
    distinct(member_id, .keep_all = TRUE) %>%
    right_join(dat, by=c("member_id")) %>%
    filter(!(is.na(rim_status_closed)))
  
  
  dat2 <- data.frame(dat2)
  datalist[[i+1]] <- dat2
}

mbr_visas = do.call(rbind, datalist)

# If available credit >= 35K, VISA ->HE
mbr_visas <- mbr_visas %>% mutate(VISA_HE = ifelse(avail_bal>=35000, 1, 0))

write.csv(mbr_visas, "mbr_visas.csv", row.names=FALSE)

##################
# Survey data
##################
# No filtering the survey recordedDate

datalist = list()
for (i in 0:15) {
  Obs_start_new <- Obs_start %m+% months(i*3)
  Obs_end_new <- Obs_end %m+% months(i*3)
  Perf_start_new <- Perf_start %m+% months(i*3)
  Perf_end_new <- Perf_end %m+% months(i*3)
  
  # Aggregate data
  dat_branch <- branch_survey #%>% filter(RecordedDate >= Obs_start_new & RecordedDate <= Obs_end_new) 
  dat_new <- new_mbr_survey #%>% filter(RecordedDate >= Obs_start_new & RecordedDate <= Obs_end_new)
  dat_cons <- new_cons_survey #%>% filter(RecordedDate >= Obs_start_new & RecordedDate <= Obs_end_new)
  dat_mtg <- new_mtg_survey #%>% filter(RecordedDate >= Obs_start_new & RecordedDate <= Obs_end_new)
  dat_email <- sec_email_survey #%>% filter(RecordedDate >= Obs_start_new & RecordedDate <= Obs_end_new)
  
  dat2 <- mbr_clean2 %>%
    left_join(dat_branch, by="member_id") %>%
    left_join(dat_new, by="member_id") %>%
    left_join(dat_cons, by="member_id") %>%
    left_join(dat_mtg, by="member_id") %>%
    left_join(dat_email, by="member_id")
    
  
  dat2 <- data.frame(dat2)
  datalist[[i+1]] <- dat2
}

mbr_survey = do.call(rbind, datalist)


# # Calculate the date gap between the contract date and the recorded date. 
# # If the cons_gap is less than two week2 (<=14) and positive, keep the rating value; otherwise, make it NA
mbr_survey$cons_gap = as.numeric(difftime(mbr_survey$RecordedDate.x.x,mbr_survey$contract_dt,units=c("days")))
# mbr_survey <- mbr_survey %>%
#   mutate(Q1_1.y = ifelse(cons_gap>= 0 & cons_gap <= 14, Q1_1.y, NA))
# 
mbr_survey$cons_gap2 = as.numeric(difftime(mbr_survey$RecordedDate.y.y,mbr_survey$contract_dt,units=c("days")))
# mbr_survey <- mbr_survey %>%
#   mutate(Q1_1.x.x = ifelse(cons_gap2>= 0 & cons_gap2 <= 14 & acct_type=="ML", Q1_1.x.x, NA)) # survey taken within 2 weeks and for ML only


# Process mbr_survey
mbr_survey <- mbr_survey %>%
  rename(branch_rating = Q1_6, member_rating = Q1_1.x,
         contract_rating = Q1_1.y, mortgage_rating = Q1_1.x.x, email_rating = Q1_1.y.y) %>%
  dplyr::select(-c(RecordedDate.x, RecordedDate.y, RecordedDate.x.x, RecordedDate.y.y, RecordedDate, cons_gap, cons_gap2)) %>%
  distinct()

# Exclude rows that have no survey rating at all
surveys = c("branch_rating", "member_rating", "contract_rating", "mortgage_rating", "email_rating")
mbr_survey$survey_taken <- rowSums( !is.na( mbr_survey [,surveys]))
mbr_survey <- mbr_survey %>% filter(survey_taken > 0)

# Convert NA of the 5 survey ratings to 0
mbr_survey[, surveys][is.na(mbr_survey[, surveys])] <- 0 

write.csv(mbr_survey, "mbr_survey.csv", row.names=FALSE)


############################################
# Merging datasets together and convert from long to wide format
###########################################

# # Clear the R environment
# rm(list = ls())
# 
# # Load the data
# mbr_ln <- read_csv("data/processed/mbr_ln.csv")
# mbr_dp <- read_csv("data/processed/mbr_dp.csv")
# mbr_visas <- read_csv("data/processed/mbr_visas.csv")
# mbr_survey <- read_csv("data/processed/mbr_survey.csv")

# Combine the above datasets
mbr_all <- dplyr::bind_rows(mbr_ln, mbr_dp)
mbr_all <- dplyr::bind_rows(mbr_all, mbr_visas)
mbr_all <- mbr_all %>% left_join(mbr_survey)

# Check NA for each column
na_count <-sapply(mbr_all, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Fill in the NA
# acct_type: fill in VISA
mbr_all %>% group_by(acct_type) %>% count()
mbr_all <- mbr_all %>%
  mutate(acct_type = ifelse(is.na(acct_type),"VISA",acct_type)) %>%
  mutate(acct_type = ifelse(VISA_HE==1 & acct_type=="VISA","HE",acct_type)) %>%
  mutate_at(vars(acct_type), as.factor)


# age: only a small number of NA, use median of a whole dataset
mbr_all <- mbr_all %>% 
  mutate(age= ifelse(is.na(age), median(age, na.rm=TRUE), age))

# Transform the data from long to wide format
# Group by member_id, transform data so that each row has only one member id
mbr_all[mbr_all == -999] <- NA # convert -999 to NA in order to sum and average
mbr_all$acct_type <- as.character(mbr_all$acct_type)
mbr_all$mrm <- as.numeric(mbr_all$mrm) - 1

# For variables that have different values for each account in a membership, aggregate them
mbr_all_agg <- mbr_all %>%
  group_by(period, member_id) %>%
  summarise(total_acct = length(acct_type), CK=sum(acct_type=="CK"), SV=sum(acct_type=="SV"), CD=sum(acct_type=="CD"),
            CRE=sum(acct_type=="CRE"), HE=sum(acct_type=="HE"), IL=sum(acct_type=="IL"),
            LOC=sum(acct_type=="LOC"), ML=sum(acct_type=="ML"), VISA=sum(acct_type=="VISA"),
            mrm = mean(mrm, na.rm = T), trm = mean(trm, na.rm = T), cur_bal = sum(cur_bal, na.rm = T),
            orig_amt = sum(orig_amt, na.rm = T), orig_rate = mean(orig_rate, na.rm=T),
            acc_duration=mean(acc_duration, na.rm=T), online_duration=mean(online_duration, na.rm=T),
            missing_payment=sum(missing_payment, na.rm = T), avg_bal=mean(avg_bal, na.rm=T),
            avg_rate=mean(avg_rate, na.rm=T), total_amt=sum(total_amt, na.rm=T), total_ct=sum(total_ct, na.rm=T),
            avail_bal = sum(avail_bal, na.rm=T), branch_rating = mean(branch_rating, na.rm=T),
            member_rating = mean(member_rating, na.rm=T), contract_rating = mean(contract_rating,na.rm=T),
            mortgage_rating=mean(mortgage_rating,na.rm=T), email_rating=mean(email_rating, na.rm=T))

# For variables that have the same value for each account in a membership, deduplicate them
mbr_all_unique <- mbr_all %>%
  dplyr::select(rim_status_closed, age, type, moving_time, rim_duration, member_id, rim_closed_dt, period) %>%
  distinct()
any(duplicated(mbr_all_unique$member_id)) # There are duplicates

# mbr_all <- mbr_all %>% 
#   arrange(rim_closed_dt) %>%
#   gather(key, value, -member_id, -acct_type) %>%  
#   unite(new.col, c(key, acct_type)) %>%   
#   spread(new.col, value) 

# Merge those above datasets together to create the wide-form mbr_all
mbr_all_final <- mbr_all_unique %>%
  inner_join(mbr_all_agg, by=c("period", "member_id"))

# Check NA for each column
na_count <-sapply(mbr_all_final, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Fill in the NA with 0
mbr_all_final[is.na(mbr_all_final)] <- 0


write.csv(mbr_all_final, "mbr_all_final.csv", row.names=FALSE)


# Create a test input data for UI
test_input <- sample_n(mbr_all_final, 100) %>% distinct(member_id, .keep_all = T)
write.csv(test_input, "test_input.csv", row.names=FALSE)

# References:
# https://www.analyticsvidhya.com/blog/2020/10/the-complete-guide-to-checking-account-churn-prediction-in-bfsi-domain/
# https://www.listendata.com/2016/08/observation-and-performance-window.html