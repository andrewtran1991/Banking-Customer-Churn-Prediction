# Objective: Cleaning individual datasets and creating the final dataset for analysis


# Clear the R environment
rm(list = ls())

##################
# Clean mbr data
##################

# Load the data
mbr_raw <- read_csv("data/raw/mbr.csv")

# Select a date to filter the data.
# 10/01/2016 to 10/31/2021 (5 years)

since_date = as.Date("2016-10-01")
  

# Filter the data to select only clients whose membership open dates are from 
mbr_raw <- mbr_raw %>% dplyr::filter(rim_open_dt >= since_date)
summary(mbr_raw)
glimpse(mbr_raw)

# check for duplicate in membership id and account id
mbr_raw <- mbr_raw %>% unique()
mbr_raw[duplicated(mbr_raw$member_id),]
mbr_raw[duplicated(mbr_raw$account_id),]

# Create a binary membership closed column
mbr_raw %>% group_by(rim_status) %>% count()
mbr_raw <- mbr_raw %>% mutate(rim_status_closed = ifelse(rim_status=="Closed",1,0))

# Convert some columns to correct data type
cols_factor <- c("rim_status", "type", "mrm", "acct_type", "status", "class_code", "rim_status_closed")
mbr_raw[cols_factor] <- lapply(mbr_raw[cols_factor], as.factor) 
#mbr_raw$rim_closed_dt <- as.Date(mbr_raw$rim_closed_dt, format="%m/%d/%Y")

summary(mbr_raw)
glimpse(mbr_raw)

# Check NA for each column
na_count <-sapply(mbr_raw, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# Select only rows with Active and Closed rim_status 
mbr <- mbr_raw %>% filter(rim_status == "Active" | rim_status == "Closed")

# Preprocess some columns
# Use "10/31/2021" as the end date for na
mbr <- mbr %>% mutate(rim_open_dt = if_else(is.na(rim_open_dt)==TRUE, effective_dt, rim_open_dt),
                      rim_closed_dt = if_else(is.na(rim_closed_dt)==TRUE, as.Date("2021-10-31"), rim_closed_dt),
                      closed_dt = if_else(is.na(closed_dt)==TRUE, as.Date("2021-10-31"), closed_dt))

mbr$rim_duration <- as.numeric(difftime(mbr$rim_closed_dt,mbr$rim_open_dt,units=c("days"))) # membership duration
mbr$acc_duration <- as.numeric(difftime(mbr$closed_dt,mbr$create_dt,units=c("days"))) # account duration
mbr$online_duration <- abs(as.numeric(difftime(mbr$Last_Login_Date,mbr$Enrollment_Date,units=c("days"))))
mbr$online_duration[is.na(mbr$online_duration)==TRUE] <- 0 # online banking duration. 0 day: no online banking or no using.
#mbr$mat_duration <- as.numeric(difftime(mbr$mat_dt,mbr$closed_dt,units=c("days")))

# Count the number of missing data by account type
na_count_by_acct_type <- mbr %>% group_by(acct_type) %>%
  summarise_all(list(~sum(is.na(.))))

# Fill in missing data
# Missing age: group by rim_open_dt and use median
mbr <- mbr %>% 
  group_by(rim_open_dt) %>% 
  mutate(age= ifelse(is.na(age), median(age, na.rm=TRUE), age))

# Missing term: CK and SV don't have terms (NA = -999); for other accounts, use average
mbr <- mbr %>%
  group_by(acct_type) %>%
  mutate(trm= ifelse(is.na(trm), mean(trm, na.rm=TRUE), trm)) %>%
  mutate(trm= ifelse(is.nan(trm), -999, trm))

# Missing original amount: orig_amt is not applicable for CK and SV (NA = -999); for other accounts, use average
mbr <- mbr %>%
  group_by(acct_type) %>%
  mutate(orig_amt= ifelse(is.na(orig_amt), mean(orig_amt, na.rm=TRUE), orig_amt)) %>%
  mutate(orig_amt= ifelse(is.nan(orig_amt), -999, orig_amt))

# Missing original rate: orig_rate is not applicable for CRE, HE, IL, LOC and ML(NA = -999); for other account, use average
mbr <- mbr %>%
  group_by(acct_type) %>%
  mutate(orig_rate= ifelse(is.na(orig_rate), mean(orig_rate, na.rm=TRUE), orig_rate)) %>%
  mutate(orig_rate= ifelse(is.nan(orig_rate), -999, orig_rate))

# Missing maturity date: this means that the clients missed payments. It is not applicable for CK and SV.
# Create a binary missing payment variable
mbr <- mbr %>%
  mutate(missing_payment = ifelse(is.na(mat_dt),1,0)) %>%
  mutate(missing_payment = ifelse(acct_type=="CK" | acct_type=="SV", 0, missing_payment))

# Load in the "moving_by_zip" data
# Compute how many time a member moved. Then merge this data into the mbr
moving_by_zip <- read_csv("data/raw/moving_by_zip.csv")
moving_by_zip <- moving_by_zip %>%
  arrange(addr_chg_dt) %>%
  group_by(member_id) %>%
  summarise(moving_time = length(new_zip), cur_zip = last(new_zip))

mbr <- mbr %>% left_join(moving_by_zip, by="member_id")
mbr <- mbr %>% mutate(moving_time = ifelse(is.na(moving_time), 0, moving_time),  # NA moving = 0
                      zip = ifelse(is.na(zip), cur_zip, zip)) # Replace a missing zip with an available cur_zip

# Missing zipcode: use -999
mbr <- mbr %>%
  mutate(zip = ifelse(is.na(zip), -999, zip))

# Select necessary variables for the clean dataset
# The rim_closed_dt is kept in order to make time series data later
mbr_clean <- mbr %>% 
  dplyr::select(rim_status_closed, age, type, mrm, acct_type, status, class_code, trm, cur_bal, orig_amt, orig_rate,
                zip, moving_time, rim_duration, acc_duration, online_duration, missing_payment, member_id, account_id, 
                rim_open_dt, rim_closed_dt, contract_dt, create_dt, closed_dt, Enrollment_Date, Last_Login_Date)

write.csv(mbr_clean, "mbr_clean.csv", row.names=FALSE)

# Remove unused files
rm(mbr_raw)
rm(mbr)

##################
# Clean bal_hist data
##################

# Load the data
bal_hist_raw <- read_csv("data/raw/bal_hist.csv")
bal_hist_raw <- bal_hist_raw %>% dplyr::filter(period_end_dt >= since_date)

summary(bal_hist_raw)
glimpse(bal_hist_raw)

# Deduplicate the data
bal_hist_raw <- bal_hist_raw %>% distinct()

# Create 2 different-account-type datasets (one is for CD, CK and SV; one is for the other types)
dp_acct = c("CD", "CK", "SV")
bal_dp <- bal_hist_raw %>% filter(acct_type %in% dp_acct)
bal_loan <- bal_hist_raw %>% filter(!(acct_type %in% dp_acct))
rm(bal_hist_raw)

write.csv(bal_dp, "bal_dp.csv")
write.csv(bal_loan, "bal_loan.csv")

##################
# Clean dp_trans data
##################

# Load the data
dp_trans <- read_csv("data/raw/dp_trans.csv")
summary(dp_trans)
glimpse(dp_trans)

# Find duplicate for each column
na_count <-sapply(dp_trans, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# There are 36,965 rows that have no account ids and member ids. Exclude these rows
dp_trans <- dp_trans %>% filter(!is.na(account_id))

# convert YM to YMD
dp_trans <- dp_trans %>% mutate(mon_date = as.Date(paste(mon,"-01",sep="")))
dp_trans$mon_date <- ceiling_date(dp_trans$mon_date, "month") - days(1)

##################
# Clean ln_trans data
##################

# Load the data
ln_trans <- read_csv("data/raw/ln_trans.csv")
summary(ln_trans)
glimpse(ln_trans)

# Find duplicate for each column
na_count <-sapply(ln_trans, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# There are 4,270 rows that have no account ids and member ids. Exclude these rows
ln_trans <- ln_trans %>% filter(!is.na(account_id))

# convert YM to YMD
ln_trans <- ln_trans %>% mutate(mon_date = as.Date(paste(mon,"-01",sep="")))

# convert YM to YMD
ln_trans <- ln_trans %>% mutate(mon_date = as.Date(paste(mon,"-01",sep="")))
ln_trans$mon_date <- ceiling_date(ln_trans$mon_date, "month") - days(1)

##################
# Clean visas data
##################
# Load the data
#mbr_clean <- read_csv("data/processed/mbr_clean.csv")
visas <- read_csv("data/raw/visas.csv")
visas <- visas %>% distinct()

# Calculate the duration of having visa credit card
# Use "10/31/2021" as the end date for na
visas <- visas %>%
  mutate(closed_dt = if_else(is.na(closed_dt)==TRUE, as.Date("2021-10-31"), closed_dt))
visas$acc_duration = as.numeric(difftime(visas$closed_dt,visas$effective_dt,units=c("days")))

# Convert data type
visas$status <- as.factor(visas$status)
visas$ext_status_code <- as.factor(visas$ext_status_code)

# Check for duplicated member_id in visas data
duplicated(visas$member_id) #there are duplicated data
visas <- visas %>% dplyr::select(-c("acct_type"))

##################
# Clean survey data
##################
# Load the data
# mbr_clean <- read_csv("data/processed/mbr_clean.csv")

# branch survey
branch_survey <- read_csv("data/raw/branch_survey.csv")
branch_survey$RecordedDate <-  as.Date(branch_survey$RecordedDate, format="%m/%d/%Y %H:%M")
any(duplicated(branch_survey$member_id))
branch_survey %>% group_by(member_id) %>% mutate(dupe = n()>1) %>% filter(dupe==TRUE) # There are duplicates

# take the last rating for each member's branch survey
branch_survey <- branch_survey %>%
  arrange(RecordedDate) %>%
  group_by(member_id) %>%
  slice(n())

# New member survey
new_mbr_survey <- read_csv("data/raw/new_mbr_survey.csv")
new_mbr_survey$RecordedDate <-  as.Date(new_mbr_survey$RecordedDate, format="%m/%d/%Y %H:%M")
any(duplicated(new_mbr_survey$member_id)) # There are duplicates
new_mbr_survey %>% group_by(member_id) %>% mutate(dupe = n()>1) %>% filter(dupe==TRUE) 
new_mbr_survey <- new_mbr_survey %>% filter(!(is.na(member_id))) 


# New contract survey
new_cons_survey <- read.csv("data/raw/new_cons_survey.csv")
new_cons_survey$RecordedDate <-  as.Date(new_cons_survey$RecordedDate, format="%m/%d/%Y %H:%M")
any(duplicated(new_cons_survey$member_id)) # There are duplicates
new_cons_survey %>% group_by(member_id) %>% mutate(dupe = n()>1) %>% filter(dupe==TRUE) 
new_cons_survey <- new_cons_survey %>% filter(!(is.na(member_id)))

# New mortgage survey
new_mtg_survey <- read.csv("data/raw/new_mtg_survey.csv")
new_mtg_survey$RecordedDate <-  as.Date(new_mtg_survey$RecordedDate, format="%m/%d/%Y %H:%M")
any(duplicated(new_mtg_survey$member_id)) # There are duplicates
new_mtg_survey %>% group_by(member_id) %>% mutate(dupe = n()>1) %>% filter(dupe==TRUE) 
new_mtg_survey <- new_mtg_survey %>% filter(!(is.na(member_id)))

# Email survey
sec_email_survey <- read.csv("data/raw/sec_email_survey.csv")
sec_email_survey$RecordedDate <-  as.Date(sec_email_survey$RecordedDate, format="%m/%d/%Y %H:%M")
any(duplicated(sec_email_survey$member_id)) # There are duplicates
sec_email_survey %>% group_by(member_id) %>% mutate(dupe = n()>1) %>% filter(dupe==TRUE) 
sec_email_survey <- sec_email_survey %>% filter(!(is.na(member_id)))
sec_email_survey <- sec_email_survey %>%
  arrange(RecordedDate) %>%
  group_by(member_id) %>%
  slice(n())  # Take the last email survey