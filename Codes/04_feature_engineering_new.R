# Objective: Feature engineering

# Load data
#dat <- read_csv("data/processed/mbr_all_final.csv")
dat <- mbr_all_final
dat <- as.data.frame(dat)
cols_factor = c("rim_status_closed", "type", "missing_payment")
dat[cols_factor] <- lapply(dat[cols_factor], as.factor) 
dat <- dat %>% dplyr::select(-c("member_id", "rim_closed_dt", "period"))

# Identify and remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat <- dat %>%
  mutate(rim_duration_outlier = remove_outliers(rim_duration))  %>%
  filter(!is.na(rim_duration_outlier)) 

dat$rim_duration_outlier <- NULL
# Divide balance by 1000 to convert it to thousand unit. It will help to fit the model
dat$cur_bal <- dat$cur_bal/1000
dat$avg_bal <- dat$avg_bal/1000



# Varible selection
# Chi-square Test for variable selection
chi.square <- vector()
p.value <- vector()
cateVar <- dat %>%
  dplyr::select(-rim_status_closed )

for (i in 1:length(cateVar)) {
  p.value[i] <- chisq.test(dat$rim_status_closed , unname(unlist(cateVar[i])), correct = FALSE)[3]$p.value
  chi.square[i] <- unname(chisq.test(dat$rim_status_closed , unname(unlist(cateVar[i])), correct = FALSE)[1]$statistic)
}

chi_sqaure_test <- tibble(variable = names(cateVar)) %>%
  add_column(chi.square = chi.square) %>%
  add_column(p.value = p.value)
knitr::kable(chi_sqaure_test)

# Drop variables that have p value >= 0.05
drop_cols <- chi_sqaure_test %>%
  filter(p.value >= 0.05) %>%
  dplyr::select(variable)
drop_cols <- drop_cols$variable
dat <- dat %>% dplyr::select(-all_of(drop_cols))


# Stratified Random Split dataset to 70% train and 30% test. 
set.seed(1234)
train.index <- createDataPartition(dat$rim_status_closed, p = .7, list = FALSE)
train <- dat[ train.index,]
test  <- dat[-train.index,]

table(train$rim_status_closed) 
round(prop.table(table(train$rim_status_closed)),3) 

table(test$rim_status_closed)
round(prop.table(table(test$rim_status_closed)),3)

# both train and test data sets contain only 1% churn.

write.csv(train, "train_original.csv", row.names=FALSE)
write.csv(test, "test.csv", row.names=FALSE)

# References:
# https://statsandr.com/blog/outliers-detection-in-r/
# https://towardsdatascience.com/chi-square-test-for-feature-selection-in-machine-learning-206b1f0b8223
# https://advstats.psychstat.org/book/mregression/selection.php