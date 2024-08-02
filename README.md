
# Predicting Banking Customer Churn

This project aims to develop a predictive model for banking customer churn using various machine learning techniques. The repository contains scripts for data cleaning, exploratory data analysis (EDA), feature engineering, model training, and a customer churn prediction application.

## Table of Contents

- [Introduction](#introduction)
- [Project Structure](#project-structure)
- [Installation](#installation)
- [Usage](#usage)
- [Files Description](#files-description)
- [Contributing](#contributing)
- [License](#license)

## Introduction

Customer churn is a significant issue in the banking industry. Identifying customers who are likely to leave the bank can help in taking proactive measures to retain them. This project uses historical customer data to build models that predict the likelihood of churn.

## Project Structure

```
.
├── 01_libraries.R
├── 02a_data_cleaning_new.R
├── 02b_data_cleaning_new.R
├── 03_EDA.Rmd
├── 04_feature_engineering_new.R
├── 05_feature_engineering_2_new.R
├── 06_ML_models.R
├── 07_survival_analysis.R
├── 08_customer_churn_app.R
├── data
│   └── (Your data files here)
├── README.md
└── .gitignore
```

## Installation

To run this project, you need to have R and RStudio installed on your machine. Additionally, you will need to install several R packages. You can do this by running the following commands in your R console:

```R
install.packages(c("dplyr", "ggplot2", "caret", "survival", "shiny", "randomForest", "xgboost"))
```

## Usage

1. **Data Cleaning:** Run the data cleaning scripts to preprocess the raw data.
   - `01_libraries.R`
   - `02a_data_cleaning_new.R`
   - `02b_data_cleaning_new.R`

3. **Exploratory Data Analysis (EDA):** Perform EDA to understand the data.
   - `03_EDA.Rmd`

4. **Feature Engineering:** Create features that will be used for model training.
   - `04_feature_engineering_new.R`
   - `05_feature_engineering_2_new.R`

5. **Model Training:** Train machine learning models to predict customer churn.
   - `06_ML_models.R`

6. **Survival Analysis:** Conduct survival analysis to understand customer churn over time.
   - `07_survival_analysis.R`

7. **Customer Churn Application:** Use the Shiny app for interactive customer churn prediction.
   - `08_customer_churn_app.R`

## Files Description
- **01_libraries.R:** Script for loading required libraries.
- **02a_data_cleaning_new.R:** Script for initial data cleaning and preprocessing.
- **02b_data_cleaning_new.R:** Continuation of data cleaning and preprocessing.
- **03_EDA.Rmd:** R Markdown file for exploratory data analysis.
- **04_feature_engineering_new.R:** Script for creating new features from the cleaned data.
- **05_feature_engineering_2_new.R:** Additional feature engineering steps.
- **06_ML_models.R:** Script for training various machine learning models.
- **07_survival_analysis.R:** Script for conducting survival analysis.
- **08_customer_churn_app.R:** Shiny application for customer churn prediction.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.

---
