# Objective: Libraries for data analysis and models

# Data Manipulation and Analysis packages
library(tidyverse)        # pipe operation
library(reshape2)         # restructure and aggregate data
library(lubridate)        # date manipulation
library(data.table)       # extension of data frame
library(stats)            # statistics

# Feature engineering
library(zoo)              # time series data
library(smotefamily)      # oversampling techniques
library(ROSE)             # random oversampling
library(MASS)             # Support Functions and Datasets
library(ranger)           # recursive partitioning
library(penalized)        # Cross-validation routines allow optimization of the tuning parameters
library(rpart.plot)       # scales and adjusts the displayed tree for best fit
library(ggcorrplot)       # correlation matrix
library(caTools)          # split data
library(doMC)             # parallel backend
registerDoMC(cores=4)

# Visualization
library(patchwork)        # make plot composition
library(vcd)              # visualization for categorical data
library(scales)           # internal scaling infrastructure used by ggplot2
library(knitr)            # dynamic report generation
library(corrplot)         # visual exploratory tool on correlation matrix
library(ROCR)             # Visualizing the Performance of Scoring Classifiers
library(ggmosaic)         # mosaic plot
library(ggplot2)          # making graphs
library(ggfortify)        # plotting tools for statistics
library(pROC)             # draw ROC

# Machine Learning
library(caret)            # streamline the model training process for complex regression and classification problems
library(lme4)             # generalized linear mixed model
library(gridExtra)        # provide useful extensions to the grid system
library(InformationValue) # Performance Analysis and Companion Functions for Binary Classification Models
library(rpart)            # building classification and regression trees
library(randomForest)     # random forest model
library(e1071)            # statistic and probabilistic algorithms (Naive Bayes, SVM, etc.)
library(ModelMetrics)     # F1 Score
library(rms)              # regression modelling

# Survival Analysis
library(survival)         # survival analysis
library(survminer)

# Create R Shiny App
library(shiny)            # create shiny app
library(shinythemes)      # shiny themes
library(DT)

