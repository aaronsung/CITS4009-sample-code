#' ---
#' title: "Classification: Data Preparation"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Liu"
#' institute: |
#'     | Department of Computer Science and Software Engineering
#'     | The University of Western Australia
#' graphics: yes
#' date: "Semester 2, 2023"
#'    
#' output:
#'   beamer_presentation:
#'     theme: "AnnArbor"
#'     colortheme: "dolphin"
#'     fonttheme: "structurebold"
#'     includes:
#'         in_header: "../common.tex"
#' ---
#'   
## ----setup, include=FALSE--------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)


#' 
#' ## KDD Cup 2009 Data
#' 
#' The KDD Cup 2009 provided a dataset about customer relationship management.
#' 
#' - The contest supplied 230 facts (features) about 50,000 credit card accounts. 
#' 
#' - The goal was to predict 
#'     + account cancellation (called \stressi{churn}), 
#'     + the innate tendency to use new products and services (called \stressi{appetency}), and 
#'     + willingness to respond favorably to marketing pitches (called \stressi{upselling})
#'     
#' Data Available at: 
#' 
#' https://github.com/WinVector/zmPDSwR
#' 
#' ## Why Three-way splitting
#' 
#' This problem has 
#' 
#' - a large number of variables, many of which have a large number
#' of possible levels. 
#' 
#' - The competition's \stress{AUC} (\stressi{area under curve}) measure, isn't particularly resistant
#' to *overfitting* (not having built-in model complexity or chance corrections). 
#' 
#' Because of this concern, we'll split our data into three sets: training, calibration, and test. 
#' 
#' - Use the training set for most of our work, and
#' we'll never look at the test set (we'll reserve it for our final report of model performance).
#' - The calibration set is used to simulate the unseen test set during modeling-
#'     + performance on the calibration set is used to estimate overfitting. 
#' 
#' ## KDD2009 Data Pre-processing 
#' 
#' \footnotesize
## ----collapse=T------------------------------------------------------------------
path <- '../../data_v2/KDD2009/'
d <- read.table(paste0(path, 'orange_small_train.data.gz'), 
                header=T, sep='\t', na.strings=c('NA',''))

churn <- read.table(
  paste0(path,'orange_small_train_churn.labels.txt'),
  header=F,sep='\t')
d$churn <- churn$V1             #___churn___
appetency <- read.table(
  paste0(path,'orange_small_train_appetency.labels.txt'),
  header=F,sep='\t')
d$appetency <- appetency$V1    # ___appetency___
upselling <- read.table(
  paste0(path,'orange_small_train_upselling.labels.txt'),
  header=F,sep='\t')
d$upselling <- upselling$V1    # ___upselling___

# d - data frame having 5000 rows and 233 (=230+3) columns;
# churn, appetency, upselling - all having 5000 rows and 1 column.

#' \normalsize
#' 
#' ## KDD2009 Data Pre-processing (Cont.)
#' 
#' \footnotesize
## --------------------------------------------------------------------------------
# do a 90/10 split to form the training and test sets.
set.seed(729375)
d$rgroup <- runif(dim(d)[1])
dTrainAll <- subset(d, rgroup<=0.9)
dTest <- subset(d, rgroup>0.9)
outcomes <- c('churn', 'appetency', 'upselling')
# names of columns that are categorical type and numerical type
vars <- setdiff(colnames(dTrainAll),  c(outcomes, 'rgroup'))
catVars <- vars[sapply(dTrainAll[, vars], class) %in% 
                  c('factor', 'character')]
numericVars <- vars[sapply(dTrainAll[, vars], class) %in%
                      c('numeric', 'integer')]
# remove the original tables
rm(list=c('d', 'churn', 'appetency', 'upselling'))

# split dTrainAll into a training set and a validation (or calibration) set
useForCal <- rbinom(n=dim(dTrainAll)[1], size=1, prob=0.1)>0
dCal <- subset(dTrainAll, useForCal)
dTrain <- subset(dTrainAll, !useForCal)

#' \normalsize
#' 
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Section 8.1-8.2)
#' 
#' - See https://github.com/WinVector/PDSwR2 for all the datasets provided by the authors Zumel et al for the book above.
#' 
#' - See https://github.com/WinVector/PDSwR2/raw/master/CodeExamples.zip for all the R code that produces all the results and almost all the plots in the book above.
