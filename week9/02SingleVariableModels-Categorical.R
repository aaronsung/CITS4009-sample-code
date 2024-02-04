#' ---
#' title: "Single Variable Classification &ndash; Categorical"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Liu"
#' institute: |
#'     | Department of Computer Science and Software Engineering
#'     | The University of Western Australia
#' graphics: yes
#' date: "Semester 2, 2023"
#' header-includes:
#'    - \usepackage[utf8]{inputenc}
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
library(gridExtra)

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

set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
outcomes <- c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in% 
                  c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars],class) %in%
                      c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))

useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)


#' 
#' 
#' ## Classification and Scoring methods in data science
#' 
#' - The simplest classification method is one that always returns 
#'     the majority category (a typical \stress{Null model})
#' 
#' - The simplest scoring (regression) method is one that always returns the
#'     average value of a subset of the original training data (a typical \stress{Null model})
#' 
#' - Other simpler methods (can be used for classification or scoring) include:
#'     + the single variable method (similar to the analyst's pivot table)
#' 
#'     + the nearest neighbor method
#'       
#'     + the Naïve Bayes method.
#' 
#' 
#' # Building Single Variable Models - Categorical
#' 
#' ## Single categorical variable model using `table()`
#' 
#' A single-variable model based on categorical features is easiest to describe as a table.
#' 
#' - Business analysts use what's called a \stressi{pivot table} (which promotes values or
#' levels of a feature to be families of new columns) and 
#' 
#' - statisticians use what's called a
#' \stressi{contingency table} (where each possibility is given a column name). 
#' 
#' In either case, the R
#' command to produce a table is `table()`. 
#' 
#' ## Single categorical variable model &ndash; example 
#' 
#' \scriptsize
## ----collapse=T------------------------------------------------------------------
outcome <- 'churn'  # We can also try the 'appetency' and 'upselling' columns.
pos <- '1'          # We are interested in when 'churn' is positive. We need
                    # to put quotes around number 1 as it is the column name
                    # of the table created.
# Column 'Var218' is a categorical column containing 'cJvF' and 'UYBR'.
table218 <- table(Var218=dTrain[,'Var218'], churn=dTrain[,outcome], useNA='ifany')
kable(table218)

print(table218[,2] / (table218[,1] + table218[,2]))

#' 
#' What does this mean when we use `Var218` to predict the outcome `churn`?
#' &mdash; if the value of `Var218` is equal to `cJvF` then the predictor
#' has $0.0596$ probability of outputting a 1.
#' 
#' \normalsize
#' 
#' ## Function to repeat model building
#' 
#' \footnotesize
## ----collapse=T------------------------------------------------------------------
# outCol: vector holding the values (known in the training step) of the
#         output column that we want to predict, e.g., the 'churn' column.
# varCol: the single variable column that is of interest. Can we use this
#         column alone to predict outCol?
# appCol: after building the model, we can apply it to this column (same
#         as varCol but may come from the calibration or test set).
mkPredC <- function(outCol, varCol, appCol) {
  pPos <- sum(outCol == pos) / length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol), varCol)
  pPosWv <- (vTab[pos, ] + 1.0e-3*pPos) / (colSums(vTab) + 1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}

#' 
#' ## Code with annotation
#' 
#' \begin{figure}
#' \includegraphics[width=1.0\linewidth]{fun_cat_single.png}
#' \end{figure}
#' 
#' 
#' 
#' ## Predict for all categorical variables
#' 
#' \scriptsize
## ----collapse=T------------------------------------------------------------------
# call the mkPredC() function for all the categorical columns
for(v in catVars) {
  pi <- paste('pred', v, sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome], dTrain[,v], dTest[,v])
}

#' \normalsize
#' 
#' Recall that `outcome` was set to the value `churn` in an earlier slide.
#' So we try to use each categorical column in `catVars` to predict
#' when `churn` is positive (i.e., equal to $1$).
#' 
#' ## Predict for all categorical variables (cont.)
#' 
#' \scriptsize
## ----collapse=T------------------------------------------------------------------
# We can inspect a few rows of the output column that has been added
# to dTrain for the categorical variable 'Var194'.
factor(dTrain[,"Var194"])["Levels"]  # how many levels are there?

rows <- c(620, 725, 9502, 40310)
dTrain[rows, c("Var194", "predVar194")]
rows <- c(842, 2885, 4507, 4510)
dCal[rows, c("Var194", "predVar194")]

#' \normalsize
#' 
#' What are the measures that we can use to evaluate the predictions
#' above?
#' 
#' ## Area under ROC curve (AUC)
#' 
#' \begin{figure}
#' \includegraphics[width=0.3\linewidth]{auc-roc.png}
#' \end{figure}
#' $$TPR/Recall/Sensitivity=\frac{TP}{TP+FN}$$
#' $$Specificity=\frac{TN}{TN+FP} \text{ \textit{Specificity} is also known as \textit{TNR})}$$
#' $$FPR=1-Specificity=\frac{FP}{TN+FP}$$
#' 
#' ## Evaluate
#' 
#' Every classifier evaluation using ROCR starts with creating a \textcolor{blue}{prediction} object created using the `prediction()` function. 
#' 
#' The `performance()` function takes a prediction object and an evaluation metric to work out the relevant measurement.
#' 
## --------------------------------------------------------------------------------
library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


#' 
#' ## Evaluate (cont.)
#' 
#' \footnotesize
## --------------------------------------------------------------------------------
for(v in catVars) {
  pi <- paste('pred', v, sep='')
  aucTrain <- calcAUC(dTrain[,pi], dTrain[,outcome])
  if (aucTrain >= 0.8) {
    aucCal <- calcAUC(dCal[,pi], dCal[,outcome])
    print(sprintf(
      "%s: trainAUC: %4.3f; calibrationAUC: %4.3f",
      pi, aucTrain, aucCal))
  }
}

#' \normalsize
#' 
#' ## 100-fold cross-validation
#' 
#' Let's inspect the AUC values of two example categorical columns a bit more.
#' 
#' \footnotesize
## ----collapse=T------------------------------------------------------------------
vars <- c('Var200', 'Var217')

for (var in vars) {
  aucs <- rep(0,100)
  for (rep in 1:length(aucs)) {
    useForCalRep <- rbinom(n=nrow(dTrainAll), size=1, prob=0.1) > 0
    predRep <- mkPredC(dTrainAll[!useForCalRep, outcome],
                     dTrainAll[!useForCalRep, var],
                     dTrainAll[useForCalRep, var])
    aucs[rep] <- calcAUC(predRep, dTrainAll[useForCalRep, outcome])
  }
  print(sprintf("%s: mean: %4.3f; sd: %4.3f", var, mean(aucs), sd(aucs)))
}

#' \normalsize
#' 
#' ## Messages from the cross-validation
#' 
#' The 100-fold repeated estimates of the AUC give a mean of $0.549$ for `Var200`
#' and $0.554$ for `Var217`, each has standard deviation: $0.014$.
#' 
#' - So the original AUC estimate of $0.565$ for `Var200` was a bit high,
#'   but our original AUC estimate of $0.553$ for `Var217` was very good.
#' 
#' - In some modelling circumstances, training set estimations
#' are good enough (linear regression is often such an example). 
#' 
#' - In many other circumstances, estimations from a single calibration set are good enough. 
#' 
#' - In extreme cases (such as fitting models with very many variables or level values), you're well
#' advised to use replicated cross-validation estimates of variable utilities and model fits.
#' 
#' - It's critical to automate the modelling steps so that we can perform cross-validation studies. 
#' 
#' ## Double density plot &ndash; useful for inspecting the predicted probabilities
#' 
#' \tiny
## ----out.width="85%", collapse=T, fig.asp=0.4------------------------------------
str(factor(dTrain[,"Var200"]))
str(factor(dTrain[,"Var217"]))
fig1 <- ggplot(dCal) + geom_density(aes(x=predVar200, color=as.factor(churn)))
fig2 <- ggplot(dCal) + geom_density(aes(x=predVar217, color=as.factor(churn)))
grid.arrange(fig1, fig2, ncol=2)

#' 
#' ## Plotting the ROC curve
#' 
#' \scriptsize
## ----out.width="30%", fig.asp=1--------------------------------------------------
library(ROCit)
# colour_id 1-7 are: black,red,green,blue,cyan,purple,gold
plot_roc <- function(predcol, outcol, colour_id=2, overlaid=F) {
    ROCit_obj <- rocit(score=predcol, class=outcol==pos)
    par(new=overlaid)
    plot(ROCit_obj, col = c(colour_id, 1),
       legend = FALSE, YIndex = FALSE, values = FALSE)
}
plot_roc(dCal$predVar200, dCal[,outcome])  #red
plot_roc(dCal$predVar217, dCal[,outcome], colour_id=3, overlaid=T) # green

#' \normalsize
#' 
#' ## Interpret and choose the best single variable model
#' 
#' As expected, each variable's training AUC is inflated compared to its calibration
#' AUC. 
#' 
#' - This is because many of these variables have thousands of levels. 
#'     + For example, `length(unique(dTrain$Var217))` is $12,434$
#' - A good trick to work around this is to sort the variables by their AUC
#' score on the calibration set (not seen during training), which is a better estimate of the
#' variable's true utility. 
#' 
#' - In our case, the most promising variable is variable 206, which has
#' both training and calibration AUCs of $0.59$. 
#' 
#' - The winning KDD entry, which was a model
#' that combined evidence from multiple features, had a much larger AUC of $0.76$.
#' 
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Section 8.1-8.2)
#' 
#' - See https://github.com/WinVector/PDSwR2 and 
#'   e https://github.com/WinVector/PDSwR2/raw/master/CodeExamples.zip for all the datasets and code provided by the authors Zumel et al for the book above.
#' 
#' 
