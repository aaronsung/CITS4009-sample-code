#' ---
#' title: "Classification Model Evaluation"
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
## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
#library(ggplot2)
#library(crayon)
#custdata <- read.table('../data/custdata.tsv',header=T,sep='\t')
#load("../data/exampleData.rData")

#' 
#' 
#' # Model Evaluation
#' 
#' ## Model types from evaluation point of view
#' 
#' - **Classification**
#' - Scoring
#' - \textcolor{gray}{Probability estimation}
#' - \textcolor{gray}{Ranking}
#' - Clustering
#' 
#' ## Models to calibrate against
#' 
#' 1. **Null models**
#'    
#'     + Such a model tells us what **low performance** looks like. e.g., a typical null model is one that returns the same answer regardless of what the input is.
#' 
#'     + Note that it can be hard to do as good as the best null model. We always assume the null model we’re comparing to is the
#' best of all possible null models
#' 
#' 
#' 1. **The best single-variable model** tells us what a simple model can achieve.
#'     + How to build single-variable models will be introduced later.
#' 
#' ## Null Models
#' 
#' Null models help to define the lower-bound of performance &mdash;
#' **\textcolor{red}{If you're not out-performing the null model, you're not delivering value.}**
#' 
#' The two most common types of null models are:
#' 
#' 1. a model that returns a single constant answer for all situations; or 
#' 
#' 2. a model that works independently of the data, i.e.,  it doesn't record any important relation or interaction between inputs and outputs.  
#' 
#' For example, 
#' 
#' - In a categorical problem, a model that always returns the most popular
#' category (as this is the easy guess that is least often wrong) is a null model
#' of type 1 above.
#' 
#' - For a score
#' model, the null model is often the average of all the outcomes (as this has
#' the least square deviation from all of the outcomes); and so on. 
#' 
#' ## The  best single variable model
#' 
#' We also suggest comparing any complicated model against the best single variable
#' model you have available. 
#' 
#' \textcolor{red}{A complicated model can't be justified
#' if it doesn't outperform the best single-variable model available from
#' your training data.}
#' 
#' Business analysts have many tools for building
#' effective single-variable models (such as pivot tables), so if your client is an
#' analyst, they're likely looking for performance above this level.
#' 
#' # Evaluating classification models
#' 
#' ## Spam email or not
#' 
## -----------------------------------------------------------------------------------------------------
path <- "../../data_v2/Spambase/spamD.tsv"
spamD <- read.table(path,header=T,sep='\t')
spamTrain <- subset(spamD,spamD$rgroup>=10)
spamTest <- subset(spamD,spamD$rgroup<10)
spamVars <- setdiff(colnames(spamD),list('rgroup','spam'))
spamFormula <- as.formula(paste('spam=="spam"',
                                paste(spamVars,collapse=' + '),
                                sep=' ~ '))
spamModel <- glm(spamFormula,family=binomial(link='logit'),
                 data=spamTrain)
spamTrain$pred <- predict(spamModel,newdata=spamTrain,
                          type='response')
spamTest$pred <- predict(spamModel,newdata=spamTest,
                         type='response')

#' 
#' 
#' ## Sample classifications
#' 
## -----------------------------------------------------------------------------------------------------
sample <- spamTest[c(7,35,224,327),c('spam','pred')]
kable(sample)

#' 
#' ## Confusion matrix
#' 
#' A table counting how often each combination of known
#' outcomes (the truth) occurred in combination with each prediction type. 
#' 
#' \footnotesize
## ----collapse=T---------------------------------------------------------------------------------------
cM <- table(truth=spamTest$spam,prediction=spamTest$pred>0.5)
kable(cM)

#' \normalsize
#' 
#' For a two-by-two confusion matrix, each cell has a special name.
#' 
#' \begin{figure}
#' \includegraphics[width=0.85\linewidth]{confusion-matrix.png}
#' \end{figure}
#' 
#' 
#' 
#' ## Measures 
#' 
#' \begin{figure}
#' \includegraphics[width=0.3\linewidth]{measures.png}
#' \includegraphics[width=0.3\linewidth]{Precisionrecall.png}
#' 
#' \end{figure}
#' 
#' ## Accuracy
#' 
#' \stressi{Accuracy} is by far the most widely known measure of classifier performance. 
#' 
#' - For a classifier,
#' *accuracy* is defined as the number of items categorized correctly divided by the
#' total number of items. 
#' 
#' - It is simply what fraction of the time the classifier is correct. At
#' the very least, you want a classifier to be accurate.
#' 
#'     + $\frac{\TP+\TN}{\TP+\FP+\TN+\FN}$, which is `(cM[1,1]+cM[2,2])/sum(cM)` \linebreak
#'       (recall that `cM` is the confusion matrix computed on a previous slide)
#' 
#' - \textcolor{red}{ACCURACY IS AN INAPPROPRIATE MEASURE FOR UNBALANCED CLASSES}
#'     + Suppose we have a situation where we have a rare event (say, severe complications during
#' childbirth). If the event we're trying to predict is rare (say, around 1% of the population), the null model -&ndash; the rare event never happens &ndash; is very accurate.
#' 
#' ## Precision and Recall
#' 
#' These terms come from the field of information
#' retrieval.
#' 
#' - *Precision* is what fraction of the items the classifier
#' flags as being in the class actually are in the class. 
#' 
#'     + $\frac{\TP}{\TP+\FP}$, which is `cM[2,2]/(cM[2,2]+cM[1,2])`
#' 
#'     + Precision is how often a positive indication turns out to be correct.    
#'     
#' - *Recall* is what fraction of the things
#' that are in the class are detected by the classifier. 
#' 
#'     + $\frac{\TP}{\TP+\FN}$ which is `cM[2,2]/(cM[2,2]+cM[2,1])`
#'     
#' 
#' ## F1 Score
#' 
#' The **F1 score** is a useful combination of precision and recall. If either precision or
#' recall is very small, then F1 is also very small. 
#' 
#' - F1 is defined as the \stress{harmonic mean of precision and recall}:
#' 
#' $$ \frac{2*\precision*\recall}{\precision+\recall}$$
#' 
#' ## Sensitivity and Specificity
#' 
#' Scientists and doctors tend to use *sensitivity* and *specificity*.
#' 
#' - **Sensitivity** is also called the \stress{true positive rate} and is exactly equal to recall:
#' 
#'   $$\text{Sensitivity} = \frac{\TP}{\TP+\FN} $$
#' 
#' - **Specificity** is also called the \stress{true negative rate}:
#'   
#'   $$\text{Specificity} =\frac{\TN}{\TN+\FP}$$
#' 
#' ## Sensitivity and Specificity (cont.)
#' 
#' Both are measures of effect: what
#' fraction of class members are identified as positive and what fraction of non-class members
#' are identified as negative.
#' 
#' - If you flip your labels
#' (switch from spam to non-spam being the class
#' you're trying to identify), you just switch sensitivity and specificity. 
#' 
#' - Any of the so-called
#' \stress{null classifiers} (classifiers that always say positive or always say negative) always
#' return a zero score on either sensitivity or specificity. So useless classifiers always score
#' poorly on at least one of these measures. 
#' 
#' - Unlike *precision* and *accuracy*, *sensitivity*
#' and *specificity* each only involves entries from a single row of the confusion matrix.
#' 
#' ## Sensitivity and Specificity (cont.)
#' 
#' \footnotesize
#' 
## ----eval=T, echo=F, out.width="80%", warning=F-------------------------------------------------------
df <- data.frame(false=c("TN", "FN"), true=c("FP", "TP"),
		 row.names=c("non-spam", "spam"))
colnames(df) <- c("FALSE", "TRUE")
kable(df)

source('08-plotR.R')
# grid.arrange(fig, fig.Sensitivity, fig.Specificity, nrow=1)

#' 
#' \normalsize
#' 
#' ## Identify the right measures that meet business needs
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{business-need.png}
#' \end{figure}
#' 
#' 
#' 
#' 
#' ##   References
#' 
#' - Practical Data Science with R. By Nina Zumel and John Mount, Manning, 2014. (Chapter 5)
#' 
#' - Interpreting ROC: https://web.uvic.ca/~maryam/DMSpring94/Slides/9_roc.pdf
