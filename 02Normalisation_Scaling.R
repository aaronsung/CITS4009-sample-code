#' ---
#' title: "Normalisation and Scaling"
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
## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' 
#' #  Data Transformation
#' 
#' ## Basic Data Transformation actions
#' 
#' - Recoding variables (previous lecture)
#' - Renaming variables (previous lecture)
#' - Dealing with Missing values (previous lecture)
#' - Dealing with Dates (previous lecture)
#' - Type Conversions
#' - Sorting data
#' - Merging datasets
#' - Subsetting datasets
#' - Use SQL statements to manipulate data frames 
#' - Use pipes
#' - Sampling
#' 
#' 
#' # Normalisation and Scaling
#' 
#' ## Normalise by mean
#' 
#' Normalisation is useful when absolute quantities are less meaningful than relative ones.
#' 
#' 
## ------------------------------------------------------------------------------------
mean.age <- mean(custdata$age)
custdata$age.normalised <- custdata$age/mean.age

#' 
#' The age much less than 1 signifies an unusually young customer;
#' much greater than 1 signifies an unusually old customer.
#' 
#' But what constitutes
#' "much less" or "much greater" than 1? &mdash; that depends on the age spread of your customers.
#' 
#' ## Z-normalisation
#' 
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{age.png}
#' \end{figure}
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
mean.age <- mean(custdata$age)
std.age <- sd(custdata$age)
custdata$age.normalised <- (custdata$age-mean.age)/std.age

#' \normalsize
#' 
#' ## Z-normalisation (cont.)
#' 
#' \footnotesize
## ----collapse=T----------------------------------------------------------------------
cat("mean.age =", mean.age, "std.age =", std.age)
# ages and normalised ages of 6 random customers
indices <- sample(1:nrow(custdata), 6, replace=F)
custdata[indices, c("age", "age.normalised")]

#' \normalsize
#' 
#' Now values less than `-1` signify customers younger than typical; </br>
#' values greater than `1` signify customers older than typical.
#' 
#' 
#' ## Standard Deviation
#' The common interpretation of standard deviation as a unit of distance
#' implicitly assumes that the data is distributed
#'  normally. 
#' 
#' For a normal distribution,
#' 
#' - roughly two-thirds of the data (about 68%) is within $\pm 1$ standard deviation from the mean. 
#' - About 95% of the data is within $\pm 2$ standard deviations from the mean. 
#' 
#' (`pnorm(1) - pnorm(-1) = 0.6826895`; \linebreak
#' `pnorm(2) - pnorm(-2) = 0.9544997`)
#' 
#' You can still use this transformation even if the data isn't normally
#' distributed, but the standard deviation is most meaningful
#' as a unit of distance if the data is unimodal and roughly symmetric
#' around the mean.
#' 
#' 
#' ##   Take home messages
#' 
#' 
#' - Appropriate data transformations can make the data easier to understand and
#' easier to model.
#' 
#' - Normalisation and re-scaling are important when relative changes are more
#' important than absolute ones.
#' 
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 4)
