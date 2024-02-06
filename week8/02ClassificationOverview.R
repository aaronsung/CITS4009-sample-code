#' ---
#' title: "An Overview on Classification"
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
## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')

#' 
#' 
#' ## Classification - automatically categorise products
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{product-classification.png}
#' \end{figure}
#' 
#' ##  What is classification?
#' 
#' Learn from data to decide how to assign labels to an
#' object. The response variable is categorical. 
#' 
#' Also known as \stress{supervised learning}, because we need \textcolor{blue}{labelled training data}. 
#'     
#' ##  What information do we need for classification?
#' 
#' **Feature collection**
#' 
#' - For example, collect product attributes and/or text descriptions of known products
#'     
#' **Build a training set**
#' 
#' - Building training data is the major expense for most
#' classification tasks, especially text-related ones.
#' 
#' - Multi-category vs two category classification
#'     + Most classification algorithms are specialized for
#' two-category, binary, or binomial, classification
#'     + Use ``one versus rest'' classifier for multi-class, better with dedicated multi-class classifiers. 
#' 
#' 
#' ## Common classification methods: **Naïve Bayes**
#' 
#' - Especially useful for problems with many
#'   input variables, categorical input variables with a very large number of
#'                   possible values, and text classification. 
#'                   
#' - **Naïve Bayes**:  Bayes would be a good
#'                   first attempt at solving the product categorization problem.
#' 
#' ## Common classification methods: **Naïve Bayes** (An example)
#' 
#' Let's say we have 300 birds and 300 dogs. From these animals,
#' we obtain the frequencies for the features indicated by the column names below:
#' 
## ----echo=F-------------------------------------------------------------------------------------------
NB_example <- data.frame(
    can.swim=c(74, 250), is.brown=c(57,62))
rownames(NB_example) <- c("Bird", "Dog")

#' 
#' \footnotesize
## ----echo=F, collapse=T-------------------------------------------------------------------------------
kable(NB_example)

#' (The numbers in the table are the frequencies of birds and dogs that can swim and that are brown, e.g., 74 out of the 300 birds can swim and the remaining 226 can't swim.)
#' \normalsize
#' 
#' Suppose that now we have an unknown animal having the following features:
#' `can.swim=TRUE` and `is.brown=TRUE`. What is the probability that it is a
#' bird? or a dog?
#' 
#' 
#' ## **Naïve Bayes** (An example) (cont.)
#' 
#' Naïve Bayes follows the Bayes rule, but it makes the "naïve" assumption that the input variables are independent:
#' 
#' \scriptsize
#' $$
#' P(\text{Bird }|\text{ can.swim} \,\& \,\text{is.brown}) =
#' \frac{P(\text{Bird } \& \text{ can.swim } \& \text{ is.brown})}
#' {P(\text{can.swim } \& \text{ is.brown})}$$
#' $$=\frac{P(\text{can.swim } \& \text{ is.brown } | \text{ Bird}) \,P(\text{Bird})}
#' {\sum\limits_{\text{animal=Bird,Dog}} P(\text{can.swim } \& \text{ is.brown } | \text{ animal}) \, P(\text{animal})}$$
#' $$=\frac{P(\text{can.swim } | \text{ Bird}) \, P(\text{is.brown } | \text{ Bird}) \, P(\text{Bird})}
#' {\sum\limits_{\text{animal=Bird,Dog}} P(\text{can.swim } \& \text{ is.brown } | \text{ animal}) \, P(\text{animal})}
#' $$
#' \normalsize
#' 
#' The independence assumption means $P(\text{can.swim } \& \text{ is.brown } | \text{ bird}) =
#' P(\text{can.swim } | \text{ bird}) \, P(\text{is.brown } | \text{ bird})$
#' 
#' The denominator term is the same for `dog`. So we only need
#' to focus on the numerator term.
#' 
#' ## **Naïve Bayes** (An example) (cont.)
#' 
#' \scriptsize
#' $$P(\text{can.swim } \& \text{ is.brown } | \text{ Bird}) \,P(\text{Bird}) =
#' \frac{74}{300}  \times \frac{57}{300} \times \frac{1}{2} = 0.02343333$$
#' 
#' \normalsize
#' 
#' 
#' \scriptsize
#' 
#' Similarly,
#' $$P(\text{can.swim } \& \text{ is.brown } | \text{ Dog}) \,P(\text{Dog}) =
#' \frac{250}{300} \times \frac{62}{300} \times \frac{1}{2} = 0.08611111$$
#' 
#' The denominator is just the total of the two numbers: $0.1095444$
#' 
#' Thus, given that the unknown animal can swim and is brown, the probability
#' 
#' * that it is a bird is $0.02343333 ~/~ 0.1095444 = 0.2139163$
#' * that it is a dog is $0.08611111 ~/~ 0.1095444 = 0.7860841$
#' 
#' \normalsize
#' 
#' Since dog has the highest probability, the predicted animal
#' is `dog`.
#' 
#' ## **Naïve Bayes classifier** 
#' 
#' Naïve Bayes works well on problems that have categorical variables.
#' 
#' The `naivebayes` library provides a `naive_bayes` function for training a Naïve Bayes classifier. For example, if our training set `df` (a data frame) is (only the first 6 rows are shown):
#' 
#' \scriptsize
## ----echo=F-------------------------------------------------------------------------------------------
library(knitr)
set.seed(123)
df <- data.frame(
    can.swim = c(sample(rep(c(T,F), c(74,226))), sample(rep(c(T,F), c(250,50)))),
    is.brown = c(sample(rep(c(T,F), c(57,243))), sample(rep(c(T,F), c(62,238)))),
    label = c(rep("Bird",300), rep("Dog", 300))
)
df <- df[sample(1:nrow(df)),]
rownames(df) <- NULL
kable(df[1:6,])

#' \normalsize
#' then training a Naïve Bayes classifier on `df` is simply:
#' \footnotesize
## ----results="hide"-----------------------------------------------------------------------------------
library(naivebayes)
model <- naive_bayes(formula=label ~ can.swim + is.brown, data=df)

#' 
#' ## Common classification methods: **Decision Trees**                 
#' 
#' 
#' - Useful when the input variables interact with the output in ``if-then" ways 
#'                   
#'     + `IF age > 65, THEN has.health.insurance=T` 
#'     
#' - They are also suitable when inputs have an AND relationship to each other when input variables are redundant or correlated. 
#'     + `IF age < 25 AND student=T, THEN...`
#'     
#' - The decision rules are more interpretable for non-technical users than the decision processes
#'                   of other classifiers. 
#' 
#' 
#' ## Common classification methods: **Decision Trees** (Cont.)
#' 
#' An example: `IF age > 65, THEN has.health.insurance=T`
#' 
#' \small
## ----collapse=T---------------------------------------------------------------------------------------
library(dplyr)
# create a small data frame df to deal with seniors (65+ years old)
df <- within(custdata, {is.senior <- age > 65}) %>%
      subset(select=c("is.senior","health.ins"))

# ggplot(df) + geom_bar(aes(x=is.senior, fill=health.ins),
#                       position="fill") +
#        labs(x="is senior", y="proportion")

# compute the proportion of seniors having health insurance
p <- sum( df$is.senior & df$health.ins ) / sum( df$is.senior )

cat("Proportion of seniors having health insurance is: ", p)

#' \normalsize
#' 
#' ## Common classification methods: **Logistic Regression**
#' 
#' 
#' - Useful for estimating the class probabilities in addition to class assignments. 
#'                      
#' - A good first choice method for binary classification problems.
#' 
#' - Probably the most important (and most used) member of a class
#'   of models called \stressi{generalized linear models}.
#' 
#' - Estimate the relative impact of different input variables on the output. 
#'     + For example, a $100 increase in transaction size increases the odds that the transaction is fraud by 2%, all else being equal.
#'     
#' - Strictly speaking, logistic regression is scoring. To turn a scoring algorithm into a classifier requires a threshold. 
#' 
#' ## Common classification methods: **Support Vector Machines**
#' 
#' - Useful when there are very many input variables or when input variables interact with the outcome or with
#'                      each other in complicated (nonlinear) ways. 
#'                      
#' - Compared to many other methods, SVMs make fewer assumptions about variable distribution. SVMs are particularly useful when the training data isn't completely representative of the data distribution in production.
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 6)
