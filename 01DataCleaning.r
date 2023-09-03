#' ---
#' title: "Data Cleaning"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Liu"
#' institute: |
#'     | Department of Computer Science and Software Engineering
#'     | The University of Western Australia
#' graphics: yes
#' date: "Semester 2, 2023"
#' 
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

#' ## Domain-Specific Data Cleaning
#' 
#' Take the `custdata` (Version 2) for example, 
#' 
## -----------------------------------------------------------------------------------------------------
custdata_v2 <- readRDS('../../data_v2/Custdata/custdata.RDS')

#' - The variable `gas_usage` mixes numeric and symbolic data: values greater than 3
#' are monthly `gas_bills`, but values from 1 to 3 are special codes. In addition,
#' `gas_usage` has some missing values.
#' 
#' - The variable `age` has the problematic value `0`, which probably means that the
#' age is unknown. In addition, there are a few customers with ages older than `100`, which may also be an error.
#' 
#' - The variable `income` has negative values. We'll assume for this discussion that
#' those values are invalid.
#' 
#' # Dealing with invalid values
#' 
#' ## Converting Invalid Values to `NA`
#' 
#' A quick way to treat the problem in the `age` and `income` variables is to **convert the invalid values to
#' NA**, as if they were missing values.
#' 
#' We can then treat the NAs using the automatic
#' missing-value treatment to be discussed a bit later.
#' 
#' We can use the `mutate()` and `na_if()` functions from the `dplyr` package:
#' 
#' - `mutate()` adds columns to a data frame or modifies existing columns.
#' - `na_if()` turns a specific problematic value into `NA`.
#' 
#' ## Example Code
#' 
## -----------------------------------------------------------------------------------------------------
library(dplyr)

customer_data <- 
  mutate(custdata_v2, 
         age = na_if(age, 0),
         income = ifelse(income < 0, NA, income))

#' 
#' \label{page1:customer_data}
#' 
#' The code above creates a new data frame `customer_data` from
#' `custdata_v2`, with
#' 
#' * zero values in the `age` column converted to `NA`s; and
#' 
#' * negative values in the `income` column converted to `NA`s.
#' 
#' 
#' # Dealing with sentinel values
#' 
#' ## Sentinel Values in `gas_usage`
#' 
#' The values 1, 2, and 3 of the `gas_usage` variable are not numeric values, but
#' \stressi{sentinel values}, i.e., \stressi{special codes}, where:
#' 
#' - the value 1 means ``Gas bill included in rent or condo fee.''
#' 
#' - the value 2 means ``Gas bill included in electricity payment.''
#' 
#' - the value 3 means ``No charge or gas not used.''
#' 
#' One way to treat the `gas_usage` variable is to 
#' 
#' - convert all the special codes (1, 2, 3) to NA, and 
#' 
#' - add three new indicator variables, one for each code. The three new indicator variables can be `gas_with_rent`, `gas_with_electricity`, and `no_gas_bill`.
#' 
#' as done in the code on the next slide.
#' 
#' ## Example Code
#' 
#' \footnotesize
## -----------------------------------------------------------------------------------------------------
customer_data <- customer_data %>%
  mutate(
    gas_with_rent = (gas_usage ==1),
    gas_with_electricity = (gas_usage == 2),
    no_gas_bill = (gas_usage == 3) 
  ) %>%
  mutate(
    gas_usage = ifelse(gas_usage < 4, 
                       NA,
                       gas_usage
                       )
  )

#' \normalsize
#' 
#' \label{page2:customer_data}
#' 
#' `%>%` is like the Unix pipe `|`: The data frame `customer_data` is passed
#' to the first `mutate()` function, whose output is passed to the second
#' `mutate()` function. The final output is stored back to `customer_data`.
#' 
#' # Dealing with Outliers
#' 
#' ## Outliers
#' 
#' If we suspect that there are outliers in a variable (column) in our
#' dataset, we should consider dealing with them in the data cleaning process. 
#' For example, in the smaller customer dataset in `custdata.tsv`,
#' the `income` variable has a negative value and some zero values.
#' 
#' \footnotesize
## ----collapse=T---------------------------------------------------------------------------------------
count.zero <- sum(custdata$income == 0)
count.neg <- sum(custdata$income < 0)
cat("Number of customers having 0 incomes: ", count.zero)
cat("Number of customers having negative incomes: ", count.neg)

#' \normalsize
#' 
#' We could treat
#' 
#' - `income = 0` as an indication that the customer was not working when the data was collected and
#' 
#' - negative incomes as outliers.
#' 
#' ## Outliers (cont.)
#' 
#' We could use `boxplot.stats()` to help us identify the
#' outliers.
#' 
#' \scriptsize
## ----collapse=T---------------------------------------------------------------------------------------
summary(custdata$income)
stat <- boxplot.stats(custdata$income)
stat$n           # number of non-NAs
stat$stats       # quartiles
length(stat$out) # number of outliers
cat("min, max outlying incomes are: ", min(stat$out), max(stat$out))

#' \normalsize
#' 
#' We can see that `boxplot.stats()` picks up many high income
#' values as outliers.  Unfortunately, it is not perfect. it doesn't recognize that
#' these are income values and should not be negative. 
#' 
#' ## Outliers (cont.)
#' 
#' We could fix the problem by turning all the negative incomes to NA
#' and run `boxplot.stats()` again:
#' 
#' \footnotesize
## ----collapse=T---------------------------------------------------------------------------------------
# create a new variable called income.mod with negative incomes set to NA
custdata$income.mod <- ifelse(custdata$income < 0, NA, custdata$income)
stat <- boxplot.stats(custdata$income.mod)

stat$n           # number of non-NAs
q <- stat$stats  # quartiles
q
length(stat$out) # number of outliers
cat("min, max outlying incomes are: ", min(stat$out), max(stat$out))

#' \normalsize
#' 
#' 
#' ## Outliers (cont.)
#' 
#' Like the missing values issue that we will look at later on,
#' we need to justify what to do with the high income values
#' that are identified as outliers.
#' 
#' - How many outliers are there?
#'   
#' - If only a few, then we can omit them, e.g., set them to NA. In our case, there are `69` customers (or `6.9%`) 
#' 
#' - However, there are `78` customers having zero income. They should perhaps be treated differently and excluded.
#' 
#' The `income` column in our data has a skew distribution. We will keep the original values for this column and take an alternative outlier treatment if needed in the future.
#' 
#' Note that outliers are not necessarily bad/incorrect values that
#' must be removed.
#' 
#' (See **Practical Data Science with R**, Section 3.1.1, page 56)
#' 
#' 
#' # Dealing with Missing Values
#' 
#' ## Missing Values
#' 
#' - An important feature of R is that it allows for NA ("not available"). 
#' 
#' - NA represents an **unknown** value. 
#' 
#' - Missing values are "contagious": almost any operation involving an unknown value will also be unknown.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
NA > 5

#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
10 == NA

#' 
#' ## Missing Values (cont.)
#' 
#' This one below is a bit hard to understand, but imagine if the first NA represent customer John's income, the second is Mary's income, both are unknown. Then certainly we don't know whether John and Mary have the same income or not.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
NA == NA

#' 
#' Use `is.na()` to check if a value is NA or not.  
#' 
#' ## Treating missing values (NAs)
#' 
#' Strategies for treating missing values vary depending on the answers to the following two questions:
#' 
#' - How many?
#'     
#' - Why they are missing?
#' 
#' Fundamentally, there are two things you can do with these variables:
#' 
#' - Drop the rows with missing values, or
#' 
#' - Convert the missing values to a meaningful value.
#' 
#' 
#' ## Counting the missing values in each variable
#' 
#' \footnotesize
## ----size="small", collapse=T-------------------------------------------------------------------------
count_missing <- function(df) {
  sapply(df, FUN = function(col) sum(is.na(col)) )
}

nacounts <- count_missing(customer_data)
hasNA = which(nacounts > 0)
nacounts[hasNA]

#' \normalsize
#' 
#' Recall that `customer_data` was created on slide
#' \pageref{page1:customer_data} and three extra variables (columns)
#' were added on slide \pageref{page2:customer_data}.
#' It has `73,262` observations and `15` variables.
#' Ten of these variables (shown above) have NAs.
#' 
#' ## When it is safe to drop rows?
#' 
#' \begin{figure}
#' \includegraphics[width=0.9\linewidth]{count_missing.png}
#' \end{figure}
#' 
#' `customer_data` has 73,262 rows, safe to drop rows with NAs in the `income` and `age` variables, but not the `is_employed` or `gas_usage` variable.
#' 
#' ## Checking locations of missing data
#' 
#' In this example, let's look at the smaller `custdata.tsv` dataset:
#' 
#' \small
## -----------------------------------------------------------------------------------------------------
custdata <- read.table('../../data/custdata.tsv',
		       header=T, sep='\t')
nacounts <- count_missing(custdata)
hasNA = which(nacounts > 0)
nacounts[hasNA]

#' \normalsize
#' 
#' ## Checking locations of missing data (cont.)
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{na-compare.png}
#' \end{figure}
#' 
#' ## Missing values &ndash; are they from the same rows? 
#' 
#' In an extremely unlikely case, all missing data may come from the same rows.
#' In `custdata`, some missing data indeed come from the same rows!
#' 
#' \footnotesize
## ----size="small", collapse=T-------------------------------------------------------------------------
summary(custdata[is.na(custdata$housing.type),
                c("recent.move","num.vehicles")])

#' \normalsize
#' 
#' The summary above shows that the 56 customers whose `housing.type` values
#' are missing also have their `recent.move` and `num.vehicles` missing &mdash;
#' they are the same 56 customers!
#' 
#' 
#' ## Missing Values &ndash; removing all rows with NAs
#' 
#' Use `na.omit()` to remove incomplete observations
#' 
## ----size="small"-------------------------------------------------------------------------------------
newdata <- na.omit(custdata)
nrow(custdata)
nrow(newdata)

#' 
#' This removes all the rows that contain `NA`s.
#' 
#' ## Missing Values &ndash; removing selective rows 
#' 
#' Another option is to use \stressi{subsetting} to remove rows that
#' have `NA`s for certain variables only: 
#' 
## ----size="small"-------------------------------------------------------------------------------------
newdata <- 
  custdata[!is.na(custdata$housing.type),]
nrow(custdata)
nrow(newdata)

#' 
#' ## Should I drop rows if only a few values are missing? 
#' 
#' **Even for a few missing values, you can lose almost all your data!**
#' 
#' \begin{figure}
#' \includegraphics[width=0.95\linewidth]{spreadedNAs.png}
#' \end{figure}
#' 
#' 
#' ## Missing values &ndash; *To Drop* or *Not to Drop*?
#' 
#' - If only a small proportion of values are missing and they tend to be for the same data points, then consider dropping those rows from your analysis, this is called \stressb{listwise deletion}.
#' 
#' - If you are missing data for a particular variable from a large portion of the observations or NAs spread throughout the data, then consider:
#'     1. If the variable is categorical, then create a new category (e.g., *missing*) for the variable. 
#'     2. If the variable is numerical,
#'         - when values are missing randomly, replace them with the mean value or an appropriate estimate, a.k.a. \stressb{imputing} missing values;
#'         - when values are missing systematically, convert them to categorical and add a new category, or replace them with zero and add a \stressi{masking variable}.
#' 
#' ## Missing values &ndash; categorical variables
#' 
#' Create a new category for the missing values, e.g., `missing` or
#' `_invalid_`. 
#' 
#' \begin{figure}
#' \includegraphics[width=0.9\linewidth]{missing_categorical.png}
#' \end{figure}
#' 
#' ## Example Code
#' 
#' In the code below, a masking variable `is.employed.fix` is created. It is the same as `is.employed` except that the NAs are mapped to a new category `missing`.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
custdata$is.employed.fix <- 
  ifelse(is.na(custdata$is.employed),
         "missing", ifelse(custdata$is.employed==T,
                           "employed", "not-employed"))
summary(as.factor(custdata$is.employed.fix))

#' 
#' Why having a masking variable? 
#' 
#' - Better to have the original variable on hand, in case we second-guess our data cleaning and want to redo it.
#' 
#' ## Missing values &ndash; Investigate a bit further
#' 
#' The fix will get the job done, but as a data scientist, one ought to be
#' interested in why so many records are missing certain information.
#' 
#' In the case of the above example, the NAs might actually encode that the
#' customer is not in the active workforce: they are a homemaker, a student, retired, or
#' otherwise not seeking paid employment.
#' 
#' So the category "missing" can be better named as "not in active workforce".
#' 
#' ## Missing values &ndash; Numerical variables
#' 
#' - One might believe that the data collection failed at random so the missing
#'   values are independent of other variables. In this case, the missing
#'   values can be replaced by the mean or an appropriate estimate (e.g., the
#'   median).
#' 
#' \begin{figure}
#' \includegraphics[width=0.55\linewidth]{missing_numerical.png}
#' \end{figure}
#' 
#' 
#' ## Missing values &ndash; imputing with better estimate
#' 
#' The estimate can be improved (potentially better than *mean*) if other variables that relate to it are used for prediction. 
#' 
#' - For instance, from data exploration we know *income* is related to *age*, *state of residence* and *marital status*.
#' 
#' - We can use a \stressi{regression model} to predict the missing income if other variables for that data point are available.
#' 
#' - Other models such as \stressi{clustering} are also applicable. 
#' 
#' \stress{How to best impute missing values is still under active research. }
#' 
#' **Note:** imputing a missing value of an input variable based on the other input variables can be applied to categorical data as well.
#' 
#' ## *Missingness* Indicator
#' 
#' A trick that has worked well is to not only replace the `NA`s with
#' the mean, but also add an additional indicator variable (e.g., `isBAD`)
#' to keep track of which data points have been altered.
#' 
#' \begin{figure}
#' \includegraphics[width=0.7\linewidth]{missing_indicator.png}
#' \end{figure}
#' 
#' ## Why *Missingness Indicators* can be useful
#' 
#' The idea is that at the modelling step, you give all the variables &ndash; `income`, `income_isBAD`, `gas_usage`, `no_gas_bill`, and so on &ndash; to the modelling algorithm, and it can determine how to best use the information to make predictions. 
#' 
#' - If the missing
#' values really are missing \stress{randomly}, then the indicator variables are uninformative, and the model should ignore them. 
#' 
#' - If the missing values are missing
#' \stress{systematically}, then the indicator variables provide useful additional information to
#' the modelling algorithm.
#' 
#' - In many situations,
#' the `isBAD` variables are sometimes even *more* informative and useful than
#' the original variables!
#' 
#' ## Take home messages
#' 
#' - Strategies for dealing with missing values depend on how many missing values there are, and whether
#' they are missing randomly or systematically.
#' 
#' - When in doubt, assume that missing values are missing systematically.
#' 
#' ## The `vtreat` package 
#' 
#' Missing values are such a common problem with data, it’s useful to have an automatic
#' and repeatable process for dealing with them.
#' 
#' `vtreat` is a package for automatically treating missing values. It creates a `treatment plan` that records all the information needed so that the data treatment process can be repeated. You then use this treatment plan 
#' 
#' - to “prepare” or treat your training data before you fit a model, and 
#' 
#' - then again to treat new data before feeding it into the model. 
#' 
#' The idea is that treated data is "*safe*", with no missing or unexpected values,
#' and should not ruin the model.
#' 
#' ## Creating and Applying a Simple Treatment Plan
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{treatment_plan.png}
#' \end{figure}
#' 
#' ## Sample Code
## -----------------------------------------------------------------------------------------------------
library(vtreat)

varlist <- setdiff(colnames(customer_data), 
                   c("custid", "health_ins"))

treatment_plan <- design_missingness_treatment(
  customer_data, varlist = varlist)

training_prepared <- prepare(treatment_plan,
                             customer_data)

nacounts <- count_missing(training_prepared)
sum(nacounts)

#' 
#' ## Examining the data treatment
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
missing.ht <- which(
  is.na(customer_data$housing_type))

columns_to_look_at <- 
  c("custid", "is_employed", "num_vehicles",
    "housing_type", "health_ins")

customer_data[missing.ht, columns_to_look_at] %>% head()

#' 
#' ## Examining the data treatment (cont.)
#' 
#' \scriptsize
## ----size="small", collapse=T-------------------------------------------------------------------------
columns_to_look_at = c("custid", 
   "is_employed", "is_employed_isBAD", "num_vehicles",
   "num_vehicles_isBAD", "housing_type", "health_ins")
training_prepared[missing.ht, columns_to_look_at] %>% head()

#' \normalsize
#' 
#' ##   References
#' 
## ----eval=T, echo=F-----------------------------------------------------------------------------------
# save the various data frame objects to file for the next Rmarkdown file
# for the lecture note.
save(custdata, customer_data, file="01output.rData")

#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapters 3 & 4)
#' 
#' - **R in Action**, *Robert I. Kabacoff*, Manning, 2011 (Chapter 4)
#' 
