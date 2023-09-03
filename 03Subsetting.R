#' ---
#' title: "Selecting and Filtering &mdash; Subsetting"
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
library(dplyr)
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' 
#' 
#' # Type conversion
#' 
#' ## Type conversion in R
#' 
#' R provides a set of functions to identify an object's data type and convert it
#' to a different data type.
#' 
#' - **Implicit type coercion** &mdash; similar to other weakly typed programming languages, R automatically determines the data type of an operation. 
#'     
#'     + For example, adding a character string to a numeric vector converts all the elements in the vector to character values.
#'     
#' - **Explicit type conversion using functions**.
#' 
#' ## Functions to check and convert to different types
#' 
#' \begin{figure}
#' \includegraphics[width=0.7\linewidth]{type.png}
#' \end{figure}
#' 
#' * Functions in the 1st column are for type checking. 
#' * Functions in the 2nd column are for type conversion. Depending on the argument
#'   passed to the function, the conversion may not be always successful/possible.
#' 
#' We can also use the `str()` function to inspect a variable's type and do conversion if needed.
#' 
#' ## Functions to check and convert to different types (cont.)
#' 
#' A few examples:
#' 
#' \footnotesize
## ----collapse=T----------------------------------------------------------------------
x <- c(7,8);  y <- "Hello"
z <- list(name=c("Rose","Jon"), height=c(1.6,1.7))
cat(is.vector(x), is.matrix(x), is.character(y), is.list(z))

str(y)

x <- as.character(x)   # convert x to vector of character type
cat(is.vector(x), is.numeric(x), is.character(x))

as.logical("hello")  # can't convert this
as.numeric("hello")  # can't convert this

#' \normalsize
#' 
#' # Sorting Data
#' 
#' ## Sorting data in R using the `order()` function
#' 
#' - By default, the sorting order is ascending. 
#' - Prepend the sorting variable with a minus sign to indicate descending order.
#' 
#' E.g., create a new data frame with the customers firstly sorted
#' by `sex` in ascending order (female then male), then by `income` in descending order: 
#' 
#' \small
## ------------------------------------------------------------------------------------
attach(custdata)
newdata <- custdata[order(sex, -income),]
detach(custdata)

#' \normalsize
#' 
#' The `attach()` function is used so that we do not have to prefix
#' each variable name by its data frame name.
#' 
#' # Subsetting datasets
#' 
#' ## Selecting or dropping variables (columns)
#' 
#' - Selecting variables
#' 
#' \small
## ----collapse=T----------------------------------------------------------------------
myvars <- c("custid", "is.employed", "income", 
            "marital.stat", "health.ins", "age")
newdata <- custdata[myvars] # newdata has 6 columns

#' \normalsize
#' 
#' - Excluding (dropping) variables
#' 
#' \small
## ----collapse=T----------------------------------------------------------------------
myvars <- names(custdata) %in% c("sex", "state.of.res")
myvars
newdata <- custdata[!myvars] # newdata has 11 - 2 = 9 columns

#' \normalsize
#' 
#' ## Selecting or dropping observations (rows)
#' 
#' \small
## ----collapse=T----------------------------------------------------------------------
newdata <- custdata[1:3,]
newdata <- custdata[which(custdata$sex=="M" &
                              custdata$age < 30),]
cat(nrow(custdata), nrow(newdata))

attach(custdata)
newdata <- custdata[which(sex=="M" & age > 30),]
detach(custdata)
cat(nrow(custdata), nrow(newdata))

#' \normalsize
#' 
#' In each of these examples, we provide the row indices and leave the column indices
#' blank (therefore choosing all columns).
#' 
#' ## Using the `subset()` function for both
#' 
#' Calling format:
#' 
#' \small
## ----eval=F, collapse=T--------------------------------------------------------------
## subset(<x>, <subset>, <select>, ...)
##    <x>      - object to subset
##    <subset> - logical expression indicating rows to keep
##    <select> - expression, indicating columns to select

#' \normalsize
#' 
#' **Example 1.** Select rows using logical expression and
#' columns using their names:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
newdata <- subset(custdata, age >= 65 | age < 24,
                  select=c("custid", "marital.stat"))

#' \normalsize
#' 
#' **Example 2.** Select columns explicitly using `col_start:col_end`:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
# custid is column 1; age is column 10
newdata <- subset(custdata, sex=="M" & age < 25,
                  select=custid:age)

#' \normalsize
#' 
#' # Using SQL to manipulate data frames
#' 
#' ## The SQL Data Frame Library - `sqldf`
#' 
#' For those who like the convenience of **Structured Query Language** (**SQL**),
#' the `sqldf` package provides querying of data frames using SQL.
#' 
#' \footnotesize
## ----warning=F-----------------------------------------------------------------------
library(sqldf)
# sql is not case sensitive, so "income" and "Income" 
# are considered the same
newdf <- sqldf("select * from custdata where 
               income <= 1000 order by age", 
               row.names=TRUE)
# kable() requires the knitr library
kable(newdf[1:5, c("custid","sex","is.employed","income","age","health.ins")])

#' \normalsize
#' 
#' 
#' ##   Take home messages
#' 
#' - Misc: type conversion & sorting
#' - Subsetting using `subset()` or `sqldf()`
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 5: Sections 5.1-5.4)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, [https://r4ds.had.co.nz/](https://r4ds.had.co.nz/) (Chapters 11,12,13,18)
