#' ---
#' title: "Converting Continuous Variables to Categorical"
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
load("01output.rData")

#' #  Recoding Variables
#' 
#' ## Recoding variables
#' 
#' - Change a continuous variable into a set of categories
#' - Create a pass/fail variable based on a set of cutoff scores
#' - Replace miscoded values with correct values
#' 
#' ## Discretizing continuous variables &ndash; Motivation
#' 
#' \footnotesize
## ----eval=F, echo=T-----------------------------------------------------------------------------------
## library(scales)
## ggplot(data = custdata,
##        mapping = aes(x=income, y=as.numeric(health.ins))) +
##   geom_jitter(alpha=1/5, height = 0.1) + geom_smooth() +
##   scale_x_log10(breaks = c(100,1000,10000,100000,1000000), labels=dollar) +
##   labs(x="income (log10 scale)", y="health.ins")

#' \normalsize
#' 
## ----out.width="50%", eval=T, echo=F------------------------------------------------------------------
library(scales)
ggplot(data = custdata, 
       mapping = aes(x=income, y=as.numeric(health.ins))) +
  geom_jitter(alpha=1/5, height = 0.1) + geom_smooth() +
  scale_x_log10(breaks = c(100,1000,10000,100000,1000000),
                labels=dollar) +
  annotate("text", x=50000, y=0.5, size=7, colour="red",
	   label=paste("insured customers", "(health.ins=TRUE (or",
		       "(numerical value 1))", sep="\n")) +
  annotate("segment", x=45000, y=0.6, xend=10000, yend=0.9,
	   arrow=arrow(), size=1, colour="red") +
  annotate("text", x=1000, y=0.4, size=7, colour="red",
	   label=paste("uninsured customers", "(health.ins=FALSE (or",
		       "numerical value 0))", sep="\n")) +
  annotate("segment", x=4500, y=0.3, xend=10000, yend=0.1,
	   arrow=arrow(), size=1, colour="red") +
  labs(x="income (log10 scale)", y="health.ins") +
  theme(text = element_text(size=16)) 

#' 
#' ## Discretizing continuous variables &ndash; Motivation (cont.)
#' 
#' \footnotesize
## ----out.width="60%"----------------------------------------------------------------------------------
ggplot(data = custdata, 
       mapping = aes(x=age, y=as.numeric(health.ins))) +
  geom_jitter(alpha = 1/5, height = 0.1) + geom_smooth() +
  theme(text = element_text(size=16))

#' \normalsize
#' 
#' ## Discretizing continuous variables &ndash; Motivation (cont.)
#' 
#' For some continuous variables, their exact values matter less than whether
#' they fall into a certain range. For example,
#' 
#' - Customers with incomes less than $20,000 have different health insurance patterns than customers with higher
#' incomes. 
#' - Customers younger than 25 and older than 65 have high probabilities of insurance coverage, because they tend to be on their parents' coverage or on a retirement plan, respectively, whereas customers between those ages
#' have a different pattern.
#' 
#' In these cases, you might want to convert the continuous `age` and `income` variables
#' into ranges, or discrete variables. Discretizing continuous variables is useful when the
#' relationship between input and output isn't linear.
#' 
#' ## Converting income into range
#' 
## -----------------------------------------------------------------------------------------------------
custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

#' 
#' ## Converting age into range
#' 
#' Use the `cut()` function, which specifies the category names automatically.  
#' 
#' \footnotesize
## -----------------------------------------------------------------------------------------------------
brks <- c(0, 25, 65, Inf)
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T)
summary(custdata$age.range)

#' \normalsize
#' 
#' The code above creates a new categorical variable `age.range`,
#' which has 3 categories.
#' 
#' ## Infinite age &ndash; immortal?
#' 
#' Age values extending to `Inf` is beyond reality for mortals. :-) In fact, age values over 120 might even be data entry errors. We can treat them as missing or unknown values. 
#' 
## ----eval=FALSE---------------------------------------------------------------------------------------
## # altering the original data
## custdata$age[custdata$age > 120] <- NA
## # or create a new variable
## custdata$age.alt <- ifelse(custdata$age > 120,
##                            NA, custdata$age)
## # then convert custdata$age.alt into range

#' 
#' ## Explicit categorisation
#' 
#' We could also define the categorization explicitly.
#' 
## -----------------------------------------------------------------------------------------------------
custdata$agecat[custdata$age > 120] <- NA
custdata$agecat[custdata$age > 65 
                & custdata$age <= 120] <- "Elder"
custdata$agecat[custdata$age > 25 
                & custdata$age <= 65] <- "Middle Aged"
custdata$agecat[custdata$age <= 25] <- "Young"

#' 
#' ## More explicit categorisation
#' 
#' The code can be written more compactly using the `within()` function. We
#' 
#' - first create a variable `agecat`, and set to missing (`NA`) for each row of the data. 
#' - then execute the remaining statements in the curly braces in order.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
custdata <- within(custdata, {
  agecat <- NA
  agecat[age > 120] <- NA
  agecat[age > 65 & age <= 120] <- "Elder"
  agecat[age > 25 & age <= 65] <- "Middle Aged"
  agecat[age <= 25] <- "Young" })

#' 
#' Note: `agecat` is of string type. It needs to be converted to factor.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
custdata$agecat <- factor(custdata$agecat)

#' 
#' 
#' # Renaming Variables
#' 
#' ## Renaming variables Manually via the Interactive Editor
#' 
#' - Use `fix()` to invoke the interactive editor.
## ----size="small", collapse=T, eval=FALSE-------------------------------------------------------------
## fix(custdata)

#' 
#' \begin{figure}
#' \includegraphics[width=0.5\linewidth]{editor.png}
#' \end{figure}
#' 
#' ## Rename variables programmatically
#' 
#' The `dplyr` package has a `rename()` function that's useful for
#' altering the names of variables. 
#' 
## ----size="small", collapse=T, eval=FALSE-------------------------------------------------------------
## rename(dataframe, newname=oldname, newname=oldname, ...)

#' 
#' For example,
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
library(dplyr)
custdata <- rename(custdata, age.cat=agecat, gender=sex)
names(custdata)

#' ##   References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 4)
#' 
#' - **R in Action**, *Robert I. Kabacoff*, Manning, 2011 (Chapter 4)
#' 
