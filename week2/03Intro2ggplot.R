#' ---
#' title: "Introduction to ggplot"
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
## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(ggplot2)
library(crayon)

#' 
#' # Single Variable Plots
#' 
#' ## Distribution of a single variable
#' 
#' 
#' - What is the peak value of the distribution?
#' - How many peaks are there in the distribution (unimodality versus bimodality)?
#' - How normal (or lognormal) is the data? 
#' - How much does the data vary? Is it concentrated in a certain interval or in a certain
#' category?
#' 
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{uni_bi_modal.png}
#' \end{figure}
#' 
#' ## Plots for single variable distribution
#' 
#' ------------- --------------------------------------------
#' Graph Type    Uses
#' ------------- --------------------------------------------
#' Histogram     Examines data range
#' 
#' Density Plot  Checks number of modes
#'               Checks if distribution is normal/lognormal/etc
#'                 
#' Boxplot       Checks for anomalies and outliers
#' 
#' Bar Chart     Compares relative or absolute frequencies
#'               of the values of a categorical variable
#' ------------- --------------------------------------------
#' 
#' 
#' # Histograms
#' 
#' ## Histograms  - the `hist` function in R
#' A basic histogram bins a variable into fixed-width buckets and returns the number of
#' data points that falls into each bucket.
#' 
## ----out.width = '60%', size="small"-----------------------------------
custdata <- read.table('custdata.tsv',header=T, sep='\t')
hist(custdata$age)

#' 
#' ## Histogram: Other useful options
#' 
#' - `breaks`: takes a sequence to specify where the breaks are
#' - `xlim`: takes the start and end point of x axis
#' - `freq`: `TRUE` for raw counts; `FALSE` for density (normalized by the total count), and the areas of the bars add to 1. This is called "density plot" in ggplot, except it is not a continuous line plot. 
#' 
## ----out.width = '40%'-------------------------------------------------
x <- custdata$age
hist(x, breaks=seq(0,150,1), xlim=c(0,100), freq = FALSE)

#' 
#' 
#' ## Adding titles
#' - Using Attributes of the function
## ----out.width = '60%'-------------------------------------------------
hist(custdata$age,main="Distribution of age",xlab="age")

#' 
#' ## Adding titles
#' - Using the `title()` function
## ----out.width = '50%'-------------------------------------------------
hist(custdata$age)
title('Distribution of age',xlab='age')

#' To remove the default title from `hist`, do: `hist(custdata$age, main="")`
#' 
#' # A layered grammar of graphs - ggplot
#' 
#' ## ggplot
#' 
#' - R has several systems for making graphs, but `ggplot2` is one of the most elegant and most versatile libraries. 
#' 
#' - `ggplot2` implements the **grammar of graphics**, a coherent system for describing and building graphs.
#' 
#' - Begin a plot with the function `ggplot()`, which takes the data and create a coordinate system that you can add **layers** to. 
#' 
#' - A reusable template for making graphs with `ggplot2` is given below. To make a graph, replace the bracketed parts in the code below with 
#'     + a dataset, 
#'     + a geom function (chart type), or
#'     + a collection of mappings (data selection for each coordinate).
#' 
## ----eval=FALSE, message=FALSE, WARNING=FALSE--------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

#' 
#' ## Histograms (ggplot2)
#'  
## ----out.width = '50%'-------------------------------------------------
library(ggplot2)
ggplot(data = custdata) +
  geom_histogram(mapping = aes(x=age),
                 binwidth=5, fill="gray")


#' 
#' ## Density plot (ggplot2)
#' 
#' In ggplot, a **density plot** is a "continuous histogram" of a variable, except the area under the density plot is equal to 1. 
#' 
#' - A point on a density plot corresponds to the fraction of data (or the percentage of data, divided by 100) that takes on a particular
#' value.
#' 
## ----out.width = '40%', message = FALSE, warning = FALSE---------------
library(ggplot2)
ggplot(custdata) + geom_density(aes(x=age)) +
	theme(text = element_text(size = 24))

#' 
#' 
#' ## References
#' 
#' - **Practical Data Science with R**. By Nina Zumel and John Mount, Manning, 2014. (Chapter 3)
#' - **R for Data Science (2e)**. By Garret Grokemund and Hardley Wickham, O'Relly, 2023. (Chapter 3)
#' - Introduction to the R language: https://users.soe.ucsc.edu/~lshiue/bioc/Rintro.ppt
#' - An Introduction to R: http://csg.sph.umich.edu/abecasis/class/815.04.pdf
#' - Differences between assignment operators in R: https://renkun.me/2014/01/28/difference-between-assignment-operators-in-r/
#' 
#' 
#' 
