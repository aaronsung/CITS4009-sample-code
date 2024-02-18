#' ---
#' title: "Bar Chart and Dot Plot"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Liu"
#' institute: |
#'     | Department of Computer Science and Software Engineering
#'     | The University of Western Australia
#' graphics: yes
#' date: "Semester 2, 2023"
#' 
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
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)

#' 
#' ## Data Type: factors
#' 
#' - Tell R that a variable is nominal by making it a factor. 
#' - The factor stores the nominal values as a vector of integers in the range `[ 1... k ]` (where `k` is the number of unique values in the nominal variable), and an internal vector of character strings (the original values) mapped to these integers.
#' 
## ----------------------------------------------------------------------
# variable gender with 20 "male" entries and
# 30 "female" entries
gender <- c(rep("male",20), rep("female", 30))
gender <- factor(gender) 

#' 
#' # Bar Charts
#' ## Bar charts (R)
#' 
#' A **bar chart** is a histogram for \stressi{discrete data}: it records the frequency of every value of a categorical variable.
#' 
#' \textcolor{red}{But the basic R barplot requires the `table()` function to do the counting}. 
#' 
#' \tiny
## ----out.width="35%", collapse=T---------------------------------------
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')
y <- table(custdata$marital.stat)   # table() carries out the aggregation
print(y)
barplot(y, main="Marital Status", xlab="Status")

#' \normalsize
#' 
#' ## Bar charts (ggplot)
#' 
#' Using `ggplot()`, the code is simpler:
#' 
#' \scriptsize
## ----out.width="60%"---------------------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat), fill="gray") + 
  theme(text = element_text(size = 24))

#' \normalsize
#' 
#' ## Horizontal Bar Charts
## ----out.width="70%", size="small"-------------------------------------
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

#' 
#' ## Bar charts are more informative if the data are sorted
#' 
#' \footnotesize
## ----size="footnotesize", collapse=T-----------------------------------
# table() aggregates according to state.of.res
statesums <- table(custdata$state.of.res) 
# as.data.frame() converts table object into a data frame
statef <- as.data.frame(statesums)
# define the column names for data frame statef
colnames(statef) <- c("state.of.res", "count")
# by default, order by statename alphabetically
summary(statef)

#' \normalsize
#' 
#' ## Sorted Bar Plots
#' 
## ----size="footnotesize", collapse=T, out.width="50%"------------------
statef <- transform(statef,
          state.of.res=reorder(state.of.res, count))
ggplot(statef)+ 
  geom_bar(aes(x=state.of.res,y=count), stat="identity", 
           fill="gray") + coord_flip() +
  theme(axis.text.y=element_text(size=rel(0.8)))

#' 
#' # Dot Plots
#' 
#' ## Dot Plots are preferred by Cleveland
#' 
#' Cleveland prefers the dot plot to the bar chart for visualizing discrete counts. 
#' 
#' * Bars are perceptually misleading. 
#' 	+ Bars are two dimensional, a difference in counts looks like a difference in bar areas, rather than merely in bar heights. 
#' 	+ the dot-and-line of a **dot plot** is not two dimensional, the viewer considers only the height difference when comparing two quantities, as they should.
#' * Bar charts or dot plot need to be sorted, to support more efficient extraction of insights.    
#' 
#'  (William S. Cleveland, The Elements of Graphing Data, Hobart Press, 1994.)
#'  
#' ## Dot plot in the WVPlots package
## ----size="small", out.width="70%"-------------------------------------
library(WVPlots)
ClevelandDotPlot(custdata, "state.of.res",
                 sort = 1, title="Customers by state") +
coord_flip()

#' 
#' ## References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapter 3)
#' 
