#' ---
#' title: "CITS4009 Exploratory Data Analysis"
#' subtitle: "Visually Compare Two Categorial Variables"
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
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
library(gridExtra)
theme_set(theme_grey(base_size = 18))
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' 
#' # Heat Maps
#' ## Visualising two categorical variables - `geom_count`
#' - The size of each circle in the plot denotes how many observations occurred at each combination of values. 
#' - Covariation will appear as a strong correlation between specific `x` values and specific `y` values.
#' 
#' \scriptsize
## ----collapse=T, out.width='42%'-----------------------------------------
ggplot(data = custdata) +
  geom_count(mapping = aes(x=marital.stat, y=housing.type))

#' \normalsize
#' 
#' Note that the counts (frequencies) in the legend are labelled as $n$.
#' 
#' ## Visualising two categorical variables - `geom_tile`
## ----size="small", collapse=T, out.width='50%'---------------------------
library(dplyr)
counting <- count(custdata, marital.stat, housing.type)
ggplot(data = counting, mapping = aes(x = marital.stat, 
                               y = housing.type)) +
    geom_tile(mapping = aes(fill = n))

#' 
#' ## Visualising two categorical variables - `geom_tile` (cont.)
#' 
#' Note that the line
#' 
#' \scriptsize
## ----eval=F--------------------------------------------------------------
## counting <- count(custdata, marital.stat, housing.type)

#' \normalsize
#' 
#' produces a data frame:
#' \scriptsize
## ------------------------------------------------------------------------
head(counting)

#' \normalsize
#' 
#' So we can use the column `n` as the aesthetic for `geom_tile` on the previous slide.
#' 
#' 
#' # Bar Charts and Position Adjustment
#' 
#' ##   Visualsing two categorical variables using Bar Charts
#' 
#' Let's examine the relationship between marital status and the probability of health
#' insurance coverage. 
#' 
#' \begin{figure}
#' \parbox{0.3\linewidth}{\centering \scriptsize \textcolor{blue}{Stacked bar chart}}
#' \parbox{0.3\linewidth}{\centering \scriptsize \textcolor{blue}{Side-by-side bar chart}}
#' \parbox{0.3\linewidth}{\centering \scriptsize \textcolor{blue}{Filled bar chart}}
#' \\ [-0.2ex]
#' \includegraphics[width=0.3\linewidth]{marital-ins.png} 
#' \includegraphics[width=0.3\linewidth]{marital-ins-side.png} 
#' \includegraphics[width=0.3\linewidth]{marital-ins-filled.png} 
#' \\ [-1ex]
#' \parbox{0.3\linewidth}{\centering ~~~}
#' \parbox{0.3\linewidth}{\centering \tiny \textcolor{red}{(position="dodge")}}
#' \parbox{0.3\linewidth}{\centering \tiny \textcolor{red}{(position="fill")}}
#' \end{figure}
#' 
#' 
#' ##   Stacked bar charts
#' 
#' \scriptsize
## ----collapse=T, out.width='40%'-----------------------------------------
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=health.ins)) +
	theme(text = element_text(size = 22))

#' 
#' (same as the left diagram on the prvious slide except that "uninsured" is stacked on the top)
#' \normalsize
#' The \stress{stacked bar chart} makes it easy to visualize the distribution of insured and uninsured
#' people in each `marital.stat` category; however, it is difficult to compare the numbers of
#' uninsured people across different `marital.stat` categories as they don't start at the same
#' level.
#' 
#' 
#' ##   Side-by-side bar charts
#' 
#' \scriptsize
## ----collapse=T, out.width='55%'-----------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x=marital.stat, fill=health.ins), position="dodge")

#' \normalsize
#' 
#' The \stress{side-by-side bar chart} makes it
#' easier to compare the number of insured (and uninsured) people across different `marital.stat` categories
#' but not the total number of people in each category.
#' 
#' ##   Filled bar charts
#' 
#' The main shortcoming of both the stacked and side-by-side bar charts is that you
#' can't easily compare the \underline{ratios} of *insured* to *uninsured* across categories, especially for rare categories like `Widowed`. You can use what `ggplot2` calls a \textcolor{blue}{filled bar chart} to plot a
#' visualization of the ratios directly.
#' 
#' The filled bar chart makes it obvious to view ratios of second variable for each category. 
#' 
#' - For example, the divorced customers are slightly more likely to be uninsured than married ones. 
#' - But we've lost the information that 
#' `Widowed`, although highly predictive of insurance coverage, is a rare category.
#' 
#' ##   Filled bar charts
#' 
#' \footnotesize
## ----out.width="60%"-----------------------------------------------------
ggplot(custdata) +
  geom_bar(aes(x=marital.stat, fill=health.ins), position="fill") +
  theme(text = element_text(size = 22))

#' \normalsize
#' 
#' Can you spot a small error in the plot above?
#' 
#' ## Overlaying bar charts
#' 
#' - The bars are super-imposed onto each other, only the higher values are shown as the lower values are shadowed.
#' 
#' - Better than stacked bars in showing relative size for each category.
#' 
#' - Similar pros and cons to side-by-side bars, but more compact when there are many categories to plot.
#' 
#' 
#' 
#' ## Overlaying bar charts (example)
#' 
#' \scriptsize
## ----out.width='55%'-----------------------------------------------------
ggplot(custdata) + geom_bar(aes(x=marital.stat, fill=housing.type),
                alpha=0.2,  position="identity") +
  theme(text = element_text(size = 18))

#' 
#' (Compare this diagram with the **heat maps** generated using `geom_count` and `geom_tile` on earlier slides)
#' 
#' \normalsize
#' 
#' ## Overlaying bar charts (example)
#' 
#' \scriptsize
## ----out.width='55%'-----------------------------------------------------
ggplot(custdata) + 
  geom_bar(aes(x= marital.stat, colour=housing.type),
          fill=NA,  position="identity") +
  theme(text = element_text(size = 18))

#' \normalsize
#' 
#' ## How to choose the *primary variable*?
#' 
#' All the bar charts shown on the previous slides have two categorical variables. One of these variables should be the \stressi{primary variable} (the $x$-axis) and the other (the \stressi{secondary variable}) should be used for aesthetic mapping. Example: Bar charts of `class` (primary) and `drv` from the `mpg` dataset:
#' 
#' \tiny
## ----out.width='70%'-----------------------------------------------------
fig1 <- ggplot(mpg) + geom_bar(aes(x=class, fill=drv), width=0.5) + 
   theme(text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1), aspect.ratio=0.8)
fig2 <- ggplot(mpg) + geom_bar(aes(x=class, fill=drv), width=0.5, position="dodge") +
   theme(text=element_text(size=16), axis.text.x=element_text(angle=45, hjust=1), aspect.ratio=0.8)
grid.arrange(fig1, fig2, nrow=1)

#' \normalsize
#' 
#' ## How to choose the *primary variable*? (cont.)
#' 
#' Example: Bar charts of `drv` (primary) and `class` from the `mpg` dataset:
#' 
#' \tiny
## ----out.width='90%'-----------------------------------------------------
fig1 <- ggplot(mpg) + geom_bar(aes(x=drv, fill=class), width=0.5) + 
   theme(text=element_text(size=16), aspect.ratio=1.2)
fig2 <- ggplot(mpg) + geom_bar(aes(x=drv, fill=class), width=0.5, position="dodge") +
   theme(text=element_text(size=16), aspect.ratio=1.2)
grid.arrange(fig1, fig2, nrow=1)

#' \normalsize
#' 
#' ##   References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapters 3&7)
