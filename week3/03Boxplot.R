#' ---
#' title: "Box Plot"
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
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(ggplot2)
library(crayon)
theme_set(theme_grey(base_size = 22))

#' 
#' 
#' ## Boxplot (R)
## ---- out.width="70%"--------------------------------------------------
custdata<- read.table('../../data/custdata.tsv',
                          header=T,sep='\t')
boxplot(custdata$age, notch=TRUE, col="gold")

#' 
#' ## Boxplot (ggplot)
#' 
## ----out.width = '40%', collapse=T-------------------------------------
ggplot(custdata) + 
  geom_boxplot(aes(y=age), outlier.colour="red",
      outlier.shape=16, outlier.size=2, notch=FALSE)

#' 
## ---- size="small", collapse=T-----------------------------------------
boxplot.stats(custdata$age)$stats

#' 
#' ## Components of a box plot
#' - **Median**
#'     + The median (middle quartile) marks the mid-point of the data and is shown by the line that divides the box into two parts. Half data points are greater than or equal to this value and half are less.
#' 
#' - **Inter-quartile range**
#'     + The middle "box" represents the middle 50% of data points for the group. The range from lower to upper quartile is referred to as the \stressi{inter-quartile range} (\stress{IQR}). 
#' 
#' - **Upper quartile**
#'     + Seventy-five percent of the data points fall below the upper quartile.
#' 
#' - **Lower quartile**
#'     + Twenty-five percent of the data points fall below the lower quartile.
#' 
#' - **Whiskers**
#'     + The upper and lower whiskers represent data outside the middle 50%. Whiskers often (but not always) stretch over a wider range than the middle quartile groups.
#'     
#' ## Components of a box plot
#' 
## ----echo=T, eval=F, size="small", collapse=T, out.width="45%"---------
## x <- rnorm(1000); boxplot(x, col="gold"); grid()
## cat(boxplot.stats(x)$stats) # we call these numbers Q0,...,Q4

#' 
## ----echo=F, eval=T, size="small", collapse=T, out.width="67%"---------
z <- rnorm(1000)
boxplot(z, col="gold")
grid()
q <- boxplot.stats(z)$stats
qlab <- paste0("Q", 0:4)
qlab[2] <- paste(qlab[2], "(lower quartile)")
qlab[3] <- paste(qlab[3], "(median)")
qlab[4] <- paste(qlab[4], "(upper quartile)")
xx <- array(1.11, c(1,5))
xx[2:4] <- 1.22
text(x=xx, y=q, labels=qlab, col="blue", cex=1.5, adj=0)
#
midx <- c(1.2, 1, 1, 1.2)
midy <- (q[-1] + q[-5]) / 2
qtile_labels <- paste("Quartile group", 1:4, "(25%)")
text(x=midx, y=midy, labels=qtile_labels, col="red", cex=1.5)
#
xx <- c(0.9, 0.9)
yy0 <- c(midy[1]-0.2, midy[4]+0.2)
yy1 <- c(midy[1], midy[4])
arrows(x0=xx, y0=yy0, x1=xx+0.095, y1=yy1, col="green", angle=20, length=0.15)
text(x=xx-0.01, y=yy0, labels=paste(c("lower","upper"), "whisker"),
     cex=1.5, font=3, adj=1, col="green")
#
xm <- 0.77
xx <- c(xm, xm)
yy <- c(q[2], q[4])
arrows(x0=xx, y0=yy, x1=xx, y1=yy[2:1], col="black", angle=20, length=0.15)
text(x=xm-0.01, y=q[3], "IQR", cex=1.5, font=2, adj=1)
#
cat(q)

#' 
#' <!-- \begin{figure} -->
#' <!-- \includegraphics[width=0.9\linewidth]{boxplot-labels.png} -->
#' <!-- \end{figure} -->
#' 
#' <!-- Picture Credit: -->
#' 
#' <!-- See also: -->
#' <!-- https://www.wellbeingatschool.org.nz/information-sheet/understanding-and-interpreting-box-plots -->
#' 
#' 
#'     
#' ## Interpreting a box plot
#' Imagine these are box plots of students' exam marks for different units. 
#' 
#' * When a box plot is comparatively short:
#'     + See example (2), this suggests that overall the students' marks do not vary greatly.
#' * When a box plot is comparatively tall:
#'     + See examples (1) and (3). These two plots suggest that the students' marks do vary a lot.   
#' * When one box plot is much higher or lower than another: 
#'     + Example: Unit 3's marks are higher than unit 4's. 
#' 
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{boxplot-compare.png}
#' \end{figure}
#' 
#' ## Interpreting a box plot
#' 
#' * Obvious differences between box plots: 
#'     
#'     + See examples (1) and (2), (1) and (3), or (2) and (4). 
#'     + Any obvious difference between box plots for comparative groups is worthy of further investigation in the *Items at a Glance* reports, e.g., the IQR for a unit's exam marks is much higher or lower than the IQR for the department's reference group box plot.
#' 
#' * When the four sections of the box plot are uneven in size:
#' 
#'     + See example (1). The median is skewed to one side of the box. This shows that many students have similar marks in quartile group 3 but their marks vary greatly in quartile group 2. The long upper whisker means that students' marks also vary a lot in quartile group 4; the very short lower whisker means that the marks are very similar in quartile group 1.
#' \begin{figure}
#' \includegraphics[width=0.3\linewidth]{boxplot-compare.png}
#' \end{figure}
#'     
#' ## Same median, different distribution 
#' 
#' * See examples (1), (2), and (3). The medians (which generally will be close to the average) are all at the same level. 
#' 
#' * However the box plots in these examples show very different distributions of students' exam marks.
#' 
#' \begin{figure}
#' \includegraphics[width=0.5\linewidth]{boxplot-compare.png}
#' \end{figure}
#' 
#' ## Mean and Std
#' 
#' * The mean is the most commonly used mathematical measure of average and is generally what is being referred to when people use the term "average" in everyday's language. 
#' 
#'     + The mean is calculated by totalling all the values in a dataset; this total is then divided by the number of values that make up the dataset.
#' 
#' * The standard deviation is a measure that summarises the amount by which every value within a dataset varies from the mean. 
#' 
#'      + Effectively it indicates how tightly the values in the dataset are bunched around the mean value.
#' 
#' \begin{figure}
#' \includegraphics[width=0.35\linewidth]{std.png}
#' \includegraphics[width=0.35\linewidth]{std_comp.png}
#' \end{figure}
#' 
#' ## Why median and IQR are better than mean and standard deviation?
#' 
#' Like mean and standard deviation, median and IQR measure the central tendency and spread, respectively, but are robust against outliers and non-normal data. They have a couple of additional advantages:
#' 
#' * Outlier Identification. IQR makes it easy to do an initial estimate of outliers:
#'     + $< Q1 - 1.5*IQR$
#'     + $> Q3 + 1.5*IQR$ 
#' * Skewness. Comparing the median to the quartile values shows whether data is skewed. 
#' 
#' ## Why median and IQR are better than mean and standard deviation?
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{mean-std.png}
#' 
#' \includegraphics[width=0.8\linewidth]{median-iqr.png}
#' \end{figure}
#' 
#' 
#' ## References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapter 3)
#' 
#' - Understanding and Interpreting Box Plots: https://www.wellbeingatschool.org.nz/information-sheet/understanding-and-interpreting-box-plots
