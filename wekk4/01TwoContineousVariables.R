#' ---
#' title: "CITS4009 Exploratory Data Analysis"
#' subtitle: "Comparing Two Continuous Variables"
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
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' # The Layered Grammar of Graphics
#' 
#' ##   ggplot - the layered grammar of graphics
#' 
## ----eval=FALSE----------------------------------------------------------
## ggplot(data = <DATA>) +
##   <GEOM_FUNCTION>(
##      mapping = aes(<MAPPINGS>),
##      stat = <STAT>,
##      position = <POSITION>
##   ) +
##   <COORDINATE_FUNCTION> +
##   <FACET_FUNCTION>

#' 
#' This new template takes seven parameters.
#' 
#' You can uniquely describe any plot as a combination of a **dataset**, a **geom**, **a set of mappings**, a **stat**, a **position adjustment**, a **coordinate system**, and a **faceting scheme**.
#' 
#' ##   Chart types - geoms
#' 
#' The chart type in ggplot is referred as types of \stressi{geoms}.
#' 
#' A *geom* is the geometrical object that a plot uses to represent data.
#' 
#' - Bar charts use bar geoms, line charts use line geoms, boxplots use
#' boxplot geoms, and so on.
#' - Scatterplots break the trend; they use the point geom.
#' - Different geoms can be used to plot the same data.
#' 
#' # Visually comparing two variables
#' 
#' ## Packages used 
#' 
#' In addition to `tidyverse` and `crayon`, the following packages are used in this set of slides: 
#' 
## ----eval=FALSE----------------------------------------------------------
## install.packages("hexbin")
## install.packages("gridExtra")
## install.packages("maps")

#' 
#' ## Summary of plots useful for two variable comparison
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{two-variables.pdf}
#' \end{figure}
#' 
#' # Scatter plots for two continuous variables
#' 
#' ##   Basic scatter plot matrices (R)
#' 
#' <!-- as.is=TRUE below would cause problems to the categorical -->
#' <!-- columns (e.g., marital.status, housing.type, sex, etc) - -->
#' <!-- they all get converted to character string and become -->
#' <!-- Character class -->
#' \small
## ----out.width='70%'-----------------------------------------------------
custdata <- read.delim('../../data/custdata.tsv', as.is=FALSE)
theme_set(theme_grey(base_size = 18))
pairs(custdata)

#' \normalsize
#' 
#' ##   Select the pairs you are interested in
## ----out.width='70%'-----------------------------------------------------
pairs(~sex+age+income+health.ins, data=custdata)

#' 
#' ##   Select the pairs you are interested in
## ----out.width='70%'-----------------------------------------------------
pairs(~displ+cty+hwy, data=mpg)

#' 
#' 
#' ##   Scatter and Smooth Plots (`ggplot`) - Code
#' 
#' ### `geom_point` and `geom_smooth`
#' 
## ----echo=TRUE-----------------------------------------------------------
# left
p1 <- ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  labs(title = "point geom")
  
# right
p2 <- ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy)) +
  labs(title = "smooth geom")

#' 
#' ## Scatter and Smooth Plots (`ggplot`) - Outputs
#' 
#' \scriptsize
## ----out.width='65%'-----------------------------------------------------
library(gridExtra)
grid.arrange(p1, p2, ncol=2)

#' \normalsize
#' 
#' The two figures are generated using the same data but different *geom*s.
#' 
#' ## The Smooth Geom
#' 
#' `geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)`
#' 
#' - `method` : smoothing method to be used. Possible values are `lm`, `glm`, `gam`, `loess`, `rlm`.
#' 
#'     + `method = "loess"`: This is the default value for small numbers of observations. It computes \stressi{a smooth local regression}. You can read more about `loess` using the R code `?loess`.
#' 
#'     + `method = "lm"`: It fits a linear model.  One can also use `lm` to fit non-linear models if the formula is known (e.g., `formula = y ~ poly(x, 3)` will fit a degree 3 polynomial).
#' 
#' - `se` : logical value. If TRUE, confidence interval is displayed.
#' 
#' - `fullrange` : logical value. If TRUE, the fit spans the full range.
#' - `level` : level of confidence interval to use. Default value is \stress{0.95}.
#' 
#' 
#' ##   Choose the right aesthetic mapping for comparison
#' 
#' An aesthetic is a visual property of the objects in your plot. 
#' 
#' Aesthetics include:
#' 
#' - the coordinates (e.g. `x=`, `y=`), which accepts variables used for each coordinate
#' - the size (`size=`)
#' - the colour (`colour=` or `color=`)
#' - the shape (`shape=`)
#' 
#' You can display a point in different ways by changing the values of its aesthetic properties. 
#' 
#' ##   Aesthetic levels
#' 
#' The word "\stress{level}" is used to describe the values of aesthetic properties. We can change the levels of a point's color, size, and shape:
#' 
#' - `color` is specified using a character string as a color name, e.g. `blue`;
#' - `size` is measured in mm;
#' - `shape` is specified as a number.
#'     + R has 25 built-in shapes that are identified by numbers. Duplicates (e.g.,
#'       0, 15, and 22) are differentiated using the interaction of the `colour` and `fill` aesthetics. 
#'         * The hollow shapes (0-14) have a border determined by `colour`; 
#'         * The solid shapes (15-20) are filled with `colour`; 
#'         * The filled shapes (21-24) have a border of `colour` and filled with `fill`.
#' 
#' 
#' \begin{figure}
#' \includegraphics[width=0.5\linewidth]{shapes.png}
#' \end{figure}
#' 
#' ##   Manually assigned aesthetic levels
#' 
## ----out.width='60%'-----------------------------------------------------
ggplot(data = mpg) +
  geom_point(
    mapping = aes(x = displ, y = hwy), 
    colour = "blue", shape = 24, fill = "red")

#' 
#' 
#' ##   Automatically assigned aesthetic levels (Code)
#' 
#' \small
## ----collapse=T----------------------------------------------------------
library(gridExtra)
p1 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))

p2 <- ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, color = drv, 
                            linetype = drv))
              
p3 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class),
             show.legend = TRUE)

p4 <- ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class, 
                           shape = drv))

#' \normalsize
#' 
#' ##   Automatically assigned aesthetic levels (graphs)
#' 
## ----out.width='76%'-----------------------------------------------------
grid.arrange(p1, p2, p3, p4, ncol=2)

#' 
#' 
#' ##   On the customer data
#' 
#' \small
## ----out.width='60%'-----------------------------------------------------
ggplot(data = custdata) +
  geom_smooth(mapping = aes(x=age, y=income, color=marital.stat),
              show.legend = TRUE)

#' \normalsize
#' 
#' ##   plotting multiple geoms on the same plot
#' 
## ----out.width='60%'-----------------------------------------------------
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))

#' 
#' ##   Don't want to specify the data multiple times? Global mappings.
#' 
#' \footnotesize
## ----out.width='60%'-----------------------------------------------------
ggplot(data = mpg,
       mapping = aes(x = displ, y = hwy, color = class)) + 
  geom_point() + geom_smooth()

#' \normalsize
#' 
#' # Hexbin for two continuous variables
#' 
#' ##   Hexbin - 2D histogram
#' 
#' Install the `hexbin` package first
#' 
#' A \stressi{hexbin plot} is like a two-dimensional histogram. 
#' 
#' The data is divided into bins, and the
#' number of data points in each bin is represented by a color or shading.
#' 
#' \vspace{1cm}
#' 
#' Use **hexbin plot** to show the relationship between two **continuous** variables **when the data is very dense**.
#' 
#' ##   Hexbin - 2D histogram example
#' 
#' \scriptsize
## ----out.width="65%"-----------------------------------------------------
ggplot(custdata, aes(x=age, y=income)) + geom_hex(binwidth=c(5, 10000)) +
  geom_smooth(color="white", se=F) + ylim(0,200000)

#' \normalsize
#' 
#' (compare this with the scatter plot on row 3, column 2 on page 10/26)
#' 
#' ##   References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapters 3&7)
