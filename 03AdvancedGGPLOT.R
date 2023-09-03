#' ---
#' title: "Advanced Visualisation"
#' subtitle: "CITS4009 Exploratory Data Analysis"
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
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' 
#' #   Statistical Transformation
#' 
#' ## Statistical Transformation - Where does count come from? 
#' 
#' Many graphs, like scatterplots, plot the raw values of a dataset. Other graphs, like bar charts, calculate new values to plot:
#' 
#' - \stressi{bar charts}, \stressi{histograms}, and \stressi{frequency polygons} bin your data and then plot the bin counts (i.e., the number of points that fall in each bin).
#' - \stressi{smoothers} fit a model to your data and then plot predictions from the model.
#' - \stressi{boxplots} compute a robust summary of the distribution and then display a specially formatted box.
#' 
#' 
#' ## Use geoms and stats interchangeably 
#' 
#' The algorithm used to calculate new values for a graph is called a **stat**, short for \stressi{statistical transformation}. 
#' 
#' \begin{figure}
#' \includegraphics[width=0.7\linewidth]{visualization-stat-bar.png}
#' \end{figure}
#' 
#' \textcolor{blue}{Every {\it geom} has a default {\it stat}; and every {\it stat} has a default {\it geom}. }
#' 
#' - Typically we can use geoms without worrying about the underlying statistical transformation.
#' 
#' ## Use geoms and stats interchangeably (example)
#' 
#' For example, you can recreate the previous plot using `stat_count()` instead of `geom_bar()`: \hspace{1cm} {\scriptsize (See \textcolor{red}{https://ggplot2.tidyverse.org/reference/})} 
#' 
## ----out.width='50%'-----------------------------------------------------
ggplot(data = mpg) + 
  stat_count(mapping = aes(x = class))

#' 
#' ## Overwrite the default stat
#' 
#' Create our own stats for plotting:
#' 
#' 
#' \small
## ------------------------------------------------------------------------
library(tibble)
demo <- tribble(
  ~cut,         ~freq,
  "Fair",       1610,
  "Good",       4906,
  "Very Good",  12082,
  "Premium",    13791,
  "Ideal",      21551
)

#' \normalsize
#' 
#' `tribble()` allows you to create small data frames row by row. 
#' 
#' The code above creates a small data frame containing two columns whose headings
#' are `cut` and `freq`.
#' 
#' ## Overwrite the default stat (`stat="identity"`)
#' 
#' Use `stat = "identity"` to map the height of the bars to the raw values of the `y` variable.
#' 
#' \small
## ----collapse=T, out.width='50%'-----------------------------------------
ggplot(data = demo) +
  geom_bar(mapping = aes(x=cut, y=freq), stat = "identity")

#' \normalsize
#' 
#' ## Display a bar chart of proportions
#' 
#' \footnotesize
## ----out.width='50%'-----------------------------------------------------
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

#' \normalsize
#' 
#' `geom_bar` has two calculated values: `count` and `prop`.
#' 
#' \footnotesize
#' https://github.com/tidyverse/ggplot2/issues/2051
#' https://www.gl-li.com/2017/08/13/ggplot2-group-overrides-default-grouping/
#' \normalsize
#' 
#' ## Plot the stats
#' 
#' \footnotesize
## ----collapse=T, out.width="50%"-----------------------------------------
ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

#' \normalsize
#' 
#' (`diamonds` is a small dataset that comes with the `ggplot2` library)
#' 
#' # Facet subplots
#' 
#' ## Subplots
#' 
#' \stress{Subplots} are helpful when you want to show different data presentations in a single view, e.g. a dashboard.
#' 
#' - `pairs()` produces a matrix of scatter plots for pairs of variables.
#'     + The variables must come from the same dataframe.
#'     
#' - `library(gridExtra)` arranges multiple plots in one view. 
#'     + can be any data frames
#'     
#' <br>
#' What if we want to visually compare the same pair of variables, but over *a subset of the data*? 
#' 
#' - use \stressi{subsetting} to extract multiple data frames?
#' 
#' 
#' 
#' ##   Facets - subplots that display subsets of the data
#' 
#' - We have seen that one way to add additional variables is with aesthetics. 
#' - Another way, particularly useful for categorical variables, is to split a plot into \stressi{facets}, i.e. \stressi{subplots}, each of which displays one subset of the data.
#' 
#' To facet your plot by a single variable, use `facet_wrap()`. 
#' 
#' - The first argument of `facet_wrap()` should be a \stressi{formula}, created with `~` followed by a variable name.
#' - Here "formula" is the name of a data structure in R, not a synonym for "equation".
#' - The variable that you pass to `facet_wrap()` should be discrete.
#' 
## ----eval=FALSE----------------------------------------------------------
## facet_wrap(~ class, nrow = 2)

#' 
#' ##   Facets - single variable example
#' 
#' \scriptsize
## ----out.width='60%'-----------------------------------------------------
ggplot(data = custdata) + 
  geom_point(mapping = aes(x = age, y = income)) + 
  facet_wrap(~ marital.stat, nrow = 2)

#' (Compare with the scatter plot for `income` against `age` and the hexbin diagram on earlier slides)
#' 
#' \normalsize
#' 
#' ##   Facets - two variable example
#' 
#' Subsetting data using two variables. 
#' 
#' \scriptsize
## ----out.width='55%'-----------------------------------------------------
ggplot(data = custdata) + 
  geom_point(mapping = aes(x = age, y = log(income))) + 
  facet_grid( sex ~ marital.stat )

#' 
#' (Try `facet_wrap` to see the difference)
#' 
#' \normalsize
#' 
#' # Coordinate systems
#' 
#' ## Coordinate flip `coord_flip()`
#' 
#' \small
## ------------------------------------------------------------------------
p1 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  theme(text = element_text(size = 12), aspect.ratio=1)
p2 <- ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip() +
  theme(text = element_text(size = 12), aspect.ratio=1)

#' \normalsize
#' 
#' ## Coordinate flip example
#' 
#' \scriptsize
## ----out.width="90%"-----------------------------------------------------
grid.arrange(p1, p2, ncol=2)

#' \normalsize
#' 
#' ## Polar coordinate
#' 
## ------------------------------------------------------------------------
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

p1 <- bar + coord_flip()
p2 <- bar + coord_polar()

#' 
#' ## Polar coordinate plot
#' 
## ----out.height='70%'----------------------------------------------------
grid.arrange(p1, p2, ncol=2)

#' 
#' ## Map Coordinates `coord_quickmap()`  
#' 
## ----out.width='90%'-----------------------------------------------------
library(maps)
w <- map_data("world")

p1 <- ggplot(w, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")

p2 <- ggplot(w, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  coord_quickmap()

#' 
#' Can simplify the second assignment statement to:
#' 
#' `p2 <- p1 + coord_quickmap()`
#' 
#' ## Map example
#' 
## ----out.width="90%"-----------------------------------------------------
grid.arrange(p1, p2, ncol=2)

#' ## Topics covered this Week
#' 
#' - The layer grammar of graphics
#'     + geom
#'     + aesthetic mapping
#'     + position adjustment
#'     + stat transformation
#'     + facets
#'     + coordinate systems
#' 
#' - What plots are suitable for two or more variables exploration?
#' 
#' - Interpreting plots for two or more variable exploration
#' 
#' ##   References
#' 
#' - **Practical Data Science with R**, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 3)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2017 (Chapters 3&7)
#' 
