#' ---
#' title: "Merging Datasets"
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
custdata <- read.table('../../data/custdata.tsv',header=T,sep='\t')

#' 
#' # Combining datasets
#' 
#' ## Adding observations (rows) to a data frame: `rbind()`
#' 
#' Vertical concatenation is typically used to add observations to a data frame.
#' 
## ----eval=FALSE----------------------------------------------------------------------
## total <- rbind(dataframeA, dataframeB)

#' 
#' The two data frames must have the same variables, but they don't have to be in the
#' same order. 
#' 
#' If `dataframeA` has variables that `dataframeB` doesn't, then before joining
#' them do one of the following:
#' 
#' - Delete the extra variables in `dataframeA`
#' - Create the additional variables in `dataframeB` and set them to NA (missing)
#' 
#' ## Combining variables (columns) of two data frames: `cbind()`
#' 
#' The two data frames must have the same number of rows.
#' 
#' Example:
#' 
#' \small
## ------------------------------------------------------------------------------------
df1 <- custdata[, c("custid","age")]
df2 <- custdata["income"]
# suppose that we now want to merge these two data frames
newdata <- cbind(df1, df2)
cat(nrow(newdata), ncol(newdata))
cat(names(newdata))

#' \normalsize
#' 
#' 
#' # Dealing with Relational Data using `merge()`
#' 
#' ## `merge()` &ndash; dealing with relational data using *join*
#' 
#' When working with a pair of tables, `merge()` will add new variables to one of the
#' data frames using one of the following criteria: 
#' 
#' - **Inner join** &mdash; only matching observations will be kept.
#' - **Left (or right) outer join** &mdash; matching observations as well as unmatched ones from left (or right) will be kept.
#' - **Full outer join** &mdash; matching observations as well as all unmatched ones from both tables are kept.
#' 
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{joins.png}
#' \end{figure}
#' 
#' ## An author-book example - authors
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
authors <- data.frame(
    surname = c("Tukey", "Venables", "Ripley",
                  "Tierney", "Winton"),
    nationality = c("US", "Australia", "NZ", "US", "UK"),
    deceased = c("yes", "yes", rep("no", 3)))
# kable() requires the knitr library
kable(authors)

#' \normalsize
#' 
#' Our first data frame `authors` has 5 rows and 3 columns.
#' 
#' ## An author-book example - books
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
             "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis", "Modern Applied Statistics",
              "LISP-STAT", "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis", "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA, "Venables & Smith"))
kable(books)

#' \normalsize
#' 
#' ## Inner join
#' 
#' `authors` and `books` do not have a \stressi{key} column (a common column) of the same name for merging.
#' We can rename the *key* column in one of the data frames.
#' 
#' \small
## ----collapse=T----------------------------------------------------------------------
# rename "surname" to "name" in the authors data frame
authorN <- within(authors, { name <- surname; rm(surname) })
kable(authorN)

#' \normalsize
#' 
#' ## Inner Join (cont.)
#' 
#' \small
## ----collapse=T----------------------------------------------------------------------
# R finds columns of matching names
m0 <- merge(authorN, books)
kable(m0)

#' - Only rows having matching values in the common column are retained
#'   in the output `m0`.
#' 
#' ## Inner Join (cont.)
#' 
#' Alternatively, we can explicitly state the matching columns in the
#' two data frames:
#' 
#' \footnotesize
## ----collapse=T----------------------------------------------------------------------
m1 <- merge(authors, books, by.x = "surname", by.y = "name")
kable(m1)

#' \normalsize
#' 
#' ## Left Outer Join
#' 
#' We can specify that we want a *left outer join*:, i.e., *all* the rows in the left
#' table (`authorN`) to appear in the output data frame:
#' 
#' \small
## ------------------------------------------------------------------------------------
m2 <- merge(authorN, books, all.x = TRUE)
kable(m2)

#' \normalsize
#' 
#' (Default value for `all.x` is `FALSE`)
#' 
#' ## Right Outer Join
#' 
#' \small
## ------------------------------------------------------------------------------------
m3 <- merge(authorN, books, all.y = TRUE)
kable(m3)

#' \normalsize
#' 
#' (Default value for `all.y` is `FALSE`)
#' 
#' ## Full Outer Join
#' 
#' \small
## ------------------------------------------------------------------------------------
m4 <- merge(authorN, books, all = TRUE)
kable(m4)

#' \normalsize
#' 
#' (same as specifying `all.x=T` and `all.y=T`)
#' 
#' ## Inner join &ndash; another example
#' 
#' Suppose that we want to create a new customer data frame having an extra
#' `income.normalised` variable, where the income of each customer is
#' normalised by the median income of state of residence of the customer.
#' 
#' **Step 1:** Calculate the state median income using the `aggregate()` function:
#' 
#' \footnotesize
## ----collapse=T----------------------------------------------------------------------
median.income <- aggregate(custdata[, 'income'],
                           list(custdata$state.of.res), median)
kable(head(median.income))
cat(nrow(median.income), ncol(median.income))

#' \normalsize
#' 
#' ## Inner join &ndash; another example (cont.)
#' 
#' - Note the default variable names after aggregation are `Group.1` and `x`.
#' 
#' **Step 2:** merge the two data frames:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
custdata <- merge(custdata, median.income,
                  by.x="state.of.res", by.y="Group.1")

#' \normalsize
#' 
#' **Step 3:** create a new column of normalised income:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
custdata$income.normalised <- with(custdata, income/x)
kable(custdata[1:6, c("custid","state.of.res","income","x","income.normalised")])

#' 
#' # Relational Data with `join(x,y)` in `dplyr`
#' 
#' ## Dealing with Realational Data using `dplyr`
#' 
#' Comparing `dplyr`'s `join` functions with the base `merge` function
#' 
#' \scriptsize
## ----eval=T, echo=F, collapse=T------------------------------------------------------
library(tibble)
table.dplyr_merge <- tribble(
  ~dplyr, 	~merge,
  "inner_join(x, y)", 	"merge(x, y)",
  "left_join(x, y)", 	"merge(x, y, all.x = TRUE)",
  "right_join(x, y)", 	"merge(x, y, all.y = TRUE)",
  "full_join(x, y)", 	"merge(x, y, all.x = TRUE, all.y = TRUE)"
  )
kable(table.dplyr_merge)

#' \normalsize
#' 
#' Comparing `dplyr`'s `join` functions with the `sql`
#' 
#' \scriptsize
## ----eval=T, echo=F, collapse=T------------------------------------------------------
table.dplyr_sql <- tribble(
  ~dplyr, 	~SQL,
  'inner_join(x, y, by = "z")',	'SELECT * FROM x INNER JOIN y USING (z)',
  'left_join(x, y, by = "z")', 	'SELECT * FROM x LEFT OUTER JOIN y USING (z)',
  'right_join(x, y, by = "z")',	'SELECT * FROM x RIGHT OUTER JOIN y USING (z)',
  'full_join(x, y, by = "z")', 	'SELECT * FROM x FULL OUTER JOIN y USING (z)'
  )
kable(table.dplyr_sql)

#' \normalsize
#' 
#' 
#' ## `dplyr` supports more joins
#' 
#' - **Mutating joins**, which add new variables to one data frame from the matching observations in another.
#'     + `inner_join(x,y)`
#'     + `left_join(x, y)`, `right_join(x, y)`, `full_join(x, y)` 
#' <!-- The output data frame has `ncol(x) + ncol(y) - 1` variables (columns). -->
#' 
#' - **Filtering joins**, which filter observations from one data frame based on whether or not they match an observation in the other table.
#'     + `semi_join(x, y)` keeps all observations in `x` that have a match in `y`.
#'     + `anti_join(x, y)` drops all observations in `x` that have a match in `y`.
#' <!--  The output data frame has `ncol(x)` variables (columns). -->
#' 
#' - **Set operations**, which treat observations as if they were set elements.
#'     + `intersect(x, y)` returns only observations in both `x` and `y`.
#'     + `union(x, y)` returns unique observations in `x` and `y`.
#'     + `setdiff(x, y)` returns observations that are in `x` but not in `y`.
#' 
#' ## Mutating joins - inner
#' 
#' \begin{figure}
#' \includegraphics[width=0.5\linewidth]{join-inner.png}
#' \includegraphics[]{join-one-to-many.png}
#' \includegraphics[width=0.5\linewidth]{join-many-to-many.png}
#' \end{figure}
#' 
#' ## Mutating joins - outer
#' 
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{join-outer.png}
#' \end{figure}
#' 
#' ## Filtering joins - `semi_join(x,y)`
#' 
#' `semi_join(x, y)` keeps all observations in `x` that have a match in `y`.
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{join-semi.png}
#' \end{figure}
#' 
#' Only the existence of a match is important; it doesn't matter which observation is matched. 
#' 
#' This means that filtering joins never duplicate rows like mutating joins do:
#' 
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{join-semi-many.png}
#' \end{figure}
#' 
#' ## Filtering joins - semi (example)
#' 
#' Filter the book data by keeping only books that have author information.
#' 
## ------------------------------------------------------------------------------------
library(dplyr)
m5 <- semi_join(books, authorN)
kable(m5)

#' 
#' ## Filtering joins - `anti_join(x,y)`
#' 
#' `anti_join` keeps all observations in `x` that don't have a match in `y`. Useful for diagnosing join mismatches.
#' 
#' \begin{figure}
#' \includegraphics[width=0.35\linewidth]{join-anti.png}
#' \end{figure}
#' 
## ------------------------------------------------------------------------------------
m6 <- anti_join(books, authorN)
kable(m6)

#' ## Set operations
#' 
#' Take the author and book data frame as an example,
#' 
## ----collapse=T----------------------------------------------------------------------
x <- authorN$name
y <- books$name

x

y

#' 
#' ## Set operations operates on a single variable
#' 
## ----collapse=T----------------------------------------------------------------------
setdiff(x, y) # What are in x but not y?

union(x, y)   # Duplications counted once only

intersect(x, y)  # Common elements in x and y

#' 
#' ## Take Home Message
#' 
#' - Merging two data frames
#' 
#' - Merging relational data
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 5: Sections 5.1-5.4)
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, [https://r4ds.had.co.nz/](https://r4ds.had.co.nz/) (Chapters 11,12,13,18)
#' 
#' 
#' <!-- ----------------------------------------------------------------------------- -->
#' 
## ----eval=T, echo=F------------------------------------------------------------------
# save the following data frames for another Rmd file (in case they are needed)
if (!file.exists("authors_books.rData")) {
	save(authors, books, file="authors_books.rData")
}

