#' ---
#' title: "Pipes (%>% or |>) "
#' subtitle: "CITS4009 Exploratory Data Analysis"
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
custdata <- read.table('../../data/custdata.tsv', header=T, sep='\t')

#' 
#' ## Piping 
#' 
#' The pipe `%>%` comes from the `magrittr` package by Stefan Milton Bache, which comes with `tidyverse`.
#' 
#' You can also use `library(magrittr)` or `library(dplyr)` to get the pipe symbol `%>%` or `|>`
#' defined in your R environment.
#' 
#' ## Why piping? &ndash; A motivating example
#' 
#' \small
## ------------------------------------------------------------------------------------
library(crayon)
little_bunny <- function(name) {
  return("Little bunny " %+% name)
} 

hop <- function(data, through) {
  return(data %+% "\nWent hopping through the " %+% through)
}

scoop <- function(data, up) {
  return(data %+% "\nScooping up the " %+% up) 
}

bop <- function(data, on) {
  return(data %+% "\nAnd bopping them on the " %+% on)
}

#' \normalsize
#' 
#' ## Options to produce the popular Children's peom
#' 
#' - Save each intermediate step as a new object.
#' - Overwrite the original object many times.
#' 
#' \scriptsize
## ----collapse=T----------------------------------------------------------------------
s <- little_bunny("Foo Foo")
s <- hop(s, "forest")
s <- scoop(s, "field mice")
s <- bop(s, "head")
cat(s)

#' \normalsize
#' 
#' - Compose functions:
#' 
#' \scriptsize
## ----collapse=T----------------------------------------------------------------------
s <-  bop(scoop(hop(little_bunny("Foo Foo"), "forest"), "field mice"), "head")
cat(s)

#' \normalsize
#'   
#' - Use the pipe.
#' 
#' ## The pipe version
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
library(dplyr)

little_bunny("Foo Foo") |>
hop(through = "forest") |>
scoop(up = "field mice") |>
bop(on = "head") |>
cat()

#' \normalsize
#' 
#' ## Using `%>%` to connect a series of operations
#' 
#' You can use `%>%` to connect any chain of operations (i.e., function calls) so long as the output
#' of the preceding function fits with the input required by the next function
#' in the chain.
#' Most R functions accept the input in the first argument. By default,
#' `%>%` will pass the output from the preceding function to the first
#' argument of the next function.
#' 
#' **Example 1.** Chaining two `mutate()` function calls using `%>%`
#' (for data cleaning in the previous lecture):
#' 
#' \footnotesize
## ----eval=F--------------------------------------------------------------------------
## customer_data <- customer_data %>%
##   mutate(
##     gas_with_rent = (gas_usage ==1),
##     gas_with_electricity = (gas_usage == 2),
##     no_gas_bill = (gas_usage == 3)
##   ) %>%
##   mutate(
##     gas_usage = ifelse(gas_usage < 4, NA, gas_usage)
##   )

#' \normalsize
#' 
#' ## Using `%>%` to connect a series of operations (cont.)
#' 
#' **Example 2.** Perform an inner join of the `authors` and `books` data frames
#' (see earlier slides), subset to select authors from NZ, and print the output data frame.
#' 
#' \footnotesize
## ----eval=T, echo=F------------------------------------------------------------------
load("authors_books.rData")

#' 
## ----collapse=T----------------------------------------------------------------------
# kable() requires the knitr library
merge(authors, books, by.x="surname", by.y="name") %>%
	subset(nationality=="NZ") %>%
	kable()

#' \normalsize
#' 
#' Note: we don't need to specify the input data frame when calling `subset()`
#' as the data comes from the output of `merge()`. The condition `nationality="NZ"`
#' which specifies the row selection becomes the 1st argument for `subset()`.
#' Similarly, no argument is needed for `kable()`.
#' 
#' ##Using `%>%` to connect a series of operations (cont.)
#' 
#' **Example 2.** (cont.)
#' 
#' The `merge()` function call in the code on the previous slide can be
#' replaced by `inner_join()`:
#' 
#' \footnotesize
## ----collapse=T----------------------------------------------------------------------
result1 <- merge(authors, books, by.x="surname", by.y="name")
result2 <- inner_join(authors, books, by=c("surname"="name"))

#' 
#' \begin{verbatim}
#' > result1[,1:3]                         > result2[,1:3]
#'    surname nationality deceased            surname nationality deceased
#' 1   Ripley          NZ       no         1    Tukey          US      yes
#' 2   Ripley          NZ       no         2 Venables   Australia      yes
#' 3  Tierney          US       no         3   Ripley          NZ       no
#' 4    Tukey          US      yes         4   Ripley          NZ       no
#' 5 Venables   Australia      yes         5  Tierney          US       no
#' \end{verbatim}
#' 
#' \normalsize
#' 
#' Comparing the two output data frames, we can see that `merge()` sorts the 
#' column `surname` in ascending (alphabetical) order but `inner_join()`
#' keeps the order of the rows unchanged.
#' 
#' ## Using `%>%` to connect a series of operations (cont.)
#' 
#' **Example 3.** Consider the following data frames:
#' 
#' \scriptsize
## ------------------------------------------------------------------------------------
library(tibble)
students <- tribble (
   ~name, ~degree, ~start.year, ~mode,
   "John", "MDS", 2020, "part-time",
   "Jack", "MIT", 2019, "full-time",
   "Rose", "BSc", 2020, "full-time",
   "Mary", "MDS", 2018, "part-time",
   "Paul", "BPhil", 2020, "full-time"
   )

degrees <- tribble (
   ~degree, ~duration,
   "BPhil", 4,
   "BSc", 3,
   "MDS", 2,
   "MIT", 2,
   "MPE", 2
)

#' \normalsize
#' 
#' ## Using `%>%` to connect a series of operations (cont.)
#' 
#' **Example 3.** cont.
#' 
#' \scriptsize
## ------------------------------------------------------------------------------------
units <- tribble (
   ~unit, ~degree,
   "CITS1401", "BSc",
   "CITS4009", "MDS",
   "CITS4009", "MIT",
   "CITS4401", "MIT",
   "CITS4402", "BPhil",
   "CITS5508", "MDS",
   "CITS5508", "MIT"
)

#' \small
#' 
#' Work out the R code, which includes using `%>%`, for
#' 
#' 1. generating a data frame containing the units that Jack needs to complete
#'   for the degree he's enrolled in. Your data frame only needs to have a
#'   single variable (column).
#' 
#' 1. finding out the year that  Mary is expected to graduate, assuming that
#'   part-time enrolments take twice the number of years compared
#'   to full-time enrolments.
#' 
#' Sample solutions are given after the **References** slide. Try to work out the R code
#' yourself before looking at them.
#' 
#' \normalsize
#' 
#' 
#' ##   References 
#' 
#' - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, https://r4ds.had.co.nz/  
#'     - Chapter 18 has a very good introduction on `%>%`
#'     - Section 27.2 also has a small example of `%>%` written as R code embedded in R markdown. In the example there, try to replace `geom_freqpoly` by `geom_histogram` and compare the two plots side-by-side (using `grid.arrange` from the `gridExtra` library).
#' 
#' \bigskip
#' 
#' \textcolor{red}{Sample solutions for the two problems for example 3 are on the next 3 slides.}
#' 
#' ## Sample solutions for example 3
#' 
#' Problem 1:
#' 
#' \scriptsize
## ------------------------------------------------------------------------------------
Jack.units <- subset(students, name=="Jack", select="degree") %>%
   merge(units) %>% subset(select="unit")
cat("Jack's list of units:")
kable(Jack.units)

#' \normalsize
#' 
#' The following will work also:
#' 
#' \scriptsize
## ----eval=F--------------------------------------------------------------------------
## Jack.units <- students %>% subset(name=="Jack", select="degree") %>%
##    merge(units) %>% subset(select="unit")

#' \normalsize
#' 
#' ## Sample solutions for example 3 (cont.)
#' 
#' Another alternative solution is:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
Jack.units <- merge(students, units) %>% 
   subset(name=="Jack", select="unit")
cat("Jack's list of units:")
kable(Jack.units)

#' \normalsize
#' 
#' Although only one `subset()` call is required here, the `merge()` function has to merge
#' two larger data frames and then discard most rows of the result.
#' 
#' ## Sample solutions for example 3 (cont.)
#' 
#' Problem 2:
#' 
#' \footnotesize
## ------------------------------------------------------------------------------------
df <- subset(students, name=="Mary") %>% inner_join(degrees, by="degree")
graduation.year <- df$start.year +
   df$duration * ifelse(df$mode == "part-time", 2, 1)
cat("Mary is expected to graduate in", graduation.year)

#' \normalsize
#' 
#' Your code does not need to be identical to the sample code above.
#' 
#' 
#' <!-- ----------------------------------------------------------------------------- -->
#' 
## ----eval=T, echo=F------------------------------------------------------------------
# save the following data frames for another Rmd file (in case they are needed)
if (!file.exists("students_degrees_units.rData")) {
        save(students, degrees, units, file="students_degrees_units.rData")
}

