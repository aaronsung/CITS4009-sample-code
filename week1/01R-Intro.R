#' ---
#' title: "Introduction to R"
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
## ----setup, echo=F-----------------------------------------------------
def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

#' 
#' ## History of R
#' - R was originally written by Ross Ihaka and
#' Robert Gentleman, at the University of
#' Auckland. The project was conceived in 1992, with an initial version released in 1995 and a stable beta version in 2000.
#' - It is an implementation of the S language,
#' which was principally developed by John
#' Chambers.
#' - In 1998, the Association for Computing
#' Machinery gave John Chambers its
#' Software Award. He said:
#' 
#' &nbsp;
#' 
#' >"S has forever altered the way people analyze,
#' visualize, and manipulate data ... It is an
#' elegant, widely accepted, and enduring
#' software system, with conceptual integrity."
#' 
#' \begin{figure}
#' https://www.r-project.org/
#' \includegraphics[width=0.2\linewidth]{Rlogo.png}
#' \end{figure}
#' 
#' ## RStudio the interactive environment
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{rstudio-console.png}
#' \end{figure}
#' ## R working environment
#' 
#' File path is relative to working directory
#' 
## ---- echo=TRUE--------------------------------------------------------
getwd()

#' 
## ---- echo=TRUE, eval=FALSE, eval=F------------------------------------
## setwd("c:/R/CITS4009")

#' 
#' Install a package using  `install.packages()`, e.g., to install the
#' `tidyverse` package:
#' 
## ---- echo = T, eval = F, size="small"---------------------------------
## install.packages("tidyverse")

#' 
#' Load a package library with `library()`.
#' 
#' ## Help if know the function name
#' 
#' Details about a specific function:
#' 
## ---- echo = TRUE, eval=F----------------------------------------------
## help(data.frame)
## 
## ## starting httpd help server ... done
## ? data.frame

#' ## Help if know the function name
#' 
#' See examples of a specific function:
#' 
## ---- echo = TRUE------------------------------------------------------
example(mean)

#' ## Help if know the function name
#' 
#' See demos of a package:
#' 
## ---- echo = TRUE, size="small"----------------------------------------
demo(graphics)

#' 
#' ## Search for a function by keywords:
#' 
#' HTML search engine lets you search for topics with regular expressions.  E.g., `help.search("csv$")` displays the links to the help pages of functions ended with `csv`.
#' 
#' Find commands containing a regular expression or object name:
#' 
## ---- echo = TRUE, size="small"----------------------------------------
apropos("var")

#' 
#' ## R as a calculator
#' 
## ---- echo = TRUE, size="small", collapse=T----------------------------
log2(32)

print(sqrt(2))

pi

seq(0, 5, length=6)

1+1:10


#' 
#' 
#' ## R as a graphic tool
#' 
## ---- echo=TRUE, out.width='80%'---------------------------------------
plot(sin(seq(0, 2*pi, length=100)))

#' 
#' ## Variables
#' 
#' Results of calculations can be stored in objects using the assignment operators:
#' 
#' - An arrow (`<-`) formed by a smaller than character and a hyphen without a space!
#' - The equal character (`=`).
#'     
## ---- echo=TRUE--------------------------------------------------------
a <- 1 + 2

#' These objects can then be used in other calculations. To print the object just enter the name of the object. 
#' 
## ---- echo=TRUE--------------------------------------------------------
a

#' 
#' ## Variable Name Restrictions
#' 
#' There are some restrictions when giving an object a name:
#' 
#' - Object names cannot contain 'strange' symbols like `!`, `+`, `-`, `#`.
#' - A dot (`.`) and an underscore (`_`) are allowed, also a name starting with a dot.
#' - Object names can contain a number but cannot start with a number.
#' - R is case sensitive, `X` and `x` are two different objects, as well as `temp` and `temP`.
#' 
#' ## Variable Types
#' Numeric
## ---- echo=TRUE, size="small", collapse=T------------------------------
a <- 49  # Numeric
sqrt(a)

b <- "The dog ate my homework" # Character String
sub("dog","cat",b)

c <- (1+1==3) # Logical
c

#' 
#' ## Vectors
#' 
#' In R a vector is an *ordered* collection of **data of the same type**
## ---- echo=TRUE, size="small"------------------------------------------
a <- c(1,2,3)
a*2

#' A single number (i.e. a scalar) is the special case of a vector with 1 element.
#' 
#' Other vector types: character strings, logical
#' 
#' ## Matrices
#' **matrix**: rectangular table of data of the same type
#' 
## ----echo=TRUE, size="small"-------------------------------------------
l
mdat
mdat[,1:2]

#' 
#' ## Arrays 
#' **Array**: higher-dimension matrix, turn a vector into multi-way dimensional matrix
## ----echo=TRUE---------------------------------------------------------
array(1:3, c(2,4)) # recycle 1:3 "2 2/3 times"

#' ## Arrays (example)
#' **Array**: higher-dimension matrix, turn a vector into multi-way dimensional matrix
## ----echo=TRUE---------------------------------------------------------
array(1:3, c(2,2,2))

#' 
#' ## Lists
#' **List**: *ordered* collection of **data of arbitrary types**. 
#' 
## ----echo=TRUE---------------------------------------------------------
doe <- list(name="john",age=28,married=F)
doe$name

#' 
#' Typically, vector elements are accessed by their index (an integer) and list elements by `$name` (a character string). But both types support both access methods. Slots are accessed by `@name`.
#' 
#' ## Data Frames
#' **Data frame**: rectangular table with rows and columns; 
#' - data within each column has the same type (e.g. number, text, logical), but 
#' - different columns may have different types.
#' Represents the typical data table that researchers come up with - like a spreadsheet.
#' 
## ----------------------------------------------------------------------
L3 <- LETTERS[1:3]
fac <- sample(L3, 5, replace = TRUE)
data.frame(x = 1, y = 1:5, fac = fac)

#' 
#' 
#' ## Take home messages
#' 
#' * The data science life cycle
#' 
#' * Basic R syntax
#' 
#' 
#' ## References
#' 
#' - **R for Data Science (2e)**, *Hadley Wickham, Garrett Grolemund*, O'Reilly, 2nd Ed. (available online: https://r4ds.hadley.nz/) (Chapters 1)
#' - **Practical Data Science with R**, *Nina Zumel, John Mount*, Manning, 2nd Ed., 2020 (available in Barry J. Marshall Library, UWA: 006.312 2020 PRA) (Part 1, Chapter 1)
#' - **Introduction to the R language:** https://users.soe.ucsc.edu/~lshiue/bioc/Rintro.ppt
#' - **An Introduction to R:** http://csg.sph.umich.edu/abecasis/class/815.04.pdf
#' - (optional) **Wikipedia:** https://en.wikipedia.org/wiki/R_(programming_language)
#' 
