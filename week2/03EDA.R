#' ---
#' title: "Exploratory Data Analysis"
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
#'   #html_document:
#'   
#'   beamer_presentation:
#'     theme: "AnnArbor"
#'     colortheme: "dolphin"
#'     fonttheme: "structurebold"
#'     
#' ---
#' 
## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(ggplot2)
library(crayon)

#' 
#' # Visualisation
#' 
#' ## What's visualisation
#' 
#' Pictures are often better than text.
#' 
#' >We cannot expect a small number of numerical values [summary statistics] to
#' consistently convey the wealth of information that exists in data. Numerical reduction
#' methods do not retain the information in the data.
#' -William Cleveland
#' 
#' The use of graphics to examine data is called *visualization*.
#' 
#' ## William Cleveland's Graphic Philosophe
#' 
#' - A fine balancing act
#'     + A graphic should display as much information as it can, with the \textcolor{blue}{lowest possible
#' cognitive strain} to the viewer. 
#' 
#' - Strive for clarity. Make the data stand out. Specific tips for increasing clarity
#' include
#'     + Avoid too many superimposed elements, such as too many curves in the
#' same graphing space.
#'     + Find the right aspect ratio and scaling to properly bring out the details of the
#' data.
#'     + Avoid having the data all skewed to one side or the other of your graph.
#' 
#' - Visualization is an iterative process. Its purpose is to answer questions about the
#' data.
#'     + Different graphics are best suited for answering different questions.
#' 
#' # Exploratory Data Analysis (EDA)
#' 
#' ## What is exploratory data analysis?
#' 
#' Exploratory data analysis, or EDA for short is a task that use visualisation and transformation to explore your data in a systematic way. 
#' 
#' EDA is an iterative cycle that involves
#' 
#' - Generate questions about your data.
#' - Search for answers by *visualising*, *transforming*, and *modelling* your data.
#' - Use what you learn to refine your questions and/or generate new questions.
#' 
#' EDA is not a formal process with a strict set of rules. More than anything, EDA is a state of mind. 
#' 
#' The **goal** during EDA is to develop an understanding of your data. 
#' 
#' The easiest way to achieve the goal is to use \textcolor{blue}{questions} as tools to guide your investigation
#' 
#' ## How to ask good questions?
#' 
#' EDA is fundamentally a creative process. 
#' 
#' Like most creative processes, the key to asking *quality* questions is to generate a large *quantity* of questions.
#' 
#' Two types of questions will always be useful for making discoveries within your data:
#' 
#' - What type of *variation* occurs within my variables?
#' - What type of *covariation* occurs between my variables?
#' 
#' ## Terms used in EDA
#' 
#' - A **variable** is a quantity, quality, or property that you can measure.
#' 
#' - A **value** is the state of a variable when you measure it. The value of a variable may change from measurement to measurement.
#' 
#' - An **observation** is a set of measurements made under similar conditions (you usually make all of the measurements in an observation at the same time and on the same object). 
#'     + An observation will contain several values, each associated with a different variable. 
#'     + An observation is also referred to as a data point.
#' 
#' - **Tabular data** is a set of values, each associated with a variable and an observation. Tabular data is tidy if each value is placed in its own "cell", each variable in its own column, and each observation in its own row.
#' 
#' ## What to look for in histograms and bar charts?
#' 
#' In both bar charts and histograms, tall bars show the common values of a variable, and shorter bars show less-common values. 
#' 
#' Places that do not have bars reveal values that were not seen in your data. 
#' 
#' To turn this information into useful questions, look for anything unexpected:
#' 
#' - Which values are the most common? Why?
#' - Which values are rare? Why? Does that match your expectations?
#' - Can you see any unusual patterns? What might explain them?
#' 
#' ## Does the data form subgroups?
#' 
#' Clusters of similar values suggest that subgroups exist in your data. To understand the subgroups, ask:
#' 
#' - How are the observations within each cluster similar to each other?
#' - How are the observations in separate clusters different from each other?
#' - How can you explain or describe the clusters?
#' - Why might the appearance of clusters be misleading?
#' 
#' ## Comparing two or more varialbes
#' - *Variation* describes the behavior within a variable, 
#' - *Covariation* describes the behavior between variables. 
#' 
#' **Covariation** is the tendency for the values of two or more variables to vary together in a related way. 
#' 
#' The best way to spot covariation is to visualise the relationship between two or more variables. How you do that should again depend on the type of variables involved.
#' 
#' - a continuous variable and a categorical (categorical variable can be used as lengend, aesthetic mapping)
#' - two categorical variables (`geom_count` and `geom_tile`)
#' - two continuous variables (`geom_point` and `geom_boxplot` and `geom_bin2d` or `geom_hex`)
#' 
#' ## Questions to ask for Covariation
#' 
#' Patterns in your data provide clues about relationships. If a systematic relationship exists between two variables it will appear as a pattern in the data. If you spot a pattern, ask yourself:
#' 
#' - Could this pattern be due to coincidence (i.e. random chance)?
#' - How can you describe the relationship implied by the pattern?
#' - How strong is the relationship implied by the pattern?
#' - What other variables might affect the relationship?
#' - Does the relationship change if you look at individual subgroups of the data?
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
