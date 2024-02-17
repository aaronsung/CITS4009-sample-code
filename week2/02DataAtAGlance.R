#' ---
#' title: "Data at a Glance"
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
## ----setup, include=FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center")
library(ggplot2)
library(crayon)

#' ## The Customer Dataset
#' Synthetic example data derived from Census PUMS data to predict the probability of health insurance coverage. 
#' 
#' Data can be obtained from: https://github.com/WinVector/zmPDSwR/tree/master/Custdata
#' 
## ----------------------------------------------------------------------
custdata <- read.table('custdata.tsv', header=T, sep='\t')

#' ## Customer Data Structure
## ---- size="small"-----------------------------------------------------
str(custdata)

#' 
#' ## Customer Data Summary
## ---- size="small"-----------------------------------------------------
summary(custdata)

#' 
#' ## Using Summary Statistics to spot problems
#' 
#' In R, you’ll typically use the `summary()` command to take your first look at the data.
#' 
#' The goal is to understand whether you have the kind of customer information that
#' 
#' - can potentially help you predict health insurance coverage, and 
#' - whether the data is of
#' good enough quality to be informative.
#' 
#' ## Looking for several common issues:
#' - Missing values
#' 
#' - Invalid values and outliers
#' 
#' - Data ranges that are too wide or too narrow
#' 
#' - The units of the data
#' 
#' ## Read the summary
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{summary.png}
#' \end{figure}
