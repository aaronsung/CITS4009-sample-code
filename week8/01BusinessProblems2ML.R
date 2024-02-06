#' ---
#' title: "Map Business Problems to Machine Learning"
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
## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
# library(ggplot2)
# library(crayon)

#' 
#' ## The modelling process in the lifecycle of a DS project
#' 
#' \begin{figure}
#' \includegraphics[width=0.4\linewidth]{../figures/06-program-life-cycle.png}
#' \end{figure}
#' 
#' \scriptsize
#' __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 6)
#' \normalsize
#' 
#' 
#' ## Machine learning - the core of data science
#' 
#' 
#' \begin{figure}
#' \includegraphics[width=0.7\linewidth]{machine-learning.png}
#' \end{figure}
#' 
#' https://towardsdatascience.com/machine-learning-types-2-c1291d4f04b1?gi=102f83adbb40
#' 
#' ## Objectives
#' 
#' \begin{figure}
#' \includegraphics[width=0.6\linewidth]{schematic.png}
#' \end{figure}
#' 
#' - Mapping business problems to machine learning tasks
#' - Evaluating model quality
#' - \textcolor{gray}{Validating model soundness}
#' 
#' ## Example of problems in an online retail company: 
#' 
#' - Predicting what customers might buy, based on past transactions
#' - Identifying fraudulent transactions
#' - Determining price elasticity (the rate at which a price increase will decrease sales, and vice versa) of various products or product classes
#' - Determining the best way to present product listings when a customer searches for an item
#' - Customer segmentation: grouping customers with similar purchasing behavior
#' - AdWord valuation: how much the company should spend to buy certain AdWords on search engines
#' - Evaluating marketing campaigns 
#' 
#' 
#' 
#' ## What methods should we use?
#' 
#' \begin{figure}
#' \includegraphics[width=0.7\linewidth]{machine-learning-extended.jpeg}
#' \end{figure}
#' 
#' 
#' https://medium.com/@chris_bour/an-extended-version-of-the-scikit-learn-cheat-sheet-5f46efc6cbb
#' 
#' 
#' 
#' ## What methods should we use?
#' 
#' \begin{figure}
#' \includegraphics[width=0.8\linewidth]{ml_map.png}
#' \end{figure}
#' 
#' http://scikit-learn.org/stable/tutorial/machine_learning_map/index.html
#' 
#' 
#' 
#' ##   References
#' 
#' - __Practical Data Science with R__, _Nina Zumel, John Mount_, Manning, 2nd Ed., 2020 (Chapter 6)
#' 
#' - 5 most useful differences between data science and machine learning: https://www.educba.com/data-science-vs-machine-learning/
