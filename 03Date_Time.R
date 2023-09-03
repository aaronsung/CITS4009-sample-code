#' ---
#' title: "Dealing with Date and Time"
#' subtitle: "CITS4009 Computational Data Analysis"
#' author: "A/Prof Wei Lu"
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
## ----setup, include=FALSE-----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.align = "center", message=FALSE, warning=FALSE)
library(knitr)
library(ggplot2)
library(crayon)

#' 
#' ## Date and time can get complicated
#' 
#' The more you learn about dates and times, the more complicated they seem to get.
#' 
#' - Does every year have 365 days?
#' 
#' - Does every day have 24 hours?
#' 
#' - Does every minute have 60 seconds?
#' 
#' Not every year has 365 days, but do you know the full rule for determining if a year is a leap year? 
#' 
#' Many parts of the world use daylight savings time (DST), so that some days have 23 hours, and others have 25. 
#' 
#' Some minutes have 61 seconds because every now and then leap seconds are added because the Earth's rotation is gradually slowing down.
#' 
#' ## The 100/400 exclusion rule of leap years
#' 
#' In the Gregorian calendar, a normal year consists of 365 days.
#' 
#'    - The actual length of a sidereal year (the time required for the Earth to revolve once about the Sun) is actually 365.2425 days.
#'       
#'       + A "leap year" of 366 days is used once every four years to eliminate the error caused by three normal (but short) years. 
#'    
#'    - However, there is still a small error that must be accounted for. 
#'    
#'       + To eliminate this error, the Gregorian calendar stipulates that a year that is evenly divisible by 100 (for example, 1900) is a leap year only if it is also evenly divisible by 400.
#'    
#' https://docs.microsoft.com/en-us/office/troubleshoot/excel/determine-a-leap-year
#' 
#' ## Number of days in a month or a year
#' 
#' \footnotesize
## ----size="small", collapse=T-------------------------------------------------------------------------
library(Hmisc)

monthDays(as.Date('2020-02-01'))
monthDays(as.Date('2022-02-01'))
monthDays(as.Date('2000-02-01'))
monthDays(as.Date('1900-02-01'))

yearDays(as.Date('1900-02-01'))

#' \normalsize
#' 
#' ## Calendar dates and times
#' 
#' There are three types of date/time data that refer to an instant in time: 
#' 
#' - A date.
#' - A time within a day. 
#' - A date-time is a date plus a time: it uniquely identifies an instant in time (typically to the nearest second)
#' 
#' # Parsing Dates
#' 
#' ## Parsing dates
#' 
#' * Dates are typically entered into R as character strings and then translated into *date* variables that are stored numerically. 
#' * The function `as.Date()` is used to make this translation.
#' * The syntax is `as.Date(x, "input_format")`, where `x` is the character string and `input_format` gives the appropriate format for reading the date.
#' 
#' \small
#' 
#' --------------   ------------------------  --------------
#'    Symbol        Meaning                   Example   
#' --------------   ------------------------  --------------   
#'    `%d`          Day as a number           (1-31) 01-31
#'    
#'    `%a`          Abbreviated weekday       Mon 
#'    
#'    `%A`          Unabbreviated weekday     Monday
#'    
#'    `%m`          Month                     (1-12) 01-12
#'    
#'    `%b`          Abbreviated month         Jan
#'    
#'    `%B`          Unabbreviated month       January
#'    
#'    `%y`          2-digit year              18
#'    
#'    `%Y`          4-digit year              2018
#' --------------   ------------------------  -------------- 
#' 
#' \normalsize
#' 
#' ## Date Example
#' 
#' The default format for inputting dates is `yyyy-mm-dd`. 
#' 
#' This statement converts the character string vector using default format.
## ----size="small", collapse=T-------------------------------------------------------------------------
mydates <- as.Date(c("2007-06-22", "2004-02-13"))
class(mydates)

#' In contrast, the following reads the data using the `mm/dd/yyyy` format.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
strDates <- c("01/05/1965", "08/16/1975")
dates <- as.Date(strDates, "%m/%d/%Y")

#' 
#' ## Current dates
#' 
#' `Sys.Date()` returns today's date, of class type `Date`.
## ----size="small", collapse=T-------------------------------------------------------------------------
Sys.Date()

#' 
#' `date()`  returns the current date and time, of class type `character`.
## ----size="small", collapse=T-------------------------------------------------------------------------
date()

#'  
#' `Sys.time()` contains timezone, of class `c("POSIXct" "POSIXt")`.
## ----size="small", collapse=T-------------------------------------------------------------------------
Sys.time()

#' 
#' # Formatting dates
#' 
#' ## Formatting dates
#' 
#' We can use `format(x, format="output_format")` to format a date variable:
#' 
## ----size="small"-------------------------------------------------------------------------------------
today <- Sys.Date()
format(today, format="%B %d %Y")

format(today, format="%A")

#' 
#' ## Extracting information from date/time
#' 
#' `weekdays()` and `months()` return a character vector of names in the locale in use.
## ----size="small", collapse=T-------------------------------------------------------------------------
weekdays(Sys.time())

#' 
#' `quarters()` returns a character vector of "`Q1`" to "`Q4`".
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
quarters(Sys.time())

#' 
#' # Extracting information from dates
#' 
#' ## Extracting information from date/time
#' 
#' `julian()` returns the number of days (possibly fractional) since the *origin* day (1970 Jan 1st), which can be changed by the `origin` argument.
#' 
#' All of the time calculations in R are done ignoring leap-seconds.
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
julian(Sys.time())

#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
julian(Sys.time(), origin = as.Date("2022-08-01"))

#' 
#' # Using dates in calculation
#' 
#' ## Dates used for calculation 
#' 
#' When R stores dates internally, they're represented as the number of days since
#' **January 1, 1970**, with negative values for earlier dates. 
#' 
#' We can check the number of days using `as.double()`
## ----size="small", collapse=T-------------------------------------------------------------------------
dob <- as.Date("1956-10-12")
dob_num_days <- as.double(dob)
dob_num_days

#' We can also convert this number back to a date object:
## ----size="small", collapse=T-------------------------------------------------------------------------
as.Date(dob_num_days, origin=as.Date("1970-01-01"))

#' 
#' 
#' ## Finding Time Difference
#' 
## ----size="small", collapse=T-------------------------------------------------------------------------
today <- Sys.Date()

d <- difftime(today, dob, units="weeks")
class(d)
d

#' 
#' ## Use Time Difference in Calculation (class `difftime` )
#' 
## ----eval=FALSE, size="small", collapse=T-------------------------------------------------------------
## difftime(time1, time2, tz,
##          units = c("auto", "secs", "mins", "hours",
##                    "days", "weeks"))

#' To get the number weeks for calculation, we can use `as.double(x)`, where `x` is of class `difftime`.
## ----size="small", collapse=T-------------------------------------------------------------------------
age <- as.double(d)/52
age

#' ##   Take home messages
#' 
#' - Parsing dates
#' 
#' - Formatting dates
#' 
#' - Extracting information out of date/time values
#' 
#' - Use date/time for calculation
#' 
#'   
#' 
#' <!-- ##   References -->
#' <!--  -->
#' <!-- - **R for Data Science**, *Hadley Wickham, Garrett Grolemund*, Chapter 16 [https://r4ds.had.co.nz/dates-and-times.html](https://r4ds.had.co.nz/dates-and-times.html) -->
