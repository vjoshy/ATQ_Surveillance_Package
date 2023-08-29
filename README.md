
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ATQ: Assesing Evaluation Metrics for Timely Epidemic Detection Models

## Background

Madeline A. Ward published a
[paper](https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-019-7521-7)
on methods for detecting seasonal influenza epidemics using school
absenteeism data. The premise is that there is a school absenteeism
surveillance system established in Wellington-Dufferin-Guelph which uses
a threshold-based (10% of students absent) approach to raise alarms for
school illness outbreaks to implement mitigating measures. Two metrics
(FAR and ADD) are proposed in that study that were found to be more
accurate.

Based on the work of Madeline, in 2021 Kayla Vanderkruk along with Drs.
Deeth and Feng wrote a
[paper](https://bmcpublichealth.biomedcentral.com/articles/10.1186/s12889-023-15747-z)
on improved metrics, namely ATQ (alert time quality), that are more
accurate and timely than the FAR-selected models. This package is based
off Kayla’s work that can be found
[here](https://github.com/vanderkk/School_Abstenteeism_Based_Influenza_Surveillance_Simulation_Study).
ATQ study assessed alarms on a gradient, where alarms were raised
incrementally before or after an optimal date were penalized for lack of
timeliness.

This ATQ package will allow users to perform simulation studies and
evaluate ATQ & FAR based metrics from them. The simulation study will
require information from census data for a region such as distribution
of number of household members, households with and without children,
and age category, etc.

This package is still a work in progress and future considerations
include streamlining simulation study workflow and generalizing
evaluation of metrics to include real data sets.

## Installation:

You can install the development version of ATQ from
[Github](https://github.com/vjoshy/ATQ_Surveillance_Package)

``` r
#install.packages("devtools")
library(devtools)
install_github("vjoshy/ATQ_Surveillance_Package")
```

## Usage

ATQ contains eight funcions which are to be used sequentially in the
following order:  

1.  catchment_sim()
2.  elementary_pop()
3.  subpop_children()
4.  subpop_noChildren()
5.  simulate_households()
6.  simepi()
7.  model_data()
8.  eval_metrics()

The output data frame from the first function is to be used as input for
the next function and so forth.

Please see example below:

``` r
library(ATQ)

#Simulate catchment data
catch_df <- catchment_sim(16, 4.12, 3.01, 5)

#simulate elementary schools for each area
elementary_df <- elementary_pop(catch_df, 4.8, 0.015)

# Enters values for prompts from subpop_children() function
f <- file()
lines <- c(0.7668901,0.3634045, 0.4329440, 0.2036515,0.5857832, 0.3071523, 0.1070645,0.4976825)
ans <- paste(lines, collapse = "\n")
write(ans, f)
options("usr_con" = f) # set connection option

# simulating households with children
house_children <- subpop_children(elementary_df, n = 2)
#> Please enter proportion of parents as a couple: Enter proportion of coupled parents with 1, 2, 3+ children separated by space:Enter proportion of single parents with 1, 2, 3+ children separated by space:Please enter proportion of children that are of elementary school age:

# Enters values for prompts from subpop_nochildren() function
lines <- c(0.23246269, 0.34281716, 0.16091418, 0.16427239, 0.09953358, 0.4277052)
ans <- paste(lines, collapse = "\n")
write(ans, f)

# simulating households without children
house_nochildren <- subpop_noChildren(house_children, elementary_df)
#> Please enter proportion of households with 1, 2, 3, 4, 5+ members separted by space: Please enter proportion of households with children:

close(f) # close the file
options("usr_con" = stdin()) # reset connection option

# simulate households and individuals data
simulation <- simulate_households(house_children, house_nochildren)

# randomly sampling 1000 rows to reduce simulation times
individuals <- simulation$individual_sim[sample(nrow(simulation$individual_sim),1000),]

# simulate epidemic %>% 
epidemic <- simepi(individuals, b=3, sus=.0019, spark=0, num_inf = 2)

# simulate laboratory confirmed cases, and school absenteeism data sets
data <- model_data(epidemic, individuals)

# calculate and return metrics in a list
region_metric <- eval_metrics(lagdata = data, type = "r", thres = seq(0.1,0.6,by = 0.05),
                                        ScYr = c(2:10), yr.weights = c(1:9)/sum(c(1:9)))
```
