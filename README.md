# `powerfor` An R Package for Electricity Forecast Comparison

## Introduction
This package provides all code used in the master thesis entitled *An Empirical Comparison of Models for  Forecasting Electricity Prices* written by **Andreas Keller Leth Laursen**. All code is written and maintained by the same author. Due to time constraint, the code is a bit messy. Thus a torough description is given here.

## Installation
You can install `powerfor` from github using `devtools` by running:

```
install.packages("devtools")
devtools::install_github("AKLLaursen/powerfor")
```

## Structure
Here the structure of the package is given, making browsing easier.

* /inst/ 
	* /inst/forecasts contains the forecasts computed for the paper.
	* /inst/matlab contains the MATLAB code written for the paper. An implementation of the GARCH model with AR mean and jump process proved impossible in R. Hence these Matlab script are provided, aimed at estimating these type models and forecasting based on them. An implementaion in R is possible using the `R.matlab` package.
	* /inst/rds contains the data used in the paper.
*  /R/
	*  /R/data_treatment.R Contains all data treatment and filtering functions utilized in the study.
	*  /R/forecast.R Contains the forecasting functions used in calculating the out-of-sample forecast of the base load price, and the individual hour price.
	*  /R/forecasting_output.R Contains the code used to calculate the statistical and economic loss functions.
	*  /R/likelihood_functions.R Contains an R implementation of the MATLAB code using `R.matlab`.
	*  /R/plot_functions.R Contains all functions for all the various plots shown in the paper.
	*  /R/RcppExports.R Contains inports of various C++ functions.
	*  /R/scraping.R Contains a function for scraping solar data off of the EEX transparancy webpage.
	*  /R/support_vector_machines.R contains a function used in selecting the optimal free parameters for the SVM models used in the paper.
	*  /R/table_output.R Contains all functions for computing the table outputs of the BICs and parameters estimates of the various mdoels.
*  /src/
	*  /src/ewma.cpp Contains a C++ function for calculating the exponentially weighted moving average of a series.
	*  /src/loglik.cpp Contains various C++ implementations of the likelihood functions used in the paper. This code was used, when attempts were made at fitting the GARCH models with AR mean and jump process using R solvers.

Any questions or comments can be directed at andreas.keller[at]gmail.com