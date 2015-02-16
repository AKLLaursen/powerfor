# powerfor

powerfor contains all code written by Andreas Keller Leth Laursen for the master thesis: "An Empirical Comparison of Models for Forecasting Electricity Prices." The code covers:

* Data scraping.
* Data treatment.
* Model estimation.
* Out of sample forecasting.
* Plots.
* Tex table outputs.

Install the package by running:
```
devtools::install_github("AKLLaursen/powerfor")
```

Note that currently a non-CRAN version of gridExtra is required to plot the ACF, PACF and various other models.