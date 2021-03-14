# MDPI special issues

This repository contains scripts to scrape MDPI website and get the number of special issues for all of their journals with an Impact Factor. They are 74 as of March 12th, 2021.

It also contains an R script to generate a summary plot.

## I just want the data!

The data you want is provided in two .csv files.

-   `journals.csv` contains basic data on 74 MDPI journals
-   `SIs.csv` contains a line for each MDPI special issue ever made, or now open and in progress, for which a deadline exists.
-   `summary.csv` is a summary dataset containing one row per journal and one column per year, and summarising the number of SIs per year.

## I just want the plot!

You want the `MPDI_special_issues_2013-21.png` file.

Or just look here:

![](MDPI_special_issues_2013-21.png)

## I want to reproduce the analysis!

1.  get your dependencies right. The code depends on

    -   `tidyverse` for everything from wrangling to plotting

    -   `rvest` for scraping

    -   `stringr` for string manipulation

    -   `purrr` to use `map` and avoid loops

    -   `ggrepel` for the plot labels

    -   `ggbeeswarm` for the position of the points in the plot

    -   `hrbrthemes` for eye-candy and beautiful typography in the plot

2.  run `scraping.R`. It takes **about 20 minutes to run**. It generates `journals.csv`, with journals basic data, and `SIs.csv`, with the list of all 55k+ special issues of the 74 MDPI journals.

3.  (if you want to plot) run `plotting.R`

## I want to change the analysis / include more journals!

Feel free to clone this repo. I would appreciate if you kept me posted on what you do.

## 
