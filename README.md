# MDPI special issues and Editorial History

## Update April 20th, 2020

I reran the analysis to account for a bug spotted by Christos Petrou on twitter. I was unaware of the existence of Collections and Sections at MDPI, and had labelled every paper in Collections and Sections as a Special Issue article. The analysis now takes into account all four types of papers (Normal and Special issues, Sections and Collections). The main message does not change.

This repository contains:

-   in the `Special Issues` folder:

    -   scripts to scrape MDPI website and get the number of special issues for all of their 74 journals with an Impact Factor;

    -   data of the scraping, ran on April 15th, 2021;

    -   analysis of the scraping data in the form of a plot and a summary table.

-   In the `Editorial History` folder:

    -   scripts to scrape the last 6 years' worth of articles published at the 74 MDPI journals with an IF;

    -   data resulting from the scraping, ran in the third week of April, 2021;

    -   analysis of the scraping data, in the form of a long .R file producing several plots and tables.

The data was the basis for a [blog post on MDPI](https://paolocrosetto.wordpress.com/2021/04/12/is-mdpi-a-predatory-publisher/) practices that appeared on my blog.

## I just want the data!

The data you want is provided in several .csv files.

-   for the Special Issues:

    -   `journals.csv` contains basic data on 74 MDPI journals

    -   `SIs.csv` contains a line for each MDPI special issue ever made, or now open and in progress, for which a deadline exists.

    -   `summary.csv` is a summary dataset containing one row per journal and one column per year, and summarising the number of SIs per year.

-   for the Editorial History and turnaround times

    -   one csv for each journal in the `Data` folder

## I just want the analysis!

Have a look at the [blog post](https://paolocrosetto.wordpress.com/2021/04/12/is-mdpi-a-predatory-publisher/). Alternatively, you find the plots and tables in the `Special Issues` and `Editorial History` subfolders.

## I want to reproduce the analysis!

1.  get your dependencies right. The code depends on

    -   `tidyverse` for everything from wrangling to plotting

    -   `rvest` for scraping

    -   `stringr` for string manipulation

    -   `purrr` to use `map` and avoid loops

    -   `lubridate` to work with dates

    -   `magrittr` for the `%$%` operator

    -   `ggridges`, `gghalves`, `ggbeeswarm`, `waffle`, `patchwork`, `ggrepel`, `ggtext`, that are all tools to extend `ggplot`

    -   `hrbrthemes` for eye-candy and beautiful typography in the plot

2.  for the Special Issues:

    1.  run `scraping.R`. It takes **about 20 minutes to run**. It generates `journals.csv`, with journals basic data, and `SIs.csv`, with the list of all 55k+ special issues of the 74 MDPI journals.

    2.  (if you want to plot) run `plotting.R`

3.  for the Editorial History:

    1.  run `scrape_editorial_history.R`. It takes **forever** to run, up to a week or more, use parallel calls, or simply do not run it. If you really want to run it consider using the Rstudio `jobs` api that allows to parallelize the scrape. This is what the `rstudio_jobs_scrape.R` script is for.

    2.  run `analyse_editorial_history.R`. It produces several different outputs and is at this point rather messy. You know, open science is messy and this is just a side project, sorry!

## I want to change the analysis / include more journals!

Feel free to clone this repo. I would appreciate if you kept me posted on what you do.

## 
