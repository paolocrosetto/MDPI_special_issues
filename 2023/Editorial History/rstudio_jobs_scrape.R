
# libraries
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(lubridate)
library(magrittr)

for (jo in iter_journals[minjo:maxjo]) {
  cat(jo, '\n')
  onejournal(jo)
}