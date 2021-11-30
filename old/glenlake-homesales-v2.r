# R script to analyze homesales in Glen Lake
#
# (C) Rob Hanssen, 2020. Licensed under GNU GENERAL PUBLIC LICENSE v3
#
# written with R 4.1.0 and tidyverse 1.3.0

# load the required libraries
library(tidyverse)
library(lubridate)
library(zoo)
source("config.r")
theme_set(theme_light())

#
# calculation for inventory
#



write_csv(yearoverview, "data/listing-overview-by-year.csv")


write_csv(yearoverview, "data/sales-overview-by-year.csv")


write_csv(soldhomes, "data/turnover-by-hometype.csv")







# dump final data file
write_csv(homesales, "data/homesales_processeddata.csv")

