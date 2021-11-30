library(tidyverse)
library(lubridate)
library(zoo)
theme_set(theme_light())


source("config.r")
source("import-data.r")
source("listings-and-sales.r")
source("inventory.r")
source("sales-price.r")
source("time-on-market.r")
source("write-data.r")

source("find_resales.r")
source("predictions.r")
source("dashboard.r")