library(tidymodels)
library(broom)
library(lubridate)
library(tidyverse)

theme_set(theme_light())

homesales <- read_csv("data/homesales_processeddata.csv") %>%
                mutate(streetname = substr(address, 5, 100)) %>%
                filter(streetname != "Bridgeport")


minimum_homes_sold <- 12

streetcount <- homesales %>%
            group_by(streetname) %>%
            summarize(count = n()) %>%
            filter(count >= minimum_homes_sold)

homesale_filter <- inner_join(homesales, streetcount, by = "streetname")


lin_mod <- lm(amount ~ hometype + saledate + streetname, data = homesale_filter)

lin_rec <- recipe(amount ~ saledate + hometype + streetname, data = homesale_filter) %>%
            step_date(saledate)

