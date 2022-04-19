library(tidyverse)
library(lubridate)
theme_set(theme_light())

load("Rdata/homesales.Rdata")

homesales_filtered <-
    homesales %>%
    filter(!is.na(saledate))

datelist <-
    with(
        homesales_filtered,
        sort(unique(c(listingdate, saledate, today())))
    )

homes_sold_last_12month <- function(tbl, date) {
    date_year_ago <- date - lubridate::years(1)
    sale_data <-
        tbl %>%
        filter(saledate > date_year_ago, saledate <= date)

    tibble::tibble(date = date, homesold = nrow(sale_data))
}

sold_last_year <-
    map_dfr(datelist, ~ homes_sold_last_12month(homesales_filtered, .x))

monthlist <- seq(
    floor_date(first(datelist), unit = "month"),
    floor_date(last(datelist), unit = "month"),
    "month"
)

sold_last_year_by_month <-
    map_dfr(monthlist, ~ homes_sold_last_12month(homesales_filtered, .x))


sold_last_year %>%
    filter(date >= first(datelist) + years(1)) %>%
    ggplot() +
    aes(date, homesold) +
    geom_line() +
    scale_x_date() +
    scale_y_continuous(limits = c(0, NA), breaks = 10 * 0:100)


current_market_size <-
    homesales %>%
    select(listingdate, saledate) %>%
    pivot_longer(everything(), names_to = "type", values_to = "date") %>%
    arrange(date) %>%
    filter(!is.na(date)) %>%
    mutate(y = ifelse(type == "listingdate", 1, -1)) %>%
    mutate(homesonmarket = cumsum(y)) %>%
    select(-y)


inner_join(current_market_size, sold_last_year) %>%
    filter(date >= first(datelist) + years(1)) %>%
    mutate(market_speed = homesonmarket / homesold * 12) %>%
    ggplot() +
    aes(date, market_speed) +
    geom_line(color = "gray50", alpha = .5) +
    scale_x_date() +
    scale_y_continuous(limits = c(0, NA), breaks = 1 * 0:100) +
    geom_hline(yintercept = 6, lty = 2, color = "gray50") +
    geom_smooth(method = "loess", se = FALSE, lty = "dashed", color = "gray50") +
    labs(
        x = "Date",
        y = "Inventory rate (in months)"
    )