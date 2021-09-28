library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

alpha <- .5
color <- "darkgreen"

homesales <-
    read_csv("data/homesales_processeddata.csv") #%>%
    #filter(hometype == "residential")

max_date <- max(max(homesales$listingdate), max(homesales$saledate, na.rm = TRUE))
max_date <- format(max_date, format = "%b %d, %Y")

homeslisted <-
    homesales %>% 
    group_by(listingyear) %>%
    summarize(count = n()) %>%
    ggplot +
        aes(x = listingyear, y = count) +
        geom_col(alpha = alpha, fill = color) +
        labs(x = "Year",
             y = "Homes listed",
             title = "Number of homes listed by year")

homessold <-
    homesales %>% 
    group_by(saleyear) %>%
    summarize(count = n()) %>%
    ggplot +
        aes(x = saleyear, y = count) +
        geom_col(alpha = alpha, fill = color) +
        labs(x = "Year",
             y = "Homes sold",
             title = "Number of homes sold by year")

timeonmarket <-
    homesales %>%
    filter(!is.na(listingyear)) %>%
    group_by(listingyear) %>%
    summarize(timeonmarket = median(timeonmarket))  %>%
    ggplot +
        aes(x = listingyear, y = timeonmarket) +
        geom_col(alpha = alpha, fill = color) +
        labs(x = "Year",
             y = "Median time on market (in days)",
             title = "Median time on market by year")

saleprice <-
    homesales %>% 
    group_by(saleyear) %>%
    summarize(saleprice = median(amount)) %>%
    ggplot +
        aes(x = saleyear, y = saleprice) +
        geom_col(alpha = alpha, fill = color) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1/1000, suffix = "K")) + 
        labs(x = "Year",
             y = "Median sale price (in $)",
             title = "Median sale price by year")


averageinventorysize <-
    homesales %>% 
    group_by(listingyear) %>%
    summarize(inventory = mean(inventory)) %>%
    ggplot +
        aes(x = listingyear, y = inventory) +
        geom_col(alpha = alpha, fill = color) +
        labs(x = "Year",
             y = "Average number of homes on the market",
             title = "Average inventory")

averageinventorytime <-
    homesales %>% 
    group_by(listingyear) %>%
    summarize(inventorytime = median(inventorytime, na.rm = TRUE)) %>%
    mutate(inventorytime = ifelse(listingyear == 2017, 0, inventorytime)) %>%
    ggplot +
        aes(x = listingyear, y = inventorytime) +
        geom_col(alpha = alpha, fill = color) +
        labs(x = "Year",
             y = "Average Inventory Time (in months)",
             title = "Average Inventory Time by year",
             caption = paste0("Last updated on ", max_date)) + 
        annotate("label", x = 2017, y = .5, label = "NO\nDATA")

overview <- (homeslisted + homessold + saleprice) / (timeonmarket + averageinventorysize + averageinventorytime)

ggsave("dashboard.png", width = 12, plot = overview)