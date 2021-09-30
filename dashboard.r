library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

alpha <- .5

homesales <-
    read_csv("data/homesales_processeddata.csv") %>%
    mutate(listingyear = factor(listingyear),
            saleyear = factor(saleyear))

max_date <- max(max(homesales$listingdate), max(homesales$saledate, na.rm = TRUE))
max_date <- format(max_date, format = "%b %d, %Y")

num_years <- homesales %>% distinct(listingyear) %>% nrow(.)

colorscale <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0, 1, length.out = num_years))

homeslisted <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(count = n()) %>%
    ggplot +
        aes(x = listingyear, y = count, fill = listingyear) +
        geom_col(alpha = alpha) +
        labs(x = "Year",
             y = "Homes listed",
             title = "Number of homes listed by year") +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none")

homessold <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    summarize(count = n()) %>%
    ggplot +
        aes(x = saleyear, y = count, fill = saleyear) +
        geom_col(alpha = alpha) +
        labs(x = "Year",
             y = "Homes sold",
             title = "Number of homes sold by year") +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none")

timeonmarket <-
    homesales %>%
#    filter(!is.na(listingyear)) %>%
    group_by(listingyear) %>%
    summarize(timeonmarket = median(timeonmarket))  %>%
    ggplot +
        aes(x = listingyear, y = timeonmarket, fill = listingyear) +
        geom_col(alpha = alpha) +
        labs(x = "Year",
             y = "Median time on market (in days)",
             title = "Median time on market by year") +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none")

saleprice <-
    homesales %>%
    filter(!is.na(saleyear)) %>%
    group_by(saleyear) %>%
    summarize(saleprice = median(amount), na.rm = TRUE) %>%
    ggplot +
        aes(x = saleyear, y = saleprice, fill = saleyear) +
        geom_col(alpha = alpha) +
        scale_y_continuous(labels = scales::dollar_format(scale = 1 / 1000, suffix = "K")) +
        labs(x = "Year",
             y = "Median sale price (in $)",
             title = "Median sale price by year") +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none")

averageinventorysize <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(inventory = mean(inventory)) %>%
    ggplot +
        aes(x = listingyear, y = inventory, fill = listingyear) +
        geom_col(alpha = alpha) +
        labs(x = "Year",
             y = "Average number of homes on the market",
             title = "Average inventory size") +
        scale_fill_manual(values = colorscale) +
        theme(legend.position = "none")

averageinventorytime <-
    homesales %>%
    group_by(listingyear) %>%
    summarize(inventorytime = median(inventorytime, na.rm = TRUE)) %>%
    mutate(inventorytime = ifelse(listingyear == "2017", 0, inventorytime)) %>%
    ggplot +
        aes(x = listingyear, y = inventorytime, fill = listingyear) +
        geom_col(alpha = alpha) +
        labs(x = "Year",
             y = "Average Inventory Time (in months)",
             title = "Average Inventory Time by year",
             caption = paste0("Last updated on ", max_date)) +
        annotate("label", x = "2017", y = .5, label = "NO\nDATA") +
        scale_fill_manual(values = colorscale) +
        annotate("label",
                 x = "2017",
                 y = 6,
                 label = "buyer's\nmarket\n\nseller's\n market") +
        geom_hline(yintercept = 6, lty = 3) +
        theme(legend.position = "none")

overview <- (homeslisted + homessold + saleprice) / (timeonmarket + averageinventorysize + averageinventorytime)

ggsave("dashboard.png", width = 12, plot = overview)