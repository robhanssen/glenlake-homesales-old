library(tidyverse)


# listed per year
yearlistingoverview <-
        homesales %>%
        group_by(listingyear, hometype, status) %>%
        summarise(listedtotal = n(),
                  .groups = "drop")

yearlistingoverview %>%
        ggplot() +
        aes(x = factor(listingyear), y = listedtotal, fill = status) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(. ~ hometype) +
        labs(x = "Year of listing",
                y = "Homes listed",
                caption = source,
                title = "Number of homes listed in Glen Lake",
                fill = "Sales status") +
        geom_text(aes(label = listedtotal),
                  position = position_dodge(width = 0.9),
                  vjust = -1) 

ggsave("graphs/listing-overview-by-year.pdf", width = 11, height = 8)

# sold per year
yearsalesoverview <-
        homesales %>%
        group_by(saleyear, hometype, status) %>%
        summarise(soldtotal = n(),
                  .groups = "drop")

yearsalesoverview %>%
        filter(!is.na(saleyear)) %>%
        ggplot() +
        aes(x = factor(saleyear), y = soldtotal, fill = hometype) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(. ~ hometype) +
        labs(x = "Year of sale",
             y = "Homes sold",
             caption = source,
             title = "Number of homes sold in Glen Lake",
             fill = "Home type") +
        geom_text(aes(label = soldtotal),
                  position = position_dodge(width = 0.9),
                  vjust = -1)

ggsave("graphs/sales-overview-by-year.pdf")


# turn-over rate per year by hometype
soldhomes <-
        homesales %>%
        group_by(listingyear, hometype) %>%
        summarise(soldhomes = n(),
                        .groups = "drop") %>%
        mutate(percent = case_when(hometype == "residential" ~ soldhomes / n_residential,
                                   hometype == "townhome" ~ soldhomes / n_townhomes,
                                   hometype == "patio home" ~ soldhomes / n_patiohomes,
                                   TRUE ~ 0
                                   )
                )

soldhomes %>%
        ggplot() +
        aes(x = listingyear, y = percent, fill = hometype) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Year of listing",
             y = "Turn-over rate (in %)",
             title = "Turn-over rate in Glen Lake",
             fill = "Hometype",
             caption = source) +
        scale_y_continuous(labels = scales::percent_format()) +
        geom_text(aes(label = scales::percent(percent, accuracy = .1)), position = position_dodge(width = 0.9), vjust = -1) + 
        annotate("text",x = max_year,y = .02,label = paste(max_year, "YTD", sep = ""))

ggsave("graphs/turnover-by-hometype.pdf")

# listing counter

max_listing <-
        homesales %>%
        group_by(listingyear) %>%
        summarize(n = n()) %>%
        slice_max(n, n = 1) %>%
        mutate(scaled_n = 10 * (n %/% 10 + 1)) %>%
        pull(scaled_n)


homesales %>%
        mutate(date = listingdate + years(max_year - as.numeric(year(listingdate))),
               y = 1) %>%
        group_by(listingyear) %>%
        mutate(listingcount = cumsum(y)) %>%
        ggplot() +
                aes(x = date, y = listingcount, color = factor(listingyear)) +
                geom_line() +
                geom_point() +
                scale_y_continuous(limit = c(0, max_listing), breaks = 10 * 0:10) +
                scale_x_date(date_break = "3 months", date_minor_breaks = "1 month", date_labels = "%b %d") +
                labs(x = "Date",
                     y = "Cumulative number of listings per year",
                     title = "Glen Lake cumulative numbers of listings by year",
                     color = "Year",
                     caption = source)

ggsave("graphs/listings-by-dayofyear.pdf")

# sale counter
max_sales <-
        homesales %>%
        group_by(saleyear) %>%
        summarize(n = n()) %>%
        slice_max(n, n = 1) %>%
        mutate(scaled_n = 10 * (n %/% 10 + 1)) %>%
        pull(scaled_n)


homesales %>%
        filter(!is.na(saledate)) %>%
        arrange(saledate) %>%
        mutate(date = saledate + years(max_year - as.numeric(year(saledate))),
               y = 1) %>%
        group_by(saleyear) %>%
        mutate(salecount = cumsum(y)) %>%
        ggplot() +
        aes(x = date, y = salecount, color = factor(saleyear)) +
        geom_line() +
        geom_point() +
        scale_y_continuous(limit = c(0, max_sales), breaks = 10 * 0:10) +
        scale_x_date(date_break = "3 months",
                        date_minor_breaks = "1 month",
                        date_labels = "%b %d") +
        labs(x = "Date",
                y = "Cumulative number of sales per year",
                title = "Glen Lake cumulative numbers of sales by year",
                color = "Year",
                caption = source)

ggsave("graphs/sales-by-dayofyear.pdf")
