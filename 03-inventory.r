library(tidyverse)


inventorycalc <-
        homesales %>%
        select(address, listingdate, saledate) %>%
        pivot_longer(ends_with("date"), names_to = "type", values_to = "date") %>%
        mutate(y = ifelse(type == "listingdate", 1, -1)) %>%
        filter(!is.na(date)) %>%
        arrange(date) %>%
        mutate(inventory = cumsum(y),
               year = year(date))

inventorycalc %>%
        ggplot +
        aes(x = date, y = inventory, color = factor(year)) + 
        geom_line() +
        labs(x = "Date",
                y = "Home inventory",
                caption = source,
                title = "Inventory of homes for sale in Glen Lake") + 
        scale_x_date(date_breaks = "1 year", date_label = "%Y") + 
        theme(legend.position = "none")

ggsave("graphs/home-inventory-fullrange.pdf", width = 11, height = 8)

inventorycalc %>%
        mutate(date = date + years(max_year - as.numeric(year(date)))) %>%
        ggplot +
        aes(x = date, y = inventory, color = factor(year)) +
        geom_line() +
        labs(x = "Date",
                y = "Home inventory",
                color = "Year",
                caption = source,
                title = "Inventory of homes for sale in Glen Lake") + 
        scale_x_date(date_breaks = "3 months", date_label = "%b")

ggsave("graphs/home-inventory.pdf", width = 11, height = 8)

# sales inventory time
#
# sales inventory is defined as #homes listed / #homes sold per month(avg last 12 months)
#
# Since the average sales per month as defined over 12 months, the calculations will only be valid 12 months after the first sale.
# To accomodate for this, the graphs will only start 12 months after the first sale.

homesales <-
        homesales %>%
        inner_join(inventorycalc %>% filter(type == "listingdate"), by = c("address", "listingdate" = "date")) %>%
        mutate(yearlysales = 0)

for (i in seq_len(nrow(homesales))) {
        ldt <- as.Date(homesales$listingdate[i])
        x <- homesales %>%
                filter(ldt - saledate >= 0 & ldt - saledate < 365) %>%
                summarise(count = n())
        homesales$yearlysales[i] <- x$count[1]
}

homesales$inventorytime <- homesales$inventory / homesales$yearlysales * 12

homesales$inventorytime[homesales$yearlysales == 0] <- NA

# define first sale + 1 year
yearafterfirstlist <- min(homesales$listingdate, na.rm = TRUE) + years(1)
lastdate <- max(homesales$listingdate, na.rm = TRUE) %m-% months(6)

homesales %>% 
        filter(listingdate > yearafterfirstlist) %>%
        group_by(listingyear, listingmonth) %>%
        summarise(avsales = mean(yearlysales, na.rm = TRUE)) %>%
        fill(avsales) %>%
        mutate(date = as.Date(paste(listingyear, "-", listingmonth, "-01", sep = ""), format = "%Y-%m-%d")) %>% 
        ggplot() +
                aes(x = date, y = avsales) +
                geom_bar(stat = "identity") +
                scale_y_continuous(limit = c(0, 60), breaks=seq(0, 60, 10)) +
                labs(x = "Date",
                     y = "Number of home sales in the last 12 months",
                     title = "Glen Lake average home sales in 12 months",
                     caption = source) + 
                geom_smooth(method = "loess", linetype = "longdash", color = "white")

ggsave("graphs/average-homesales-per-12-months.pdf")

homesales %>%
        filter(listingdate > yearafterfirstlist) %>% 
        rename(date = listingdate) %>%
        ggplot() +
                aes(x = date, y = inventorytime) +
                geom_hline(yintercept = 6, lty = 2, color = "red") +
                annotate("text", x = lastdate, y = 6.5, label = "Buyer's Market") +
                annotate("text", x = lastdate, y = 5.5, label = "Seller's Market") +
                geom_line() +
                scale_y_continuous(limit = c(0, 12), breaks = seq(0, 12, 2)) +
                xlab("Date") +
                ylab("Inventory time (months)") +
                labs(caption = source) +
                ggtitle("Glen Lake average inventory time") +
                geom_smooth(method = "loess", linetype = "longdash", color = "white") 

ggsave("graphs/average-inventory-time.pdf")

