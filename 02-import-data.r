message("02-import-data.r")
# import data
library(tidyverse)

homesales <-
        read_csv(homesale_file,
                comment = "#",
                col_types = cols(listingdate = col_date(format = "%m-%d-%Y"),
                                 saledate = col_date(format = "%m-%d-%Y")
                                )
        ) %>%
        mutate(listingyear = year(listingdate),
                listingmonth = month(listingdate),
                saleyear = year(saledate),
                salemonth = month(saledate),
                dayofyear = yday(listingdate),
                timeonmarket = saledate - listingdate,
                hometype = factor(hometype,
                                levels = c("residential", "patio home", "townhome")
                                )
        ) %>%
        mutate(timeonmarket = case_when(is.na(saledate) ~ today() - listingdate,
                                        TRUE ~ timeonmarket
                                        ),
                status = case_when(is.na(saledate) & undercontract == 1 ~ "Under Contract",
                                   is.na(saledate) ~ "For Sale",
                                   TRUE ~ "Sold"
                                   )
        ) %>%
        mutate(status = factor(status,
                               levels = c("Sold", "Under Contract", "For Sale")
                               )
                ) %>%
        arrange(listingdate)

save(file = "Rdata/homesales.Rdata", homesales)

# update source tag
lastupdate <-
        homesales %>%
        pivot_longer(ends_with("date"), values_to = "date") %>%
        summarize(date = max(date, na.rm = TRUE)) %>%
        pull(date)


caption <- paste(caption_source,
                "\nLast updated: ",
                format(today(),
                format = "%b %d, %Y"),
                "\nLatest data: ",
                format(lastupdate, format = "%b %d, %Y"),
                sep = "")

max_year <- year(lastupdate)
