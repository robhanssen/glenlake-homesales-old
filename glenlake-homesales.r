#
# R script to analyze homesales in Glen Lake
#
# (C) Rob Hanssen, 2020. Licensed under GNU GENERAL PUBLIC LICENSE v3
#
# written with R 4.0.2 and tidyverse 1.3.0

#
# load the required libraries
#
library(tidyverse)
library(lubridate)
source("config.r")
library(zoo)
theme_set(theme_light())
#
# read datafile
#
homesales <- read_csv(homesale_file, 
                      comment = "#",
                      col_types = cols(listingdate = col_date(format = "%m-%d-%Y"),
                                       saledate = col_date(format = "%m-%d-%Y")
                                       )
                      ) %>%
             arrange(listingdate)

#
# calculation for inventory
#
listingdate <- tibble(address = homesales$address, 
                      listingdate = homesales$listingdate,
                      type = "listing",
                      y = 1)

saledate <- tibble(address = homesales$address,
                   listingdate = homesales$saledate,
                   type = "sale",
                   y = -1)

summation <- bind_rows(listingdate, saledate) %>% 
                arrange(listingdate) %>%
                mutate(inventory = cumsum(y)) %>%
                select(-y) %>%
                filter(type == "listing")

homesales <- homesales %>% 
                inner_join(summation) %>%
                select(-type)

cumulativelisting <- listingdate %>% 
                mutate(year = year(listingdate)) %>%
                group_by(year) %>%
                summarise(listingcount = cumsum(y))

homesales <- bind_cols(homesales, cumulativelisting) %>%
                select(-year)
#
# create additional information
#
homesales %>% mutate(
                        listingyear = year(listingdate),
                        listingmonth = month(listingdate),
                        saleyear = year(saledate),
                        salemonth = month(saledate),
                        dayofyear = yday(listingdate),
                        timeonmarket = saledate - listingdate,
                        hometype = factor(hometype,
                                          levels = c("residential", "patio home", "townhome")
                                          ),
                        status = "Sold"
) -> homesales

# data clean-up for unsold homes
homesales$timeonmarket[is.na(homesales$saledate)] <- today() - homesales$listingdate[is.na(homesales$saledate)]
homesales$status[is.na(homesales$saledate)] <- "For Sale"
homesales$status[is.na(homesales$saledate) & homesales$undercontract == 1] <- "Under Contract"
homesales$status <- factor(homesales$status, levels = c("Sold", "Under Contract", "For Sale"))

# unconfirmed sales for researching
homesales %>%
        filter(undercontract == 1 & !is.na(saledate)) %>%
        select(address:hometype, undercontract) %>%
        write_csv("data/unconfirmed_sales.csv")

# unsold homes
homesales %>%
        filter(is.na(saledate)) %>%
        select(address, listingdate, timeonmarket, hometype, undercontract) %>%
        arrange(-timeonmarket) %>%
        write_csv("data/unsold_homes.csv")

# update source tag
lastupdate <- max(
                max(homesales$listingdate, na.rm = TRUE),
                max(homesales$saledate, na.rm = TRUE)
                )

source <- paste(source,
                "\nLast updated: ",
                format(today(),
                format = "%b %d, %Y"),
                "\nLatest data: ",
                format(lastupdate, format = "%b %d, %Y"),
                sep = "")

maxyear <- year(lastupdate)
#
# processing and data output
#

# for-sale inventory by year
maxinventory <- ceiling(max(summation$inventory) / 10) * 10

# homesales %>%   mutate(date=dayofyear-1+as.Date("2020-01-01", format="%Y-%m-%d")) %>%
#                 ggplot() + aes(date, inventory, color=factor(listingyear)) + geom_line() + geom_point() + scale_y_continuous(limits=c(0,maxinventory)) +
#                 scale_x_date(date_break="3 months",date_minor_breaks="1 month",date_labels = "%b %d") +
#                 xlab("Date") + ylab("Current sale inventory") + ggtitle("Inventory of homes for sale in Glen Lake") + labs(color = "Year", caption=source)


# alternative way

summation <- bind_rows(listingdate, saledate) %>%
                arrange(listingdate) %>%
                mutate(inventory = cumsum(y)) %>%
                select(-y) %>%
                filter(!is.na(listingdate))

summation %>% mutate(dayofyear = yday(listingdate),
                     year = year(listingdate),
                     date = dayofyear - 1 + as.Date("2020-01-01", format = "%Y-%m-%d")
                     ) %>%
                ggplot() +
                        aes(date, inventory, color = factor(year)) +
                        geom_line() +
                        geom_point() +
                        scale_y_continuous(limits = c(0, maxinventory)) +
                        scale_x_date(date_break = "3 months", date_minor_breaks = "1 month", date_labels = "%b %d") +
                        xlab("Date") +
                        ylab("Current sale inventory") +
                        ggtitle("Inventory of homes for sale in Glen Lake") +
                        labs(color = "Year", caption = source)

ggsave("graphs/home-inventory.pdf", width = 11, height = 8)


summation %>% mutate(dayofyear = yday(listingdate),
                     year = year(listingdate), 
                     date = dayofyear - 1 + as.Date("2020-01-01", format = "%Y-%m-%d")) %>% 
                     ungroup() %>%
                ggplot() +
                        aes(listingdate, inventory, color = factor(year)) +
                        geom_line() +
                        geom_point() +
                        scale_y_continuous(limits = c(0, maxinventory)) +
                        scale_x_date(date_break = "1 year", date_minor_breaks = "3 month", date_labels = "%b %Y") +
                        xlab("Date") +
                        ylab("Current sale inventory") +
                        ggtitle("Inventory of homes for sale in Glen Lake") +
                        labs(color = "Year", caption = source) +
                        theme_light()

ggsave("graphs/home-inventory-fullrange.pdf", width = 11, height = 8)

# listed per year
yearoverview <- homesales %>%
                group_by(listingyear, hometype, status) %>%
                summarise(listedtotal = n())

yearoverview %>% ggplot() +
                        aes(x = factor(listingyear), y = listedtotal, fill = status) +
                        geom_bar(stat = "identity", position = "dodge") +
                        facet_wrap(. ~ hometype) +
                        xlab("Year of listing") +
                        ylab("Homes listed") +
                        ggtitle("Number of homes listed in Glen Lake") +
                        labs(fill = "Sales status", caption = source) +
                        geom_text(aes(label = listedtotal), position = position_dodge(width = 0.9), vjust = -1) 

ggsave("graphs/listing-overview-by-year.pdf", width = 11, height = 8)
write_csv(yearoverview, "data/listing-overview-by-year.csv")

# sold per year
yearoverview <- homesales %>%
                        group_by(saleyear, hometype, status) %>%
                        summarise(soldtotal = n())

yearoverview %>% 
                filter(!is.na(saleyear)) %>%
                ggplot() + 
                        aes(x = factor(saleyear), y = soldtotal) +
                        geom_bar(stat = "identity", position = "dodge") +
                        facet_wrap(. ~ hometype) +
                        xlab("Year of sale") +
                        ylab("Homes sold") +
                        ggtitle("Number of homes sold in Glen Lake") +
                        labs(fill = "Sales status", caption = source) +
                        geom_text(aes(label = soldtotal), position = position_dodge(width = 0.9), vjust = -1)

ggsave("graphs/sales-overview-by-year.pdf")
write_csv(yearoverview, "data/sales-overview-by-year.csv")

# median time on market by year and hometype
timeonmarket <- homesales %>%
                        group_by(listingyear, hometype, status) %>%
                        summarise(mediantimeonmarket = median(timeonmarket, na.rm = TRUE))

timeonmarket %>% ggplot() +
                        aes(x = listingyear, y = mediantimeonmarket, fill = hometype) +
                        geom_bar(stat = "identity", position = "dodge") +
                        xlab("Year of listing") +
                        ylab("Median time on market (in days)") +
                        ggtitle("Median time on market for homes in Glen Lake") +
                        labs(fill = "Home type", caption = source) +
                        geom_text(aes(label = round(mediantimeonmarket, 0)), position = position_dodge(width = 0.9), vjust = -1) +
                        facet_wrap(hometype ~ status, ncol = 3)

write_csv(timeonmarket, "data/median-time-on-market.csv")
ggsave("graphs/median-time-on-market.pdf")

# # boxplot of time on market by year and hometype
# homesales %>% ggplot() + aes(x=factor(listingyear),y=timeonmarket, fill=hometype) + geom_boxplot() + facet_wrap(.~hometype) + #geom_jitter() +
#                     xlab("Year of listing") + ylab("Time on market (in days)") + ggtitle("Distribution of time on market for sold homes in Glen Lake") + labs(fill = "Home type", caption=source) +
#                     scale_y_continuous(limits=c(0,350))
# ggsave("graphs/boxplot-time-on-market.pdf")

homesales %>%
        filter(status == "Sold") %>%
        ggplot() +
                aes(x = factor(listingyear), y = timeonmarket, fill = hometype) +
                geom_violin(draw_quantiles = 0.5) +
                facet_wrap(. ~ hometype) + #geom_jitter() +
                xlab("Year of listing") +
                ylab("Time on market (in days)") +
                ggtitle("Distribution of time on market for sold homes in Glen Lake") +
                labs(fill = "Home type", caption=source) +
                scale_y_continuous(limits = c(0,350)) +
                geom_jitter(alpha = .5, width = .2)

ggsave("graphs/violinplot-time-on-market.pdf", width = 11, height = 8)


# turn-over rate per year by hometype
soldhomes <- homesales %>%
                group_by(listingyear, hometype) %>%
                summarise(soldhomes = n())

soldhomes$percent <- 0
soldhomes$percent[soldhomes$hometype == "residential"] <- soldhomes$soldhomes[soldhomes$hometype == "residential"] / n_residential * 100
soldhomes$percent[soldhomes$hometype == "townhome"] <- soldhomes$soldhomes[soldhomes$hometype == "townhome"] / n_townhomes * 100
soldhomes$percent[soldhomes$hometype == "patio home"] <- soldhomes$soldhomes[soldhomes$hometype == "patio home"] / n_patiohomes * 100

soldhomes %>%
        ggplot() +
                aes(x = listingyear, y = percent, fill = hometype) +
                geom_bar(stat = "identity", position = "dodge") +
                xlab("Year of listing") +
                ylab("Turn-over rate") +
                ggtitle("Turn-over rate in Glen Lake") +
                labs(fill = "Home type", caption = source) +
                geom_text(aes(label = paste(round(percent, 0), "%")), position = position_dodge(width = 0.9), vjust = -1) + 
                annotate("text",x = maxyear,y = 1,label = paste(maxyear, "YTD", sep = ""))

write_csv(soldhomes, "data/turnover-by-hometype.csv")
ggsave("graphs/turnover-by-hometype.pdf")

# listing counter

maxlisting <- ceiling(max(homesales$listingcount) / 10) * 10

homesales %>%
        mutate(date = dayofyear - 1 + as.Date("2020-01-01", format = "%Y-%m-%d")) %>%
        ggplot() +
                aes(x = date, y = listingcount, color = factor(listingyear)) +
                geom_line() +
                geom_point() +
                scale_y_continuous(limit = c(0, maxlisting)) +
                scale_x_date(date_break = "3 months", date_minor_breaks = "1 month", date_labels = "%b %d") +
                xlab("Date") +
                ylab("Cumulative number of listings per year") +
                labs(color = "Year", caption = source) +
                ggtitle("Glen Lake cumulative numbers of listings by year")

ggsave("graphs/listings-by-dayofyear.pdf")

# sale counter
salecounter <- saledate %>%
                arrange(listingdate, na.rm = TRUE) %>%
                mutate(year = year(listingdate)) %>%
                group_by(year) %>%
                mutate(salecount = cumsum(-y),
                       dayofyear = yday(listingdate)
                       ) %>%
                filter(year > 0) %>%
                mutate(date = dayofyear - 1 + as.Date("2020-01-01", format = "%Y-%m-%d"))

maxsales <- ceiling(max(salecounter$salecount) / 10) * 10

salecounter %>% 
        ggplot() + 
                aes(x = date, y = salecount, color = factor(year)) +
                geom_line() +
                geom_point() +
                scale_x_date(date_break = "3 months", date_minor_breaks = "1 month", date_labels = "%b %d") +
                xlab("Date") +
                ylab("Cumulative number of home sales per year") +
                labs(color = "Year", caption = source) +
                ggtitle("Glen Lake cumulative numbers of home sales by year")

ggsave("graphs/sales-by-dayofyear.pdf")


# sales price by hometype

medianprice <- homesales %>% 
                filter(status == "Sold") %>%
                group_by(saleyear, hometype) %>%
                summarise(medianprice = median(amount))

write_csv(medianprice, "data/median-price.csv")

medianprice %>% 
        ggplot(aes(x = saleyear, y = medianprice, fill = hometype)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(. ~ hometype) +
        xlab("Year of sale") +
        ylab("Median sale price") +
        ggtitle("Yearly median sale price of homes") +
        labs(fill = "Home type", caption=source) +
        geom_text(aes(label = paste("$", round(medianprice, 0))), position = position_dodge(width = 0.9), vjust = -1) +
        annotate("text", x = maxyear,y = 10000, label = paste(maxyear, "YTD", sep = ""))

ggsave("graphs/median-saleprice.pdf", width = 11, height = 8)

homesales %>% 
        filter(status == "Sold") %>%
        ggplot(aes(x = factor(saleyear), y = amount, fill = hometype)) +
        geom_violin(draw_quantiles = .5) +
        facet_wrap(. ~ hometype) +
        xlab("Year of sale") +
        ylab("Sales price") + 
        geom_jitter(width = .1, alpha = .2) +
        theme(legend.position = "none")  +
        labs(caption = source) +
        ggtitle("Glen Lake sales price distribution by year and hometype")

ggsave("graphs/sales-price-distribution.pdf")


# sales inventory time
#
# sales inventory is defined as #homes listed / #homes sold per month(avg last 12 months)
#
# Since the average sales per month as defined over 12 months, the calculations will only be valid 12 months after the first sale.
# To accomodate for this, the graphs will only start 12 months after the first sale.

homesales$yearlysales <- 0

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
yearafterfirstlist <- min(homesales$listingdate, na.rm=TRUE) + years(1)
lastdate <- max(homesales$listingdate, na.rm=TRUE) - months(6)

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
                        xlab("Date") + 
                        ylab("Number of home sales in the last 12 months") + 
                        labs(caption = source) +
                        ggtitle("Glen Lake average home sales in 12 months") + 
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


# dump final data file
write_csv(homesales, "data/homesales_processeddata.csv")

homesales %>%
        select(address, listingdate, saledate) %>% 
        pivot_longer(listingdate:saledate, names_to = "datetype", values_to = "date") %>%
        arrange(date) %>%
        mutate(year = year(date),
               dayofyear = yday(date), 
               pdate = dayofyear + as.Date("2020-01-01"),
               y = ifelse(datetype == "listingdate", 1, -1)
               ) %>% 
        group_by(year, datetype) %>%
        summarize(pdate = pdate, 
                  countup = cumsum(y),
                  countup = abs(countup)
                  ) %>%
        ungroup() %>%
                ggplot() +
                aes(pdate, countup, color = datetype) +
                geom_line() +
                facet_wrap(. ~ year)

ggsave("graphs/listings-and-sales-by-date.pdf", height = 8, width = 11)

source("find_resales.r")