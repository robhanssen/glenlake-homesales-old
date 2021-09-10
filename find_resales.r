library(tidyverse)
library(lubridate)
library(scales)
source("config.r")

homesales <- read_csv("data/homesales_processeddata.csv")
glenlakehomes <- read_csv("glenlakehomes.csv")
hometypes <- read_csv("hometypes.csv")

totalhomes <- with(glenlakehomes, sum(numberofhomes))
totalsold <-  nrow(homesales)
averageturnover <- totalsold / totalhomes



dupes <- homesales %>%
            filter(duplicated(address) | duplicated(address, fromLast = TRUE)) %>% 
            arrange(address) %>%
            group_by(address) %>%
            mutate(lagtime = listingdate - lag(saledate),
                   lagtime = time_length(lagtime, "months"),
                   streetname = substr(address, 5, 100)
                   ) %>%
            ungroup() %>%
            select(address:hometype, lagtime, streetname)

caption <- source

dupes %>%
       filter(!is.na(lagtime)) %>% 
       arrange(lagtime) %>%
       mutate(addresslabel = paste0(address, " (", round(lagtime, 1), ")")) %>%
       ggplot +
              aes(x = fct_reorder(addresslabel, -lagtime), y = lagtime, fill = hometype) +
              geom_bar(stat = "identity") +
              labs(y = "Time between sale and next listing (in months)",
                   x = "Address (relisting time in months)",
                   fill = "Type of home", caption = caption) +
       scale_y_continuous(breaks = 12 * 1:10) +
       coord_flip()

ggsave("graphs/home-resale-time.pdf", width = 8, height = 11)

minsaleyear <- with(homesales, min(saleyear, na.rm = TRUE))
maxsaleyear <- with(homesales, max(saleyear, na.rm = TRUE))
data_time_length <- with(homesales,
                     time_length(
                                 difftime(max(listingdate, na.rm = TRUE),
                                 min(listingdate, na.rm = TRUE)),
                                 unit = "year"
                                 )
                     )

homesales %>% 
       inner_join(hometypes) %>%
       mutate(streetname = substr(address, 5, 100),
              streetname = ifelse(is.na(hometypeAB) | streetname == "Grays Harbor",
                                   streetname,
                                   paste(streetname, hometypeAB))
                                   ) %>%
       group_by(streetname) %>%
       summarize(countbystreet = n()) %>%
       ungroup() %>%
       right_join(glenlakehomes) %>%
       mutate(countbystreet = ifelse(is.na(countbystreet), 0, countbystreet)) %>%
       mutate(turnover = countbystreet / numberofhomes,
              residencetime = data_time_length / turnover 
               ) %>%
       mutate(homecount = cut(numberofhomes,
                              breaks = c(0, 10, 20, 100), 
                              labels = c("10 homes or less per street",
                                       "11-20 homes per street",
                                       "over 20 homes per street"
                                       )
                            )
              ) -> hometurnover

turnoversd <- with(hometurnover, sd(turnover))
turnoverlimits <- averageturnover + c(-1, 1) * turnoversd * qnorm(0.975) / sqrt(nrow(hometurnover))

hometurnover %>%
       mutate(turnoverwarning = cut(turnover, 
                                    c(0, turnoverlimits[1], turnoverlimits[2], turnoverlimits[2] * 2, 1000), 
                                    label = c("low", "ave", "higher", "high")
                                    )
              ) -> hometurnover

colorset <- c("low" = "green",
              "ave" = "gold", 
              "higher" = "orange",
              "high" = "red")


caption <- paste0(source,
                     "\nDotted line: neigborhood average (",
                     round(100 * turnoverlimits[1], 0),
                     "-",
                     round(100 * turnoverlimits[2], 0),
                     "%)",
                     "\n PH: patio home; TH: townhome")

hometurnover %>%
       ggplot +
              aes(x = fct_reorder(streetname, turnover), y = turnover, fill = turnoverwarning) +
              scale_y_continuous(labels = percent_format(), breaks = .25 * 1:10) + 
              geom_col() +
              facet_wrap(~ homecount, scale = "free_y") +
              ggtitle("Home turn-over by street") +
              labs(x="Street", y = paste0("Turn-over rate (",minsaleyear,"-",maxsaleyear,")"), caption = caption) +
              geom_hline(yintercept = turnoverlimits, lty = 2, color = "gray50") +                
              scale_fill_manual(values=colorset) +
              coord_flip() +
              theme_light() +
              theme(legend.position = "none")

ggsave("graphs/turnover-by-street.pdf", width=11, height=8)

hometurnover %>%
       ggplot +
              aes(x = fct_reorder(streetname, residencetime), y = residencetime, fill = turnoverwarning) +
              #scale_y_continuous(labels = comma_format(), breaks = 1:10) + 
              geom_col() +
              facet_wrap(~ homecount, scale = "free_y") +
              ggtitle("Residence time by street") +
              labs(x="Street", y = "Average residence time (in years)") +
              #geom_hline(yintercept = turnoverlimits, lty = 2, color = "gray50") +                
              scale_fill_manual(values=colorset) +
              coord_flip() +
              theme_light() +
              theme(legend.position = "none")

ggsave("graphs/residencetime-by-street.pdf", width=11, height=8)

# market value

homesales %>%
       mutate(year = saleyear) %>%
       group_by(year, hometype) %>%
       summarize(marketvalue = sum(amount, na.rm = TRUE), .groups = "drop") %>%
       ggplot + 
              aes(x = year, y = marketvalue / 1e3, fill = factor(hometype, levels = c("townhome", "patio home", "residential"))) + 
              geom_col() +
              scale_y_continuous(labels = dollar_format()) + 
              labs(title = "Glen Lake total market value",
                   x = "Year",
                   y = "Total market value (in thousands of USD)",
                   fill = "Home type",
                   caption = source)  + 
       annotate("text", x = maxyear, y = 1000, label = paste(maxyear, "YTD", sep = "\n"))

ggsave("graphs/market-value.pdf", width=11, height=8)


marketmodels <- function(tbl) {
       lm(marketvalue ~ dayofyear, data = tbl)
}

valuebyyear <- homesales %>% 
       filter(!is.na(saleyear)) %>%
       mutate(dayofyear = yday(saledate)) %>%
       arrange(saledate) %>%
       group_by(saleyear) %>%
       mutate(marketvalue = cumsum(amount)) %>% select(saledate, saleyear, dayofyear, marketvalue)

valuebyyear_model <- valuebyyear %>%
       nest() %>% 
       mutate(marketmodel = map(data, marketmodels))

augmentdata <- function(tbl) {
       tbl %>% broom::augment(
                            newdata = tibble(dayofyear = c(1,365)),
                            interval = "confidence"
                            )
}

modeldata <-
       valuebyyear_model %>%
       mutate(modeldata = map(marketmodel,
                           augmentdata)
                           ) %>%
       unnest(modeldata)

modelinfo <-
    valuebyyear_model %>%
    mutate(modelinfo = map(marketmodel, broom::glance)) %>%
    unnest(modelinfo)

modelparameters <-
    valuebyyear_model %>%
    mutate(modelparameters = map(marketmodel, broom::tidy)) %>%
    unnest(modelparameters)

valuebyyear %>%
       ggplot +
              aes(x = dayofyear, y = marketvalue, color = factor(saleyear)) + 
              geom_line() +
              scale_y_continuous(labels = dollar_format()) + 
              labs(title = "Glen Lake total market value",
                   x = "Day of year",
                   y = "Total market value (in thousands of USD)",
                   color = "Year",
                   caption = source) +
       scale_color_discrete()  +
       geom_line(data = modeldata, aes(y=.fitted), lty = 2) +
       theme_light()

ggsave("graphs/market-value-by-dayofyear.pdf", width = 11, height = 8)


this_year <- year(today())
maxyear <- max(homesales$saleyear, na.rm = TRUE)

lwr <- modeldata %>%
       filter(saleyear == this_year, dayofyear == max(dayofyear)) %>%
       pull(.lower) / 1e6

upr <- modeldata %>%
       filter(saleyear == this_year, dayofyear == max(dayofyear)) %>%
       pull(.upper) / 1e6

subtitle = paste0("Expected value for ", this_year, " is between $", round(lwr, 1), "M and $", round(upr, 1),"M.")

valuebyyear %>%
       filter(marketvalue == max(marketvalue)) %>%
       mutate(predicted = ifelse(saleyear != this_year, TRUE, FALSE)) %>%
       ggplot + 
              aes(x = saleyear, y = marketvalue, fill = predicted) +
              geom_col() + 
              geom_errorbar(aes(y = .fitted, ymin = .lower, ymax = .upper, fill = TRUE), width = .2, data=modeldata %>% filter(dayofyear == max(dayofyear))) +
              scale_y_continuous(labels = scales::dollar_format()) + 
              labs(title = "Glen Lake total market value expectation",
                   subtitle = subtitle,
                   x = "Year",
                   y = "Total market value (in USD)",
                   fill = "Predicted value",
                   caption = source) +
       annotate("text", x = this_year, y = 1e6, label = paste(this_year, "Prediction", sep = "\n")) + 
       theme(legend.position = "none")

ggsave("graphs/market-value-prediction.pdf", width=11, height=8)