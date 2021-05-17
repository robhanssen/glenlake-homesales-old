library(tidyverse)
library(lubridate)
library(scales)

homesales <- read_csv("data/homesales_processeddata.csv")
glenlakehomes <- read_csv("glenlakehomes.csv")

totalhomes = with(glenlakehomes, sum(numberofhomes))
totalsold =  nrow(homesales)
averageturnover = totalsold / totalhomes

dupes <- homesales %>% 
            filter(duplicated(address) | duplicated(address, fromLast = TRUE)) %>% 
            arrange(address) %>% 
            group_by(address) %>%
            mutate(lagtime = listingdate - lag(saledate), lagtime=time_length(lagtime, "months"), 
                   streetname = substr(address, 5, 100)) %>%
            ungroup() %>%
            select(address:hometype, lagtime, streetname)

dupes %>%   filter(!is.na(lagtime)) %>% 
            arrange(hometype,address) %>% 
            ggplot  + aes(x=fct_reorder(address,lagtime), y=lagtime, fill=hometype) + 
            geom_col() + 
            labs(y="Time betweens sale and next listing (in months)", x="Address", fill="Type of home") + 
            scale_y_continuous(breaks=12*1:10) +
            coord_flip()

ggsave("graphs/home-resale-time.pdf", width=8, height=11)

homesales %>% mutate(streetname = substr(address, 5, 100)) %>% 
                group_by(streetname) %>% 
                summarize(countbystreet = n()) %>% ungroup() %>%
                right_join(glenlakehomes) %>% mutate(countbystreet = ifelse(is.na(countbystreet),0,countbystreet)) %>%
                mutate(turnover = countbystreet / numberofhomes) %>% 
                mutate(homecount = cut(numberofhomes, breaks=c(0,10,20,100), labels=c("<10","11-20",">20"))) %>%
                ggplot + aes(x=fct_reorder(streetname,numberofhomes), y=turnover) + 
                scale_y_continuous(labels=percent_format(), breaks=.25*1:10) + 
                geom_hline(yintercept=averageturnover, lty=2) +
                geom_col() + facet_wrap(~homecount, scale="free_y") + 
                labs(x="Street (ordered by size)", y="Turn-over rate") +
                coord_flip() +
                theme_light()

ggsave("graphs/turnover-by-street.pdf", width=11, height=8)