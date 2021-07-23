library(tidyverse)
library(lubridate)
library(scales)
source("config.r")

homesales <- read_csv("data/homesales_processeddata.csv")
glenlakehomes <- read_csv("glenlakehomes.csv")
hometypes <- read_csv("hometypes.csv")

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

caption <- source

dupes %>%   filter(!is.na(lagtime)) %>% 
            arrange(lagtime) %>% mutate(addresslabel = paste0(address, " (",round(lagtime,1),")")) %>%
            ggplot  + aes(x = fct_reorder(addresslabel, -lagtime), y = lagtime, fill = hometype) + 
            geom_bar(stat="identity") + 
            labs(y="Time between sale and next listing (in months)", x="Address (relisting time in months)", fill="Type of home", caption=caption) + 
            scale_y_continuous(breaks=12*1:10) +
            coord_flip()

ggsave("graphs/home-resale-time.pdf", width = 8, height = 11)

minsaleyear = with(homesales, min(saleyear, na.rm = TRUE))
maxsaleyear = with(homesales, max(saleyear, na.rm = TRUE))

homesales %>% inner_join(hometypes) %>%
              mutate(streetname = substr(address, 5, 100),
                     streetname= ifelse(is.na(hometypeAB) | streetname=="Grays Harbor", 
                                          streetname,
                                          paste(streetname, hometypeAB))) %>%
                group_by(streetname) %>% 
                summarize(countbystreet = n()) %>% ungroup() %>% 
                right_join(glenlakehomes) %>% mutate(countbystreet = ifelse(is.na(countbystreet),0,countbystreet)) %>%
                mutate(turnover = countbystreet / numberofhomes) %>% 
                mutate(homecount = cut(numberofhomes, breaks=c(0,10,20,100), labels=c("10 homes or less per street","11-20 homes per street","over 20 homes per street"))) -> hometurnover

turnoversd = with(hometurnover, sd(turnover) )
turnoverlimits = averageturnover + c(-1,1) * turnoversd * qnorm(0.975) / sqrt(nrow(hometurnover))
# turnoverlimits = averageturnover + std.err * c(-1,1)

hometurnover %>% mutate(turnoverwarning = cut(turnover, c(0,turnoverlimits[1],turnoverlimits[2],turnoverlimits[2]*2, 1000), label=c("low","ave","higher","high"))) -> hometurnover

colorset = c( "low" = "green",
              "ave" = "gold", 
              "higher" = "orange",
              "high" = "red")


caption = paste0(source,
                     "\nDotted line: neigborhood average (", 
                     round(100*turnoverlimits[1],0),
                     "-",
                     round(100*turnoverlimits[2],0),
                     "%)",
                     "\n PH: patio home; TH: townhome")

hometurnover %>% #View()
              ggplot + aes(x=fct_reorder(streetname,turnover), y=turnover, fill=turnoverwarning) + 
                scale_y_continuous(labels=percent_format(), breaks=.25*1:10) + 
                geom_col() + facet_wrap(~homecount, scale="free_y") + 
                ggtitle("Home turn-over by street") + 
                labs(x="Street", y=paste0("Turn-over rate (",minsaleyear,"-",maxsaleyear,")"), caption=caption) +
                geom_hline(yintercept=turnoverlimits, lty=2, color="gray50") +                
                scale_fill_manual(values=colorset) +
                coord_flip() +
                theme_light() +
                theme(legend.position = "none")

ggsave("graphs/turnover-by-street.pdf", width=11, height=8)
