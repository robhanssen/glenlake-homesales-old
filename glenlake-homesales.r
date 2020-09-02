#
# R script to analyze homesales in Glen Lake
#
# (C) Rob Hanssen, 2020. Licensed under GNU GENERAL PUBLIC LICENSE v3
#


#
# load the required libraries
#
library(tidyverse)
library(lubridate)
#
# constants
#
source = "Sources: realtor.com, zillow.com, spartanburgdeeds.com"
n_townhomes = 32
n_patiohomes = 32
n_residential = 484 - (n_townhomes + n_patiohomes)
#
# import via files
#
homesale_file = "homesalesdata-source.csv"
homesales <- read_csv(homesale_file, 
        col_types = cols(listingdate = col_date(format = "%m-%d-%Y"), 
                         saledate = col_date(format = "%m-%d-%Y")
                         )
                    )

#
# calculation for inventory
#
listingdate <- tibble(address=homesales$address, listingdate=homesales$listingdate, type="listing",y=1)
saledate <- tibble(address=homesales$address, listingdate=homesales$saledate, type="sale", y=-1)
summation = bind_rows(listingdate, saledate) %>% arrange(listingdate) %>% mutate(inventory=cumsum(y)) %>% select(-y) %>% filter(type=="listing")
homesales <- homesales %>% inner_join(summation) %>% select(-type)

#
# create additional information
#
homesales$listingyear = year(homesales$listingdate)
homesales$listingmonth = month(homesales$listingdate)
homesales$saleyear = year(homesales$saledate)
homesales$salemonth = month(homesales$saledate)
homesales$dayofyear = yday(homesales$listingdate)
homesales$timeonmarket = homesales$saledate - homesales$listingdate

homesales$status = "Sold"
homesales$status[is.na(homesales$saledate)] = "For Sale"

write_csv(homesales, "data/homesales_processeddata.csv")

# update source tag
latestlistingdate = max(homesales$listingdate)
source <- paste(source,"\nLast updated: ",format(latestlistingdate, format="%b %d, %Y"))
#
# processing and data output
#

# for-sale inventory by year
homesales %>% ggplot() + aes(dayofyear, inventory, color=factor(listingyear)) + geom_line() + geom_point() +
                xlab("Day of year") + ylab("Current sale inventory") + ggtitle("Inventory of homes for sale in Glen Lake") + labs(color = "Year", caption=source)

ggsave("graphs/homeinventory.pdf")

# sold per year
yearoverview <- homesales %>% group_by(listingyear,hometype, status) %>% summarise(listedtotal = n())
write_csv(yearoverview, "data/overview-by-year.csv")

# median time on market by hometype
timeonmarket <- homesales %>% group_by(listingyear,hometype) %>% summarise(mediantimeonmarket = median(timeonmarket, na.rm=TRUE)) 
timeonmarket %>% ggplot() + aes(x=listingyear,y=mediantimeonmarket, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Median time on market (in days)") + ggtitle("Median time on market for sold homes in Glen Lake") + labs(fill = "Home type", caption=source) +
                    geom_text(aes(label=round(mediantimeonmarket,0)), position=position_dodge(width=0.9), vjust=-1)

write_csv(timeonmarket,"data/median-time-on-market.csv")
ggsave("graphs/median-time-on-market.pdf")

homesales %>% ggplot() + aes(x=factor(listingyear),y=timeonmarket) + geom_boxplot() + facet_wrap(.~hometype) +
                    xlab("Year of listing") + ylab("Time on market (in days)") + ggtitle("Distribution of time on market for sold homes in Glen Lake") + labs(fill = "Home type", caption=source) +
                    scale_y_continuous(limits=c(0,350))
ggsave("graphs/boxplot-time-on-market.pdf")                


# percentage of homes sold per year by hometype
soldhomes <- homesales %>% filter(status=="Sold") %>% group_by(listingyear, hometype) %>% summarise(soldhomes=n()) 
soldhomes$percent = 0
soldhomes$percent[soldhomes$hometype=="residential"] = soldhomes$soldhomes[soldhomes$hometype=="residential"] / n_residential * 100
soldhomes$percent[soldhomes$hometype=="townhome"] = soldhomes$soldhomes[soldhomes$hometype=="townhome"] / n_townhomes * 100
soldhomes$percent[soldhomes$hometype=="patio home"] = soldhomes$soldhomes[soldhomes$hometype=="patio home"] / n_patiohomes * 100

soldhomes %>% ggplot() + aes(x=listingyear, y=percent, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Percentage of homes sold") + ggtitle("Percentage of homes sold in Glen Lake") + labs(fill = "Home type", caption=source) +
                    geom_text(aes(label=paste(round(percent,0),"%")), position=position_dodge(width=0.9), vjust=-1)



write_csv(soldhomes,"data/percentages-sold.csv")
ggsave("graphs/percentage-sold.pdf")