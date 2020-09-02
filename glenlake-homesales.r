#
# R script to calculate home inventory in Glen Lake
#

#
# load the required libraries
#
library(tidyverse)
library(lubridate)


#
# constants
#
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
#
# processing and data output
#

# for-sale inventory by year
homesales %>% ggplot() + aes(dayofyear, inventory, color=factor(listingyear)) + geom_line() + geom_point() +
                xlab("Day of year") + ylab("Current sale inventory") + ggtitle("Inventory of homes for sale in Glen Lake") + labs(color = "Year")

ggsave("graphs/homeinventory.pdf")

# sold per year
yearoverview <- homesales %>% group_by(listingyear,hometype, status) %>% summarise(listedtotal = n())
write_csv(yearoverview, "data/overview-by-year.csv")

# median time on market by hometype
timeonmarket <- homesales %>% group_by(listingyear,hometype) %>% summarise(mediantimeonmarket = median(timeonmarket, na.rm=TRUE)) 
timeonmarket %>% ggplot() + aes(x=listingyear,y=mediantimeonmarket, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Median time on market (in days)") + ggtitle("Median time on market for sold homes in Glen Lake") + labs(fill = "Home type") +
                    geom_text(aes(label=floor(mediantimeonmarket)), position=position_dodge(width=0.9), vjust=-1)


write_csv(timeonmarket,"data/median-time-on-market.csv")
ggsave("graphs/median-time-on-market.pdf")

# percentage of homes sold per year by hometype
soldhomes <- homesales %>% filter(status=="Sold") %>% group_by(listingyear, hometype) %>% summarise(soldhomes=n()) 
soldhomes$percent = 0
soldhomes$percent[soldhomes$hometype=="residential"] = soldhomes$soldhomes[soldhomes$hometype=="residential"] / n_residential * 100
soldhomes$percent[soldhomes$hometype=="townhome"] = soldhomes$soldhomes[soldhomes$hometype=="townhome"] / n_townhomes * 100
soldhomes$percent[soldhomes$hometype=="patio home"] = soldhomes$soldhomes[soldhomes$hometype=="patio home"] / n_patiohomes * 100

soldhomes %>% ggplot() + aes(x=listingyear, y=percent, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Percentage of homes sold") + ggtitle("Percentage of homes sold in Glen Lake") + labs(fill = "Home type") +
                    geom_text(aes(label=paste(floor(percent),"%")), position=position_dodge(width=0.9), vjust=-1)



write_csv(soldhomes,"data/percentages-sold.csv")
ggsave("graphs/percentage-sold.pdf")