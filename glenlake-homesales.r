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
source("config.r")

#
# read datafile
#
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

cumulativelisting <- listingdate %>% mutate(year = year(listingdate)) %>% group_by(year) %>% summarise(listingcount=cumsum(y))
homesales <- bind_cols(homesales,cumulativelisting) %>% select(-year)
#
# create additional information
#
homesales$listingyear = year(homesales$listingdate)
homesales$listingmonth = month(homesales$listingdate)
homesales$saleyear = year(homesales$saledate)
homesales$salemonth = month(homesales$saledate)
homesales$dayofyear = yday(homesales$listingdate)
homesales$timeonmarket = homesales$saledate - homesales$listingdate
homesales$hometype= factor(homesales$hometype, levels=c("residential","patio home", "townhome"))

homesales$status = "Sold"
homesales$status[is.na(homesales$saledate)] = "For Sale"
homesales$status = factor(homesales$status, levels=c("For Sale", "Sold"))

write_csv(homesales, "data/homesales_processeddata.csv")

# update source tag
lastupdate = max(max(homesales$listingdate, na.rm=TRUE), max(homesales$saledate, na.rm=TRUE))
source <- paste(source,"\nLast updated: ",format(lastupdate, format="%b %d, %Y"))
maxyear = year(lastupdate)
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

# median time on market by year and hometype
timeonmarket <- homesales %>% group_by(listingyear,hometype) %>% summarise(mediantimeonmarket = median(timeonmarket, na.rm=TRUE)) 
timeonmarket %>% ggplot() + aes(x=listingyear,y=mediantimeonmarket, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Median time on market (in days)") + ggtitle("Median time on market for sold homes in Glen Lake") + labs(fill = "Home type", caption=source) +
                    geom_text(aes(label=round(mediantimeonmarket,0)), position=position_dodge(width=0.9), vjust=-1) + facet_wrap(.~hometype)
write_csv(timeonmarket,"data/median-time-on-market.csv")
ggsave("graphs/median-time-on-market.pdf")

# boxplot of time on market by year and hometype
homesales %>% ggplot() + aes(x=factor(listingyear),y=timeonmarket, fill=hometype) + geom_boxplot() + facet_wrap(.~hometype) + #geom_jitter() +
                    xlab("Year of listing") + ylab("Time on market (in days)") + ggtitle("Distribution of time on market for sold homes in Glen Lake") + labs(fill = "Home type", caption=source) +
                    scale_y_continuous(limits=c(0,350))
ggsave("graphs/boxplot-time-on-market.pdf")                


# turn-over rate per year by hometype
soldhomes <- homesales %>% filter(status=="Sold") %>% group_by(listingyear, hometype) %>% summarise(soldhomes=n()) 
soldhomes$percent = 0
soldhomes$percent[soldhomes$hometype=="residential"] = soldhomes$soldhomes[soldhomes$hometype=="residential"] / n_residential * 100
soldhomes$percent[soldhomes$hometype=="townhome"] = soldhomes$soldhomes[soldhomes$hometype=="townhome"] / n_townhomes * 100
soldhomes$percent[soldhomes$hometype=="patio home"] = soldhomes$soldhomes[soldhomes$hometype=="patio home"] / n_patiohomes * 100

soldhomes %>% ggplot() + aes(x=listingyear, y=percent, fill=hometype) + geom_bar(stat="identity", position="dodge") +
                    xlab("Year of listing") + ylab("Turn-over rate") + ggtitle("Turn-over rate in Glen Lake") + labs(fill = "Home type", caption=source) +
                    geom_text(aes(label=paste(round(percent,0),"%")), position=position_dodge(width=0.9), vjust=-1) + annotate("text",x=maxyear,y=1,label=paste(maxyear,"YTD", sep=""))

write_csv(soldhomes,"data/turnover-by-hometype.csv")
ggsave("graphs/turnover-by-hometype.pdf")

# listing counter
homesales %>% ggplot() + aes(x=dayofyear, y=listingcount, color=factor(listingyear)) + geom_line() + geom_point() +
                xlab("Day of year") + ylab("Cumulative number of listings per year") + labs(color="Year", caption=source) +
                ggtitle("Glen Lake cumulative numbers of listings by year")
ggsave("graphs/listings-by-dayofyear.pdf")

# sale counter
salecounter <- saledate %>% arrange(listingdate, na.rm=TRUE) %>% mutate(year=year(listingdate)) %>% 
                        group_by(year) %>% mutate(salecount=cumsum(-y), dayofyear=yday(listingdate)) %>% 
                        filter(year > 0)

salecounter %>% ggplot() + aes(x=dayofyear, y=salecount, color=factor(year)) + geom_line() + geom_point() + 
                xlab("Day of year") + ylab("Cumulative number of home sales per year") + labs(color="Year", caption=source) +
                ggtitle("Glen Lake cumulative numbers of home sales by year") 
ggsave("graphs/sales-by-dayofyear.pdf")


# sales price by hometype

homesales %>% filter(status=="Sold") %>% ggplot()+aes(x=factor(saleyear), y=amount, fill=hometype) + geom_boxplot() + facet_wrap(.~hometype) +
                                                xlab("Year of sale") + ylab("Sales price") + 
                                                labs(caption=source) + ggtitle("Glen Lake sales price distribution by year and hometype")
ggsave("graphs/sales-price-distribution.pdf")
medianprice <- homesales %>% filter(status=="Sold") %>% group_by(saleyear, hometype) %>% summarise(medianprice=median(amount))
write_csv(medianprice, "data/median-price.csv")