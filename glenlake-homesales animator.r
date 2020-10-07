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
#
# read datafile
#
homesales <- read_csv(homesale_file, 
        col_types = cols(listingdate = col_date(format = "%m-%d-%Y"), 
                         saledate = col_date(format = "%m-%d-%Y")
                         )
                    ) %>% arrange(listingdate)

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
homesales %>% mutate(
                        listingyear = year(listingdate),
                        listingmonth = month(listingdate),
                        saleyear = year(saledate),
                        salemonth = month(saledate),
                        dayofyear = yday(listingdate),
                        timeonmarket = saledate - listingdate,
                        hometype = factor(hometype, levels=c("residential","patio home", "townhome")),
                        status = "Sold"
) -> homesales

# data clean-up for unsold homes
homesales$timeonmarket[is.na(homesales$saledate)] = today()-homesales$listingdate[is.na(homesales$saledate)]
homesales$status[is.na(homesales$saledate)] = "For Sale"
homesales$status[is.na(homesales$saledate) & homesales$undercontract==1] = "Under Contract"
homesales$status = factor(homesales$status, levels=c("Sold", "Under Contract", "For Sale"))

# update source tag
lastupdate = max(max(homesales$listingdate, na.rm=TRUE), max(homesales$saledate, na.rm=TRUE))
source <- paste(source,"\nLast updated: ",format(today(), format="%b %d, %Y"),"\nLatest data:", format(lastupdate, format="%b %d, %Y"), sep="" )
maxyear = year(lastupdate)
#
# processing and data output
#

trailingzeros <- function(number)
{
        numdig = floor(log10(number))
        out = 
}
print(trailingzeros(100))

j=0
# listed per year
for(i in seq(as.Date("2020-01-01"), today(), by="days"))
{
        filename = paste0("animate/homes", formatC(j, width=4,flag="0"),".png")     
        yearoverview <- homesales %>% 
                filter(listingdate < i) %>%
                group_by(listingyear,hometype, status) %>% summarise(listedtotal = n())
        yearoverview %>% ggplot() + aes(x=factor(listingyear), y=listedtotal, fill=status) + geom_bar(stat="identity", position="dodge") + facet_wrap(.~hometype) +
                    xlab("Year of listing") + ylab("Homes listed") + ggtitle("Number of homes listed in Glen Lake") + labs(fill = "Sales status", caption=source) +
                    geom_text(aes(label=listedtotal), position=position_dodge(width=0.9), vjust=-1) 
        ggsave(filename)
        j = j+1      
}

yearoverview <- homesales %>% group_by(listingyear,hometype, status) %>% summarise(listedtotal = n())
yearoverview %>% ggplot() + aes(x=factor(listingyear), y=listedtotal, fill=status) + geom_bar(stat="identity", position="dodge") + facet_wrap(.~hometype) +
                    xlab("Year of listing") + ylab("Homes listed") + ggtitle("Number of homes listed in Glen Lake") + labs(fill = "Sales status", caption=source) +
                    geom_text(aes(label=listedtotal), position=position_dodge(width=0.9), vjust=-1) 

ggsave("animate/listing.png")

