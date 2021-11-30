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

# homes on market
homesales %>%
        filter(is.na(saledate), is.na(undercontract)) %>%
        select(address, listingdate, timeonmarket, hometype, undercontract) %>%
        arrange(-timeonmarket) %>%
        write_csv("data/homes_on_market.csv")


write_csv(medianprice, "data/median-price.csv")

write_csv(yearsalesoverview, "data/sales-by-year.csv")

write_csv(yearlistingoverview, "data/listings-by-year.csv")

write_csv(timeonmarket, "data/median-time-on-market.csv")