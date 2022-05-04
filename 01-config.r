message("01-config.r")
#
# definition of constants, etc.
#

YEAR <- year(today())

homesale_file <- "sources/homesalesdata-source.csv"
caption_source <- paste0("\U00A9 ", YEAR, ", Rob Hanssen\nSources: realtor.com, zillow.com, spartanburgdeeds.com, other public sources")
n_townhomes <- 32
n_patiohomes <- 34
n_residential <- 482 - (n_townhomes + n_patiohomes)