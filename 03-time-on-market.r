library(tidyverse)

# median time on market by year and hometype
timeonmarket <- homesales %>%
                        group_by(listingyear, hometype, status) %>%
                        summarise(mediantimeonmarket = median(timeonmarket, na.rm = TRUE))

timeonmarket %>%
        ggplot() +
        aes(x = listingyear, y = mediantimeonmarket, fill = hometype) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Year of listing",
             y = "Median time on market (in days)",
             title = "Median time on market for homes in Glen Lake",
             fill = "Home type",
             caption = source
             ) +
        geom_text(aes(label = round(mediantimeonmarket, 0)),
                  position = position_dodge(width = 0.9),
                  vjust = -1) +
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
        geom_jitter(alpha = .5, width = .2) +
        facet_wrap(. ~ hometype) +
        labs(x = "Year of listing",
             y = "Time on market (in days)",
             title = "Distribution of time on market for sold homes in Glen Lake",
             fill = "Home type",
             caption = source) +
        scale_y_continuous(limits = c(0, 350))

ggsave("graphs/violinplot-time-on-market.pdf", width = 11, height = 8)

