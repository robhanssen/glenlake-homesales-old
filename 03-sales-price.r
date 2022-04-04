library(tidyverse)

# sales price by hometype
medianprice <- homesales %>%
        filter(status == "Sold") %>%
        group_by(saleyear, hometype) %>%
        summarise(
                medianprice = median(amount),
                .groups = "drop"
        )

max_median <-
        medianprice %>%
        slice_max(medianprice, n = 1) %>%
        mutate(scaled_max = (medianprice %/% 1e5 + 1) * 1e5) %>%
        pull(scaled_max)

medianprice %>%
        ggplot(aes(x = saleyear, y = medianprice, fill = hometype)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(. ~ hometype) +
        scale_y_continuous(
                labels = scales::dollar_format(
                        scale = 1e-3,
                        suffix = "K"
                ),
                breaks = 1e5 * 0:10,
                limits = c(0, max_median)
        ) +
        labs(
                title = "Yearly median sale price of homes",
                x = "Year of sale",
                y = "Median sale price (in $)",
                fill = "Home type",
                caption = source
        ) +
        geom_text(aes(label = scales::dollar(medianprice,
                scale = 1e-3,
                accuracy = 1,
                suffix = "K"
        )),
        position = position_dodge(width = 0.9),
        vjust = -1
        ) +
        annotate("text",
                x = max_year,
                y = 30000,
                label = paste(max_year, "YTD", sep = ""),
                angle = 90
        )

ggsave("graphs/median-saleprice.pdf", width = 11, height = 8)

homesales %>%
        filter(status == "Sold") %>%
        ggplot(aes(x = factor(saleyear), y = amount, fill = hometype)) +
        geom_violin(draw_quantiles = .5) +
        facet_wrap(. ~ hometype) +
        geom_jitter(width = .1, alpha = .2) +
        scale_y_continuous(
                labels = scales::dollar_format(
                        scale = 1e-3,
                        suffix = "K"
                ),
                breaks = 1e5 * 0:10
        ) +
        labs(
                title = "Glen Lake sales price distribution by year and hometype",
                x = "Year",
                y = "Sales price (in $)",
                caption = source
        ) +
        theme(legend.position = "none")

ggsave("graphs/sales-price-distribution.pdf")