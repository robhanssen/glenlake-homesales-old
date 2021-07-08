library(tidyverse)
library(lubridate)
library(tidymodels)
library(timetk)
library(forecast)
library(sweep)


homesales <- read_csv("data/homesales_processeddata.csv")

homesales %>% ggplot() + aes(x=saledate, y=amount) + geom_point() + facet_wrap(~hometype) + geom_smooth(method="lm")

price <- homesales %>% 
                    filter(hometype=="residential") %>%
                    filter(!is.na(amount), !is.na(saledate)) %>% 
                    select(saledate,saleyear,salemonth, amount) %>% group_by(saleyear,salemonth) %>% summarize(saledate=floor_date(mean(saledate), unit="month"), amount = median(amount) ) %>%
                    ungroup() %>%
                    select(saledate,amount)

MODEL_YEARS = 2

future =  tibble(saledate=seq.Date(
                                from = min(price$saledate),
                                to = max(price$saledate)+years(MODEL_YEARS), 
                                by = "1 month")
                                   ) %>% 
                        mutate(year = year(saledate), month = month(saledate)
                        )


priceseries <- price %>% tk_ts(start = min(year(.$saledate)), frequency=12)

etsmod <- priceseries %>% ets()

etsdata <- etsmod %>% sw_augment()

fcast <- etsmod %>%
    forecast(h = 24)


modelcomment = ""

autoplot(priceseries) +
  autolayer(fcast, series="Forecast", PI = TRUE) +
  geom_point(data=etsdata, aes(x=index, y=.actual)) + 
  scale_y_continuous(labels=scales::dollar_format()) + 
  theme(legend.position = "none")  +
  expand_limits(y=0) + 
  labs( x="Date", 
        y="Account Balance", 
        color="Account Type", 
        title="Glenlake HOA total available funds", 
        subtitle=paste0("Model prediction for the next ", MODEL_YEARS," years"), 
        caption=paste0("Black line represents actual values\nRed line is predicted value\nModel: ", modelcomment)
        )

library(xgboost)
library(modeltime)


interactive = FALSE

price %>%
  plot_time_series(saledate, amount, .interactive = interactive)

model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(amount ~ saledate, data = price)

model_fit_arima_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(amount ~ saledate + as.numeric(saledate) + factor(month(saledate, label = TRUE), ordered = F),
        data = price)

model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(amount ~ saledate, data = price)

model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(amount ~ saledate, data = price)    


model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(amount ~ as.numeric(saledate) + factor(month(saledate, label = TRUE), ordered = FALSE),
        data = price)

model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth") 

recipe_spec <- recipe(amount ~ saledate, data = price) %>%
    step_date(saledate, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(saledate)) %>%
    step_normalize(date_num) %>%
    step_rm(saledate)
  
wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(price)

models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    wflw_fit_mars
)




# models_tbl <- modeltime_table(    model_fit_arima_no_boost)


calibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = price)

calibration_tbl %>%
    modeltime_forecast(
        new_data    = future %>% filter(saledate>as.Date(max(price$saledate))),
        actual_data = price
    ) %>%  #filter(.index==max(future$date)) %>% arrange(.value) %>% View()
    plot_modeltime_forecast(
#       .legend_max_width = 25, # For mobile screens
      .interactive      = interactive, 
      .conf_interval_show = FALSE
    ) + scale_y_continuous(labels=scales::dollar_format()) + scale_x_date(breaks="1 year", date_label="%Y") + 
      expand_limits(y=0) + 
  labs( x="Date", 
        y="Home price", 
        color="Model", 
        title="Glenlake HOA home prices", 
        subtitle=paste0("Model prediction for the next ", MODEL_YEARS," years"), 
        caption=paste0("Black line represents actual values\nRed line is predicted value\nModel: ", modelcomment)
        )    