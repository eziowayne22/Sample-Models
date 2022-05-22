#install.packages("modeltime", dependencies = TRUE)
install.packages("timetk")
install.packages('Rcpp')
update.packages('Rcpp')


library(tidyverse)
library(modeltime)
library(timetk)
library(Rcpp)
library(tidymodels)
library(lubridate)
#library(ggplot2)

gold_data <- read_csv("gold_price_data.csv")

#convert date with lubridate
gold_data_new <- mutate(gold_data,dte_day=mdy(Date)) %>% 
#remove unnecessary columns; go back to 1989
select(-c("Date")) %>% 
  rename("price"="Value") %>% 
  filter(year(dte_day)>1990)
View(gold_data_new)

#Visualize at the outset
ggplot(data=gold_data_new,mapping = aes(x = dte_day,y = price, color="gold"))+
  geom_point()

gold_data_new %>% plot_time_series(dte_day,price)

#Create the train and test splits

splits <-  time_series_split(
  gold_data_new,
  assess = "60 months",
  cumulative = TRUE
)

splits %>% 
  tk_time_series_cv_plan( ) %>% 
  plot_time_series_cv_plan(dte_day,price)


#Start the Forecast

#ARIMA
arima_model <- arima_reg() %>% 
  set_engine("auto_arima") %>% 
  fit(price~dte_day, data = training(splits))

arima_model


#TSLM
tslm_model <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit(price~as.numeric(dte_day), data = training(splits))

tslm_model

#Create mmodeltime table

model_tbl <- modeltime_table(
  arima_model,
  tslm_model
)

#calibrate to assess residuals
calib_tbl <- model_tbl %>% 
  modeltime_calibrate(testing(splits))
#accuracy
calib_tbl %>% modeltime_accuracy()

#Test set viz

calib_tbl %>% 
  modeltime_forecast(
    new_data = testing(splits),
    actual_data = gold_data_new
  ) %>% 
  plot_modeltime_forecast()

#Forecast Future

future_forecast_tbl <- calib_tbl %>% 
  modeltime_refit(gold_data_new) %>% 
  modeltime_forecast(
        h            = "36 months",
        actual_data = gold_data_new

  )

future_forecast_tbl %>% 
  plot_modeltime_forecast()


View(future_forecast_tbl)

