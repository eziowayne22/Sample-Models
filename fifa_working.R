#Import, explore data, build a model
install.packages("usemodels")
library(tidyverse)
library(tidymodels)
library(usemodels)
library(ggplot2)

fifa_21_players <- read_csv('players_21.csv')

#filter to just strikers

fifa_21_strikers <- fifa_21_players %>% 
                    filter(player_positions=='ST')

#predict sriker va;ue based on player attributes, not overall--- likely too predictive
#give relevant columns, filter out 0 eur players
fifa_21_strikers_refined <- fifa_21_strikers %>% 
                            select(short_name,age,height_cm,weight_kg, nationality,
                                   league_name, league_rank,skill_moves,value_eur,team_jersey_number,
                                   pace,shooting,passing,dribbling,physic,attacking_finishing) %>% 
                            filter(value_eur>0) %>% 
                            mutate(salary=log10(value_eur))


#split up the data into test and train
set.seed(123)
fifa_split <- initial_split(fifa_21_strikers_refined, strata = salary)
fifa_st_train <- training(fifa_split)
fifa_st_test <- testing(fifa_split)


#Create the linear regression model
#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #see if we can predict with shooting, passing and dribbling ratings
  fit(salary ~ attacking_finishing, data = fifa_st_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x =fifa_st_train %>% select(attacking_finishing),
    y =fifa_st_train %>% pull(salary)
  )
lm_xy_fit

lm_form_fit %>% extract_fit_engine()

model_res <- 
  lm_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
#> [1] "matrix" "array"
param_est
tidy(lm_form_fit)

#Testing off of withheld data
Test_FIFA_salary <- predict(lm_form_fit, new_data = fifa_st_test)
Output_Test_FIFA_salary <- bind_cols(fifa_st_test,Test_FIFA_salary ) %>% 
  #remove unnecessary columns
  select(short_name,.pred) %>% 
  #rename prediction column
  rename(predicted_salary=.pred) %>% 
#Let's take the antilog of salary to get a better proxy for salary to see how well the model performed
mutate(final_salary=10^(predicted_salary))
View(Output_Test_FIFA_salary)
View(RB_test)

Withheld_FIFA_salary <- Output_Test_FIFA_salary %>% 
  inner_join(fifa_21_strikers_refined, by=c("short_name"="short_name")) %>% 
  select(short_name,final_salary,value_eur)
View(Withheld_FIFA_salary)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
fifa_metrics <- metric_set(rmse, rsq, mae)
fifa_metrics(Withheld_FIFA_salary, truth = value_eur, estimate = final_salary)

