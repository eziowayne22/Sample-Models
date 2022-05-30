#author: John McGrath
#date: 5/30/2022
#project: Pokemon Classification:S Logistic Regression
#description: Train a model to classify pokemon as legendary or non-legendary based on base stats & catch rate
#Ouput logistic regression - test performance on test dataset


library(tidyverse)
library(tidymodels)

#Import the data
pokemon_data <- read_csv("pokemon.csv")

#pre-process
View(pokemon_data)
#train-test split
pokemon_data_refined <- pokemon_data %>% 
                        select(name,height_m,weight_kg,base_total,capture_rate,is_legendary) %>% 
                        mutate(legendary_status=as_factor(is_legendary)) %>% 
                        mutate(capture_rate_num=as.numeric(capture_rate),na.rm= TRUE) %>% 
                        filter(name!="Minior") %>% 
                        select(name,base_total,capture_rate_num,is_legendary,legendary_status) 
  
View(pokemon_data_refined)
View(scale_standardized_pokemon_df)

#scale the predictors
#Normalize the predictors as part of pre-processing to get them on the same scale
scale_standardized_pokemon_df <- as.data.frame(scale(pokemon_data_refined[2:3])) 
scale_standardized_pokemon <- as_tibble(scale_standardized_pokemon_df) %>% 
  rename(base_total_std=base_total,capture_rate_std=capture_rate_num) 
#remove unnecessary columns
pokemon_final <- bind_cols(pokemon_data_refined,scale_standardized_pokemon)
View(pokemon_final)

pokemon_split <- initial_split(pokemon_final,prop=0.75)
pokemon_train <- training(pokemon_split)
pokemon_test <- testing(pokemon_split)

#Create the logistic regression model
log_model <- 
  logistic_reg() %>% 
  set_engine("glm")

log_form_fit <- 
  log_model %>% 
  # fit the model
  #see if we can predict with base_stats and capture_rate
  fit(legendary_status ~ base_total_std + capture_rate_std, data = pokemon_train)

#predict
log_form_fit %>% extract_fit_engine()

model_res <- 
  log_form_fit %>% 
  extract_fit_engine() %>% 
  summary()

# The model coefficient table is accessible via the `coef` method.
param_est <- coef(model_res)
class(param_est)
#> [1] "matrix" "array"
param_est
tidy(log_form_fit)

#evaluate

#Testing off of withheld data
test_pokemon <- predict(log_form_fit, new_data = pokemon_test)
View(test_pokemon)
Output_pokemon <- bind_cols(test_pokemon,pokemon_test ) %>% 
  #remove unnecessary columns
  select(name,.pred_class) %>% 
  #rename prediction column
  rename(predicted_legendary_status=.pred_class)

View(Output_pokemon)

withheld_pokemon <- Output_pokemon %>% 
  inner_join(pokemon_final, by=c("name"="name")) %>% 
  select(name,predicted_legendary_status,legendary_status)
View(withheld_pokemon)

#evaluate test set

withheld_pokemon_v2 <- withheld_pokemon %>% 
  mutate(test = ifelse(predicted_legendary_status==legendary_status, TRUE, FALSE))

View(withheld_pokemon_v2)

row_number <- nrow(withheld_pokemon_v2)
accuracy <- 
  withheld_pokemon_v2 %>%
  count(test) %>%
  mutate(percentage=n/row_number)
  
View(accuracy)


