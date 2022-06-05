#Get libraries
library(tidyverse)
library(ggplot2)
library(tidymodels)

#Read in the Rushing Data
Rushing_2021_PFF_data <- read_csv("2021_Season_RBs_PFF.csv")
#Filter to Runningbacks
RB_Rushing_2021_PFF_data<- filter(Rushing_2021_PFF_data, position == 'HB')

#Read in the Receiving Data
Receiving_2021_PFF_data <- read_csv("2021_Season_Receiving_PFF.csv")

#Read in the Passing Data
Passing_2021_PFF_data <- read_csv("2021_Season_Passing_PFF.csv")

#Read in the Historical RB Data
Historical_2017_2021_College_Rookie_RB_Data <- read_csv("2017_2021_RB_Data_Model.csv")
View(Historical_2017_2021_College_Rookie_RB_Data)

#read in 2022 rb
New_College_Rookie_RB_Data <- read_csv("RBs_2022_Reg_post_draft.csv")
View(New_College_Rookie_RB_Data)

#Generate random number for draft rounds of top RBS
new_predict_round_Hall <- New_College_Rookie_RB_Data %>% 
  filter(Player_Name =="Breece Hall") %>% 
  mutate(predicted_round <- floor(runif(1,1,3))) %>% 
View(new_predict_round_Hall)

new_predict_round_Walker <- New_College_Rookie_RB_Data %>% 
  filter(Player_Name =="Kenneth Walker III") %>% 
  mutate(predicted_round <- floor(runif(1,1,3)))
View(new_predict_round_Walker)


new_predict_round_good <- New_College_Rookie_RB_Data %>% 
  filter(Player_Name =="Isaiah Spiller"|Player_Name =="Rachaad White"|Player_Name =="Zamir White"|Player_Name =="Dameon Pierce") %>% 
  mutate(predicted_round <- floor(runif(1,3,5)))
View(new_predict_round_good)

#Read in the Historical WR Data
Historical_2017_2021_College_Rookie_WR_Data <- read_csv("2017_2021_WR_Data.csv")
View(Historical_2017_2021_College_Rookie_WR_Data)

Rookie_WR_2022_Data <- read_csv("2022_WR_Proj_Data.csv")
View(Rookie_WR_2022_Data)


#Filter to WRs
WR_Receiving_2021_PFF_data<- filter(Receiving_2021_PFF_data, position == 'WR')

#Filter to TEs
TE_Receiving_2021_PFF_data<- filter(Receiving_2021_PFF_data, position == 'TE')

#Limit dataset to NFL Draft Eligble RBs
NFL_Combine_Players <- read_csv("Combine_Participants_Offense_2.csv")
View(NFL_Combine_Players)

#Inner join RBs to get draft eligible backs

Fantasy_RB_Prospects <- RB_Rushing_2021_PFF_data %>% inner_join(NFL_Combine_Players,by = c("player" = "Player_Name"))
View(Fantasy_RB_Prospects)

#Inner join WRs to get draft eligible backs
View(WR_Receiving_2021_PFF_data)
Fantasy_WR_Prospects <- WR_Receiving_2021_PFF_data %>% inner_join(NFL_Combine_Players,by = c("player" = "Player_Name"))
View(Fantasy_WR_Prospects)

#Inner join TEs to get draft eligible backs
Fantasy_TE_Prospects <- TE_Receiving_2021_PFF_data %>% inner_join(NFL_Combine_Players,by = c("player" = "Player_Name"))
View(Fantasy_TE_Prospects)

#Inner join TEs to get draft eligible backs
Fantasy_QB_Prospects <- QB_Passing_2021_PFF_data %>% inner_join(NFL_Combine_Players,by = c("player" = "Player_Name"))

#let's clean up the historical data
#mutate to add a per 100 touches column
mutate(Historical_2017_2021_College_Rookie_RB_Data$Fantasy_Pts_Per_100_touches<-Historical_2017_2021_College_Rookie_RB_Data$Rookie_Year_Points_per_touch*as.numeric(100))

#Let's predict fantasy points using weight adjusted 40 for RBs with over 75 rookie touches, trained on data from 2017,2018,2019,2020
Training_data_RB <- filter(Historical_2017_2021_College_Rookie_RB_Data, Rookie_year_Touches>50)

#Using simple linear regression

#the basic syntax in Base R is lm( y ~ x, data)... where y is the response, x is the predictor,data is the dataset storing the vals
#attach(Training_data_RB)

#tidy version of 80-20 data split
#DO I want to stratify this by a column..
RB_split <- initial_split(Training_data_RB, prop = 0.80)

RB_split
#runnning the above is the printed info 2344/586/2930
#training data/test data/size of the original pool of samples

RB_train <- training(RB_split)
RB_test  <-  testing(RB_split)
View(RB_train)
#Create the linear regression model
#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #fit(Rookie_year_fantasy_points ~ PFF_Rec + Weight_Adjusted_40, data = RB_train)
  fit(Rookie_Year_Points_per_touch ~ PFF_Rec + Weight_Adjusted_40, data = RB_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = RB_train %>% select(PFF_Rec, Weight_Adjusted_40),
    y = RB_train %>% pull(Rookie_Year_Points_per_touch)
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
lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Predict
RB_2022_sample <- read_csv("RBs_2022_Reg.csv")

#Testing off of withheld data
RB_test_small <- RB_test %>% slice(1:5)
Test_Point_Prediction <- predict(lm_form_fit, new_data = RB_test_small)
Output_Test_RB_prediction <- bind_cols(RB_test_small,Test_Point_Prediction ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_Points_Per_Touch=.pred) %>% 
  arrange(desc(Predicted_Fantasy_Points_Per_Touch))
View(Output_Test_RB_prediction)

#------------------------------------------------------------------------------
PPR_Point_Prediction <- predict(lm_form_fit, new_data = RB_2022_sample)
View(PPR_Point_Prediction)
View(RB_2022_sample)

Output_RB_prediction <- bind_cols(RB_2022_sample,PPR_Point_Prediction) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
rename(Predicted_Fantasy_Points_2022_Per_Touch=.pred) %>% 
  arrange(desc(Predicted_Fantasy_Points_2022_Per_Touch))
View(Output_RB_prediction)

#Send the output to a csv file, in our current working directory
write.table(Output_RB_prediction, file="Exported_RB_Prediction_2022_Pre_Draft_3.csv",row.names=F, sep=",")

RB_test_small %>% 
  select(Rookie_year_fantasy_points) %>% 
  bind_cols(predict(lm_form_fit, RB_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, RB_test_small, type = "pred_int")) 

#Maybe I should predict points per touch as planned then project out touches based on landing spot?

#-------------------------------------------------------------------------------------------------------------------

#Let's use a similar method to predict touches
#train on regression, may have to account for team stats like rush/pass split etc


#-----------------------------------------------------------------------------------------------------------------------

#Predict touches/game here
#tidy version of 80-20 data split
#DO I want to stratify this by a column..
RB_split <- initial_split(Training_data_RB, prop = 0.80)

RB_split
#running the above is the printed info 2344/586/2930
#training data/test data/size of the original pool of samples

RB_train <- training(RB_split)
RB_test  <-  testing(RB_split)
View(RB_train)
#Create the linear regression model
#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #fit(Rookie_year_fantasy_points ~ PFF_Rec + Weight_Adjusted_40, data = RB_train)
  fit(Rookie_Year_Points_per_touch ~ PFF_Rec + Weight_Adjusted_40, data = RB_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = RB_train %>% select(PFF_Rec, Weight_Adjusted_40),
    y = RB_train %>% pull(Rookie_Year_Points_per_touch)
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
lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Predict
RB_2022_sample <- read_csv("RBs_2022_Reg.csv")

#Testing off of withheld data
RB_test_small <- RB_test %>% slice(1:5)
Test_Point_Prediction <- predict(lm_form_fit, new_data = RB_test_small)
Output_Test_RB_prediction <- bind_cols(RB_test_small,Test_Point_Prediction ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_Points_Per_Touch=.pred) %>% 
  arrange(desc(Predicted_Fantasy_Points_Per_Touch))
View(Output_Test_RB_prediction)

#------------------------------------------------------------------------------
PPR_Point_Prediction <- predict(lm_form_fit, new_data = RB_2022_sample)
View(PPR_Point_Prediction)
View(RB_2022_sample)

Output_RB_prediction <- bind_cols(RB_2022_sample,PPR_Point_Prediction) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_Points_2022_Per_Touch=.pred) %>% 
  arrange(desc(Predicted_Fantasy_Points_2022_Per_Touch))
View(Output_RB_prediction)
#--------------------------------------------------------------------------
#Send the output to a csv file, in our current working directory
write.table(Output_RB_prediction, file="Exported_RB_Prediction_2022_Pre_Draft_3.csv",row.names=F, sep=",")
#-------------------------------------------
RB_test_refined <- RB_test %>% 
  select(Player_Name,PFF_Rec,Weight_Adjusted_40)

PPR_Point_Prediction_2 <- predict(lm_form_fit, new_data = RB_test_refined)

Output_RB_prediction_2 <- bind_cols(RB_test_refined,PPR_Point_Prediction_2) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_Points_2022_Per_Touch=.pred) %>% 
  arrange(desc(Predicted_Fantasy_Points_2022_Per_Touch))
#below is the poitns per touch predict for the withheld "test" data; for Runningbacks
View(Output_RB_prediction_2)

#now we have to join= the "true" knowwn point per touch values that these runningbacks had during their rookie years
Withheld_RB_Test_Prediction <- Output_RB_prediction_2 %>% 
  inner_join(Historical_2017_2021_College_Rookie_RB_Data, by=c("Player_Name"="Player_Name")) %>% 
  select(Player_Name,Predicted_Fantasy_Points_2022_Per_Touch,Rookie_Year_Points_per_touch)
View(Withheld_RB_Test_Prediction)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
RB_metrics <- metric_set(rmse, rsq, mae)
RB_metrics(Withheld_RB_Test_Prediction, truth = Rookie_Year_Points_per_touch, estimate = Predicted_Fantasy_Points_2022_Per_Touch)



RB_test %>% 
  select(Rookie_year_fantasy_points) %>% 
  bind_cols(predict(lm_form_fit, RB_test)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, RB_test, type = "pred_int"))

View(RB_test)
#Maybe I should predict points per touch as planned then project out touches based on landing spot?



#points per games played-- special teams RB are still a data point-- injured players
#include those players



#use some of the functions from the yardstick package to judge the effectiveness of the model

rmse(ames_test_res, truth = Sale_Price, estimate = .pred)

#compute multiple metrics at once
RB_metrics <- metric_set(rmse, rsq, mae)
RB_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

#----------------------------------------------------------------------------------------------------
#predict touches per game using draft round using player's draft round & team's prior year RB success


#let's clean up the historical data

#Let's predict fantasy points using weight adjusted 40 for RBs with over 25 rookie touches, trained on data from 2017,2018,2019,2020
Training_data_RB <- filter(Historical_2017_2021_College_Rookie_RB_Data, Rookie_year_Touches>75)
View(Training_data_RB)

#Predict touches/game here
#tidy version of 80-20 data split
#DO I want to stratify this by a column..
RB_split <- initial_split(Training_data_RB, prop = 0.80)

RB_split
#running the above is the printed info 2344/586/2930
#training data/test data/size of the original pool of samples

RB_train <- training(RB_split)
RB_test  <-  testing(RB_split)
View(RB_train)
#Create the linear regression model
#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #fit what is predictive of rookie back usage-- existing back is old or he sucks... + Incumbent_RB_Age + Prior_Year_RB_Value
  #rather than being old or bad, finding out the draft team's prior year carry distribution is more indicative
  #of potential opportunity present for a back
  fit(Touches_per_Game ~ Draft_Round + Incumbent_RB_Age + Prior_Year_RB_Value, data = RB_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = RB_train %>% select(Draft_Round, Incumbent_RB_Age, Prior_Year_RB_Value),
    y = RB_train %>% pull(Touches_per_Game)
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

lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Testing off of withheld data
Test_Touches_per_Game <- predict(lm_form_fit, new_data = RB_test)
Output_Test_Touches_game <- bind_cols(RB_test,Test_Touches_per_Game ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Touches_per_game=.pred) 
  #arrange(desc(Predicted_Touches))
View(Output_Test_Touches_game)
View(RB_test)

Withheld_RB_Touches <- Output_Test_Touches_game %>% 
  inner_join(Historical_2017_2021_College_Rookie_RB_Data, by=c("Player_Name"="Player_Name")) %>% 
  select(Player_Name,Predicted_Touches_per_game,Touches_per_Game)
View(Withheld_RB_Touches)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
RB_metrics <- metric_set(rmse, rsq, mae)
RB_metrics(Withheld_RB_Touches, truth = Touches_per_Game, estimate = Predicted_Touches_per_game)

#-------------------------------------------------------------------------------------

#LET'S SEE IF the prior year rb1's carry numbers can help us predict rookie RB targets
#Let's predict fantasy points using weight adjusted 40 for RBs with over 10 rookie touches, trained on data from 2017,2018,2019,2020
Training_data_RB <- filter(Historical_2017_2021_College_Rookie_RB_Data, Rookie_year_Touches > 10) %>% 
  select(Player_Name,PFF_Rec,Draft_Selection, Weight_Adjusted_40, Rookie_year_fantasy_points)
View(Training_data_RB)
#Normalize the predictors as part of pre-processing to get them on the same scale
Scale_Training_standardize <- as.data.frame(scale(Training_data_RB[2:4]))
standardized_predictors <- as_tibble(Scale_Training_standardize) %>% 
  rename(PFF_Rec_std=PFF_Rec,Draft_Selection_std=Draft_Selection,Weight_Adjusted_40_std=Weight_Adjusted_40)

#combine the columns of new standardized predictors
View(standardized_predictors)
training_final <- bind_cols(Training_data_RB,standardized_predictors)
View(training_final)


#Predict  here
#tidy version of 85-15 data split
#DO I want to stratify this by a column..
RB_split <- initial_split(training_final, prop = 0.85)

RB_split
#running the above is the printed info 2344/586/2930
#training data/test data/size of the original pool of samples

RB_train <- training(RB_split)
RB_test  <-  testing(RB_split)
#View(RB_train)
#Create the linear regression model
#specify type
#Normalization for size of predictors- pre-processing- scale them

#step scale;


lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
#predict points off of 
  fit(Rookie_year_fantasy_points ~ Draft_Selection_std + PFF_Rec_std, data = RB_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = RB_train %>% select(Draft_Selection_std, PFF_Rec_std),
    y = RB_train %>% pull(Rookie_year_fantasy_points)
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

#Normalization
param_est
tidy(lm_form_fit)

ggplot(data = RB_train) + 
  geom_point(mapping = aes(x = Rookie_year_fantasy_points, y = PFF_Rec_std))
lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Testing off of withheld data- Cross Validation
Test_Fantasy_RB_points <- predict(lm_form_fit, new_data = RB_test)
Output_Test_Fantasy_RB_points <- bind_cols(RB_test,Test_Fantasy_RB_points ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_points=.pred) 
#arrange(desc(Predicted_Touches))
View(Output_Test_Fantasy_RB_points)
View(RB_test)

Withheld_RB_Points <- Output_Test_Fantasy_RB_points %>% 
  inner_join(Historical_2017_2021_College_Rookie_RB_Data, by=c("Player_Name"="Player_Name")) %>% 
  select(Player_Name,Predicted_Fantasy_points,Rookie_year_fantasy_points)
View(Withheld_RB_Points)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
RB_metrics <- metric_set(rmse, rsq, mae)
RB_metrics(Withheld_RB_Points, truth = Rookie_year_fantasy_points, estimate = Predicted_Fantasy_points)

#Let's apply the model to the 2022 RB class
#Let's predict fantasy points using weight adjusted 40 for RBs with over 10 rookie touches, trained on data from 2017,2018,2019,2020
standardized_RB_predictors <- New_College_Rookie_RB_Data %>% 
  select(Player_Name,PFF_Rec,Draft_Selection, Weight_Adjusted_40)
View(standardized_RB_predictors)
#Normalize the predictors as part of pre-processing to get them on the same scale
Scale_standardized_RB <- as.data.frame(scale(standardized_RB_predictors[2:4]))
Scaled_standardized_RB_TB <- as_tibble(Scale_standardized_RB) %>% 
  rename(PFF_Rec_std=PFF_Rec,Draft_Selection_std=Draft_Selection,Weight_Adjusted_40_std=Weight_Adjusted_40)

#combine the columns of new standardized predictors
Scaled_standardized_RB_TB <- bind_cols(standardized_RB_predictors ,Scaled_standardized_RB_TB)
View(Scaled_standardized_RB_TB)



#Testing off of withheld data
New_Fantasy_RB_points <- predict(lm_form_fit, new_data = Scaled_standardized_RB_TB)
View(New_Fantasy_RB_points )
Predict_Fantasy_RB_points <- bind_cols(Scaled_standardized_RB_TB,New_Fantasy_RB_points ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_points=.pred) 
#arrange(desc(Predicted_Touches))
View(Predict_Fantasy_RB_points)

Predicted_Fantasy_RB_points_2 <- mutate(Predict_Fantasy_RB_points, upper_bound=Predicted_Fantasy_points+50) %>% 
  mutate(Predict_Fantasy_RB_points, lower_bound=pmax(Predicted_Fantasy_points-50),0) 
#replace negative values with 0
n <- nrow(Predicted_Fantasy_RB_points_2)
View(n)
for (i in 1:n){
  if(Predicted_Fantasy_RB_points_2$lower_bound[i]<0){
    Predicted_Fantasy_RB_points_2$lower_bound[i] = 0
  }
}
View(Predicted_Fantasy_RB_points_2)

write.table(Predict_Fantasy_RB_points, file="Exported_RB_Prediction_2022_Post_Draft_5_3_selection_rec.csv",row.names=F, sep=",")

#We can't test the validity of the predictions because the rookie years haven't happened

#Use the rmse of the test data to set the ranges for player outcomes


View(Predict_Fantasy_RB_points)



#----------------------------------------------------------------
#LET'S SEE If draft selection is more predictive than round
Training_data_RB <- filter(Historical_2017_2021_College_Rookie_RB_Data, Rookie_year_Touches > 25)
View(Training_data_RB)

#Predict touches/game here
#tidy version of 80-20 data split
#DO I want to stratify this by a column..
RB_split <- initial_split(Training_data_RB, prop = 0.80)

RB_split
#running the above is the printed info 2344/586/2930
#training data/test data/size of the original pool of samples

RB_train <- training(RB_split)
RB_test  <-  testing(RB_split)
View(RB_train)
#Create the linear regression model
#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #rather than being old or bad, finding out the draft team's prior year carry distribution is more indicative
  fit(Rookie_year_fantasy_points ~ Draft_Selection, data = RB_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = RB_train %>% select(Draft_Selection),
    y = RB_train %>% pull(Rookie_year_fantasy_points)
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

#PLOT POINTS IN RELATION TO DRAFT SELECTION, REVERSE THE SCALE TO MORE EASILY IDENTIFY TREND
ggplot(Historical_2017_2021_College_Rookie_RB_Data,aes(Draft_Selection,Rookie_year_fantasy_points))+geom_point()+scale_x_reverse()
ggplot(RB_train,aes(Draft_Selection,Rookie_Year_Points_per_touch))+geom_point()+scale_x_reverse()

ggplot(RB_train,aes(Draft_Round,Rookie_year_fantasy_points))+geom_point()+scale_x_reverse()
ggplot(RB_train,aes(PFF_Rec,Rookie_year_fantasy_points))+geom_point()
ggplot(RB_train,aes(Pass_block_grade,Rookie_Year_Points_per_touch))+geom_point()


lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Testing off of withheld data
Test_Fantasy_RB_points <- predict(lm_form_fit, new_data = RB_test)
Output_Test_Fantasy_RB_points <- bind_cols(RB_test,Test_Fantasy_RB_points ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_points=.pred) 
#arrange(desc(Predicted_Touches))
View(Output_Test_Fantasy_RB_points)
View(RB_test)

Withheld_RB_Points <- Output_Test_Fantasy_RB_points %>% 
  inner_join(Historical_2017_2021_College_Rookie_RB_Data, by=c("Player_Name"="Player_Name")) %>% 
  select(Player_Name,Predicted_Fantasy_points,Rookie_year_fantasy_points)
View(Withheld_RB_Points)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
RB_metrics <- metric_set(rmse, rsq, mae)
RB_metrics(Withheld_RB_Points, truth = Rookie_year_fantasy_points, estimate = Predicted_Fantasy_points)

#Let's apply the model to the 2022 RB class

#Testing off of withheld data
New_Fantasy_RB_points <- predict(lm_form_fit, new_data = New_College_Rookie_RB_Data)
Predict_Fantasy_RB_points <- bind_cols(New_College_Rookie_RB_Data,New_Fantasy_RB_points ) %>% 
  #remove unnecessary columns
  select(Player_Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Fantasy_points=.pred) 
#arrange(desc(Predicted_Touches))
View(Predict_Fantasy_RB_points)

Predicted_Fantasy_RB_points_2 <- mutate(Predict_Fantasy_RB_points, upper_bound=Predicted_Fantasy_points+50) %>% 
  mutate(Predict_Fantasy_RB_points, lower_bound=Predicted_Fantasy_points-50)

View(Predicted_Fantasy_RB_points_2)
write.table(Predicted_Fantasy_RB_points_2, file="Exported_RB_Prediction_2022_Pre_Draft_4_16.csv",row.names=F, sep=",")

#---------------------------------------------------------------------------
RB_ranger <- rand_forest(trees = 100, mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(Rookie_year_fantasy_points ~ Draft_Selection, data = RB_train)

View(RB_ranger)
















#-----------------------------------------------------------------------------------------------------------

#predict WR


#let's clean up the historical data

#Let's predict fantasy points using PFF_Rec, Draft Round and Breakout Age data from 2017,2018,2019,2020
Training_data_WR <- filter(Historical_2017_2021_College_Rookie_WR_Data, Rookie_Year_Fantasy_Points>10)
View(Training_data_WR)



#standardize everyhting
WR_std <- Training_data_WR%>% 
select(Name,Draft_Round,Breakout_Age,Rookie_Year_Fantasy_Points)
View(WR_std)

#Normalize the predictors as part of pre-processing to get them on the same scale
Scale_standardized_WR <- as.data.frame(scale(WR_std[2:3]))
Scaled_standardized_WR_TB <- as_tibble(Scale_standardized_WR) %>% 
  rename(Breakout_Age_std=Breakout_Age,Draft_Round_std=Draft_Round) 
  #remove unnecessary columns
  training_testing_final_WR <- bind_cols(WR_std,Scaled_standardized_WR_TB)
  View(training_testing_final_WR)
  #now split out train and test from the scaled data
  WR_split <- initial_split(training_testing_final_WR, prop = 0.85)
  
  WR_split
  #running the above is the printed info 2344/586/2930
  #training data/test data/size of the original pool of samples
  
  WR_train <- training(WR_split)
  WR_test  <-  testing(WR_split)
  
#Create the linear regression model; now we have the standardized wr predictor variables that are weighted appropriately
  
  
  lm_model <- 
    linear_reg() %>% 
    set_engine("lm")
  
  lm_form_fit <- 
    lm_model %>% 
    # fit the model
    #predict points off of 
    fit(Rookie_Year_Fantasy_Points ~ Draft_Round_std + Breakout_Age_std, data = WR_train)
  
  lm_xy_fit <- 
    lm_model %>% 
    fit_xy(
      x = training_final_WR %>% select( Draft_Round_std, Breakout_Age_std),
      y = training_final_WR %>% pull(Rookie_Year_Fantasy_Points)
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
  
  #Normalization
  param_est
  tidy(lm_form_fit)
  

#combine the columns of new standardized predictors

  #Testing off of withheld data
  New_Fantasy_WR_points <- predict(lm_form_fit, new_data = WR_test)
  Predict_Fantasy_RB_points <- bind_cols(New_College_Rookie_WR_Data,New_Fantasy_WR_points ) %>% 
    #remove unnecessary columns
    select(Player_Name,.pred) %>% 
    #rename prediction column
    rename(Predicted_Fantasy_points=.pred) 
  #arrange(desc(Predicted_Touches))
  View(Predict_Fantasy_RB_points)
  
  Predicted_Fantasy_RB_points_2 <- mutate(Predict_Fantasy_RB_points, upper_bound=Predicted_Fantasy_points+50) %>% 
    mutate(Predict_Fantasy_RB_points, lower_bound=Predicted_Fantasy_points-50)
  
  View(Predicted_Fantasy_RB_points_2)
  write.table(Predicted_Fantasy_RB_points_2, file="Exported_RB_Prediction_2022_Pre_Draft_4_16.csv",row.names=F, sep=",")

#specify type
lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

lm_form_fit <- 
  lm_model %>% 
  # fit the model
  #fit what is predictive of rookie back usage-- existing back is old or he sucks... + Incumbent_RB_Age + Prior_Year_RB_Value
  #rather than being old or bad, finding out the draft team's prior year carry distribution is more indicative
  #of potential opportunity present for a back
  fit(Rookie_Year_Points_per_Game ~ Draft_Round + Breakout_Age, data = WR_train)

lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = WR_train %>% select(Draft_Round, Breakout_Age),
    y = WR_train %>% pull(Rookie_Year_Points_per_Game)
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

lm_form_fit
#PFF_REC im college has a statistically significant p value <0.05

#Testing off of withheld data
Test_WR_Val <- predict(lm_form_fit, new_data = WR_test)
Output_Test_WR <- bind_cols(WR_test,Test_WR_Val ) %>% 
  #remove unnecessary columns
  select(Name,.pred) %>% 
  #rename prediction column
  rename(Predicted_Pts_per_Game=.pred) 
#arrange(desc(Predicted_Touches))
View(Output_Test_WR)
View(WR_test)

Withheld_WR_Pts <- Output_Test_WR %>% 
  inner_join(Historical_2017_2021_College_Rookie_WR_Data, by=c("Name"="Name")) %>% 
  select(Name,Predicted_Pts_per_Game,Rookie_Year_Points_per_Game)
View(Withheld_WR_Pts)

#now we have the truth and the prediction; use the TIDY yardstick functions to estimate the effectiveness of the model
WR_metrics <- metric_set(rmse, rsq, mae)
WR_metrics(Withheld_WR_Pts, truth = Rookie_Year_Points_per_Game, estimate = Predicted_Pts_per_Game)

#Let's Plug in the 2022 Rookies and see how their Points per game come out


WR_2022_Val <- predict(lm_form_fit, new_data = Rookie_WR_2022_Data)
View(WR_2022_Val)






