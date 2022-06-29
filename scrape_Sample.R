#author: John McGrath
#date: 6/28/2022
#project: Scrape Barbell Data off the Rogue website
#description: Get the top 40 featured items on rogues barbell page; order them by price
#Output: a csv file with that day's items

library(tidyverse)
library(rvest)
library(lubridate)

install.packages("sqldf")
library(sqldf)
install.packages("taskscheduleR")

---------------------------------------------------------------------------------------
rogue <-  "https://www.roguefitness.com/weightlifting-bars-plates/barbells"
#given a link, read_html gives you the html source code for the page
rogue_link <-  read_html(rogue)

#use selector gadget to get the element desired, pipe to html text to extract that text
rogue_name <-  rogue_link %>% html_nodes(".title") %>% html_text()

rogue_name_2 <- as.data.frame(rogue_name) %>% 
  filter(rogue_name!="")

#top 40 items
rogue_name_3 <- tail(rogue_name_2,40)

rogue_price <- rogue_link %>% html_nodes(".price") %>% html_text()

  rogue_price_2 <- as.data.frame(rogue_price) %>% 
                  filter(rogue_price!="")

#Provide the current date when it runs
Date_stamp <- as.data.frame(Sys.Date()) %>% 
  rename(date_of_extract="Sys.Date()")

test <- bind_cols(rogue_name_3,rogue_price_2,Date_stamp)

#Remove the dollar sign so we can order by price descending
df_rogue <- as.data.frame(test) %>% 
  mutate(numeric_price=as.double(gsub("\\$", "",rogue_price))) %>% 
  arrange(desc(numeric_price)) %>% 
  select(date_of_extract,rogue_name,rogue_price)

View(df_rogue)

#Record the output
write.table(df_rogue, file="Rogue_Barbell_40_2022_06_28.csv",row.names=F, sep=",")







