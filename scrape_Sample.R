#author: John McGrath
#date: 6/28/2022
#project: Scrape Barbell Data off the Rogue website
#description: Get the top 40 featured items on rogues barbell page; order them by price
#Output: a csv file with that day's items

library(tidyverse)
library(rvest)
library(lubridate)
library(janitor)

url <- 'https://www.roguefitness.com/weightlifting-bars-plates/barbells?sort=price.desc&n_item=80'
rogue_link <- read_html(url)

#use selector gadget to get the element desired, pipe to html text to extract that text
rogue_name <-  rogue_link %>% html_nodes(".title") %>% html_text()
rogue_name_2 <- as.data.frame(rogue_name) %>% 
  filter(rogue_name!="")

#top 40 items
rogue_name_3 <- tail(rogue_name_2,80)

rogue_price <- rogue_link %>% html_nodes(".price") %>% html_text()

  rogue_price_2 <- as.data.frame(rogue_price) %>% 
                  filter(rogue_price!="")

#Provide the current date when it runs
date_of_extract <- as.data.frame(as.Date(Sys.Date()))
test <- bind_cols(rogue_name_3,rogue_price_2,date_of_extract) %>% 
  rename(new_date="as.Date(Sys.Date())")

#Remove the dollar sign so we can order by price descending
df_rogue <- as.data.frame(test) %>% 
  mutate(numeric_price=as.double(gsub("\\$", "",rogue_price))) %>% 
  #mutate(new_date=as.character(date_of_extract)) %>% 
  #kick out slashes; case when rogue name contains '\'  remove it
  mutate(rogue_name_2=gsub('\\/', "@",rogue_name)) %>% 
  arrange(desc(numeric_price)) %>% 
  select(new_date,rogue_name_2,rogue_price)

#Record the output
write.table(df_rogue, paste("data/","Rogue_Barbell_",toString(Sys.Date()),".csv"),row.names=F, sep=",")







