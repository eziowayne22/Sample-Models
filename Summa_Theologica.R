#author: John McGrath
#date: 7/2/2022
#project: Finding the Top 25 Words
#description: import the data, tidy and filter out noise, visualize
#Output: 3 wordclouds that show the top 25 words, distinguished by color and size


library(gutenbergr)
library(Rcpp)
library(tidytext)
library(tidyverse)
install.packages("wordcloud")
library(wordcloud)

#summa part 1

summa_1 <- gutenberg_download(c(17611))


tidy_summa_1 <- summa_1 %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)


#get the word count
new_common_words <- tidy_summa_1 %>% 
  count(word, sort = TRUE) %>% 
  mutate(non_num=as.integer(word)) %>% 
  filter(is.na(non_num)== TRUE) %>% 
  filter(!word %in% c('obj','reply','de')) %>% 
  select(word,n)
ungroup() 

# plot the 25 most common words

pal <- brewer.pal(8,"Paired")
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.1, "Top 25 Words: Summa Theologica, Part I (Prima Pars)")
new_common_words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 25, colors=pal, main="Title"))

#summa part 2

summa_2 <- gutenberg_download(c(17897))


tidy_summa_2 <- summa_2 %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)


#get the word count
new_2_common_words <- tidy_summa_2 %>% 
  count(word, sort = TRUE) %>% 
  mutate(non_num=as.integer(word)) %>% 
  filter(is.na(non_num)== TRUE) %>% 
  filter(!word %in% c('obj','reply','de','ii
','_______________________
','_i','_on')) %>% 
  filter(n!=1027, n!=746) %>% 
  select(word,n) %>% 
ungroup() 

#-------------------------

# plot the 50 most common words

pal <- brewer.pal(8,"Paired")
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.1, "Top 25 Words: Summa Theologica, Part I-II (Pars Prima Secundae)")
new_2_common_words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 25, colors=pal, main="Title"))

#summa part 3

summa_3 <- gutenberg_download(c(19950))



tidy_summa_3 <- summa_3 %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words)

#get the word count filter out the top 25 range of non filler word/strings
new_3_common_words <- tidy_summa_3 %>% 
  count(word, sort = TRUE) %>% 
  mutate(non_num=as.integer(word)) %>% 
  filter(is.na(non_num)== TRUE) %>% 
  filter(!word %in% c('obj','reply','de')) %>% 
  filter(n!=785, n!=644) %>% 
  select(word,n) %>% 
ungroup() 

#-------------------------

# plot the 25 most common words
pal <- brewer.pal(8,"Paired")
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.1, "Top 25 Words: Summa Theologica, Part III (Tertia Pars) ")
new_3_common_words %>% 
  with(wordcloud(word, n, random.order = FALSE, max.words = 25, colors=pal, main="Title"))