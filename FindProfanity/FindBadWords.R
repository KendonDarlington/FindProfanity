#Install packages if you need them
# install.packages('tidyverse')
# install.packages('sentimentr')

#Import packages
library(tidyverse)
library(sentimentr)

#Import our data. You need to go download RAW_recipes.csv from Kaggle and drop that file into your project folder.
#https://www.kaggle.com/shuyangli94/food-com-recipes-and-user-interactions?select=RAW_recipes.csv
df <- read_csv('RAW_recipes.csv')

#Lets restrict our data and clean it up a bit
df <- df %>% 
  select(name, ingredients) %>% 
  mutate(ingredients =str_replace_all(ingredients, "[[:punct:]]", " ")) %>% 
  head(100)

#Lets define our own lexicon of cuss words
customCussWords <- c('bacon', 'lettuce', 'tomatoes')

#Flag records that have profanity
dfRecordsWithProfanity <- df %>% 
  rowwise() %>% 
  mutate(ProfanityCount = sum(profanity(ingredients, profanity_list = customCussWords)$profanity_count)) %>% 
  filter(ProfanityCount > 0)
dfRecordsWithProfanity


#Create a function to return our cuss words
f_getProfanity<- function(text, lexicon){
  returnText <- ''
  for(word in lexicon){
    if(grepl(word, text)){
      returnText <- paste(returnText, word)
    }
  }
  return(returnText)
}

#List the profanity found
df %>% 
  rowwise() %>% 
  mutate(ProfanityCount = sum(profanity(ingredients, profanity_list = customCussWords)$profanity_count)) %>% 
  filter(ProfanityCount > 0) %>% 
  mutate(ProfanityFound = f_getProfanity(ingredients, customCussWords)) 

  





