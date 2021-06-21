library(dplyr)
library(readr)
shake <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/Shakespeare_data.csv')
library(stringr)
?stringr

# All good? Great. Let’s go.

# Q1 How may rows in the data?
nrow(shake)
names(shake)
unique(shake$Player)
shake$PlayerLine[1:10]

shake

# Q2 Create a dataframe named spoken. This should be those lines which are
# spoken by an actor/actress (figure it out).
spoken <- shake %>% 
  filter(!is.na(Player))

# Q3 How many lines are spoken?
nrow(spoken)
  
# Q4 Create a column called first_word. This should be the first word of each
# spoken line.
spoken <- spoken %>% 
  mutate(first_word = word(PlayerLine))
  

# Q5 What is the most common first word spoken?

spoken %>% 
  group_by(first_word) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  filter(count == max(count))

# Q6 Create a boolean column named “King.” This should indicate whether or not
# the word “King” was spoken in any given line.

spoken <- spoken %>% 
  mutate(King = grepl(' King',PlayerLine))

spoken %>% 
  filter(King == TRUE)

# Q7 Improve the above by making sure that it includes both lower and uppercase
# variations of “king.”
spoken <- spoken %>% 
  mutate(King = grepl(' KING', toupper( PlayerLine)))

spoken %>% 
  filter(King == TRUE) %>% 
  nrow()

# Q8 Figure out which play has the word “king” mentioned most?

spoken %>% 
  filter(King == TRUE) %>% 
  group_by(Play) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count))
  
# Q9 What percentage of lines in Hamlet mention the word "king?

spoken %>% 
  group_by(Play) %>% 
  summarize(count = n(), n = sum(King), percent = n*100/count) %>% 
  filter(Play == 'Hamlet')


unique(shake$Play)

# Q10 How many times does the word “woman” appear in each play?
spoken %>% 
  group_by(Play) %>% 
  mutate(NoWomen = sum(grepl('WOMEN', toupper(PlayerLine) ))) %>% 
  summarize(count = n())

# Q11 How many words are there in all Shakespeare plays?
shake %>% 
  summarise(no_words = length(strsplit(PlayerLine, ' ')))

# shake %>% 
#   filter(Play == "Hamlet") %>% 
#   mutate(n = length(strsplit(PlayerLine, ' ')))

# words(shake$PlayerLine)

# Q12 How many letters are there in each Shakespeare play?

shake %>% 
  group_by(Play) %>% 
  mutate(no_letters = nchar(tolower(PlayerLine))) %>% 
  summarise(sum(no_letters))

# Q13 Which character says the most words?
spoken %>% 
  group_by(Player) %>% 
  mutate(no_words = str_count(PlayerLine, '\\w+')) %>% 
  summarise(t_words = sum(no_words)) %>% 
  filter(t_words == max(t_words))

# Q14 Which character says the least words?
spoken %>% 
  group_by(Player) %>% 
  mutate(no_words = str_count(PlayerLine, '\\w+')) %>% 
  summarise(t_words = sum(no_words)) %>% 
  filter(t_words == min(t_words))

# Q15 What is the lines(s) of the character who says the least words?
spoken %>% 
  filter(Player == 'Thieves')
  

# Q16 Make a table of plays with one row per play and variables being: (a)
# number of lines, (b) number of words, (c) number of characters, (d) number of
# letters, (e) number of mentions of “Brew.”
shake %>% 
  select(Play, PlayerLine) %>% 
  group_by(Play) %>% 
  summarise(n_lines = n())
  View()



# -------------------------
# library(dplyr)
# library(readr)
library(tidytext)
trump <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/trumptweets.csv')

nrow(trump)
ncol(trump)
names(trump)

# Q1 In the current format, one row of data is equal to one ?
  
# Q2 Create a variable called line. This should be 1, 2, 3, 4, etc.
trump %>% 
  mutate(line = 1:nrow(trump))

# Q3 Create a variable called text. This should be an exact copy of content.
trump <- trump %>% 
  mutate(text = content)

# Q4 Use the unnest_tokens function to reshape the data for better text processing.
# simple <- trump %>%
#   select(date, retweets, favorites, line) %>%
#   unnest_tokens(word, text)

simple <- trump %>%
  select(-mentions, -hashtags, -geo, -content, -link, -id) %>%
  unnest_tokens(word, text)


# Q5 What format is the data in now (ie, one row is equal to )? = one tweet word
# per row


# Q6 Take a minute to read about the tidytext package at
# https://www.tidytextmining.com/tidytext.html.

# Q7 What is the most common word used by Trump?
simple %>% 
  group_by(word) %>% 
  tally() %>% 
  arrange(desc(n))


# Q8 Use substr to create a year variable.
simple <- simple %>% 
  mutate(year = substr(date, 1, 4))

# Q9 What is the most common word used by Trump each year?
simple %>% 
  group_by(year, word) %>% 
  tally() %>% 
  filter(n == max(n))
  
  
# Q10 Create a variable named month using substr.
simple$date[1]

simple <- simple %>% 
  mutate(month = substr(date, 6, 7))
simple$month

# Q11 What is the most common word used by Trump each month?
simple %>% 
  group_by(year, month, word) %>% 
  tally() %>% 
  group_by(year, month) %>% 
  filter(n == max(n)) %>%
  View()

  
# Q12 Create a dataframe with one word per row, and a column called freq saying
# how many times that word was used.
freq_word <- simple %>% 
  group_by(word) %>% 
  summarise(freq = n()) %>% 
  arrange(desc(freq))

# Q13 Load up the wordcloud library.
# install.packages('wordcloud')
library(wordcloud)

# Q14 Subset the dataframe created in number 12 to only include the top 100 words.
freq_word <- freq_word[1:100,]

# Q15 Create a wordcloud of Trump’s top 100 words.
?wordcloud
wordcloud(freq_word$word, freq_word$freq )

# Q16 Are you ready to do some sentiment analysis? Great.--> Maybe


# Q17 Create a dataframe named sentiments by running the following: 

sentiments <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/sentiments.csv')

# Q18 What is the sentiments dataset? --> it labels positive or negative
# sentiment that a word carries
?sentiments

# Q19 Create another dataset named polarity by running the following: 
# install.packages('textdata')
library(textdata)

polarity <- get_sentiments("afinn")
?get_sentiments

# Q20 Use left_join to combine polarity and sentiments into one dataset named emotions.

emotions <- left_join(sentiments, polarity) %>% filter(!duplicated(word))

# Q21 Use left_join to combine the trump data and the emotions data.

simple <- left_join(simple, emotions)

# Q22 Have a look at the simple (Trump) data. What do you see? --> lots of NA
  
# Q23 Get an overall polarity score (using the value variable) for the entire dataset. Is it positive or negative?
simple %>% 
  summarise(score = sum(value, na.rm = TRUE))

simple %>% 
  filter(year == '2015') %>% 
  summarise(t_anger = sum(anger, na.rm = TRUE))
  

