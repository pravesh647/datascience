library(lubridate)
?lubridate

today <- today()
my_birthday <- '1998-06-04'
her_birthday <- '2001-01-31'
str(today)
str(my_birthday)
str(her_birthday)
my_birthday <- as_date(my_birthday)
her_birthday <- as_date(her_birthday)
str(my_birthday)
str(her_birthday)
today - my_birthday
my_birthday  + 10000
today - my_birthday
her_birthday + 10000
today - her_birthday



# Times

n <- now()
n
str(n)
n+1
n + seconds(1)
n + minute(1)
n + hours(1)

hour(n)
minute(n)

round_date(n, unit = 'minute')
later <- now()
str(later)

# -----------------------

library(readr)
library(dplyr)
library(ggplot2)
coughs <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/coughs.csv')

nrow(coughs)
names(coughs)
View(coughs)
# Q1 Create a dow (day of week) column.

coughs <- coughs %>% 
  mutate(dow = weekdays(date_time))

# Q2 Create a date (without time) column.
coughs <- coughs %>% 
  mutate(date = date(date_time))

# Q3 How many coughs happened each day?
max(coughs$date_time)
min(coughs$date_time)
unique(coughs$date)

coughs %>% 
  group_by(date) %>% 
  summarise(total = sum(is_cough))

# Q4 Create a chart of coughs by day.
coughs %>% 
  group_by(date) %>% 
  summarise(total = sum(is_cough)) %>% 
  ggplot(aes(date, total)) +
  geom_bar(stat = 'identity')

# Q5 Look up floor_date. Use it to get the number of coughs by date-hour.
# ?floor_date
coughs %>% 
  filter(is_cough) %>% 
  group_by(floor_date(date_time, 'hour')) %>% 
  summarise(t_hour = sum(is_cough)) %>% 
  arrange( desc(t_hour))

# Q6 Create an hour variable.
coughs <- coughs %>% 
  mutate(hour = hour(date_time))

# Q7 Use the hour variable to create a night_day column indicating whether the
# cough was occurring at night or day.
coughs <- coughs %>% 
  mutate(night_day = ifelse(hour<=7 | hour >20, 'night', 'day'))

# Q8 Does Galileo cough more at night or day?
coughs %>% 
  filter(is_cough) %>% 
  group_by(night_day) %>% 
    tally

# ---------------------------------------------------------------------------

# install.packages('tidygeocoder')
# install.packages('weatherr')

library(dplyr)
library(weatherr)
library(tidygeocoder)
?geo
sewanee_location <- geo('Sewanee, TN')
sewanee_location
fc <- locationforecast(sewanee_location$lat, sewanee_location$long)
View(fc)

ggplot(fc, aes(time, temperature)) +
  geom_point()+
  geom_line()

fc <- fc %>% 
  mutate(f = temperature*(9/5)+32)



# Average cloudiness per day
avg_cloudiness <- fc %>% 
  mutate(date = date(time)) %>% 
  group_by(date) %>% 
  summarise(avg_cloudi = mean(cloudiness))

avg_cloudiness %>% 
  ggplot(aes(date, avg_cloudi)) +
  geom_point()+
  geom_line()



fc <- fc %>% 
  mutate(perfect = temperature>15
         & temperature<25 
         & cloudiness<=40 
         & humidity < 60
         & windSpeed_mps <=5)

fc %>% 
  summarise(perfect_percent = mean(perfect)*100)

fc %>% 
  ggplot(aes(time, humidity)) +
  geom_point()+
  geom_line()



# ----------------------------------------------------

# http://www.datascience.pizza/trump.html

library(dplyr)
library(readr)
library(tidytext)
library(lubridate)
trump <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/trumptweets.csv')
stop_words <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/stopwords.csv')

# Q1 In the current format, one row of data is equal to one tweet
nrow(trump)  
nrow(stop_words)

# Q2 Create a variable called line. This should be 1, 2, 3, 4, etc.
trump <- trump %>% 
  mutate(line = 1:nrow(trump))

# Q3 Create a variable called text. This should be an exact copy of content.
trump <- trump %>% 
  mutate(text = content)

# Q4 Use the unnest_tokens function to reshape the data for better text processing.
simple <- trump %>%
  select(-mentions, -hashtags, -geo, -content, -link, -id) %>%
  unnest_tokens(word, text)

# Q5 What format is the data in now (ie, one row is equal to one word in a tweet)?
names(simple)
# Q6 Take a minute to read about the tidytext package at https://www.tidytextmining.com/tidytext.html.

# Q7 What is the most common word used by Trump?
simple %>% 
  group_by(word) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  
# length(unique(simple$word))  
  
# Q8 Use substr to create a year variable.
simple <- simple %>% 
  mutate(year = year(date))

str(simple$year)


# Q9 What is the most common word used by Trump each year?
simple %>% 
  group_by(year, word) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count))
  
# Q10 Create a variable named month using substr.
simple <- simple %>% 
  mutate(month = month(date))


# Q11 What is the most common word used by Trump each month?
simple %>% 
  group_by(month, word) %>% 
  summarise(count = n()) %>% 
  filter(count == max(count))


# Q12 Create a dataframe with one word per row, and a column called freq saying
# how many times that word was used.
words_used <- simple %>% 
  group_by(word) %>% 
  summarise(freq = n())

# Q13 Load up the wordcloud library.
library(wordcloud)

# Q14 Subset the dataframe created in number 12 to only include the top 100 words.
words_used <- words_used %>% 
  arrange(desc(freq)) %>% 
  head(100)

words_used <- words_used[1:100,]  

# Q15 Create a wordcloud of Trump’s top 100 words.
wordcloud(words_used$word, words_used$freq)

words_used <- words_used %>% 
  filter(word)
  
stop_words <- stop_words %>% 
  mutate(remove_me = TRUE)

simple <- simple %>% 
  left_join(stop_words, by='word')

simple <- simple %>% 
  mutate(remove_me = ifelse(is.na(remove_me), FALSE, remove_me)) 

simple <- simple %>% 
  rename(stop_word = remove_me)

# Q16 Are you ready to do some sentiment analysis? Great. NOOOOOOO!!!!!

# Q17 Create a dataframe named sentiments by running the following: 
sentiments <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/sentiments.csv')

# Q18 What is the sentiments dataset?
?sentiments
  
# Q19 Create another dataset named polarity by running the following: polarity
# <- get_sentiments("afinn")
