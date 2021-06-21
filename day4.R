library(readr)
library(dplyr)
df<- read_csv("https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/deaths.csv")
titanic <- df

# arrange
# mutate
# select

just_class_and_age <- titanic %>%
  select(Pclass, Age) %>%
  arrange(Age)

# summarise: make the dataset shorter
titanic %>%
  summarize(average_age = mean(Age, na.rm = TRUE))
titanic %>%
  summarise(average_faaaaarrreeeee = mean(Fare))
my_summarized_table <- titanic %>%
  summarise(max_fare = max(Fare),
            min_fare = min(Fare),
            avg_fare = mean(Fare))

# rename
titanic <- titanic %>%
  rename(money = Fare,
         sex = Sex,
         age = Age)

# mutate = add another variable / make the dataset wider
titanic <- titanic %>%
  mutate(usd = money * 1.41)
titanic %>%
  summarise(total_usd = sum(usd))

# who spent more to get on the titanic: men or women
titanic %>%
  group_by(sex) %>%
  summarise(avg_spent = mean(usd),
            n_people = length(Survived),
            max_spent = max(usd),
            min_spent = min(usd))

mini_titanic <-  titanic %>% select(Name, age, sex)
ncol(mini_titanic)


# --------------------
# Exercise from below link
# http://www.datascience.pizza/dataframes.html#dplyr

library(dplyr)
# install.packages('babynames')
library('babynames')
# ?babynames

bn <- babynames
View(bn)
names(babynames)

turn_of_century <- bn %>% filter( year=='1990')
# View(turn_of_century)

table(bn$sex)
boys <- bn %>% filter( sex=='M')
# View(boys)

# Q4
moms_gen <- bn %>% filter(sex=='F', year=='1980')
View(moms_gen)

# Q5
moms_gen <- moms_gen %>% arrange(n)
moms_gen[1,]

# Q6
moms_gen_ordered <-  moms_gen %>% arrange(desc(n))
# View(moms_gen_ordered)

# Q7
boys2k <- bn %>% filter(sex == 'M')
boys2k

# Q8
boys2k <- boys2k %>% arrange(desc(n))
paste0('Most popular:', boys2k$name[1])

# Q9
boys2k %>% filter(year == '2000', name=='Joseph') %>% select(prop)

# Q10
bn %>% filter(year == '2020')
print('neither')

# Q11
print('0')

