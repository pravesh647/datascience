library(readr)

library(dplyr)

df <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/deaths.csv')
student_names <- c('Caroline', 'Waverly', 'Nika', 'Pravesh', 'Jonah',
                  'Martha', 'Mehrael', 'Sophie', 'Katherine', 'Dirk', 'Jeremiah', 'Rebecca',
                  'Kate', 'Zach', 'Shehryar', 'Ngan', 'Vincent', 'Esarrah', 'Pratham',
                  'Kirstyn', 'Feza')

# 1) How many people are in the dataset.
colnames(df)
nrow(df)

# 2) Use summarize to count the number of men and women.
df %>% 
  group_by(Sex) %>% 
  summarise(counts = n())

# 3) Use summarize to count the number of people in each class.
df %>% 
  group_by(Pclass) %>% 
  summarise(counts = n())

# 4) Use summarize to count the number of men and women in each class.
df %>% 
  group_by(Pclass, Sex) %>% 
  summarise(counts = n())

# 5) Use mutate to create a variable called died. This should be a boolean based on the Survived column (in which 1 means the person survived, and 0 means the person died).
df <- df %>% 
  mutate(died = Survived == 0)

# 6) Use mutate to create a variable called child. This should be a boolean based on the Age column, indicating if someone was less than 18 years old.
df <- df %>% 
  mutate(child = Age < 18)

# Removing observations with NA Age
df <- df %>% filter(!is.na(Age))

# 7)Create a different dataframe for men vs. women. Name them accordingly.
# df$Sex
men_df <- df %>% filter(Sex == 'male')
women_df <- df %>% filter(Sex == 'female')

# 8) Create a different dataframe for class 1, class 2, and class 3. Name them
# accordingly.
c1_df <- df %>% filter(Pclass == 1)
c2_df <- df %>% filter(Pclass == 2)
c3_df <- df %>% filter(Pclass == 3)


# 9) For each of the 3 datasets you’ve just created, what is the death rate?
class_data <- df %>%  group_by(Pclass) %>% summarise(drate = sum(died)*100/length(Pclass))

#10) For each of the 3 class, how many children died?
child_death <- df %>% group_by(Pclass) %>% summarise(d_child = sum(child & died))
child_death  


# 11) Now, using the full dataset, calculate the child-specific death rate for
# each combination of class and sex (ie, “first class females,” “third class
# males,” etc.).
child_drate <-df %>% 
  group_by(Pclass, Sex) %>% 
  summarize(n_died = sum(died & child), n = length(child), child_drate = n_died*100/n)

child_drate

# 12) What did you find? What might explain that?





# Team Exercise ---------------

# Load library
library(gsheet)
library(dplyr)

# Read in data

survey  <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1iVt9FX9J2iv3QFKBM7Gzb9dgva70XrW1lxMV4hpekeo/edit?resourcekey#gid=204634767')

# remake the names 
names(survey) <- c('time', 'sex', 'age','sib', 'dad_mus',
'person_mus', 'joe_mus_is', 'eyesight', 'height', 'shoe_size', 'bday',
'money_or_love', 'rps_skill', 'num_pan', 'cats_dogs', 'name')


# Q1
# Create a dataframe called old_people. This should include only people older
# than 20. Write code to calculate the number rows in your new dataframe.
nrow(survey)
old_people <- survey %>% filter(age>20)
View(old_people)
nrow(old_people)

# Q2
# Create a dataframe called captivated. This should include all those people who
# find Joe’s moustache to be “deeply captivating.” Write code to calculate the
# number rows in your new dataframe.

captivated <- survey %>% filter(joe_mus_is == 'Deeply captivating')
nrow(captivated)

# Q3
# Create a dataframe called special_people. This should be people who are taller
# than 175cm, prefer cats over dogs, and consider themselves to be average at
# rock, paper, scissors.

special_people <- survey %>% filter(height > 175, cats_dogs == 'Cats', rps_skill == 'Average' )
special_people

# Q4
# In the full dataset (survey), do more people like cats or dogs? What about
# among “special” people?

survey %>% 
  group_by(cats_dogs) %>% 
  summarise(count = n())

# In survey dataset, people like dogs more

special_people %>% 
  group_by(cats_dogs) %>% 
  summarise(count = n())

# In special_people dataset, people like cats more



# Q5 Create a new variable in survey called “std_shoes” that standardizes shoe
# sizes by converting men’s shoe size to women’s (There is an approximate 1.5
# size difference between Men’s and Women’s sizing (e.g., a men’s size 7 is
# roughly equivalent to a women’s size 8.5)
survey$sex
survey <- survey %>% mutate(std_shoes = ifelse(sex == 'Male'
                                               , shoe_size*1.5
                                               , shoe_size))

# Q6
# Get the avg shoe size by sex (Male, Female, Prefer not to say)
survey %>%  
  group_by(sex) %>% 
  summarise(avg_size = mean(std_shoes))

# Q7
# Get Average age, height, & number of siblings, by the sex
survey %>% 
  group_by(sex) %>% 
  summarise(avg_age = mean(age), avh_h = mean(height), avg_sib = mean(sib, na.rm = TRUE))


# Q8 Do people that have ever had a mustache think there will be more pandemics
# on average than those who have never had a mustache? --> Yes
survey %>% group_by(person_mus) %>% 
  summarise(avg_pan = mean(num_pan))


# Q9 Do people that prefer cats have smaller feet on average than those who
# prefer dogs?
survey %>% 
  group_by(cats_dogs) %>%
  summarise(avg_feet = mean(std_shoes, na.rm = TRUE))

# Q10
# Is eyesight associated with moustache perception?
survey %>% group_by(person_mus, eyesight) %>% summarise(count = n())

# Q11 What percentage of people think they are better than average at rock
# scissors paper?
survey %>% 
  group_by(rps_skill) %>% 
  summarise(n = nrow(survey), n_better = n(), percent = n_better*100/n)

# Q12 What percentage of men and women think they are better than average at
# rock scissors paper?

survey %>% 
  group_by(sex) %>%
  summarise(bet_avg = sum(rps_skill == 'Better than average'), counts = n()) %>%
  mutate(per = bet_avg/ counts)

# Q13
# How many people think money matters more than love? 5 vs 10

survey %>% group_by(money_or_love) %>% tally

# Q14 Create a dataframe, grouped by whether or not people’s dads had
# moustaches, with variables showing each of the following: the maximum age,
# maximum height,minimum number of pandemics, and average number of siblings
dad_mous_info <- survey %>% 
  group_by(dad_mus) %>% 
  summarise(max_age = max(age), 
            min_height = min(height),
            mim_pan = min(num_pan),
            avg_sib = mean(sib, na.rm = TRUE))

dad_mous_info

# Q15
# What percentage of men have terrible eyesight?
names(survey)
survey$eyesight

survey %>% 
  group_by(sex) %>% 
  summarise(ter_eye = sum(eyesight == 'Terrible (need glasses/contacts to get by)'), n_sex = n()  )  %>% 
  mutate(ter_eye_per = ter_eye*100/n_sex)


# Q16
# How many women have a shoe size of 9 or more?
survey %>% 
  filter(sex == 'Female', shoe_size>=9) %>% 
  nrow()

# Q17 Create a variable in the survey dataset named days_old. Use the bday
# variable and subtract it from Sys.Date()
survey <- survey %>% 
  mutate(days_old = Sys.Date() - bday)
survey$days_old

# Q18
# What is the standard deviation of age?

sd(survey$age, na.rm = TRUE)

# Q19 Create a one-column dataset which contains the name(s) of the person(s)
# with the most number of siblings (hint: use the following dplyr verbs in this
# order: filter, select).
max_sib_people <- survey %>% 
  filter(sib == max(sib, na.rm = TRUE)) %>% 
  select(name)
max_sib_people

# Q20
# Tell me something interesting about this dataset.
# Jeremiah Studivant has 9 freaking siblings












