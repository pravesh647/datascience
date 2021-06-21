# ------ install packages ---------

# install.packages("ggplot2")
# installed.packages("ggplot2")
library(ggplot2)

# install.packages("dplyr")
# installed.packages("dplyr")
library(dplyr)

# install.packages("readr")
# installed.packages("dplyr")
library(dplyr)

# install.packages('RColorBrewer')
library(RColorBrewer)

# install.packages('tidyr')
# installed.packages("tidyr")
library(tidyr)

# install.packages('gapminder')
library(gapminder)

# install.packages('readr')
library(readr)

# install.packages('gsheet')
library(gsheet)

# install.packages('fueleconomy')
library(fueleconomy)

# ---------------------------------

# survey <- gsheet::gsheet2tbl('https://docs.google.com/spreadsheets/d/1iVt9FX9J2iv3QFKBM7Gzb9dgva70XrW1lxMV4hpekeo/edit?resourcekey#gid=204634767')
# View(survey)
# rm(survey)

View(vehicles)

vehicles[1,]
vehicles[,1]
names(vehicles)


vehicles$model

vehicles[,5:10]
table(vehicles$drive)

hasNA <- c()
for (i in 1:length(names(vehicles))) {
  hasNA[i] <- any(is.na(vehicles[,i]))
}

names(vehicles)[hasNA]
length(which(is.na(vehicles$trans)))

clean_trans <- na.omit(vehicles$trans)
# OR
clean_trans <-  vehicles[!is.na(vehicles$trans),]
View(clean_trans)
any(is.na(clean_trans))


# -------------------

df <- read_csv('https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/deaths.csv')
nrow(df)
summary(df)

9 %>% sqrt

sample(10:20, 2, replace = FALSE) %>% sum %>% sqrt

seq(1,100, 1) %>% sqrt %>% sd %>% mean

# ------------

boat_women <- df[df$Sex=='female',]
boat_men <- df %>% filter(Sex == 'male')


old_boat <- df %>% filter(Age>=72)
old_men <- df %>% filter(Age>=72, Sex=='male')

df <- df %>% 
  mutate(days_old = Age*365.25) %>% 
  mutate(months_old = days_old/30)
