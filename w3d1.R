# setwd('~/Sites/datascience')
# getwd()

library('gapminder')
library('ggplot2')
library('dplyr')

gm <- gapminder

# Q8 Create a histogram of life expectancy in 1982
names(gm)
gm %>% 
  filter(year == 1982) %>% 
  ggplot(aes(lifeExp))+
  geom_histogram()


# Q9 Create a line plot for population in Asia, colored by country. Make the lines
# a bit thicker and more transparent.
gm %>% 
  filter(continent == 'Asia') %>% 
  ggplot(aes(year, pop, color = country))+
  geom_line(alpha = 0.5, size = 2)
  

# Q10 Add new x and y axis labels, as well as a chart title.
gm %>% 
  filter(continent == 'Asia') %>% 
  ggplot(aes(year, pop, color = country))+
  geom_line(alpha = 0.5, size = 2)+
  labs(title = "Population in Asia, coloured by country",
       x = "YEAR",
       y = 'POP')

unique(gm$continent)
# Q11 Create a bar chart of all European countries gdp per capita in 2002
gm %>% 
  filter(continent == 'Europe', year == '2002') %>% 
  ggplot(aes(country, gdpPercap))+
  geom_bar(stat = 'identity')

# Q12 Make the bars transparent and filled with the color blue.
gm %>% 
  filter(continent == 'Europe', year == '2002') %>% 
  ggplot(aes(country, gdpPercap), fill = "BLUE")+
  geom_bar(stat = 'identity', alpha = 0.5)


# Q13 Create a new data set called the_nineties that only contains years from the
# 1990s.
the_nineties <- gm %>% 
  filter(year > 1990 & year < 1999)

# Q14 Save this dataset to your repository (use write.csv)
write.csv(the_nineties, '1990s data')
getwd()
