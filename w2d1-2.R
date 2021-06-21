library(ggplot2)
library(readr)
# install.packages('ggthemes')
library(ggthemes)
library(dplyr)
library(babynames)
titanic<- read_csv("https://raw.githubusercontent.com/databrew/intro-to-data-science/main/data/deaths.csv")

titanic %>% 
  ggplot(aes(Age, Fare))+
  geom_point()



bn <- babynames
?babynames
names(bn)

head(bn)

francis <- bn %>% 
  filter(name == "Maria" | name == "Mary", sex == "F")

ggplot(francis, aes(year, n, color=name))+
  geom_line()
  

god <- bn %>% 
  filter(name == "Francis")

ggplot(god, aes(year, n))  +
  geom_line()+
  facet_wrap(~sex)+
  geom_point()

# -----------------------
# Practice exercises 2

library(readr)
library(dplyr)
library(gapminder)
gm <- gapminder::gapminder

# 
# How many rows are in the dataset?
nrow(gm)

#   How many columns are in the dataset?
ncol(gm)

#   What are the names of the columns?
names(gm)

#   What is the oldest year in the dataset?
min(gm$year)

#   What is the country/year with the greatest population in the dataset?
gm %>% 
  filter(pop == max(pop))
  
#   Get the average GDP per capita for each continent in 1952.
gm %>% 
  filter(year == 1952) %>% 
  group_by(continent) %>% 
  summarise(avg = mean(gdpPercap))

# Get the average GDP per capita for each continent for the most recent year in the dataset.
gm %>% 
  filter(year == max(year)) %>% 
  group_by(continent) %>% 
  summarise(avg = mean(gdpPercap))
  
# Average GDP is a bit misleading, since it does not take into account the
# relative size (in population) of the different countries (ie, China is a lot
# bigger than Cambodia). Look up the function weighted.mean. Use it to get the
# average life expectancy by continent for the most recent year in the dataset,
# weighted by population.
?weighted.mean 

avg_lifeExp <- gm %>% 
  filter(year == max(year)) %>% 
  group_by(continent) %>% 
  summarise(avg = weighted.mean(lifeExp, pop, na.rm = TRUE))


# Make a barplot of the above table (ie, average life expectancy by continent,
# weighted by population).

avg_lifeExp %>% 
ggplot(aes(continent, avg))+
  geom_bar(stat = 'identity')
  
# Make a point plot in which the x-axis is country, and the y-axis is GDP. Add
# the line theme(axis.text.x = element_text(angle = 90)) in order to make the
# x-axis text vertically aligned. What’s the problem with this plot? How many
# points are there per country?
names(gm)
ggplot(gm, aes(country, gdpPercap))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))
# ----> the text in x-axis text vertically aligned cannot be read. multiple
# points per country

# Q11 Make a new version of the above, but filter down to just the earliest year
# in the dataset.

gm %>% 
  filter(year == min(year)) %>% 
ggplot(aes(country, gdpPercap))+
  geom_point()+
  theme(axis.text.x = element_text(angle = 90))

# Q12
# Make a scatterplot of life expectancy and GDP per capita, just for 1972.
gm %>% 
  filter(year == '1972') %>% 
  ggplot(aes(lifeExp, gdpPercap)) +
  geom_point()

# Q13
# Make the same plot as above, but for the most recent year in the data.

gm %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(lifeExp, gdpPercap)) +
  geom_point()

# Q14
# Make the same plot as the above, but have the size of the points reflect the
# population.
gm %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(lifeExp, gdpPercap, size = pop)) +
  geom_point()


# Q15 Make the same plot as the above, but have the color of the points reflect
# the continent.
gm %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(lifeExp, gdpPercap, size = pop, color = continent)) +
  geom_point()

# Q16 Filter the data down to just the most recent year in the data, and make a
# histogram (geom_histogram) showing the distribution of GDP per capita.

gm %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(gdpPercap)) +
  geom_histogram()

# Q17 Get the average GDP per capita for each continent/year, weighted by the
# population of each country.
avg_gdp_conti <- gm %>% 
  group_by(continent, year) %>% 
  summarise(x = weighted.mean(gdpPercap, pop, na.rm = TRUE))

avg_gdp_conti  

# Q18 Using the data created above, make a plot in which the x-axis is year, the
# y-axis is (weighted) average GDP per capita, and the color of the lines
# reflects the content.

avg_gdp_conti %>% 
  ggplot(aes(year, x, color = continent)) +
  geom_line()

# Q19 Make the same plot as the above, but facet the plot by continent.
avg_gdp_conti %>% 
  ggplot(aes(year, x, color = continent)) +
  geom_line()+
  facet_wrap(~ continent)

# Q20 Make the same plot as the above, but remove the coloring by continent.
avg_gdp_conti %>% 
  ggplot(aes(year, x)) +
  geom_line()+
  facet_wrap(~ continent)

# Q21 Make a plot showing France’s population over time.

gm %>% 
  filter(country == 'France') %>% 
  ggplot(aes(pop, year)) +
  geom_point() +
  geom_line()


# Q22 Make a plot showing all European countries’ population over time, with
# color reflecting the name of the country.

gm %>% 
  filter(continent == 'Europe') %>% 
  ggplot(aes(year, pop, color = country)) +
  geom_point() +
  geom_line()


# Q23 Create a variable called status. If GDP per capita is over 20,000, this should
# be “rich”; if between 5,000 and 20,000, this should be “middle”; if this is
# less than 5,000, this should be “poor.”

gm <- gm %>% 
  mutate(status = ifelse(gdpPercap > 20000, 'rich', ifelse(gdpPercap >=5000, 'middle', 'poor')))

# Q24 Create an object with the number of rich countries per year.

richperpear <- gm %>% 
  filter(status == 'rich') %>% 
  group_by(year) %>% 
  summarise(count = n())


# Q25 Create an object with the percentage of countries that were rich each year.

percent_richperpear <- gm %>% 
  filter(status == 'rich') %>% 
  group_by(year) %>% 
  summarise(count = n(),
            t_count = length(unique(gm$country)),
            percent = count*100/t_count)
percent_richperpear

# Q26 Create a plot showing the percentage of countries which were rich each year.
percent_richperpear %>% 
  ggplot(aes(year, percent))+
  geom_point()+
  geom_line()

# Q27 Create an object with the number of people living in poor countries each year.
pop_poor_country <- gm %>% 
  filter(status == 'poor') %>% 
  group_by(year) %>% 
  summarize(n = sum(pop))


# Q28 Create a chart showing the number of people living in rich, medium, and poor
# countries per year (line chart, coloring by status).
gm %>% 
  group_by(status, year) %>% 
  summarize(n = sum(pop)) %>% 
  ggplot(aes(year, n, color = status)) +
  geom_line()


# Q29 Create a chart showing the life expectancy in Somalia over time.
gm %>% 
  filter(country == 'Somalia') %>% 
  ggplot(aes(year, lifeExp))+
  geom_point() +
  geom_line()

# Q30 Create a chart showing GDP per capita in Somalia over time.
gm %>% 
  filter(country == 'Somalia') %>% 
  ggplot(aes(year, gdpPercap))+
  geom_point() +
  geom_line()
  

# Q31 Create a histogram of life expectancy for the most recent year in the
# data. Facet this chart by continent.
gm %>% 
  filter(year == max(year)) %>% 
  ggplot(aes(lifeExp)) +
  geom_histogram() +
  facet_wrap(~ continent)


# Q32 Create a barchart showing average continent-level GDP over time, weighted
# for population, with one bar for each year, stacked bars with the color of the
# bars indicating continent (geom_bar(position = 'stack')).

gm %>% 
  group_by(continent, year) %>% 
  summarise(avg_gdp_conti = weighted.mean(gdpPercap, pop, na.rm = TRUE)) %>% 
  ggplot(aes(year, avg_gdp_conti, fill = continent)) +
  geom_bar(position = 'stack', stat = 'identity')+
  facet_wrap(~continent)
  

# Q33 Create the same chart as above, but with bars side-by-side
# (geom_bar(position = 'dodge'))
gm %>% 
  group_by(continent, year) %>% 
  summarise(avg_gdp_conti = weighted.mean(gdpPercap, pop, na.rm = TRUE)) %>% 
  ggplot(aes(year, avg_gdp_conti, fill = continent)) +
  geom_bar(position = 'dodge', stat = 'identity')+
  facet_wrap(~continent)


# Q34 Generate 3-5 more charts / tables that show interesting things about the
# data.


# Q35 Make the above charts as aesthetically pleasing as possible.



