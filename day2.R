x <- 10
y <- 12
z <- c(x,y)
z
z[1]


sleeptime <- c(3, 2, 3, 0, 3, 5, NA, 5, NA)
sleeptime[3]> sleeptime[7]


( over_under <- sleeptime-8 )
sum(sleeptime) - 56
sum(sleeptime)

min(sleeptime)

min(sleeptime) * 7

avg36 <- mean(sleeptime[3:6])

# ex K
avg36*365 - 52.1*56

sleeptime[-NA]

sleeptime[-match(c(NA), sleeptime)]

sum(sleeptime, na.rm = TRUE)
sleeptime[-which(is.na(sleeptime))]
max(sleeptime[-which(is.na(sleeptime))])
which.max(sleeptime)
sleeptime[which.max(sleeptime)]
which(sleeptime==0)


years <-  c(1988, 2000, 1999, 1999, 2002, 2001, 2001, 2001, 2001, 2000, 1998,
2002, 2002, 2002, 2000, 2000, 1921, 1998, 1990, 1985)

summary(years)

( ages <- 2021-years )

summary(ages)

?sort()
sorted_aegs <- sort(ages)
range(ages)
sd(ages)
?length()
length(ages)
old <- 70
old_people <- ages > old 
old_people[7]
ages[12]-old
years[which(old_people)]
min(years)

di <- seq(1,6, by = 1)
result <- sample(di, size = , replace = TRUE)
table(result)




# -------------------

nummies <- seq(1,100, by=1)
little_nummies <- nummies[nummies<=30]
these_are_big <- nummies >= 70
big_nummies  <- nummies[which(these_are_big)]
not_that_big <- (nummies >= 30 & nummies <= 70)
meh_nummies <- nummies[not_that_big]
length(meh_nummies)
sum(meh_nummies)
