## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(roller)

## ------------------------------------------------------------------------
basic_device <- device()
basic_device

## ------------------------------------------------------------------------
basic_device$sides
basic_device$prob

## ------------------------------------------------------------------------
# create an unfair object
loaded_die <- device(prob = c(.8, .2))
loaded_die

# create a coin
irish_euro <- device(sides = c("continent", "harp"))

# normal six sided die
die <- device(sides = 1:6, prob = rep(1/6, 6))

## ------------------------------------------------------------------------
loaded_roll50 <- roll(device = loaded_die, times = 50)
loaded_roll50

euro_roll50 <- roll(device = irish_euro, times = 50)
euro_roll50

die_roll100 <- roll(device = die, times = 100)
die_roll100

## ------------------------------------------------------------------------
names(die_roll100)
die_roll100$rolls
die_roll100$sides
die_roll100$prob
die_roll100$total

## ------------------------------------------------------------------------
euro_roll50

euro_roll150 <- euro_roll50 + 100
euro_roll150

euro_roll50$rolls == euro_roll150$rolls[c(1:50)]

## ------------------------------------------------------------------------
euro_roll50
euro_roll50[1]
euro_roll50[1] <- "continent"
euro_roll50[1]
euro_roll50 # Notice the different initial value

## ------------------------------------------------------------------------
die_roll100_sum <- summary(die_roll100)
names(die_roll100_sum)
class(die_roll100_sum)
names(die_roll100_sum$freqs)
die_roll100_sum$freqs$side
die_roll100_sum$freqs$count
die_roll100_sum$freqs$prop

## ------------------------------------------------------------------------
plot(die_roll100)

