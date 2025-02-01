# Life_Expectancy
Income, Life Expectancy, and Health Expenditure (2001-2019)

library(rigr)
library(lattice)
library(vtable)
rm(list = ls())
## Set working directory
setwd("~/Desktop/BIOST 512/")
## Read data (that is in our working directory)
dat <- read.csv("BIOST512-subset-LifeExpectancy.csv")
# Visualize
boxplot(LifeExpectancy2019 ~ IncomeGroup, data = dat,
xlab = "Income Group", ylab = "Life Expectancy 2019",
main = "Boxplot of Life Expectancy by Income Group")
xyplot(LifeExpectancy2019 ~ HealthExpenditure2019, data = dat,
xlab = "Expenditures", ylab = "Life Expectancy 2019",
type = c("p", "r"),
auto.key = TRUE,
par.settings = list(superpose.symbol = list(pch = 16)))

xyplot(LifeExpectancy2019 ~ HealthExpenditure2019, data = dat,
groups = IncomeGroup,
xlab = "Expenditures", ylab = "Life Expectancy 2019",
type = c("p", "r"),
auto.key = TRUE,
par.settings = list(superpose.symbol = list(pch = 16)))
sumtable(dat)
## Question 1: Is life expectancy in year 2019 associated with countries’ income groups?
## Is this just simple ANOVA?
model <- regress("mean", LifeExpectancy2019 ~ IncomeGroup, data=dat)
model
## Question 2: Adjusting for expenditures on health, is life expectancy in year 2019 associated with
countries’ income groups?
## Primary so is this ANCOVA!!! Associated means --> confounding
model <- regress("mean", LifeExpectancy2019 ~ IncomeGroup+HealthExpenditure2019, data=dat)
model

## Question 3: Adjusting for expenditures on health, does the association between life expectancy in year
2019 and countries’ income
## groups depend (interaction) on the life expectancy in year 2001? Carefully interpret the interaction.
model <- regress("mean", LifeExpectancy2019 ~
IncomeGroup+HealthExpenditure2019+LifeExpectancy2001+IncomeGroup:LifeExpectancy2001,
data=dat)
model
