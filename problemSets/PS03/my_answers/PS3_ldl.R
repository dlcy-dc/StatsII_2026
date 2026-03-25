#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "dplyr", "ggplot2", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)
str(gdp_data)

# 3 variables needed
# GDPWdiff = Difference in GDP between year t and t−1. Possible categories include:”positive”, ”negative”, or ”no change”
# REG: 1=Democracy; 0=Non-Democracy
# OIL: 1=if the average ratio of fuel exports to total exports in 1984-86 exceeded 50%; 0= otherwise

#restructure dataset for coding
gdp_data <- gdp_data %>%
  select(CTYNAME, YEAR, GDPWdiff, REG, OIL) %>%
  mutate(GDPWdiff = case_when(
      GDPWdiff > 0  ~ "positive",
      GDPWdiff < 0  ~ "negative",
      GDPWdiff == 0 ~ "no change"),
    GDPWdiff = factor(GDPWdiff, levels = c("no change", "negative", "positive")),
    REG = factor(REG, levels = c(0, 1), labels = c("Non-Democracy", "Democracy")),
    OIL = factor(OIL, levels = c(0, 1), labels = c("No", "Yes")))

str(gdp_data)

# with data recoded I can move to part 1

########
 # Part 1
########

# no change is already set as the reference category so no further recoding is required.

#run model
model1_unordered <- multinom(GDPWdiff ~ REG + OIL, data = gdp_data)
summary(model1_unordered)

#get p values
z <- summary(model1_unordered)$coefficients/summary(model1_unordered)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# format output for stargazer

stargazer(model1_unordered,
          column.labels = c("Negative", "Positive"),
          covariate.labels = c("Democracy", "Oil Exporter"),
          dep.var.labels = "GDP Change (Ref: No Change)",
          title = "Unordered Multinomial Logit: GDP Change",
          type = "latex")

########
# Part 2
########

# create ordered GDPwdiff appropriate for the ordered multinomial logit.

gdp_data <- gdp_data %>%
  mutate(GDPWdiffo = factor(GDPWdiff, 
                           levels = c("negative", "no change", "positive"), 
                           ordered = TRUE))

# run ordered model

model1_ordered <- polr(GDPWdiffo ~ REG + OIL, data = gdp_data, Hess = TRUE)
summary(model1_ordered)

# extract p-values
z2 <- summary(model1_ordered)$coefficients[,3]
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2
p2

# format output for stargazer

stargazer(model1_ordered,
          covariate.labels = c("Democracy", "Oil Exporter"),
          dep.var.labels = "GDP Change",
          title = "Ordered Multinomial Logit: GDP Change",
          type = "latex")

#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")
str(mexico_elections)

#check data
table(mexico_elections$PAN.governor.06)
table(mexico_elections$competitive.district)

# adjusting dummies for clarity
mexico_elections <- mexico_elections %>%
  mutate(
    competitive.district = factor(competitive.district, 
                                  levels = c(0, 1), 
                                  labels = c("Safe Seat", "Swing District")),
    PAN.governor.06 = factor(PAN.governor.06, 
                             levels = c(0, 1), 
                             labels = c(" Non-PAN Affiliated", "PAN Affiliated")))

####
# Part 1
####

#run poisson
model2_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06,
                      data = mexico_elections,
                      family = poisson)

summary(model2_poisson)

# stargazer for interpreting in paper
stargazer(model2_poisson,
          covariate.labels = c("Swing District", "Marginality", "PAN Governor"),
          dep.var.labels = "PAN Visits (2006)",
          title = "Poisson Regression: PAN Presidential Candidate Visits",
          type = "latex")

####
# Part 2
####

####
# Part 3
####
