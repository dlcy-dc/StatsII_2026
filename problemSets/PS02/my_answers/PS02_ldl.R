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

lapply(c("stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_2026/blob/main/datasets/climateSupport.RData?raw=true"))

# check data structure.
str(climateSupport)
table(climateSupport$choice)
table(climateSupport$countries)
table(climateSupport$sanctions)

# what are the factor levels? 
levels(climateSupport$countries)
levels(climateSupport$sanctions)
# ordered factors produce polynomial outputs, which is not what the question is looking for.
# hence unorder the factors
climateSupport$countries <- factor(climateSupport$countries, ordered = FALSE)
climateSupport$sanctions <- factor(climateSupport$sanctions, ordered = FALSE)

# check levels again
levels(climateSupport$countries)
levels(climateSupport$sanctions)

# additive model - based off binary DV fit a logit model
model1 <- glm(choice ~ countries + sanctions,
              data = climateSupport,
              family = binomial)
# summarise model
summary(model1)
# run stargazer
stargazer(model1,
          type = "latex",
          report = "vc*p",
          title="Sanctions and country participation on support for climate initiatives",
          align=TRUE,
          intercept.top = TRUE,
          intercept.bottom = FALSE)

# global null creation and LRT test
model0 <- glm(choice ~ 1,
              data = climateSupport,
              family = binomial)

anova(model0, model1, test = "LRT")

#checking probabilities from log odds

mcoef <- summary(model1)$coefficients
exp(mcoef)


#####################
# Problem 2
#####################

# a and b are the same because sanctions 5% and sanctions 15% hold countries constant.

summary(model1)
# 15% is -0.13325, 5% is 0.19186 - simple maths finds the difference in log odds
-0.13325 - 0.19186
# answer is a log odds of -0.32511
# finding the odds ratio
exp(-0.32511)
# odds ratio is 0.7224479

# c is more complex and requires using the predict function to 

# for log odds
predict(model1,
        newdata = data.frame(countries="80 of 192", sanctions="None"),
        type="link")
# answer = 0.06369783 change in log odds

# for probability
predict(model1,
        newdata = data.frame(countries="80 of 192", sanctions="None"),
        type="response")
# answer = 0.5159191 or a 51.59% probability of support.


#####################
# Problem 3
#####################

# model with interaction term 
model2 <- glm(choice ~ countries*sanctions,
              data = climateSupport,
              family = binomial)
# summarise model
summary(model2)

# run stargazer
stargazer(model1, model2,
          type = "latex",
          title="Sanctions and country participation on support for climate initiatives: models compared",
          align=TRUE,
          no.space = TRUE,
          intercept.top = TRUE,
          intercept.bottom = FALSE)

#LRT for both models - does model2 improve fit.
anova(model1, model2, test = "LRT")
