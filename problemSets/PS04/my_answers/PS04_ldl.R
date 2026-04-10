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

lapply(c("eha", "nnet", "MASS", "dplyr", "survival", "survminer", "stargazer", "sampleSelection"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")
summary(child)
str(child)
table(child$sex)

# response var construction vars (time, time2, event) -> (enter, exit, event)
# m.age = mother age
# sex = infant gender


# base model - build cox regression

child_surv <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
summary(child_surv)

#check model fit
drop1(child_surv, test = "Chisq")

#hazard rate percentage for girls
1 - 0.921074


# print to stargazer
stargazer(child_surv,
          type = "latex",
          title = "Hazard Rate Of Child Mortality: Mother's Age and Sex of Child",
          dep.var.labels=c("Hazard rate of child mortatlity"),
          covariate.labels=c("Mother's Age", "Sex of Child"))

#####################
# Problem 2
#####################

# load data
disaster_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")
str(disaster_data)
names(disaster_data)
summary(disaster_data)

# outcome variables
# selection equation = binContribution - binary, is a contribution made?
# outcome equation =  = originalContributionMillionUSDLogged - amount of relief that is provided

# Input variables #
# occurrences
# deathsEM
# normalizedDamageEMLogged

# ensure outcome variables are appropriately coded

unique(disaster_data$binContribution)
# outcome 1 is suitable for model

sum(is.na(disaster_data$originalContributionMillionUSDLogged))
# outcome 2 needs to have contribution's of 0 recoded to NA,
# Heckmann model needs this to allow it to interpret values as censored.
# outcome model is conditional on selection model - the value is 0 because they recieved no aid - but we want to estimate what it may have been.

disaster_data <- disaster_data %>%
  mutate(originalContributionMillionUSDLogged = ifelse(binContribution == 0, NA, originalContributionMillionUSDLogged))

# check again for NAs and that data is largely in order
sum(is.na(disaster_data$originalContributionMillionUSDLogged))

unique(disaster_data$originalContributionMillionUSDLogged)

# run heckmann model
heckmann <- heckit(selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
       outcome = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
       data = disaster_data)
summary(heckmann)

#create stargazer table for probit
stargazer(heckmann,
          type = "latex",
          selection.equation = TRUE,
          column.labels = c("Disaster Relief Provision"),
          title = "Selection Model")

stargazer(heckmann,
          type = "latex",
          selection.equation = FALSE,
          column.labels = c("Logged Contribution (in Million USD)"),
          title = "Outcome Model")
