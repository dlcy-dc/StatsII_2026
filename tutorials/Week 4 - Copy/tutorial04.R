##################
#### Stats II ####
##################

###############################
#### Tutorial 4: Logit ####
###############################

# In today's tutorial, we'll begin to explore logit regressions
#     1. Estimate logit regression in R using glm()
#     2. Practice makes inferences using logit regression
#     3. Compare logit models

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

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

## Binary logits:

# Employing a sample of 1643 men between the ages of 20 and 24 from the U.S. National Longitudinal Survey of Youth.
# Powers and Xie (2000) investigate the relationship between high-school graduation and parents' education, race, family income, 
# number of siblings, family structure, and a test of academic ability. 

#The dataset contains the following variables:
# hsgrad Whether: the respondent was graduated from high school by 1985 (Yes or No)
# nonwhite: Whether the respondent is black or Hispanic (Yes or No)
# mhs: Whether the respondent’s mother is a high-school graduate (Yes or No)
# fhs: Whether the respondent’s father is a high-school graduate (Yes or No)
# income: Family income in 1979 (in $1000s) adjusted for family size
# asvab: Standardized score on the Armed Services Vocational Aptitude Battery test 
# nsibs: Number of siblings
# intact: Whether the respondent lived with both biological parents at age 14 (Yes or No)

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt")

str(graduation)

graduation$hsgrad <- as.factor(graduation$hsgrad)
graduation$nonwhite <- as.factor(graduation$nonwhite)
graduation$mhs <- as.factor(graduation$mhs)
graduation$fhs <- as.factor(graduation$fhs)
graduation$intact <- as.factor(graduation$intact)


yn_vars <- c("hsgrad", "nonwhite", "mhs", "fhs", "intact")
graduation[yn_vars] <- lapply(graduation[yn_vars], factor)




# (a) Perform a logistic regression of hsgrad on the other variables in the data set.
# Compute a likelihood-ratio test of the omnibus null hypothesis that none of the explanatory variables influences high-school graduation. 
# Then construct 95-percent confidence intervals for the coefficients of the seven explanatory variables. 
# What conclusions can you draw from these results? Finally, offer two brief, but concrete, interpretations of each of the estimated coefficients of income and intact.

## full model
full <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact,
            data = graduation,
            family = binomial(link = "logit")
            )
full
summary(full)

# Question - how does coefficient translate into english here?

#when all vars = 0 - log odds of graduating from highschool = intercept
# assuming keeping other covariates constant - nonwhite higher logg odds of .8 of graduating

## Null model
Null <- glm(hsgrad ~ 1,
            data = graduation,
            family = binomial(link = "logit")
)
Null

## Likelihood ratio test
anova(Null, full, test = "LRT")

# p value is whether LRT is satisfied or not - here it is less, so at least one of the variables explains variation = reationship between predictor and outcome variables

### confidence interval

confint(full)

# whether 0 in range of conf interval -> no statistically significant relationship - cannot reject the null hypothesis

### conclusions
exp(0.05309) -> # 1.054525 -> 5%
exp(0.71911) -> #doubles

# income is integer var - 1 unit increas in family income is associated with 0.05 logg odds increas in respondent getting highschool grad holding others constant.
  

# (b) The logistic regression in the previous problem assumes that the partial relationship between the log-odds of high-school graduation and number of siblings is linear. 
# Test for nonlinearity by fitting a model that treats nsibs as a factor, performing an appropriate likelihood-ratio test. 
# In the course of working this problem, you should discover an issue in the data. 
# Deal with the issue in a reasonable manner. 
# Does the result of the test change?
  
# effect of logg odds on y

graduation$nsibs_f <- factor(graduation$nsibs)

  full2 <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_f + intact,
              data = graduation,
              family = binomial(link = "logit")
  )
summary(full2) 
anova(full, full2, test = "LRT")
anova(full2, full, test = "LRT")

#linearity assumption of nsibs did not improve by turning to factor
# high standard errors - coefficient is not estimated properly - typically number of cases

graduation_clean <- subset(graduation, nsibs >= 0) # removing 3
graduation_clean$nsibs_cat <- cut(
  graduation_clean$nsibs,
  breaks = c(-1, 1, 3, 5, 10, 20),
  labels = c("0-1", "2-3", "4-5", "6-10", "11+")
)
str(graduation_clean)

table(graduation_clean$nsibs_cat, graduation_clean$nsibs)

full3 <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs_cat + intact,
             data = graduation_clean,
             family = binomial
)
summary(full3)

full_new <- glm(hsgrad ~ nonwhite + mhs + fhs + income + asvab + nsibs + intact,
             data = graduation_clean,
             family = binomial
)
summary(full)

anova(full_new, full3, test = "LRT")
# linear relationship holds true - its just fine as it is
