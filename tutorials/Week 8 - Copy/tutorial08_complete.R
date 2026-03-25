##############################################
# Tutorial 5 
# Ordered and Multinomial Logistic Regression
##############################################

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

lapply(c("MASS", "nnet", "ggplot2"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)
# Outcome: attitude toward working mothers

# check basic cross-tabs
summary(workingMoms)
ftable(xtabs(~ gender + year + attitude, data = workingMoms))

# do some wrangling
workingMoms$attitude <- factor(workingMoms$attitude, 
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree",
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0,1),
                           labels = c("Non-white", "White"))
workingMoms$year <- factor(workingMoms$year,
                           levels = c("Year1977", "Year1989"),
                           labels = c("1977", "1989"))

ftable(xtabs(~ gender + year + attitude, data = workingMoms))
prop.table(table(workingMoms$gender, workingMoms$attitude), 1)
prop.table(table(workingMoms$gender[workingMoms$year=="1977"],
                 workingMoms$attitude[workingMoms$year=="1977"]), 1)

prop.table(table(workingMoms$gender[workingMoms$year=="1989"],
                 workingMoms$attitude[workingMoms$year=="1989"]), 1)

ggplot(workingMoms, aes(attitude, prestige)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3) +
  scale_x_discrete(labels=function(x){sub("\\s", "\n", x)}) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_grid(gender ~ year)

# a) Perform an ordered (proportional odds) logistic regression

ord.log <- polr(attitude ~ ., data = workingMoms, Hess = TRUE)
summary(ord.log)

# Calculate a p value
ctable <- coef(summary(ord.log))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# Calculate confidence intervals
(ci <- confint(ord.log))

# convert to odds ratio
exp(cbind(OR = coef(ord.log), ci))

# How do we interpret these coefficients?
## A one-unit increase in X changes the log-odds of being in a higher attitude category by𝛽.
## The same β applies to every threshold.

## Age: Holding other variables constant, on average, for each additional year of age, the odds of being in a more supportive category decrease by about 2.1%.
##So, Older people are less supportive of working mothers.

## Education: Each additional year of education increases the odds of being in a more supportive attitude category by about 6.9%.
##More educated people are more supportive.

## prestige: Higher occupational prestige slightly increases the odds of being more supportive.
##However, not statistically significant at the 5% level

## Gender: Men have 52% lower odds of being in a more supportive category compared with women.
##Men are less supportive of working mothers.

## year1989: People in 1989 had about 69% higher odds of being in a more supportive category compared with 1977.
##Support for working mothers increased over time.

## racewhite: White respondents have 32% lower odds of being in a more supportive category compared with non-white respondents.



# education range
x <- seq(min(workingMoms$education), max(workingMoms$education), length=100)

# coefficients
beta <- coef(ord.log)["education"]

# thresholds
u1 <- ord.log$zeta[1]
u2 <- ord.log$zeta[2]
u3 <- ord.log$zeta[3]

# cumulative logits
logit1 <- u1 - beta * x
logit2 <- u2 - beta * x
logit3 <- u3 - beta * x

# plot
plot(x, logit1, type="l", lwd=2, ylim=c(min(logit1), max(logit3)),
     ylab="Log-odds", xlab="Education")
lines(x, logit2, col="blue", lwd=2)
lines(x, logit3, col="red", lwd=2)



# b) fit a multinomial logit model
# set a reference level for the outcome
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")
##Compare every category to Strongly Disagree.

# run model
mult.log <- multinom(attitude ~ ., data = workingMoms)
summary(mult.log)
##So each predictor has different coefficients for each category.
##This is the main difference from ordered logit.

## Interpretation:
##Agree vs Strongly Disagree (age = -0.025): A one-year increase in age decreases the log-odds of agreeing (vs strongly disagreeing) by 0.025.

exp(coef(mult.log))
# get p values
z <- summary(mult.log)$coefficients/summary(mult.log)$standard.errors
(p <- (1 - pnorm(abs(z), 0, 1)) * 2)

# how do we interpret these coefficients?

## Interpretation:
##Agree vs Strongly Disagree (age = 0.975): Each additional year of age reduces the odds of Agree vs Strongly Disagree by about 2.5%

##More education increases the odds of being in any more supportive category relative to strongly disagreeing.
##Each additional year of education increases the odds of strongly agreeing vs strongly disagreeing by about 15%.

##Compared with women, men are slightly more likely to say Disagree than Strongly Disagree
##but much less likely to Agree
##and far less likely to Strongly Agree
##Men have about 71% lower odds of strongly agreeing (vs strongly disagreeing) compared with women.

##In 1989, the odds of strongly agreeing (vs strongly disagreeing) were about 3.2 times higher.

##White respondents are less likely to express supportive attitudes relative to strongly disagreeing.


# we can use predicted probabilities to help our interpretation
pp <- data.frame(fitted(mult.log))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp$Strongly.Disagree,
                D = pp$Disagree,
                A = pp$Agree,
                SA = pp$Strongly.Agree))

##Multinomial logit coefficients are log-odds, which are difficult to interpret.
##Predicted probabilities translate the model into something intuitive:
##“Given these characteristics, what is the probability of each attitude?”
##For Example: Instead of saying coefficient = 0.45
##You can say: The model predicts a 45% probability of agreeing.

# c) Consider gender as an interaction
mult.log.int <- multinom(attitude ~ gender * ., data = workingMoms)
#Assumption: The effect of every variable on attitude can differ for men and women.
summary(mult.log.int)

z.int <- summary(mult.log.int)$coefficients/summary(mult.log.int)$standard.errors
(p.int <- (1 - pnorm(abs(z.int), 0, 1)) * 2)

#Interpretations: Look at the Agree equation: age = -0.011; genderMale:age = -0.0265
#For women: Effect of age: -0.011
#Older women are slightly less likely to Agree vs Strongly Disagree.
#For men: Effect of age: -0.011 + (-0.0265) = -0.0375
#So: Age reduces the probability of agreeing more strongly for men than for women.

#From Strongly Agree equation: education = 0.2216; genderMale:education = -0.1437
#For women: Effect of education: 0.2216
#Education strongly increases the chance of Strongly Agree vs Strongly Disagree.
#For men: Effect: 0.2216 − 0.1437 = 0.0779
#So: Education increases support much less for men than for women.

pp.int <- data.frame(fitted(mult.log.int))
head(data.frame(attitude = workingMoms$attitude,
                SD = pp.int$Strongly.Disagree,
                D = pp.int$Disagree,
                A = pp.int$Agree,
                SA = pp.int$Strongly.Agree))

#anova(mult.log, mult.log.int, test = "Chisq")
