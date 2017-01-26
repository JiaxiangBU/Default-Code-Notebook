# Description
# Wooldridge, J. M. (2012). Introductory econometrics: A modern approach
# This is my 1st textbook I learn in econometries, which is useful. 
# I will share my code for the questions for some chapters.
# This is for EGLS model, at the p378, on the Ch.8.

# Check Heteroscedasticity.
load("~/Desktop/R data sets for 5e/smoke.RData") # You can download the dataset online.
attach(data)
fit <-lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn)
res <- resid(fit)
res.1 <- lm(log(res^2) ~ log(income) + log(cigpric) + educ + age + agesq + restaurn)

# find LM stat (like F test)
library(sandwich)
library(lmtest)
bptest(fit) # Check LM stat

# EGLS way
g <- fitted(res.1)
h <- exp(g)

fit2 <-lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn, weights = 1/h)
library(stargazer)
stargazer(fit,res.1,fit2, # This is a well-viewed code for Word, install library(stargazer).
          type = "text",out="models.doc")
