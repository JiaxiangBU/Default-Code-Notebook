# regress res.1
load("~/Desktop/R data sets for 5e/smoke.RData")
attach(data)
fit <-lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn)
res <- resid(fit)
res.1 <- lm(log(res^2) ~ log(income) + log(cigpric) + educ + age + agesq + restaurn)


# find LM stat
library(sandwich)
library(lmtest)
bptest(fit) # check LM stat

g <- fitted(res.1)
h <- exp(g)

fit2 <-lm(cigs ~ log(income) + log(cigpric) + educ + age + agesq + restaurn, weights = 1/h)
library(stargazer)
stargazer(fit,res.1,fit2,
          type = "text",out="models.doc")

### comments: I run the right answer, haha!
