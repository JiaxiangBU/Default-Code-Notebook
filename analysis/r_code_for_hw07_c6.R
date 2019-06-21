




# i -----------------------------------------------------------------------


# sleep = beta_0(1 + Dm) + beta_1(1+Dm) totwrk + beta_2(1+Dm) educ + beta_3(1+Dm) age + beta_4(1+Dm) age^2 + beta_5(1+Dm) yngkid + u
data(sleep75)
mdata <- subset(data, male == 1) # same as Xu’s solution
fdata <- subset(data, male == 0) # same as Xu’s solution
nrow(mdata)
nrow(fdata)
C61m <-
    lm(sleep ~ totwrk  + educ + age + agesq + yngkid, mdata) #regression on male sample
C61f <-
    lm(sleep ~ totwrk  + educ + age + agesq + yngkid, fdata) #regression on female sample
library(stargazer)
stargazer(C61m, C61f, type = "text")


# (ii) do a Chow test -----------------------------------------------------


names(data)
C61 <-
    lm(sleep ~ totwrk  + educ + age + agesq + yngkid) # do not count on gender on intercept & vars
C62 <-
    lm(sleep ~ male + male * (totwrk  + educ + age + agesq + yngkid))
summary(C62)

### idea from https://thetarzan.wordpress.com/2011/06/16/the-chow-test-in-r-a-case-study-of-yellowstones-old-faithful-geyser/
## Run three regressions (1 restricted, 2 unrestricted)

## Calculate sum of squared residuals for each regression
SSRr = resid(C61) ^ 2
SSRurm = resid(C61m) ^ 2
SSRurf = resid(C61f) ^ 2

## K is the number of regressors in our model
K = 5 + 1

## Computing the Chow test statistic (F-test)
numerator = (sum(SSRr) - (sum(SSRurm) + sum(SSRurf))) / K
denominator = (sum(SSRurm) + sum(SSRurf)) / (nrow(data) - 2 * K)
chow = numerator / denominator
round(chow, 4)
# [1] 2.1164

## Calculate P-value
round(1 - pf(chow, K, (nrow(data) - 2 * K)), 4)
# [1] 0.0495

# use anova to double check
anova(C61, C62)

# (iii) -------------------------------------------------------------------


names(data)
C62 <-
    lm(
        sleep ~ male + totwrk + male * totwrk + educ + male * educ + age + male *
            age + agesq + male * agesq + yngkid + male * yngkid
    )
C63 <- lm(sleep ~ male + totwrk  + educ + age + agesq + yngkid)
anova(C63, C62)
