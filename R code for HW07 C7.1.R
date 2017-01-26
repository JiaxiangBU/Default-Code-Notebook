rm(list=ls())
load("~/Desktop/R data sets for 5e/wage1.RData")
attach(data)

feduc12.5 <- female*(educ-12.5)
C72 <- lm(lwage ~ female + educ + feduc12.5 + exper + expersq+tenure + tenursq)
summary(C72)

# Call:
#   lm(formula = lwage ~ female + educ + feduc12.5 + exper + expersq +
#        tenure + tenursq)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max
# -1.83265 -0.25261 -0.02374  0.25396  1.13584
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.3888060  0.1186871   3.276  0.00112 **
#   female      -0.2963450  0.0358358  -8.270 1.14e-15 ***
#   educ         0.0823692  0.0084699   9.725  < 2e-16 ***
#   feduc12.5   -0.0055645  0.0130618  -0.426  0.67028
# exper        0.0293366  0.0049842   5.886 7.11e-09 ***
#   expersq     -0.0005804  0.0001075  -5.398 1.03e-07 ***
#   tenure       0.0318967  0.0068640   4.647 4.28e-06 ***
#   tenursq     -0.0005900  0.0002352  -2.509  0.01242 *
#   ---
#   Signif. codes:
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4001 on 518 degrees of freedom
# Multiple R-squared:  0.441,    Adjusted R-squared:  0.4334
# F-statistic: 58.37 on 7 and 518 DF,  p-value: < 2.2e-16

round(cor(female, female*educ),4)
[1] 0.9636
round(cor(female,feduc12.5),4)
[1] -0.0533
round(mean(educ),4)
[1] 12.5627