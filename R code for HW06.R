# C2
# WAGE1

load("~/Desktop/R data sets for 5e/wage1.RData")
attach(data)
C2 <- lm(lwage ~ educ + exper + expersq)
summary(C2)

# Call:
#   lm(formula = lwage ~ educ + exper + expersq)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.96387 -0.29375 -0.04009  0.29497  1.30216 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.1279975  0.1059323   1.208    0.227    
# educ         0.0903658  0.0074680  12.100  < 2e-16 ***
#   exper        0.0410089  0.0051965   7.892 1.77e-14 ***
#   expersq     -0.0007136  0.0001158  -6.164 1.42e-09 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4459 on 522 degrees of freedom
# Multiple R-squared:  0.3003,	Adjusted R-squared:  0.2963 
# F-statistic: 74.67 on 3 and 522 DF,  p-value: < 2.2e-16

# 1.    Find the range exper ≥ 28.7276
sum(exper >28.7276)
# [1] 121

# alternative
data1 = subset(data, exper > 28.7276)
nrow(data1)
# [1] 121

# C3
# WAGE2

load("~/Desktop/R data sets for 5e/wage2.RData")
attach(data)
C3 <- lm(lwage ~ educ + exper + educ*exper)
summary(C3)

# Call:
#   lm(formula = lwage ~ educ + exper + educ * exper)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.88558 -0.24553  0.03558  0.26171  1.28836 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.949455   0.240826  24.704   <2e-16 ***
#   educ         0.044050   0.017391   2.533   0.0115 *  
#   exper       -0.021496   0.019978  -1.076   0.2822    
# educ:exper   0.003203   0.001529   2.095   0.0365 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3923 on 931 degrees of freedom
# Multiple R-squared:  0.1349,	Adjusted R-squared:  0.1321 
# F-statistic: 48.41 on 3 and 931 DF,  p-value: < 2.2e-16

# lwage = ß0 + θ educ + ß2 exper + ß3 (educ*exper- 10*educ) +u
cross <- educ*(exper-10)
C32 <- lm(lwage ~ educ + exper + cross)
summary(C32)

# Call:
#   lm(formula = lwage ~ educ + exper + cross)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.88558 -0.24553  0.03558  0.26171  1.28836 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.949455   0.240826  24.704   <2e-16 ***
#   educ         0.076080   0.006615  11.501   <2e-16 ***
#   exper       -0.021496   0.019978  -1.076   0.2822    
# cross        0.003203   0.001529   2.095   0.0365 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3923 on 931 degrees of freedom
# Multiple R-squared:  0.1349,	Adjusted R-squared:  0.1321 
# F-statistic: 48.41 on 3 and 931 DF,  p-value: < 2.2e-16

confint(C32, "educ", level = .95)
  #           2.5 %     97.5 %
  # educ 0.06309735 0.08906171

# C4

load("~/Desktop/R data sets for 5e/gpa2.RData")
attach(data)
C4 <- lm(sat ~ hsize + hsizesq)
summary(C4)

# Call:
#   lm(formula = sat ~ hsize + hsizesq)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -562.38  -93.07   -3.71   90.62  507.72 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  997.981      6.203 160.875  < 2e-16 ***
#   hsize         19.814      3.991   4.965 7.14e-07 ***
#   hsizesq       -2.131      0.549  -3.881 0.000106 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 138.9 on 4134 degrees of freedom
# Multiple R-squared:  0.00765,	Adjusted R-squared:  0.007169 
# F-statistic: 15.93 on 2 and 4134 DF,  p-value: 1.279e-07

C42 <- lm(log(sat) ~ hsize + hsizesq)
summary(C42)

# Call:
#   lm(formula = log(sat) ~ hsize + hsizesq)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.77744 -0.08493  0.00557  0.09465  0.40946
# 
# Coefficients:
#   Estimate Std. Error  t value
# (Intercept)  6.8960291  0.0061515 1121.032
# hsize        0.0196029  0.0039572    4.954
# hsizesq     -0.0020872  0.0005444   -3.834
# Pr(>|t|)
# (Intercept)  < 2e-16 ***
#   hsize       7.57e-07 ***
#   hsizesq     0.000128 ***
#   ---
#   Signif. codes:
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1377 on 4134 degrees of freedom
# Multiple R-squared:  0.007773,    Adjusted R-squared:  0.007293
# F-statistic: 16.19 on 2 and 4134 DF,  p-value: 9.885e-08

# C8

load("~/Desktop/R data sets for 5e/hprice1.RData")
attach(data)
C8 <- lm(price ~ lotsize + sqrft + bdrms)
summary(C8)

# Call:
#   lm(formula = price ~ lotsize + sqrft + bdrms)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.026  -38.530   -6.555   32.323  209.376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.177e+01  2.948e+01  -0.739  0.46221    
# lotsize      2.068e-03  6.421e-04   3.220  0.00182 ** 
#   sqrft        1.228e-01  1.324e-02   9.275 1.66e-14 ***
#   bdrms        1.385e+01  9.010e+00   1.537  0.12795    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 59.83 on 84 degrees of freedom
# Multiple R-squared:  0.6724,	Adjusted R-squared:  0.6607 
# F-statistic: 57.46 on 3 and 84 DF,  p-value: < 2.2e-16


a <- (lotsize-10000)
b <- (sqrft-2300)
c <- (bdrms-4)
C82 <- lm(price ~ a + b +c)
summary(C82)

# Call:
#   lm(formula = price ~ a + b + c)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.026  -38.530   -6.555   32.323  209.376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 3.367e+02  7.374e+00  45.658  < 2e-16 ***
#   a           2.068e-03  6.421e-04   3.220  0.00182 ** 
#   b           1.228e-01  1.324e-02   9.275 1.66e-14 ***
#   c           1.385e+01  9.010e+00   1.537  0.12795    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 59.83 on 84 degrees of freedom
# Multiple R-squared:  0.6724,	Adjusted R-squared:  0.6607 
# F-statistic: 57.46 on 3 and 84 DF,  p-value: < 2.2e-16

round(confint(C32,level = .95),4)
2.5 %       97.5 %
#               2.5 % 97.5 %
# (Intercept)  5.4768 6.4221
# educ         0.0631 0.0891
# exper       -0.0607 0.0177
# cross        0.0002 0.0062

round(qt(df=84,0.975),4)
# [1] 1.9886