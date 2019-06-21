#C2
load("~/Desktop/R data sets for 5e/wage2.RData")
attach(data)
# (i)
C21 <- lm(lwage ~ educ + exper + tenure + married + black + south + urban)
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

# (ii)
expersd <- exper^2
tenuresd <- tenure^2
C22 <- lm(lwage ~ educ + exper + tenure + married + black + south + urban + I(exper^2) + I(tenure^2))
# summary(C22)
# Call:
#   lm(formula = lwage ~ educ + exper + tenure + married + black + 
#        south + urban + I(exper^2) + I(tenure^2))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.98236 -0.21972 -0.00036  0.24078  1.25127 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.3586757  0.1259143  42.558  < 2e-16 ***
#   educ         0.0642761  0.0063115  10.184  < 2e-16 ***
#   exper        0.0172146  0.0126138   1.365 0.172665    
# tenure       0.0249291  0.0081297   3.066 0.002229 ** 
#   married      0.1985470  0.0391103   5.077 4.65e-07 ***
#   black       -0.1906636  0.0377011  -5.057 5.13e-07 ***
#   south       -0.0912153  0.0262356  -3.477 0.000531 ***
#   urban        0.1854241  0.0269585   6.878 1.12e-11 ***
#   I(exper^2)  -0.0001138  0.0005319  -0.214 0.830622    
# I(tenure^2) -0.0007964  0.0004710  -1.691 0.091188 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3653 on 925 degrees of freedom
# Multiple R-squared:  0.255,	Adjusted R-squared:  0.2477 
# F-statistic: 35.17 on 9 and 925 DF,  p-value: < 2.2e-16

anova(C2,C22)

# (iii)
educblack <- educ*black
C23 <- lm(lwage ~ educ + exper + tenure + married + black + south + urban + educblack)
summary(C23)

# Call:
#   lm(formula = lwage ~ educ + exper + tenure + married + black + 
#        south + urban + educblack)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.97782 -0.21832  0.00475  0.24136  1.23226 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.374817   0.114703  46.859  < 2e-16 ***
#   educ         0.067115   0.006428  10.442  < 2e-16 ***
#   exper        0.013826   0.003191   4.333 1.63e-05 ***
#   tenure       0.011787   0.002453   4.805 1.80e-06 ***
#   married      0.198908   0.039047   5.094 4.25e-07 ***
#   black        0.094809   0.255399   0.371 0.710561    
# south       -0.089450   0.026277  -3.404 0.000692 ***
#   urban        0.183852   0.026955   6.821 1.63e-11 ***
#   educblack   -0.022624   0.020183  -1.121 0.262603    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3654 on 926 degrees of freedom
# Multiple R-squared:  0.2536,	Adjusted R-squared:  0.2471 
# F-statistic: 39.32 on 8 and 926 DF,  p-value: < 2.2e-16

# (iv)
# D1(married and black), D2(married and nonblack), D3(single and black) and D4(single and nonblack)
D1 <- married*black
D2 <- married*(1-black)
D3 <- (1-married)*black
D4 <- (1-married)*(1-black)
data$type <- with(data, factor(type, levels=c("D2", "D3", "D4"))) #dummy vars I define
C24 <- lm(lwage ~ educ + exper + tenure + south + urban + type)


C24 <- lm(lwage ~ educ + exper + tenure + south + urban + D2 + D3 + D4)
summary(C24)

# Call:
#   lm(formula = lwage ~ educ + exper + tenure + south + urban + 
#        D2 + D3 + D4)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.98013 -0.21780  0.01057  0.24219  1.22889 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  5.413242   0.110277  49.088  < 2e-16 ***
#   educ         0.065475   0.006253  10.471  < 2e-16 ***
#   exper        0.014146   0.003191   4.433 1.04e-05 ***
#   tenure       0.011663   0.002458   4.745 2.41e-06 ***
#   south       -0.091989   0.026321  -3.495 0.000497 ***
#   urban        0.184350   0.026978   6.833 1.50e-11 ***
#   D2           0.179466   0.040539   4.427 1.07e-05 ***
#   D3          -0.250268   0.094089  -2.660 0.007951 ** 
#   D4          -0.009448   0.056013  -0.169 0.866083    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3656 on 926 degrees of freedom
# Multiple R-squared:  0.2528,	Adjusted R-squared:  0.2464 
# F-statistic: 39.17 on 8 and 926 DF,  p-value: < 2.2e-16
