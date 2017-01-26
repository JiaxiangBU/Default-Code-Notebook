# (i)
# sleep = ß0(1 + Dm) + ß1(1+Dm) totwrk + ß2(1+Dm) educ + ß3(1+Dm) age + ß4(1+Dm) age^2 + ß5(1+Dm) yngkid + u
rm(list=ls())
load("~/Desktop/R data sets for 5e/sleep75.RData")
attach(data)
mdata <- subset(data, male==1) # same as Xu’s solution
fdata <- subset(data,male==0) # same as Xu’s solution
nrow(mdata)
# [1] 400
nrow(fdata)
# [1] 306
C61m <- lm(sleep ~ totwrk  + educ + age + agesq + yngkid, mdata) #regression on male sample
C61f <- lm(sleep ~ totwrk  + educ + age + agesq + yngkid, fdata) #regression on female sample
library(stargazer)
stargazer(C61m,C61f,type = "text")

# ==================================================================
#   Dependent variable:              
#   ----------------------------------------------
#   sleep                     
# (1)                    (2)          
# ------------------------------------------------------------------
#   totwrk                     -0.182***              -0.140***       
#   (0.024)                (0.028)        
# 
# educ                       -13.052*                -10.205        
# (7.414)                (9.589)        
# 
# age                          7.157                 -30.357        
# (14.320)                (18.531)       
# 
# agesq                       -0.045                  0.368         
# (0.168)                (0.223)        
# 
# yngkid                      60.380                 -118.283       
# (59.023)                (93.188)       
# 
# Constant                 3,648.208***            4,238.729***     
#   (310.039)              (384.892)       
# 
# ------------------------------------------------------------------
#   Observations                  400                    306          
# R2                           0.156                  0.098         
# Adjusted R2                  0.146                  0.083         
# Residual Std. Error   402.290 (df = 394)      436.992 (df = 300)  
# F Statistic         14.590*** (df = 5; 394) 6.495*** (df = 5; 300)
# ==================================================================
#   Note:                                  *p<0.1; **p<0.05; ***p<0.01

# (ii) do a Chow test
names(data)
C61 <- lm(sleep ~ totwrk  + educ + age + agesq + yngkid) # do not count on gender on intercept & vars
C62 <- lm(sleep ~ male + male*(totwrk  + educ + age + agesq + yngkid))
summary(C62)
# Call:
#   lm(formula = sleep ~ male + male * (totwrk + educ + age + agesq + 
#                                         yngkid))
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2485.02  -226.74     7.93   257.77  1376.91 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4238.72933  367.85193  11.523  < 2e-16 ***
#   male        -590.52107  488.79159  -1.208   0.2274    
# totwrk        -0.13995    0.02643  -5.294 1.61e-07 ***
#   educ         -10.20514    9.16432  -1.114   0.2658    
# age          -30.35657   17.71049  -1.714   0.0870 .  
# agesq          0.36794    0.21345   1.724   0.0852 .  
# yngkid      -118.28256   89.06187  -1.328   0.1846    
# male:totwrk   -0.04217    0.03667  -1.150   0.2506    
# male:educ     -2.84724   11.96795  -0.238   0.8120    
# male:age      37.51316   23.12332   1.622   0.1052    
# male:agesq    -0.41271    0.27591  -1.496   0.1352    
# male:yngkid  178.66277  108.10510   1.653   0.0988 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 417.6 on 694 degrees of freedom
# Multiple R-squared:  0.1306,	Adjusted R-squared:  0.1168 
# F-statistic: 9.479 on 11 and 694 DF,  p-value: 4.947e-16
### idea from https://thetarzan.wordpress.com/2011/06/16/the-chow-test-in-r-a-case-study-of-yellowstones-old-faithful-geyser/
## Run three regressions (1 restricted, 2 unrestricted)

## Calculate sum of squared residuals for each regression
SSRr = resid(C61)^2
SSRurm = resid(C61m)^2
SSRurf = resid(C61f)^2

## K is the number of regressors in our model
K = 5+1

## Computing the Chow test statistic (F-test)
numerator = ( sum(SSRr) - (sum(SSRurm) + sum(SSRurf)) ) / K
denominator = (sum(SSRurm) + sum(SSRurf)) / (nrow(data) - 2*K)
chow = numerator / denominator
round(chow,4)
# [1] 2.1164

## Calculate P-value
round(1-pf(chow, K, (nrow(data) - 2*K)),4)
# [1] 0.0495

# use anova to double check
anova(C61,C62)
# Analysis of Variance Table
# 
# Model 1: sleep ~ totwrk + educ + age + agesq + yngkid
# Model 2: sleep ~ male + male * (totwrk + educ + age + agesq + yngkid)
# Res.Df       RSS Df Sum of Sq      F  Pr(>F)  
# 1    700 123267451                              
# 2    694 121052555  6   2214896 2.1164 0.04949 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (iii)
names(data)
C62 <- lm(sleep ~ male + totwrk + male*totwrk + educ + male*educ + age + male*age + agesq + male*agesq + yngkid + male*yngkid)
C63 <- lm(sleep ~ male + totwrk  + educ + age + agesq + yngkid)
anova(C63,C62)
# Analysis of Variance Table
# 
# Model 1: sleep ~ male + totwrk + educ + age + agesq + yngkid
# Model 2: sleep ~ male + totwrk + male * totwrk + educ + male * educ + 
#   age + male * age + agesq + male * agesq + yngkid + male * 
#   yngkid
# Res.Df       RSS Df Sum of Sq      F Pr(>F)
# 1    699 122147777                           
# 2    694 121052555  5   1095222 1.2558 0.2814

# (iv) no code
