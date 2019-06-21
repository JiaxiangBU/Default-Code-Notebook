# C2
load("~/Desktop/R data sets for 5e/lawsch85.RData")
attach(data)
# rm(list=ls())
head(data)


# Solve “NA”
myvars=c("lsalary","LSAT","GPA","llibvol","lcost","rank")
newdata=data[myvars]
newdata1=na.omit(newdata)
nrow(newdata1)
# [1] 136

fit <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank, newdata1)
summary(fit)
# Call:
#   lm(formula = lsalary ~ LSAT + GPA + llibvol + lcost + rank, data = newdata1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.301356 -0.084982 -0.004359  0.077935  0.288614 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.3432260  0.5325192  15.667  < 2e-16 ***
#   LSAT         0.0046965  0.0040105   1.171  0.24372    
# GPA          0.2475239  0.0900370   2.749  0.00683 ** 
#   llibvol      0.0949932  0.0332543   2.857  0.00499 ** 
#   lcost        0.0375538  0.0321061   1.170  0.24427    
# rank        -0.0033246  0.0003485  -9.541  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1124 on 130 degrees of freedom
# Multiple R-squared:  0.8417,	Adjusted R-squared:  0.8356 
# F-statistic: 138.2 on 5 and 130 DF,  p-value: < 2.2e-16


# resfit <- lm(lsalary ~ llibvol + lcost + rank, newdata1)
# summary(resfit)
# 
# Call:
#   lm(formula = lsalary ~ llibvol + lcost + rank, data = newdata1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max
# -0.37741 -0.09032 -0.01234  0.08746  0.30128
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  9.903840   0.359125  27.578  < 2e-16
# llibvol      0.129905   0.034124   3.807 0.000215
# lcost        0.023708   0.030472   0.778 0.437958
# rank        -0.004178   0.000310 -13.478  < 2e-16
# 
# (Intercept) ***Call:
#   lm(formula = lsalary ~ llibvol + lcost + rank, data = newdata1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.37741 -0.09032 -0.01234  0.08746  0.30128 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.903840   0.359125  27.578  < 2e-16 ***
#   llibvol      0.129905   0.034124   3.807 0.000215 ***
#   lcost        0.023708   0.030472   0.778 0.437958    
# rank        -0.004178   0.000310 -13.478  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1198 on 132 degrees of freedom
# Multiple R-squared:  0.8174,	Adjusted R-squared:  0.8133 
# F-statistic:   197 on 3 and 132 DF,  p-value: < 2.2e-16

anova(fit,resfit)
# AnalysisAnalysis of Variance Table
# 
# Model 1: lsalary ~ LSAT + GPA + llibvol + lcost + rank
# Model 2: lsalary ~ llibvol + lcost + rank
# Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1    130 1.6427                                  
# 2    132 1.8942 -2  -0.25151 9.9518 9.518e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Solve “NA”
myvars2=c("lsalary","LSAT","GPA","llibvol","lcost","rank","clsize","faculty")
newdata2=data[myvars2]
newdata3=na.omit(newdata2)
nrow(newdata3)
# [1] 131

fit2 <- lm(lsalary ~ LSAT + GPA + llibvol + lcost + rank+clsize+faculty, newdata3)
summary(fit2)

# Call:
#   lm(formula = lsalary ~ LSAT + GPA + llibvol + lcost + rank + 
#        clsize + faculty, data = newdata3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.299686 -0.082282 -0.009353  0.078702  0.269288 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.416e+00  5.523e-01  15.239  < 2e-16 ***
#   LSAT         5.582e-03  4.180e-03   1.336  0.18416    
# GPA          2.661e-01  9.325e-02   2.853  0.00508 ** 
#   llibvol      5.516e-02  4.040e-02   1.365  0.17466    
# lcost        2.967e-02  3.468e-02   0.856  0.39393    
# rank        -3.428e-03  3.573e-04  -9.594  < 2e-16 ***
#   clsize       1.342e-04  1.535e-04   0.874  0.38379    
# faculty      6.748e-05  3.999e-04   0.169  0.86629    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1131 on 123 degrees of freedom
# Multiple R-squared:  0.844,	Adjusted R-squared:  0.8351 
# F-statistic: 95.05 on 7 and 123 DF,  p-value: < 2.2e-16

resfit2 <- lm(lsalary ~ LSAT+GPA+llibvol + lcost + rank, newdata3)
summary(resfit2)

# Call:
#   lm(formula = lsalary ~ LSAT + GPA + llibvol + lcost + rank, data = newdata3)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.304535 -0.085977  0.000037  0.075671  0.282873 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  8.3504583  0.5391245  15.489  < 2e-16 ***
#   LSAT         0.0047926  0.0040779   1.175  0.24212    
# GPA          0.2424009  0.0915964   2.646  0.00918 ** 
#   llibvol      0.0836992  0.0344590   2.429  0.01657 *  
#   lcost        0.0446594  0.0328899   1.358  0.17696    
# rank        -0.0034108  0.0003569  -9.556  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.113 on 125 degrees of freedom
# Multiple R-squared:  0.8416,	Adjusted R-squared:  0.8352 
# F-statistic: 132.8 on 5 and 125 DF,  p-value: < 2.2e-16

anova(fit2,resfit2)
# Analysis of Variance Table
# 
# Model 1: lsalary ~ LSAT + GPA + llibvol + lcost + rank + clsize + faculty
# Model 2: lsalary ~ LSAT + GPA + llibvol + lcost + rank
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    123 1.5732
# 2    125 1.5974 -2 -0.024259 0.9484 0.3902

# C9
rm(list=ls())
load("~/Desktop/R data sets for 5e/discrim.RData")
attach(data)

# Solve “NA”
C9vars=c("lpsoda","prpblck","lincome","prppov")
newdataQ9=data[C9vars]
newdataQ92=na.omit(newdataQ9)
nrow(newdataQ92)
# [1] 401

Q9 <- lm(lpsoda ~ prpblck + lincome + prppov, newdataQ92)
summary(Q9)

# Call:
#   lm(formula = lpsoda ~ prpblck + lincome + prppov, data = newdataQ92)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.32218 -0.04648  0.00651  0.04272  0.35622 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.46333    0.29371  -4.982  9.4e-07 ***
#   prpblck      0.07281    0.03068   2.373   0.0181 *  
#   lincome      0.13696    0.02676   5.119  4.8e-07 ***
#   prppov       0.38036    0.13279   2.864   0.0044 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.08137 on 397 degrees of freedom
# Multiple R-squared:  0.08696,	Adjusted R-squared:  0.08006 
# F-statistic:  12.6 on 3 and 397 DF,  p-value: 6.917e-08

attach(newdataQ92)

cor(lincome,prppov)
# [1] -0.8402069

# Solve “NA”
attach(data)
C9vars=c("lpsoda","prpblck","lincome","prppov","lhseval")
newdataQ922=data[C9vars]
newdataQ923=na.omit(newdataQ922)
nrow(newdataQ923)
# [1] 401
Q92 <- lm(lpsoda ~ prpblck + lincome + prppov + lhseval, newdataQ923)
summary(Q92)

# Call:
#   lm(formula = lpsoda ~ prpblck + lincome + prppov + lhseval, data = newdataQ923)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.30652 -0.04380  0.00701  0.04332  0.35272 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.84151    0.29243  -2.878 0.004224 ** 
#   prpblck      0.09755    0.02926   3.334 0.000937 ***
#   lincome     -0.05299    0.03753  -1.412 0.158707    
# prppov       0.05212    0.13450   0.388 0.698570    
# lhseval      0.12131    0.01768   6.860 2.67e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.07702 on 396 degrees of freedom
# Multiple R-squared:  0.1839,	Adjusted R-squared:  0.1757 
# F-statistic: 22.31 on 4 and 396 DF,  p-value: < 2.2e-16

Q93 <- lm(lpsoda ~ prpblck  + lhseval, newdataQ923)
anova(Q92,Q93)
# Analysis of Variance Table
# 
# Model 1: lpsoda ~ prpblck + lincome + prppov + lhseval
# Model 2: lpsoda ~ prpblck + lhseval
# Res.Df    RSS Df Sum of Sq      F  Pr(>F)  
# 1    396 2.3493                              
# 2    398 2.3911 -2 -0.041797 3.5227 0.03045 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


var(log(psoda))
[1] NA

rm(list=ls())
#log(psoda) = ß0 + ß1 prpblck + ß2 log(income) + ß3 prppov +u
load("~/Desktop/R data sets for 5e/discrim.RData")
attach(data)
# Solve “NA”
C9vars=c("lpsoda","prpblck","lincome","prppov")
newdataC9=data[C9vars]

newdataC92=na.omit(newdataC9)
nrow(newdataC92)
# [1] 401
C9plus <- lm(lpsoda ~ prpblck + lincome + prppov, newdataC92)
summary(C9plus)

# Call:
#   lm(formula = lpsoda ~ prpblck + lincome + prppov, data = newdataC92)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.32218 -0.04648  0.00651  0.04272  0.35622 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.46333    0.29371  -4.982  9.4e-07 ***
#   prpblck      0.07281    0.03068   2.373   0.0181 *  
#   lincome      0.13696    0.02676   5.119  4.8e-07 ***
#   prppov       0.38036    0.13279   2.864   0.0044 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.08137 on 397 degrees of freedom
# Multiple R-squared:  0.08696,	Adjusted R-squared:  0.08006 
# F-statistic:  12.6 on 3 and 397 DF,  p-value: 6.917e-08

attach(newdataC92)
round(var(lpsoda),4)
# [1] 0.0072

# C3
# For example
load("~/Desktop/R data sets for 5e/crime1.RData")
attach(data)
# we estimate the restricted model by regressing narr86 on pcnv, ptime86, and qemp86
exvars=c("narr86","pcnv","ptime86","qemp86")
newdataexres=data(exvars)
newdataexres=data[exvars]
newdataexres2=na.omit(newdataexres)
nrow(newdataexres2)
# [1] 2725

exres <- lm(narr86 ~ pcnv + ptime86 + qemp86)
summary(exres)

# I find there is not NA in the regression

# Call:
#   lm(formula = narr86 ~ pcnv + ptime86 + qemp86)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -0.7118 -0.4031 -0.2953  0.3452 11.4358 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.711772   0.033007  21.565  < 2e-16 ***
#   pcnv        -0.149927   0.040865  -3.669 0.000248 ***
#   ptime86     -0.034420   0.008591  -4.007 6.33e-05 ***
#   qemp86      -0.104113   0.010388 -10.023  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8416 on 2721 degrees of freedom
# Multiple R-squared:  0.04132,	Adjusted R-squared:  0.04027 
# F-statistic:  39.1 on 3 and 2721 DF,  p-value: < 2.2e-16

exred <- resid(exres)

# question
rm(list=ls())
load("~/Desktop/R data sets for 5e/bwght.RData")
attach(data)
# restricted model
C3vars=c("bwght","cigs","parity","faminc","motheduc","fatheduc")
newdataC3=data[C3vars]
newdataC32=na.omit(newdataC3)
nrow(newdataC32)
# [1] 1191

C3res <- lm(bwght ~ cigs + parity + faminc, newdataC32)
C3 <- lm(resid(C3res) ~ cigs + parity + faminc + motheduc + fatheduc, newdataC32)
summary(C3)

# Call:
#   lm(formula = resid(C3res) ~ cigs + parity + faminc + motheduc + 
#        fatheduc, data = newdataC32)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -95.796 -11.960   0.643  12.679 150.879 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept) -0.945597   3.728453  -0.254   0.7998  
# cigs         0.001916   0.110348   0.017   0.9862  
# parity      -0.044671   0.659406  -0.068   0.9460  
# faminc      -0.011020   0.036562  -0.301   0.7631  
# motheduc    -0.370450   0.319855  -1.158   0.2470  
# fatheduc     0.472394   0.282643   1.671   0.0949 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19.79 on 1185 degrees of freedom
# Multiple R-squared:  0.00242,	Adjusted R-squared:  -0.001789 
# F-statistic: 0.5749 on 5 and 1185 DF,  p-value: 0.7193