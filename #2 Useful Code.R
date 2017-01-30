
# Find num. of elements of col
length()

# Download bptest, use the button at the top.
# Find the y_hat, use fitted or predict
pchisq(value, df=k, lower.tail = F)

# overall F-test, use linearHypothesis(fit, c("x1 = 0","x2 = 0", vcov=vcovHC(fit, type="HC0"))
linearHypothesis(fit, c("x1 = 0","x2 = 0", vcov=vcovHC(fit, type="HC0"))

# Solve “NA”, use na.omit
myvars=c("lsalary","LSAT","GPA","llibvol","lcost","rank")
newdata=data[myvars]
newdata1=na.omit(newdata)
nrow(newdata1) #Check the No. which is reduced, if there exist NA rows.

# Find No.
inlfhat <- fitted(fit)
infhar_gt1 <- subset(inlfhat,inlfhat>1) # Divid the dataset
length(infhar_gt1)
infhar_gt0 <- subset(inlfhat,inlfhat<0)
length(infhar_gt0)

# Rename programmatically
library(reshape)
mydata <- rename(mydata, c(oldname="newname"))


# Or
# You can re-enter all the variable names in order
# changing the ones you need to change.the limitation is that you need to enter all of them!
names(data) <- c("Num","Gender","F1", "F2","M1","M2","GPA","DQ","PV","SV","SR","RQ")
                 # But I think it is a little bit waste of time.

# When do a regression with dummy variable, use as.factor()
fit <- lm(y ~ x + as.factor(z))
                 
