
# find num. of elements of col --------------------------------------------


length()

# download bptest, use the button at the top.
# find the y_hat, use fitted or predict

pchisq(value, df=k, lower.tail = F)

# overall F-test, use linearHypothesis(fit, c("x1 = 0","x2 = 0", vcov=vcovHC(fit, type="HC0"))
linearHypothesis(fit, c("x1 = 0","x2 = 0", vcov=vcovHC(fit, type="HC0"))

load("~/Desktop/R data sets for 5e/fertil1.RData")


# use guide of official resource ------------------------------------------


library(shiny)
runExample("01_hello")


# Solve “NA” --------------------------------------------------------------


myvars=c("lsalary","LSAT","GPA","llibvol","lcost","rank")
newdata=data[myvars]
newdata1=na.omit(newdata)
nrow(newdata1)


# find num. ---------------------------------------------------------------


inlfhat <- fitted(fit)
infhar_gt1 <- subset(inlfhat,inlfhat>1)
length(infhar_gt1)
infhar_gt0 <- subset(inlfhat,inlfhat<0)
length(infhar_gt0)

# rename interactively
fix(data) # results are saved on close

# rename programmatically
library(reshape)
mydata <- rename(mydata, c(oldname="newname"))


# RENAME VARS -------------------------------------------------------------


# you can re-enter all the variable names in order
# changing the ones you need to change.the limitation
# is that you need to enter all of them!
names(data) <- c("Num","Gender","F1", "F2","M1","M2","GPA","DQ","PV","SV","SR","RQ")