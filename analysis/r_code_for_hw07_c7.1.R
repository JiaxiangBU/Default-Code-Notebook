library(wooldridge)
data(wage1)
feduc12.5 <- wage1$female * (wage1$educ - 12.5)
female <- wage1$female
educ <- wage1$educ

C72 <-
    lm(lwage ~ female + educ + feduc12.5 + exper + expersq + tenure + tenursq,
       data = wage1)
summary(C72)

round(cor(female, female * educ), 4)
round(cor(female, feduc12.5), 4)
round(mean(educ), 4)
