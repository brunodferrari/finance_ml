#https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb

library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

boston = read.csv("boston.csv")

latlontree = rpart(MEDV ~ LAT + LON, data=boston)

prp(latlontree)

plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2],boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>21.2],boston$LAT[fittedvalues>=21.2], col="blue", pch="$")

latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree)


# Split the data
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree)


###################
german = read.csv("german_credit.csv", stringsAsFactors = F)

tree = rpart(default ~ account_check_status + duration_in_month + credit_history +
        purpose + credit_amount + savings + present_emp_since + 
        installment_as_income_perc + personal_status_sex + other_debtors + present_res_since +
        property + age + other_installment_plans + housing +
        credits_this_bank + job + people_under_maintenance + telephone + foreign_worker, data = german, method = "class")
 
fit = predict(tree, type = "class")


prp(tree) 
printcp(tree) 

names(german)

library("caret")
library("e1071")

orig = as.numeric(german$default)


confusionMatrix(fit, as.factor(german$default))

mod2 = rpart(default ~ account_check_status + duration_in_month + credit_history +
                      credit_amount + savings, data = german, method = "class")
prp(mod2)

fit2 = predict(mod2, type = "class")
confusionMatrix(fit2, as.factor(german$default))
summary(mod2)
