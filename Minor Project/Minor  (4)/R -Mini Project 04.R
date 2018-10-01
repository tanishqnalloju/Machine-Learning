bankfull = read.csv(file.choose())
head(bankfull)
#Hypothesis Test
#CHI SQUARE
#NULL - There is no relationship between education and y
#Alternate - There is  relationship between education and y
chisq.test(table(bankfull$y,bankfull$education))

#ttest
#NULL - There is no significant difference Average in Prices of Automatic cars - Both are Equal
#Alternate - There is significant difference Average in Prices of Automatic and MAnual - Both are not equal
yes <- bankfull[ which(bankfull$y == "yes"),]
no  <- bankfull[ which(bankfull$y == "no"),]

t.test(yes$age,no$age)

bankfull$month <- NULL
bankfull$day <- NULL

model <- glm (y ~ .,data = bankfull, family = binomial)
predict <- predict(model, type = 'response')
table(bankfull$y, predict > 0.5)
summary(model)

?randomForest
bankfullforest = randomForest(y~.,data = bankfull,ntree = 1000,do.trace = 100)
print(bankfullforest)


library(monmlp)
r <- monmlp.fit(x, y, hidden1=3, n.ensemble=15, monotone=1, bag=TRUE)
z <- monmlp.predict(x = x, weights = r)

?monmlp




#

bankfullreg=lm(y~.,data=bankfull[-1])
bankfullpredict=predict(bankfullreg,bankfull[-1])



#
bankfullreg=lm(y~.,data=bankfull[-1])
bankfullpredict=predict(bankfullreg,bankfull[-1])
head(bankfullpredict)
summary(bankfullreg)

