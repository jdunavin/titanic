# Set working directory and import data files
setwd("D:/Jason")
train <- read.csv("D:/Jason/Data/titanic/train.csv")
test <- read.csv("D:/Jason/Data/titanic/test.csv")
View(train)
View(test)

#Show datatypes
str(train)

#check out the Survived column
table(train$Survived)
prop.table(table(train$Survived))

#Our first (lame) prediction - everyone dies
test$Survived <- rep(0,418)

#Prepare a submission for this lame prediction and write
# out a csv containing the results
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file=".\\Projects\\titanic\\theyalldie.csv",row.names = FALSE)

# Part II - Gender + class model
# Check out the data some more, summarize by gender
summary(train$Sex)
prop.table(table(train$Survived, train$Sex))

# That was dumb, I want the row proportions - 2 does the columns
prop.table(table(train$Survived, train$Sex),1)

# Based on that, say all women live and all men die, then submit
test$Survived <- 0
test$Survived[test$Sex=='female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file=".\\Projects\\titanic\\genderR.csv",row.names = FALSE)

# Let's look at ages
summary(train$Age)

# Let's put a child indicator on
train$Child <- 0
train$Child[train$Age < 18] <- 1

# Look at child survivors
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
# Those are dumb, show the proportions
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Create 4 fare bins
train$Fare2 <- '30+'
train$Fare2[train$Fare>=20 & train$Fare<30] <- '20-30'
train$Fare2[train$Fare>=10 & train$Fare<20] <- '10-20'
train$Fare2[train$Fare<10] <- '0-10'

# Show results by gender, class, farebin
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# It's weird that 3rd class females who paid $20 or more
# mostly died, while most women survived... right?

# Make more predictions
test$Survived<-0
test$Survived[test$Sex == 'female'] <-1
test$Survived[test$Sex == 'female' & test$Pclass==3 & test$Fare>=20]<-0

#Create another submission
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file=".\\Projects\\titanic\\femfare.csv",row.names = FALSE)

#Now let's mess with decision trees!
library(rpart)

# Fit the tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

# Look at it
plot(fit)
text(fit)

#Those look like crap, let's install some cool stuff to look
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Try this out for a plot
# Need to fix this so it looks clearer to read
fancyRpartPlot(fit, cex=0.60)

# Make a prediction based on this tree and send it
Prediction <- predict(fit, test, type="class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file=".\\Projects\\titanic\\tree1.csv",row.names = FALSE)

