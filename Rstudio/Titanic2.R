# This script put me in roughly 1400th place

# Set working directory and import data files
setwd("D:/Jason/Projects/titanic/Rstudio")
train <- read.csv("D:/Jason/Data/titanic/train.csv")
test <- read.csv("D:/Jason/Data/titanic/test.csv")
View(train)
View(test)

# imports needed
library(ggplot2)
library(dplyr)
library(magrittr)
library(randomForest) # This package was made by the method's original inventors!

# Combine the training and test files
test$Survived <- NA
combi <- rbind(train, test)

# The names were imported as factors, bring them back as a string
combi$Name <- as.character(combi$Name)

# How to get at the titles in the name
strsplit(combi$Name[1], split='[,.]')
strsplit(combi$Name[1], split='[,.]')[[1]]
strsplit(combi$Name[1], split='[,.]')[[1]][2]
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

# Get rid of the leading space
combi$Title <- sub(' ', '', combi$Title)

# Condense all these titles into a few categories
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir','Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

# Compute family size
combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Find out which Embarked values are blank
which(combi$Embarked=='')

# They are rows 62 and 830. Fill them in with S
combi$Embarked[c(62,830)] <- "S"
combi$Embarked <- factor(combi$Embarked)

# Check out age
summary(combi$Age)

# There are 263 NA's, we gotta clean these up. Let's show
# an age distribution by title

# This tells us which titles are on records that have missing
# ages
table(combi$Title[is.na(combi$Age)])

# Visualize ages on titles
ggplot(combi, aes(x=combi$Title, y=combi$Age)) +
  geom_boxplot()

# There's still a wide array of titles
summary(combi$Title)

# But that's okay, I now want to calculate the median age of each title.
dump <- combi[is.na(combi$Age)==FALSE,]
medages <- aggregate(dump$Age, list(dump$Title), median)
names(medages)[names(medages)=='Group.1'] <- 'Title'
names(medages)[names(medages)=='x'] <- 'Age2'

combi <- merge(combi,medages,by='Title')
#Not sure if this is right
combi$Age[is.na(combi$Age)] <- combi$Age2

#Need to turn factor data back into factors
factor_vars<-c('Pclass','Sex','Embarked','Title')
combi[factor_vars]<-lapply(combi[factor_vars],function(x) as.factor(x))

# At this point the data should be clean, let's split them back apart
train1 = combi[combi$PassengerId >= 1 & combi$PassengerId <=891,]
test1 = combi[combi$PassengerId>891,]

# What are we going to try? How about a random forest?

#Make the model
rfmodel <- randomForest(factor(Survived)~Pclass+Title+Sex+Age+Embarked+FamilySize+Fare, data=train1, nodesize=2, proximity=TRUE)

#Plot the error rate by #trees grown
plot(rfmodel, ylim=c(0,0.36))
legend('topright', colnames(rfmodel$err.rate), col=1:3, fill=1:3)

#Variable importance
importance    <- importance(rfmodel)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

#Create a submission
prediction <- predict(rfmodel, test1)
pred <- data.frame(prediction)
pred$id <- row.names(pred)

# They're all out of order, so join them back to the test file
testsoln <- data.frame(id=row.names(test1),test1)
testsoln <- merge(testsoln, pred, by="id")

solution <- data.frame(PassengerID = testsoln$PassengerId, Survived =testsoln$prediction)
write.csv(solution, file = 'rf_mod_Solution2.csv', row.names = F)
