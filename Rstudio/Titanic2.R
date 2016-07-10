# Set working directory and import data files
setwd("D:/Jason/Projects/titanic/Rstudio")
train <- read.csv("D:/Jason/Data/titanic/train.csv")
test <- read.csv("D:/Jason/Data/titanic/test.csv")
View(train)
View(test)

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
ggplot(combi, aes(x=combi$Title, y=combi$Age))
  + geom_boxplot()