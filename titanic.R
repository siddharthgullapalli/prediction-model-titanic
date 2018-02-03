train <- read.csv(file.choose(),header=TRUE)
test = read.csv(file.choose(),header =TRUE)
str(test)
str(train)
table(train$Survived)
prop.table(table(train$Survived))
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived),1)
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0
prop.table(table(train$Child, train$Survived),1)
test_one <- test
test_one$Survived <- 0
test_one$Survived[test$Sex == "female"] <-1
library(rpart)
str(train)
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(my_tree_two)
text(my_tree_two)
library(rattle)   
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(my_tree_two)
my_prediction <- predict(my_tree_two, newdata = test, type = "class")
my_solution <- data.frame(PassengerId =test$PassengerId , Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file ="my_solution.csv", row.names = FALSE)
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                         +                        data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
plot(my_tree_three)
text(my_tree_three)
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,data= train_two,method = "class")
fancyRpartPlot(my_tree_four)
plot(my_tree_four)
text(my_tree_four)