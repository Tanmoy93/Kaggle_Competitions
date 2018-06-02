library(readr)
library(ggplot2)
library(caret)
library(psych)
library(randomForest)
library(stringr)
library(dplyr)
train <- read_csv("C:/Users/ACER/Downloads/train.csv")
test <- read_csv("C:/Users/ACER/Downloads/test.csv")
colSums(is.na(train))
colSums(is.na(test))
train$Is_train<-TRUE
test$Is_train<-FALSE
test$Survived<-"NA"
titanic.combined<-rbind(train,test)
colSums(is.na(titanic.combined))
titanic.combined$Survived<-as.factor(titanic.combined$Survived)
titanic.combined$Pclass<-as.factor(titanic.combined$Pclass)
titanic.combined$Sex<-as.factor(titanic.combined$Sex)
titanic.combined$Embarked<-as.factor(titanic.combined$Embarked)
fix(titanic.combined)
g1<-ggplot(data=train)+geom_bar(aes(x=Pclass,fill=Survived))
plot(g1)
g2<-ggplot(data=train)+geom_bar(aes(x=Pclass,fill=Sex))
plot(g2)
fix(titanic.combined)
embark_fare<-titanic.combined %>%
             filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2)
titanic.combined$Embarked[c(62, 830)] <- 'C'
titanic.combined[1044,]
titanic.combined$Fare[1044]<-median(titanic.combined[titanic.combined$Pclass=="3"
                            & titanic.combined$Embarked=="S",]$Fare,na.rm = T)
mean_age<-mean(titanic.combined$Age,na.rm = TRUE)
titanic.combined$Age<-ifelse(is.na(titanic.combined$Age),mean_age,titanic.combined$Age)
colSums(is.na(titanic.combined))
train<-titanic.combined[titanic.combined$Is_train==TRUE,]
test<-titanic.combined[titanic.combined$Is_train==FALSE,]
pd<-sample(2,nrow(train),replace = TRUE,prob = c(0.8,0.2))
train_actual<-train[pd==1,]
train_test<-train[pd==2,]
rf_model<-randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = train_actual)
pred1<-predict(rf_model,train)
tab1<-table(predicted=pred,Actual=train_actual$Survived)
print(tab1)
1-sum(diag(tab1))/sum(tab1)

pred2<-predict(rf_model,train_test)
tab2<-table(predicted=pred2,Actual=train_test$Survived)
print(tab2)
1-sum(diag(tab2))/sum(tab2)

pred3<-predict(rf_model,test)
summary(pred3)
solution<-data.frame(PassengerID=test$PassengerId,Survived=pred3)
write.csv(solution,file = "rf_model_solution.csv",row.names = F)
