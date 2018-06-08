library(readr)
library(ggplot2)
library(psych)
library(corrplot)
library(caret)
library(randomForest)
library(corrplot)
train<-read.csv(file.choose(),stringsAsFactors = F,header = T)
test<-read.csv(file.choose(),stringsAsFactors = F,header = T)
train$Is_train<-TRUE
test$Is_train<-FALSE
test$SalePrice<-0
total<-rbind(train,test)
ggplot(data=total[!is.na(total$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
sort(colSums(is.na(total)),decreasing = T)
colnames(total)
str(total)
summary(total)
ggplot(data = total)+geom_boxplot(aes(x=PoolQC,y=PoolArea,fill="Red"))
which(is.na(total$PoolQC)&total$PoolArea>0)
total[c(2421,2504),"PoolQC"]<-"Ex"
total[2600,"PoolQC"]<-"Fa"
vec1<-which(is.na(total$PoolQC))
total$PoolQC<-as.character(total$PoolQC)
total[vec1,"PoolQC"]<-"None"
total$MiscFeature<-as.character(total$MiscFeature)
total[2550,"MiscFeature"]<-"Gar2"
vec2<-which(is.na(total$MiscFeature))    
total[vec2,"MiscFeature"]<-"None"
total$MiscFeature<-as.factor(total$MiscFeature)
total$Alley<-as.character(total$Alley)
vec3<-which(is.na(total$Alley))
total[vec3,"Alley"]<-"No alley access"
total$Fence<-as.character(total$Fence)
vec4<-which(is.na(total$Fence)) 
total[vec4,"Fence"]<-"No Fence"
ggplot(data = total)+geom_boxplot(aes(x=FireplaceQu,y=Fireplaces,fill="blue"))
vec5<-which(is.na(total$FireplaceQu))
total$FireplaceQu<-as.character(total$FireplaceQu)
total[vec5,"FireplaceQu"]<-"No Fireplace"
LotFrontage_median<-median(total$LotFrontage,na.rm = T)
total$LotFrontage<-ifelse(is.na(total$LotFrontage),LotFrontage_median,total$LotFrontage)
vec6<-which(is.na(total$GarageYrBlt))
total[vec6,"GarageYrBlt"]<-0
total$GarageFinish<-as.character(total$GarageFinish)
total$GarageQual<-as.character(total$GarageQual)
total$GarageCond<-as.character(total$GarageCond)
total$GarageType<-as.character(total$GarageType)
vec7<-which(is.na(total$GarageFinish))
vec8<-which(is.na(total$GarageQual))
vec9<-which(is.na(total$GarageCond))
vec10<-which(is.na(total$GarageType))  
total[vec7,"GarageFinish"]<-"No Garage"
total[vec8,"GarageQual"]<-"No Garage"
total[vec9,"GarageCond"]<-"No Garage"
total[vec10,"GarageType"]<-"No Garage"
total$BsmtFinType1<-as.character(total$BsmtFinType1)
vec11<-which(is.na(total$BsmtFinType1))
total[vec11,"BsmtFinType1"]<-"No Basement"
total$BsmtFinType2<-as.character(total$BsmtFinType2)
total[479,"BsmtFinType2"]<-"ALQ"
vec12<-which(is.na(total$BsmtFinType2))
total[vec12,"BsmtFinType2"]<-"No Basement"
total$BsmtExposure<-as.character(total$BsmtExposure)
total$BsmtCond<-as.character(total$BsmtCond)
total$BsmtQual<-as.character(total$BsmtQual)
vec13<-which(is.na(total$BsmtExposure))
vec14<-which(is.na(total$BsmtCond))
vec15<-which(is.na(total$BsmtQual))
total[vec13,"BsmtExposure"]<-"No Basement"
total[vec14,"BsmtCond"]<-"No Basement"
total[vec15,"BsmtQual"]<-"No Basement"
mean_MasVnrArea<-mean(total$MasVnrArea,na.rm = T)
total$MasVnrArea<-ifelse(is.na(total$MasVnrArea),mean_MasVnrArea,total$MasVnrArea)
ggplot(data = total)+geom_boxplot(aes(x=MasVnrType,y=MasVnrArea),color="red")
total$MasVnrType<-as.character(total$MasVnrType)
total[c(which(is.na(total$MasVnrType),total$MasVnrArea>0)),"MasVnrType"]<-"BrkCmn"
total$MSZoning<-as.character(total$MSZoning)
vec16<-which(is.na(total$MSZoning))
total[vec16,"MSZoning"]<-"RL"
total$Utilities<-as.character(total$Utilities)
Vec17<-which(is.na(total$Utilities))
total[c(1916,1946),"Utilities"]<-"AllPub"
total[c(2121,2189),"BsmtFullBath"]<-0
total[c(2121,2189),"BsmtHalfBath"]<-0
total$Functional<-as.character(total$Functional)
total[c(2217,2474),"Functional"]<-"Typ"
total$Exterior1st<-as.character(total$Exterior1st)
total$Exterior2nd<-as.character(total$Exterior2nd)
total[2152,"Exterior1st"]<-"VinylSd"
total[2152,"Exterior2nd"]<-"VinylSd"
total$BsmtFinSF1<-ifelse(is.na(total$BsmtFinSF1),mean(total$BsmtFinSF1,na.rm = T),total$BsmtFinSF1)
total$BsmtFinSF2<-ifelse(is.na(total$BsmtFinSF2),mean(total$BsmtFinSF2,na.rm = T),total$BsmtFinSF2)
total$BsmtUnfSF<-ifelse(is.na(total$BsmtUnfSF),mean(total$BsmtUnfSF,na.rm = T),total$BsmtUnfSF)
total$TotalBsmtSF<-ifelse(is.na(total$TotalBsmtSF),mean(total$TotalBsmtSF,na.rm = T),total$TotalBsmtSF)
total[1380,"Electrical"]<-"SBrkr"
total$KitchenQual<-as.character(total$KitchenQual)
total[1556,"KitchenQual"]<-"TA"
total[2577,"GarageCars"]<-2
total$GarageArea<-ifelse(is.na(total$GarageArea),mean(total$GarageArea,na.rm = T),total$GarageArea)
total$SaleType<-as.character(total$SaleType)
total[2490,"SaleType"]<-"WD"
total$MSSubClass<-as.factor(total$MSSubClass)
total$MSZoning<-as.factor(total$MSZoning)
total$Street<-as.factor(total$Street)
total$Alley<-as.factor(total$Alley)
total$LotShape<-as.factor(total$LotShape)
total$LandContour<-as.factor(total$LandContour)
total$Utilities<-as.factor(total$Utilities)
total$LotConfig<-as.factor(total$LotConfig)
total$LandSlope<-as.factor(total$LandSlope)
total$Neighborhood<-as.factor(total$Neighborhood)
total$Condition1<-as.factor(total$Condition1)
total$Condition2<-as.factor(total$Condition2)
total$BldgType<-as.factor(total$BldgType)
total$HouseStyle<-as.factor(total$HouseStyle)
total$OverallQual<-as.factor(total$OverallQual)
total$OverallCond<-as.factor(total$OverallCond)
total$RoofStyle<-as.factor(total$RoofStyle)
total$RoofMatl<-as.factor(total$RoofMatl)
total$Exterior1st<-as.factor(total$Exterior1st)
total$Exterior2nd<-as.factor(total$Exterior2nd)
total$MasVnrType<-as.factor(total$MasVnrType)
total$ExterQual<-as.factor(total$ExterQual)
total$ExterCond<-as.factor(total$ExterCond)
total$Foundation<-as.factor(total$Foundation)
total$BsmtQual<-as.factor(total$BsmtQual)
total$BsmtCond<-as.factor(total$BsmtCond)
total$BsmtExposure<-as.factor(total$BsmtExposure)
total$BsmtFinType1<-as.factor(total$BsmtFinType1)
total$BsmtFinType2<-as.factor(total$BsmtFinType2)
total$Heating<-as.factor(total$Heating)
total$HeatingQC<-as.factor(total$HeatingQC)
total$CentralAir<-as.factor(total$CentralAir)
total$KitchenQual<-as.factor(total$KitchenQual)
total$Functional<-as.factor(total$Functional)
total$FireplaceQu<-as.factor(total$FireplaceQu)
total$GarageType<-as.factor(total$GarageType)
total$GarageFinish<-as.factor(total$GarageFinish)
total$GarageQual<-as.factor(total$GarageQual)
total$GarageCond<-as.factor(total$GarageCond)
total$PavedDrive<-as.factor(total$PavedDrive)
total$PoolQC<-as.factor(total$PoolQC)
total$Fence<-as.factor(total$Fence)
total$MiscFeature<-as.factor(total$MiscFeature)
total$SaleType<-as.factor(total$SaleType)
total$SaleCondition<-as.factor(total$SaleCondition)
train<-total[total$Is_train==TRUE,]
test<-total[total$Is_train==FALSE,]
pd<-sample(2,nrow(train),replace = T,prob = c(0.8,0.2))
train_model<-train[pd==1,]
train_validate<-train[pd==2,]
train_model$Is_train<-NULL
train_validate$Is_train<-NULL
pred1<-lm(SalePrice~.-1,data = train_model)
summary(pred1)
pred2<-lm(SalePrice~LotArea+ Neighborhood+Condition2+OverallCond+ YearBuilt+RoofMatl+
          BsmtQual+BsmtFinSF1+BsmtUnfSF+SalePrice+MSZoning+Street+
          LotShape+LotConfig+MasVnrArea+ExterQual+BsmtExposure+BsmtFinType1+BsmtFinType2,data = train_model)
summary(pred2)
train_model_new<-train[1:1460,c("LotArea", "Neighborhood", "Condition2", "OverallCond", "YearBuilt", "RoofMatl",
                                  "BsmtQual", "BsmtFinSF1","BsmtUnfSF","SalePrice","MSZoning","Street",
                                "LotShape","LotConfig","MasVnrArea","ExterQual","BsmtExposure","BsmtFinType1","BsmtFinType2"
                                )]
test_model_new<-train[1:1459,c("LotArea", "Neighborhood", "Condition2", "OverallCond", "YearBuilt", "RoofMatl",
                               "BsmtQual", "BsmtFinSF1","BsmtUnfSF","SalePrice","MSZoning","Street",
                               "LotShape","LotConfig","MasVnrArea","ExterQual","BsmtExposure","BsmtFinType1","BsmtFinType2"
)]
pred4<-lm(lm(SalePrice~LotArea+Neighborhood+Condition2+OverallCond+YearBuilt+RoofMatl+
               BsmtQual+BsmtFinSF1+BsmtUnfSF,data = train_model_new))
q<-predict(pred4,train_model_new)
head(train_model_new$SalePrice)
head(q)
r<-predict(pred4,test_model_new)
test_model_new$Id<-test$Id
results <- data.frame(Id=test_model_new$Id, SalePrice=r)
write.csv(results, 'House_Prediction.csv', row.names=FALSE)
rf_model<-randomForest(SalePrice~.,data=train_model_new)
pred5<-predict(rf_model,train_model_new)
head(train$SalePrice)
head(pred5)
final<-predict(rf_model,test_model_new)
results2<-data.frame(ID=test_model_new$Id,SalePrice=final)
write.csv(results2,"Final_Submission.csv",row.names = F)
