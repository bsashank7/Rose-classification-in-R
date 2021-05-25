library(e1071)
library(caret)
library(rpart)
library(randomForest)
library(party)
df<- read.csv(file.choose(),header = F)
df
sindex<-sample(2,nrow(df),prob=c(0.7,0.3),replace = T)
train_data<-df[sindex==1,]
test_data<-df[sindex==2,]

trcon<-trainControl(method='repeatedcv',
                    number=10,
                    repeats=10,
                    classProbs = T)

knn_fit<-train(V5~., data=train_data ,
               method='knn',
               tuneLength=20,
               trControl=trcon,
               preProc= c('center','scale'),
               tuneGrid = expand.grid(k=c(1:50)))

knn_fit ## k= 3, accuracy=96

pred_train<-predict(knn_fit,newdata = train_data)
pred_test<-predict(knn_fit,newdata = test_data)

conf_train<-table(observed=train_data$V5,predicted=pred_train)
conf_test<-table(observed=test_data$V5,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 86,99


####### decision tree #####

dtmodel<- rpart(V5~.,data=train_data,
                method= 'class',
                parms=list(split='information'))

pred_train<- predict(dtmodel,newdata= train_data,type = 'class')
pred_test<- predict(dtmodel,newdata= test_data,type = 'class')

conf_train<-table(observed=train_data$V5,predicted=pred_train)
conf_test<-table(observed=test_data$V5,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 92, 97

######### random forest #####
train_data$V5<-as.factor(train_data$V5)
test_data$V5<-as.factor(test_data$V5)
tune_rf=tuneRF(train_data[,-5],train_data$V5,
               stepFactor = 1,plot = T,
               ntreeTry = 100,trace = T,improve = 0.05)

rf <- randomForest(V5~., data= train_data,
                   importance=T,ntree = 300,mtry=2)

pred_train<- predict(rf,newdata= train_data,type = 'class')
pred_test<- predict(rf,newdata= test_data,type = 'class')

conf_train<-table(observed=train_data$V5,predicted=pred_train)
conf_test<-table(observed=test_data$V5,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 0.94,100


###### svm ######

trcon<-trainControl(method='repeatedcv',
                    number=5,
                    repeats=5)

svm_cv<- train(V5~., data=train_data,
               method='svmRadial',
               tuneGrid=expand.grid(C=seq(0,5,length=20),sigma=10^(-7:1)),
               preProc=c('center','scale'),
               trControl=trcon)

pred_train<- predict(svm_cv,newdata= train_data)
pred_test<- predict(svm_cv,newdata= test_data)

conf_train<-table(observed=train_data$V5,predicted=pred_train)
conf_test<-table(observed=test_data$V5,predicted=pred_test)

accuracy_train<-sum(diag(conf_train))/sum(conf_train)
accuracy_test<-sum(diag(conf_test))/sum(conf_test)

accuracy_test;accuracy_train # 94, 97
