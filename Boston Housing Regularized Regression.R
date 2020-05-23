getwd()
setwd('C:/Users/for/Desktop') #'C:/Users/user/Desktop/���Ѱ�/������ ���̴�'
Boston<-read.csv('Boston housing.csv')
Boston
library(pastecs)
stat.desc(Boston)
cor(Boston)
plot(Boston$MEDV~Boston$NOX)
plot(Boston$MEDV~Boston$CHARS)
plot(Boston$MEDV~Boston$RAD)
plot(Boston$MEDV~Boston$RM)
plot(Boston$MEDV~Boston$CRIM)
plot(Boston$MEDV~Boston$ZN)
plot(Boston$MEDV~Boston$INDUS)
plot(Boston$MEDV~Boston$AGE)
plot(Boston$MEDV~Boston$DIS)
plot(Boston$MEDV~Boston$TAX)
plot(Boston$MEDV~Boston$PTRATIO)
plot(Boston$MEDV~Boston$B)
plot(Boston$MEDV~Boston$LSTAT)
outlier<-c(which(Boston$RM<6.5&Boston$MEDV==50))
outlier
Boston[outlier,]
Bostonout<-Boston[-outlier,]
Bostonout

cor(Bostonout)
set.seed(100)
sample.no<-sample(1:nrow(Bostonout),nrow(Bostonout)*0.7)
Boston.train<-Bostonout[sample.no,]
Boston.test<-Bostonout[-sample.no,]
str(Boston.train)
str(Boston.test)


##Original
#�Ʒ��ڷ�1
fit.case1<-lm(formula=MEDV~.,data=Boston.train)
summary(fit.case1) # R-sq=0.7853
1-sum((Boston.train$MEDV- fit.case1$fitted.values)^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.77535596
sqrt(mean((fit.case1$residuals)^2)) #RMSE=4.231898
#���ڷ�1
res.case1<-Boston.test$MEDV-predict(fit.case1,newdata=Boston.test) #���ڷ� ����
1-sum(res.case1^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2) #���ڷ� R-sq =0.7749042
sqrt(mean(res.case1^2)) #���ڷ� RMSE =4.293972

#case1 �Ʒ��ڷ� AIC
AIC1<-nrow(Boston.train)*log(sum(fit.case1$residuals^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*14
AIC1
#case1 ���ڷ� AIC
AIC01<-nrow(Boston.test)*log(sum(res.case1^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*14
AIC01

##���� ln(MEDV), �Է� All
#�Ʒ��ڷ�2
fit.case2<-lm(formula=log(MEDV)~.,data=Boston.train) 
summary(fit.case2) 
1-sum((Boston.train$MEDV- exp(fit.case2$fitted.values))^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.8167173
sqrt(mean((exp(fit.case2$fitted.values)-Boston.train$MEDV)^2))#RMSE=3.822539
#���ڷ�2
res.case2<-Boston.test$MEDV-exp(predict(fit.case2,newdata=Boston.test)) #���ڷ�2 ����
1-sum(res.case2^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2) #���ڷ�2 R-sq=0.8030233
sqrt(mean(res.case2^2)) #���ڷ�2 RMSE=4.016825

#case2 �Ʒ��ڷ� AIC
AIC2<-nrow(Boston.train)*log(sum((exp(fit.case2$fitted.values)-Boston.train$MEDV)^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*14
AIC2
#case2 ���ڷ� AIC
AIC02<-nrow(Boston.test)*log(sum(res.case2^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*14
AIC02

##���� ln(MEDV) �Է� RM,PTRATIO,LSTAT
#�Ʒ��ڷ�3
fit.case3<-lm(formula=log(MEDV)~RM+PTRATIO+LSTAT,data=Boston.train) 
summary(fit.case3)
1-sum((Boston.train$MEDV- exp(fit.case3$fitted.values))^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.7736898
sqrt(mean((exp(fit.case3$fitted.values)-Boston.train$MEDV)^2))#RMSE=4.247598
#���ڷ�3
res.case3<-Boston.test$MEDV-exp(predict(fit.case3,newdata=Boston.test)) #���ڷ�3 ����
1-sum(res.case3^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2) #���ڷ�3 R-sq=0.7580907
sqrt(mean(res.case3^2)) #���ڷ�3 RMSE=4.451453

#case3 �Ʒ��ڷ� AIC
AIC3<-nrow(Boston.train)*log(sum((exp(fit.case3$fitted.values)-Boston.train$MEDV)^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*4
AIC3
#case3 ���ڷ� AIC
AIC03<-nrow(Boston.test)*log(sum(res.case3^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*4
AIC03

##���� ln(MEDV), �Է� RM^2,NOX^2,ln(DIS),ln(RAD),ln(LSTAT)
#�Ʒ��ڷ�4
fit.case4<-lm(formula=log(MEDV)~RM^2+NOX^2+log(DIS)+log(RAD)+log(LSTAT),data=Boston.train) #ln(MEDV),5����
summary(fit.case4) 
1-sum((Boston.train$MEDV- exp(fit.case4$fitted.values))^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.7550804
sqrt(mean((exp(fit.case4$fitted.values)-Boston.train$MEDV)^2)) #RMSE 4 =4.418786
#���ڷ�4
res.case4<-Boston.test$MEDV-exp(predict(fit.case4,newdata=Boston.test)) # ���ڷ� ����
1-sum(res.case4^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2) # ���ڷ�4 R-sq= 0.7434846
sqrt(mean(res.case4^2)) #���ڷ�4 RMSE= 4.583868

#case4 �Ʒ��ڷ� AIC
AIC4<-nrow(Boston.train)*log(sum((exp(fit.case4$fitted.values)-Boston.train$MEDV)^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*6
AIC4

#case4 ���ڷ� AIC
AIC04<-nrow(Boston.test)*log(sum(res.case4^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*6
AIC04

##���� MEDV, �Է� RM,PTRATIO,LSATAT
#�Ʒ��ڷ�5
fit.case5<-lm(formula=MEDV~RM+PTRATIO+LSTAT,data=Boston.train) 
summary(fit.case5)
1-sum((Boston.train$MEDV- fit.case5$fitted.values)^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.7209154
sqrt(mean((fit.case5$fitted.values-Boston.train$MEDV)^2)) #RMSE = 4.716927
#���ڷ�5
res.case5<-Boston.test$MEDV-predict(fit.case5,newdata=Boston.test)# ���ڷ� ����
1-sum(res.case5^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2)#���ڷ�5 R-sq=0.7269089
sqrt(mean(res.case5^2)) #���ڷ�5 RMSE=4.729652

#case5 �Ʒ��ڷ� AIC
AIC5<-nrow(Boston.train)*log(sum((fit.case5$fitted.values-Boston.train$MEDV)^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*4
AIC5
#case5 ���ڷ� AIC
AIC05<-nrow(Boston.test)*log(sum(res.case5^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*4
AIC05

##���� MEDV, �Է� RM^2,NOX^2,ln(DIS),ln(RAD),ln(LSTAT)
#�Ʒ��ڷ�6
fit.case6<-lm(formula=MEDV~RM^2+NOX^2+log(DIS)+log(RAD)+log(LSTAT),data=Boston.train)
summary(fit.case6)
1-sum((Boston.train$MEDV- fit.case6$fitted.values)^2)/sum((Boston.train$MEDV - mean(Boston.train$MEDV))^2) #R-sq=0.7563259
sqrt(mean((fit.case6$fitted.values-Boston.train$MEDV)^2)) #RMSE = 4.407536
#���ڷ�6
res.case6<-Boston.test$MEDV-predict(fit.case6,newdata=Boston.test)# ���ڷ�6 ��
1-sum(res.case6^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2)#���ڷ�6 R-sq=0.7646994
sqrt(mean(res.case6^2)) #���ڷ�6 RMSE=4.390227

#case6 �Ʒ��ڷ� AIC
AIC6<-nrow(Boston.train)*log(sum((fit.case6$fitted.values-Boston.train$MEDV)^2))-nrow(Boston.train)*log(nrow(Boston.train))+2*6
AIC6
#case6 ���ڷ� AIC
AIC06<-nrow(Boston.test)*log(sum(res.case6^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*6
AIC06

#RAD ���� ������ ó��
rad2<-0
rad3<-0
rad4<-0
rad5<-0
rad6<-0
rad7<-0
rad8<-0
rad24<-0
BostonBin<-cbind(Bostonout,rad2,rad3,rad4,rad5,rad6,rad7,rad8,rad24)
BostonBin$rad2[BostonBin$RAD==2]<-1
BostonBin$rad3[BostonBin$RAD==3]<-1
BostonBin$rad4[BostonBin$RAD==4]<-1
BostonBin$rad5[BostonBin$RAD==5]<-1
BostonBin$rad6[BostonBin$RAD==6]<-1
BostonBin$rad7[BostonBin$RAD==7]<-1
BostonBin$rad8[BostonBin$RAD==8]<-1
BostonBin$rad24[BostonBin$RAD==24]<-1

BostonBin$RAD<-NULL
BostonBin.train<-BostonBin[sample.no,]
BostonBin.test<-BostonBin[-sample.no,]
BostonBin

##������ ���� ó��
fit.cat<-lm(log(MEDV)~. + I(RM^2) + I(NOX^2) + log(LSTAT) + log(DIS) - RM - NOX -DIS, data= Boston.train)
summary(fit.cat) #R-sq = 0.8267
#�Ʒ��ڷ�Bin
1-sum((BostonBin.train$MEDV- exp(fit.cat$fitted.values))^2)/sum((BostonBin.train$MEDV - mean(BostonBin.train$MEDV))^2) #R-sq=0.8516811
sqrt(mean((exp(fit.cat$fitted.values)-BostonBin.train$MEDV)^2)) #RMSE = 3.438662
#���ڷ�Bin
res.cat<-BostonBin.test$MEDV-exp(predict(fit.cat,newdata=BostonBin.test))# ���ڷ� ����
1-sum(res.cat^2)/sum((Boston.test$MEDV-mean(Boston.test$MEDV))^2)#���ڷ�R-sq=
sqrt(mean(res.cat^2)) #���ڷ� RMSE=

#�Ʒ��ڷ� AIC
AIC.cat<-nrow(BostonBin.train)*log(sum((exp(fit.cat$fitted.values)-BostonBin.train$MEDV)^2))-nrow(BostonBin.train)*log(nrow(BostonBin.train))+2*21
AIC.cat
#���ڷ� AIC
AIC.cat0<-nrow(Boston.test)*log(sum(res.cat^2))-nrow(Boston.test)*log(nrow(Boston.test))+2*21
AIC.cat0

##��ȯ ������ �߰�
BostonTrans<-with(BostonBin,cbind(BostonBin,RM2=RM^2,NOX2=NOX^2,LLSTAT=log(LSTAT),LDIS=log(DIS)))
BostonTrans.train<-BostonTrans[sample.no,]
BostonTrans.test<-BostonTrans[-sample.no,]

##OLS
#�Ʒ��ڷ�
fit.OLS<-lm(log(MEDV)~.,data=BostonTrans)
summary(fit.OLS)
1-sum((BostonTrans.train$MEDV- exp(fit.OLS$fitted.values))^2)/sum((BostonTrans.train$MEDV - mean(BostonTrans.train$MEDV))^2) #R-sq=
sqrt(mean((exp(fit.OLS$fitted.values)-BostonTrans.train$MEDV)^2)) #RMSE
#���ڷ�
res.OLS<-BostonTrans.test$MEDV-exp(predict(fit.OLS,newdata=BostonTrans.test)) # ���ڷ� ����
1-sum(res.OLS^2)/sum((BostonTrans.test$MEDV-mean(BostonTrans.test$MEDV))^2) # ���ڷ�4 R-sq= 0.864796
sqrt(mean(res.OLS^2)) #���ڷ� RMSE= 3.328225

##OLS AIC
#�Ʒ��ڷ�
AIC.OLS<-nrow(BostonTrans.train)*log(sum((exp(fit.OLS$fitted.values)-BostonTrans.train$MEDV)^2))-nrow(BostonTrans.train)*log(nrow(BostonTrans.train))+2*25
AIC.OLS
#���ڷ�
AIC.OLS0<-nrow(BostonTrans.test)*log(sum(res.OLS^2))-nrow(BostonTrans.test)*log(nrow(BostonTrans.test))+2*25
AIC.OLS0
##stepwise
#�Ʒ�����
fit.stepwise<-step(fit.OLS,direction=��both��)
summary(fit.stepwise)
fit.stepwise$coefficients
1-sum((BostonTrans.train$MEDV- exp(fit.stepwise$fitted. values))^2) /sum((BostonTrans.train$MEDV - mean(BostonTrans.train$MEDV))^2)
sqrt(mean((exp(fit.stepwise$fitted. values)-BostonTrans.train$MEDV)^2))

#������
res. stepwise<-BostonTrans.test$MEDV-exp(predict (fit.stepwise, newdata=BostonTrans.test)) #������ ����
1-sum(res.stepwise^2)/sum((BostonTrans.test$MEDV-mean(BostonTrans.test $MEDV))^2)

sqrt(mean(res.stepwise^2))

##stepwise AIC
#�Ʒ����� AIC
AIC.stepwise<-nrow(BostonTrans.train)*log(sum((exp(fit.stepwise$fitted.values)-BostonTrans.train$MEDV)^2))-nrow(BostonTrans. train)*log(nrow(BostonTrans.train))+2*13
AIC.stepwise

#������ AIC
AIC.stepwise0<-nrow(BostonTrans.test)*log(sum(res.stepwise^2))-nrow(BostonTrans.test)*log(nrow(BostonTrans.test))+2*13
AIC.stepwise0

##glmnet
library(gimnet)
#Ridge
fit.Ridge<-glmnet(as.matrix(BostonTrans.train[,-13]), Bostontrans.train[,13], alpha=0)
plot(fit.Ridge, xvar='lambda')
set.seed (100)
fit.Ridge.cv<-cv.glmnet(as.matrix(BostonTrans.train[,-13]), BostonTrans.train[,13], alpha=0)
plot(fit.Ridge. cv)
grid.Ridge<-seq(fit.Ridge.cv$lambda.min,fit.Ridge.cv$lambda.1se, length.out=5)
grid.Ridge
fit.RidgeNew<-glmnet(as.matrix(BostonTrans.train[,-13]),BostonTrans.train[,13],alpha=0,lambda=grid.Ridge)
fit.RidgeNew
head(fit.RidgeNew)

#�Ʒ����� R-sq, RMSE
Ridge.fitted.value<-log(predict(fit.RidgeNew, newx=as.matrix(BostonTrans.train[,-13]))) #������
1-colSums((BostonTrans.train$MEDV-exp(Ridge.fitted.value))^2)/sum((BostonTrans.train$MEDV-mean(Bostontrans.train$MEDV))^2) #R-Sq
sqrt(colMeans((exp(Ridge.fitted.value) -BostonTrans.train$MEDV)^2)) #RMSE
#������ R-sq, RMSE
res.Ridge<-BostonTrans. test $MEDV-predict (fit.RidgeNew,newx=as.matrix(Bostontrans.test [,-13])) #������
1-colSums(res. Ridge2)/sum((BostonTrans.test$MEDV-mean(BostonTrans.test $MEDV))^2) # R-sq
sqrt(colMeans(res.Ridge^2)) #RMSE
##Ridge AIC
# �Ʒ����� AIC
AIC.Ridge<-nrow(BostonTrans.train)*log (col sums ((exp(Ridge.fitted.value)-BostonTrans.train$MEDV)^2))-nrow(BostonTrans.train)* log(nrow(BostonTrans.train))+2*24
AIC.Ridge
#AIC
AIC. Ridge0<-nrow(BostonTrans.test)*log(colSums(res.Ridge^2))-nrow(BostonTrans.test)*log(nrow(BostonTrans.test))+2*24
AIC. Ridge0

##Lasso
fit. Lasso<-glmnet(as.matrix(BostonTrans.train[,-13]),BostonTrans.train[,13], alpha=1)
plot(fit.Lasso,xvar='lambda')
set. seed (100)
fit.Lasso.cv<-cv.glmnet(as.matrix(BostonTrans.train[,-13]), BostonTrans.train[,13], alpha=1)
plot(fit.Lasso.cv)
grid.Lasso<-seq(fit.Lasso.cv$lambda.min, fit.Lasso.cv$lambda.1se, length.out=5)
grid. Lasso 
fit.LassoNew<-glmnet(as.matrix(BostonTrans.train[,-13]), BostonTrans.train[,13], alpha=1,lambda=grid.Lasso)
fit.LassoNew
head(fit.LassoNew)

#�Ʒ����� R-sq, RMSE
Lasso.fitted.value<-log(predict(fit.LassoNew, newx=as.matrix(BostonTrans.train[,-13]))) #������
1-colSums((BostonTrans.train$MEDV-exp(Lasso.fitted.value))^2)/ sum((BostonTrans.train$MEDV-mean(BostonTrans.train$MEDV))^2) #R-sq
sqrt(colMeans((exp(Lasso.fitted.value)-BostonTrans.train$MEDV)^2)) #RMSE
#������ R-sq, RMSE
res.Lasso<-BostonTrans.test $MEDV-predict(fit.LassoNew, newx=as.matrix(BostonTrans.test[,-13])) #������ ����
1-colSums(res.Lasso^2)/sum((BostonTrans.test $MEDV-mean(BostonTrans.test $MEDV))^2) # R-sq 
sqrt(colMeans(res.Lasso^2)) #RMSE

##Lasso AIC
#�Ʒ����� AIC
AIC.Lasso<-nrow(BostonTrans.train)*log(colSums((exp(Lasso.fitted.value)-BostonTrans.train$MEDV)^2))-nrow(BostonTrans.train)*log(nrow(BostonTrans.train))+2*24
AIC. Lasso
#������ AIC
AIC.Lasso0<-nrow(BostonTrans.test)*log (colSums(res.Lasso^2))-nrow(BostonTrans.test)*log(nrow(BostonTrans.test))+2*24
AIC.Lasso0