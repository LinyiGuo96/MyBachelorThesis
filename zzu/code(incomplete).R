set.seed(3)
trainindex9<-sample(1:m,round(0.9*m))
trainset9<-P[trainindex9,]
testset9<-P[-trainindex9,]
L9<-glm(LOSS~.,data=trainset9,family = binomial)
Lguess9<-predict(L9,type = "response",newdata = testset9)
guess9<-ifelse(Lguess9>0.5,1,0)
table(guess9,testset9$LOSS)
Lguesstrain<-predict(L,type="response",newdata=trainset)
guesstrain<-ifelse(Lguesstrain>0.5,1,0)
table(guesstrain,trainset$LOSS)
for(i in 1:10)
{foldtest<-P[folds[[i]],]
 foldtrain<-P[-folds[[i]],]
 foldL<-glm(LOSS~.,data=foldtrain,family = binomial)
 foldpre<-predict(foldL,type = "response",newdata = foldtest)
 foldpre<-ifelse(foldpre>0.5,1,0)
 print(table(foldpre,foldtest$LOSS))}
#C50+holdout
install.packages("C50")#错误率0.129939719,训练集0.132969558
library(C50)
treecontrol<-C5.0Control(CF=0.25,winnow=T,noGlobalPruning=F,minCases =20)
treemodel <- C5.0(LOSS~.,data=trainset,rules=F,control =treecontrol)
treepre<-predict(treemodel,testset)
table(treepre,testset$LOSS)
treepre_train<-predict(treemodel,trainset)
table(treepre_train,trainset$LOSS)
table(treepre)
C5imp(treemodel)
plot(treemodel,main="The C5.0 Tree(Hold-out CV)",font.main=4)
#C50 + 10-fold 
library(C50)
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
require(caret)
#10-fold method+ CART(错误率0.140763052) trainset  0.130332812
folds<-createFolds(y=P$LOSS,k=10)
sum_error=0
sum_error1=0
sum_errortrain=0
for(i in 1:10){
  foldtest<-P[folds[[i]],]
  foldtrain<-P[-folds[[i]],]
  cartmodel_fold<-rpart(LOSS~.,data=foldtrain,method="class")
  cartpre<-predict(cartmodel_fold,foldtest)
  cartpre<-ifelse(cartpre[,2]>0.5,1,0)
  cartpre_train<-predict(cartmodel_fold,foldtrain)
  cartpre_train<-ifelse(cartpre_train[,2]>0.5,1,0)
  sum_error<-sum_error+sum(ifelse(foldtest$LOSS==cartpre,0,1))
  sum_error1<-sum_error1+sum(table(cartpre,foldtest$LOSS)[1,2])
  sum_errortrain<-sum_errortrain+sum(ifelse(foldtrain$LOSS==cartpre_train,0,1))
  }
print(sum_error)
print(sum_error1)
print(sum_errortrain)
?rpart
library(rpart)
library(rpart.plot)
#hold-out+ cart
cartmodel_holdout<-rpart(LOSS~.,data=trainset,method = "class")
rpart.plot(cartmodel_holdout,main="CART(Hold-out CV)",cex=0.7)
#prediction(错误率0.138647019) trainset (0.129236071)
cartpre_holdout<-predict(cartmodel_holdout,testset)
cartpre_holdout<-ifelse(cartpre_holdout[,2]>0.5,1,0)
table(cartpre_holdout,testset$LOSS)
cartpre_holdout
table(cartpre_holdout)
cartpre_holdouttrain<-predict(cartmodel_holdout,trainset)
cartpre_holdouttrain<-ifelse(cartpre_holdouttrain[,2]>0.5,1,0)
table(cartpre_holdouttrain,trainset$LOSS)
save.image()
savehistory()
q()
library(ggplot2)
?ggplot
?geom_text
CONTRACT.count<-data.frame(read.table("clipboard",header = T))
summary(CONTRACT.count)
library(ggplot2)
#画图 添加数据失败
table(CONTRACT.count$CONTRACT,CONTRACT.count$count)
CONTRACT.count$CONTRACT<-as.factor(CONTRACT.count$CONTRACT)
ggplot(CONTRACT.count,aes(x=CONTRACT,y=count,fill=LOSS))+geom_bar(stat = "identity",position = "dodge")
+geom_text(aes(x=CONTRACT,fill=LOSS,label=count))
savehistory()
save.image()
q()
install.packages("OIsurv")

library(OIsurv)

data(tongue)
attach(tongue)
my.surv<-Surv(time[type==1], delta[type==1])
my.fit<-survfit(my.surv~1)   #Kaplan-Meier
summary(my.fit)
plot(my.fit)
#比较type=1和type=2这两个组的存活函数
my.fit1<-survfit(Surv(time,delta)~type)
plot(my.fit1)

#计算风险函数
H.hat<--log(my.fit$surv)
H.hat<-c(H.hat, tail(H.hat,1))

print(my.fit, print.rmean=TRUE)  

#检验两个存活函数是否有区别  
survdiff(Surv(time, delta) ~ type) # output omitted

detach(tongue)
?Surv
savehistory()
q()
#survival analysis
library(survival)
surv<-survfit(Surv(A$DURATION,A$LOSS==1)~1)
summary(surv)
plot(surv)
surv_EXPRODUCTS<-survfit(Surv(A$DURATION,A$LOSS==1)~A$EXPRODUCTS)
opar<-par(no.readonly = T)
plot(surv_EXPRODUCTS,lty=c(1,2),col=c("red","blue"))
     legend("bottomleft",c("0","1"),title="EXPRODUCTS",lty=c(1,2),col=c("red","blue"))

par(opar)
plot()=c("red","blue"),main="是否签订合约对应的生存函数")
plot(surv_CHANGE,col=c("red","blue"),main="是否改变业务对应的生存函数")
plot(surv_FEE,col=c("red","blue"),main="不同套餐对应的生存函数")
summary(surv_EXPRODUCTS)
coxfit<-coxph(Surv(DURATION,as.numeric(LOSS)==2)~
                FEE+EXTIME+EXDATA+CHANGE+CONTRACT+EXPRODUCTS+GROUP,data=A)
summary(coxfit)
coxfit
savehistory()
save.image()
library(survival)
library(survminer)
library(xlsx)
D<-data.frame(read.xlsx2(file="C:\\Users\\GuoLY\\Desktop\\毕业论文\\CUSTOMERLOSS02.xlsx",sheetName = 1))
coxfit<-coxph(Surv(DURATION,LOSS==1)~
                FEE+EXTIME+EXDATA+CHANGE+CONTRACT+EXPRODUCTS+GROUP,data=D)
D$LOSS<-as.numeric(as.character(D$LOSS))
D$DURATION<-as.numeric(as.character(D$DURATION))
D$EXTIME<-as.numeric(as.character(D$EXTIME))
D$EXDATA<-as.numeric(as.character(D$EXDATA))
summary(coxfit)
?predict.coxph
predict(coxfit,D,type="response")
summary(predict(coxfit,D,type="response"))
#检验
install.packages("mfp")
?mfp
library(mfp)
f<-mfp(Surv(DURATION,LOSS==1)~
         FEE+EXTIME+EXDATA+CHANGE+CONTRACT+EXPRODUCTS+GROUP,family = cox,data=D)
summary(f)
#模型诊断 PH假定
testph<-cox.zph(coxfit)
testph
S<-function(t)
{m<-t/2
return(m)
}
predict(S,2)
S(t=2)
plot(survfit(coxfit,D))
attach(A)
hist(EXTIME,breaks = seq (-3000,5000,100))
hist(EXDATA,breaks = seq (-2200,2600,100))
user<-c(54730.60,64124.50,74721.40,85900.30,98625.30,111215.50,122911.30,128609.30,127139.70,132193.40)
year<-c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)
plot(year,user,type="b",cex.lab=1.5,cex.axis=1.5)
?skewness
?e1071
?Fbasics
library(e1071)
library(fBasics)
skewness(A$EXTIME)
skewness(A$EXDATA)
?skewness
??skewness
l<-glm(LOSS~.,data=P,family=binomial())
summary(l)
library(ggplot2)
ggplot(data=A,aes(x=CONTRACT,fill=LOSS))+geom_bar(stat="count",position = "dodge")+scale_fill_manual(values = c("gray","black"))
ggplot(data=A,aes(x=GROUP,fill=LOSS))+geom_bar(stat="count",position = "dodge")+scale_fill_manual(values = c("gray","black"))
?ggplot
?scale_fill_brewer
?scale_fill_manual
savehistory()
save.image()
q()
