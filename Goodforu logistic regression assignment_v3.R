library(gains)
library(dplyr)
library(irr)
library(caret)
getwd()
good<-read.csv("goodforu-class12.csv")
table(good$X23)
good1<-good[,c(1,3,10,17,24,31)]
summary(good1)
str(good1)
good1$good_badA<-ifelse(good1$X23>=6,"1","0")
table(good1$good_badA)
str(good1)
good1$good_badA<- as.numeric(good1$good_badA)
## (why do we need good/bad variable as numeric and not factor?)
names(good1)
s<-sample(nrow(good1),0.7*nrow(good1))
train<-good1[s,]
test<-good1[-s,]
head(train)
fit.full<-glm(good_badA~X2+X9+X16+X30,data = train,family = binomial())
summary(fit.full)
confint(fit.full)
names(fit.full)
predicted_fit_values<-fit.full$fitted.values
head(predicted_fit_values)
head(train)
pred_conf<-ifelse(predicted_fit_values>0.5,1,0)
## if instead of 0,1, we claissify good bad as G or B in pred_conf, it runs an error in ROC model (has more than 2 factors) - why?
table(pred_conf,train$good_badA)
library(ROCR)
pred_roc<-prediction(pred_conf,train$good_badA)
unique(train$good_badA)
unique(pred_conf)
perf<-performance(pred_roc,"tpr","fpr")
plot(perf)
auc<-performance(pred_roc,"auc")
auc<-unlist(slot(auc,"y.values"))
predict_test<-predict(fit.full,newdata=test, type = "response")
head(predict_test,15)
head(test,15)
## only 2 of 4 true correctly predicted by predict_test for first 15 records


##summary reveals (inferred from line 23):
Brand Perception of A is impacted as follows:
-negatively by presence of farm grown ingredients like potato, corn or wheat
-negatively by presence of zero grams trans fat
-negatively by presence of natural oil
-positively by minimal processing,i.e., less processed, higher the brand perception




