telecom<-read.csv("telecomfinal.csv")
names(telecom)
setwd("C:/JigBD0206")
class(telecom)
names(telecom)
summary(telecom)
telecom<-telecom[,c(1,32,65,12,37,49,29,10,15,21,39,60,31,25,58,5,14,70,73,74,22,43)]
head(telecom)
str(telecom)

## 1 inspecting mou_Mean
quantile(telecom$mou_Mean,p=c(1:100)/100,na.rm = TRUE)
library(dplyr)
index_mou_na<-which(is.na(telecom$mou_Mean))
sum(length(index_mou_na))
##only 181 records with NA so we will delete
telecom<-telecom[-index_mou_na,]
length(telecom$churn)
telecom$mou_ntile<-ntile(telecom$mou_Mean,10)
table(telecom$mou_ntile)
churn_by_mou_mean<-aggregate(telecom$churn,by=list(telecom$mou_ntile),mean)
names(churn_by_mou_mean)[2]<-"%churn"
plot(churn_by_mou_mean)
## churn declines with increasing decile groups of mou_Mean
## significant for modelling

## 2 inspecting months
summary(telecom$months)
telecom$months_ntile<-ntile(telecom$months,5)
table(telecom$months_ntile)
churn_by_months<-aggregate(telecom$churn,by=list(telecom$months_ntile),mean)
names(churn_by_months)[2]<-"%churn"
## no consistent trend of months of usage and churn
##dont include for modelling

##inspecting callwait_Mean
attach(telecom)
names(telecom)
summary(callwait_Mean)
quantile(telecom$callwait_Mean,p=c(1:100)/100,na.rm = TRUE)
telecom$callwait_Mean_ntile<-ntile(telecom$callwait_Mean,10)
churn_by_callwait_Mean<-aggregate(telecom$churn,by=list(telecom$callwait_Mean_ntile),mean)
names(churn_by_callwait_Mean)[2]<-"%churn"
plot(churn_by_callwait_Mean)
## lowest 20% of callwait mean show a very high churn rate (counter-intuitive), however the churn rate increases with increase in call wait mean time after the lowest 40% of records
## include for modelling
## call wait beyond .33 leads to churn and plateaus after it reaches .66

names(telecom)

## inspecting change_mou
summary(change_mou)
index_chng_mou_na<-which(is.na(telecom$change_mou))
sum(length(index_chng_mou_na))
##233 records with NA - small no so delete
telecom<-telecom[-index_chng_mou_na,]
quantile(telecom$change_mou,p=c(1:100)/100,na.rm = TRUE)
telecom$change_mou_ntile<-ntile(telecom$change_mou,20)
##deciles from 1 to 10 are ordered by highest drop in mou (-ve) to lowest
churn_by_change_mou<-aggregate(telecom$churn,by=list(telecom$change_mou_ntile),mean)
names(churn_by_change_mou)[2]<-"%churn"
plot(churn_by_change_mou)
## not a very clear trend but the churn rate reduces as the drop in mou reduces
## include for modelling

attach(telecom)
## inspecting blck_dat_Mean
summary(blck_dat_Mean)
quantile(blck_dat_Mean,p=c(1:100)/100,na.rm = TRUE)
## 98% of customers do not face call data block
index2<-filter(telecom,blck_dat_Mean>0)
dim(index2)
table(index2$churn)
##only about 20% of the customers, with >0 call block mean, churned >> shows no correlation between blck_dat_Mean and churn

## inspecting mean # of cust care calls
names(telecom)
attach(telecom)
summary(custcare_Mean)
quantile(custcare_Mean,p=c(1:100)/100,na.rm = TRUE)
library(dplyr)
telecom$custcare_Mean_ntile<-ntile(telecom$custcare_Mean,10)
churn_by_custcare_mean<-aggregate(telecom$churn,by=list(telecom$custcare_Mean_ntile),mean)
names(churn_by_custcare_mean)[2]<-"%churn"
plot(churn_by_custcare_mean)
churn_by_custcare_mean
## 1st group represents a subset of data that has very high churn, 3rd to 5th group no churn


## Inspecting overage minutes
summary(telecom$ovrmou_Mean)
quantile(ovrmou_Mean,p=c(1:100)/100,na.rm = TRUE)
library(dplyr)
telecom$ovrmou_Mean_ntile<-ntile(telecom$ovrmou_Mean,10)
churn_by_ovrmou<-aggregate(telecom$churn,by=list(telecom$ovrmou_Mean_ntile),mean)
names(churn_by_ovrmou)[2]<-"%churn"
plot(churn_by_ovrmou)
## churn increases wit increase in ovrmou_Mean for ovrmou>0
## include for modelling


#3inspecting rev_Mean
names(telecom)
summary(telecom$rev_Mean)
attach(telecom)
telecom$rev_Mean_ntile<-ntile(telecom$rev_Mean,10)
churn_by_rev_mean<-aggregate(telecom$churn,by=list(telecom$rev_Mean_ntile),mean)
names(churn_by_rev_mean)[2]<-"%churn"
plot(churn_by_rev_mean)
## churn has no clear correlation with rev_mean 
## ignore for glm

names(telecom)
##inspecting dataoverage 
summary(telecom$datovr_Mean)
quantile(telecom$datovr_Mean,p=c(1:100)/100,na.rm = TRUE)
attach(telecom)
telecom$datovr_Mean_ntile<-ntile(telecom$datovr_Mean,10)
churn_by_datovr_mean<-aggregate(telecom$churn,by=list(telecom$datovr_Mean_ntile),mean)
names(churn_by_datovr_mean)[2]<-"%churn"
plot(churn_by_datovr_mean)
## top 20% of data overage customers are susceptible to churn but no difference in value between that group and the 60% of records
## no inference can be drawn from this
## ignore for glm modelling

##inspecting drop data calls
summary(telecom$drop_dat_Mean)
quantile(telecom$drop_dat_Mean,p=c(1:100)/100,na.rm = TRUE)
attach(telecom)
a<-filter(telecom,drop_dat_Mean<.33&churn==1)
dim(a)
b<-filter(telecom,drop_dat_Mean<.33)
dim(b)
percent<-nrow(a)/nrow(b)
c<-filter(telecom,drop_dat_Mean>.33&churn==1)
dim(c)
d<-filter(telecom,drop_dat_Mean>.33)
dim(d)
percent_1<-nrow(c)/nrow(d)
##there is a very weak correlation between drop_dat_Mean and churn
#3 very small difference (2.6% POINTS)between customers with mean around 0 and those with mean >.33

##inspecting drop voice calls
summary(telecom$drop_vce_Mean)
quantile(telecom$drop_vce_Mean,p=c(1:100)/100,na.rm = TRUE)
attach(telecom)
telecom$drop_vce_ntile<-ntile(telecom$drop_vce_Mean,20)
churn_by_drop_vce_mean<-aggregate(telecom$churn,by=list(telecom$drop_vce_ntile),mean)
names(churn_by_drop_vce_mean)[2]<-"%churn"
plot(churn_by_drop_vce_mean)
##broken into higher # of bins to study the plot better
## no clear trend
##ignore for glm

names(telecom)
summary(age1)
telecom$age1
index_age<-which(is.na(telecom$age1))
sum(length(index_age))
##remove 1144 NA records
telecom<-telecom[-index_age,]
## creating a dummy dataset to inspect age
index_age0<-which(telecom$age1==0)
sum(length(index_age0))
telecom_age_d<-telecom[-index_age0,]
###
telecom_age_d$age1_ntile<-ntile(telecom_age_d$age1,10)
churn_by_age1<-aggregate(telecom_age_d$churn,by=list(telecom_age_d$age1_ntile),mean)
names(churn_by_age1)[2]<-"%churn"
plot(churn_by_age1)
quantile(telecom$age1,p=c(1:100)/100,na.rm = TRUE)
##no clear trend, ignore for glm


names(telecom)
summary(telecom$crclscod)
telecom$crdtscore_ntile<-ntile(telecom$crclscod,10)
churn_by_crdtscore<-aggregate(telecom$churn,by=list(telecom$crdtscore_ntile),mean)
names(churn_by_crdtscore)[2]<-"%churn"
plot(churn_by_crdtscore)
## no clear correlation between credit score and churn 
##but unsually high churn for people in credit score group 3
## no churn in group 4 and 5
## ignore for glm

##inspecting ACTVSUBS
summary(telecom$actvsubs)
unique(telecom$actvsubs)
telecom$actvsubs_ntile<-ntile(telecom$actvsubs,20)
churn_by_actvsubs<-aggregate(telecom$churn,by=list(telecom$actvsubs_ntile),mean)
names(churn_by_actvsubs)[2]<-"%churn"
plot(churn_by_actvsubs)
quantile(telecom$actvsubs,p=c(1:100)/100,na.rm = TRUE)
## no clear trend
## ignore for glm

## inspecting asl_flag
summary((telecom$asl_flag))
churn_by_asl_flag<-aggregate(telecom$churn,by=list(telecom$asl_flag),mean)
plot(churn_by_asl_flag)
churn_by_asl_flag
##very clear trend between asl_flag and churn
##include for glm

##inspecting income
summary(telecom$income)
unique(telecom$income)
index_income<-which(is.na(telecom$income))
sum(length(index_income))
telecom_income_d<-telecom[-index_income,]
unique(telecom_income_d$income)
plot(telecom_income_d$income,telecom_income_d$churn)
churn_by_income<-aggregate(telecom_income_d$churn,by=list(telecom_income_d$income),mean)
## no clear trend, ignore

## inspecting marital status
summary(telecom$marital)
plot(telecom$marital,telecom$churn)
churn_by_marital<-aggregate(telecom$churn,by=list(telecom$marital),mean)
churn_by_marital
## absence of any trend, ignore

##inspecting occupation of 1st household member
unique(telecom$occu1)
plot(telecom$occu1,telecom$churn)
churn_by_occu1<-aggregate(telecom$churn,by=list(telecom$occu1),mean)
churn_by_occu1
telecom$occu1_far_rel_slfempCler_slfempStu<-ifelse(telecom$occu1==9|telecom$occu1=="B"|telecom$occu1=="G"|telecom$occu1=="I",1,0)
telecom$occu1_others<-ifelse(telecom$occu1=="Z",1,0)
## farmers have no churn
##religious and self employed clerical and self employed students have low churns
## people in other category have a much higher churn than avg

## inspecting avg6mou
summary(telecom$avg6mou)
index_avg6mou<-which(is.na(telecom$avg6mou))
sum(length(index_avg6mou))
telecom_avg6mou_d<-telecom[-index_avg6mou,]
telecom_avg6mou_d$avg6mou_ntile<-ntile(telecom_avg6mou_d$avg6mou,10)
churn_by_avg6mou<-aggregate(telecom_avg6mou_d$churn,by=list(telecom_avg6mou_d$avg6mou_ntile),mean)
plot(churn_by_avg6mou)
telecom<-telecom[-index_avg6mou,]
telecom$avg6mou_ntile<-ntile(telecom$avg6mou,10)
## clear trend of negative correlation
## include for glm

names(telecom)
summary(telecom)
telecom_1<-telecom[,c(12,23,24,25,26,27,28,29,30,2)]
set.seed(1100)
s<-sample(nrow(telecom_1),.7*nrow(telecom_1))
train<-telecom_1[s,]
test<-telecom_1[-s,]
model1<-glm(churn~.,data = train,family = binomial())
summary(model1)
library(car)
vif(model1)
## dropping mou_Mean due to multicollinearity and also high p value
model1_rev<-glm(churn~callwait_Mean_ntile+change_mou_ntile+custcare_Mean_ntile+ovrmou_Mean_ntile+occu1_far_rel_slfempCler_slfempStu+avg6mou_ntile+occu1_others+asl_flag,data = train,family = binomial())
vif(model1_rev)
##no multicollinearity among selected idvs after dropping mou_Mean
summary(model1_rev)
## dropping dummy variables of occu1
names(telecom_1)
model2<-glm(churn~callwait_Mean_ntile+change_mou_ntile+custcare_Mean_ntile+ovrmou_Mean_ntile+avg6mou_ntile+asl_flag,data = train,family = binomial())
vif(model2)
summary(model2)

##Model accuracy tests

predicted<-model2$fitted.values
head(predicted,15)
head(train$churn,15)
## 100% accuracy
length(predicted)

## determine auc
library(ROCR)
pred<-prediction(predicted,train$churn)
perf<-performance(pred,"tpr","fpr")
plot(perf)
auc<-performance(pred,"auc")
auc@y.values
##auc = 0.7

##determine confusion matrix
predconfu<-ifelse(predicted>0.5,"1","0")
table(predconfu,train$churn)
## shows 80% accuracy

predconfu_1<-ifelse(predicted>0.7,"1","0")
table(predconfu_1,train$churn)
##775% accuracy based on y-value of .7 in auc

## testing the model on test data
validate<-predict(model2,type = "response",newdata = test)
summary(validate)
head(validate,10)
head(test$churn,10)

model_test<-glm(churn~callwait_Mean_ntile+change_mou_ntile+custcare_Mean_ntile+ovrmou_Mean_ntile+avg6mou_ntile+asl_flag,data = test,family = binomial())
predicted_test<-model_test$fitted.values
length(pred)
pred_test<-prediction(predicted_test,test$churn)
perf_test<-performance(pred_test,"tpr","fpr")
plot(perf_test)
auc_test<-performance(pred_test,"auc")
auc_test@y.values
## auc value is .71 with test data

summary(model2)
exp(confint(model2))

## Q1 - 5 most important factors driving churn seem to be - 
##  avg6mou
## asl_flag
## callwait_Mean
## change_mou
## ovrmou_Mean

##< above selected based on estimate value, hence extent of impact, from a list of statistically significant idvs

# Q2
## billing is an imp factor as observed by the significance of overage of mins of usage
## service quality - yes, service quality proxy of callwait mean time negatively impacts the churn
## network quality - none of the proxies of network quality showed a significant relation with churn rate so network quality is not a strong indicator of churn
## data usage  - no significant relation between data usage and churn, however churn is impacted by overall usage of voice and data together
## in essence, billing/cost and service quality impact churn more meaningfully than network quality and/or data usage

## Q.3
## Rate plan migration - yes, there should be a focus on customers with an overage of mou for optimal bill plan, to reduce churn
## contd...churn goes up as the overage of mou goes beyond 0...based on budget, all or partial set of such customers, based on extent of overage linked churn, can be targeted

## Q.4

## Proactive retention plan
## a. Migrate customers with positive overage (>0) of mou to an optimal billing plan 
## b. Focus on customers with a high avg6mou - these are valuable customers
## b.1. Also, devise promotional strategies to lift mou usage as a tool to stem churn
tel_asl_y<-filter(tel_sub,asl_flag=="Y")
summary(tel_asl_y)
## c.ASL_FLAG - customers with ASL (i.e., prepaid cust) churn much less and produce much higher rev : focus on these and also devise plans for higher uptake of prepaid plans
##.d.A plan to address b1 will reduce the drop in mou, which is a strong lead indicator of churn

## Q.5
## CHURN, REV
## Ans given below (line 376-392) and (line 394 - 409), considering we can target only 20% of the customers in our data

tel<-read.csv("telecomfinal.csv")
attach(tel)
names(tel)
tel_sub<-tel[,c("churn","ovrmou_Mean","crclscod","occu1","callwait_Mean","change_mou","avg6mou","custcare_Mean","asl_flag","rev_Mean")]
summary(tel_sub)
## mean rev = 59
## churn rate = 23.9%
## ovrmou = 40.19
## callwait mean = 1.87
## change_mou = -9.18
#3 avg6mou =521.4
## custcare 1.88
## asl - y = 9984

attach(tel_sub)
library(dplyr)
tel_1<-filter(tel_sub,ovrmou_Mean > 0 & asl_flag=="N")
summary(tel_1)
unique(tel_1$occu1)
## mean rev = 69.83
## churn rate = 25.75%
## ovrmou = 69.9
## callwait mean = 2.4
## change_mou = -3.1
#3 avg6mou = 627
## custcare 1.8
## 
#3 Ans 5 - part 1 (line 376-392)
tel_sub$crclscod_ntile<-ntile(tel_sub$crclscod,10)
table(tel_sub$crclscod_ntile)
tel_2<-filter(tel_sub,crclscod_ntile==3) ## credit score =AA
summary(tel_2)
dim(tel_2)
tel_2_a<-filter(tel_2,rev_Mean>59)
## mean rev = 54.77
## churn rate = 63.8%
## ovrmou = 35
## callwait mean = 1.2
## change_mou = -9.4
## avg6mou = 422

## 41% OF TOTAL CHURN FROM 10% OF RECORDS ## these customers have a mean rev abt 4 pts less than popu avg rev so this sample is meaningful from a rev standpoint
## these customers should be focused on for churn prevention and also increase in mou - as these customers imply a higher
## contd....than avg per mou rev (mean rev here is < 10% lower than popu mean rev but avg6mou is ~ 20% lower than that of popu)

## Ans 5 - part 2 (Line 394 - 409)

## inspecting the churn profile of top customers by rev

quantile(tel_sub$rev_Mean,p=c(1:100)/100,na.rm = TRUE)

library(dplyr)
tel_rev<-filter(tel_sub,rev_Mean>80)
summary(tel_rev)
##top 20 percentile customers have rev mean > double of population rev mean
## they also have a > 2x of population overage of mou
## their avg mou for past 6 months is also> 2x that of population
## these customers should be targeted for both retention by proactively optimizing their bill plan to reduce their overage and also to promote
##...contd...more upsell through plans to increase their overall mou (which they have a high propensity for)
## this subset of customer records is very different in profile to the earlier one with ~ 64% churn rate - if our budget is limited
## then we may choose 100% of records in tel_2 (10% of population size) and top 50% of records from object tel_rev


