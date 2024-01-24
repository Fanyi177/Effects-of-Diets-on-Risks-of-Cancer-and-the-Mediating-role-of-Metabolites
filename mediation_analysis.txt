#####Dietary and NMR data were combined####
#
library(data.table)
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
colnames(UKBdata2)
UKBdata3<-subset(UKBdata2,select=c(1,7:22,59,168,219))
#
NMR9<-fread("E:/deskbook/NMR9.csv",header=T)
colnames(NMR9)
NMR10<-subset(NMR9,select=c(1,42,53,60,76,77,80,85,86,135,166))
colnames(NMR10)
#common eid
common_eids <- intersect(UKBdata3$eid, NMR10$eid)
matched_data1 <- UKBdata3[UKBdata3$eid %in% common_eids,]
matched_data2 <- NMR10[NMR10$eid %in% common_eids,]
# extract UKBdata3 samples without common_eids
UKBdata3_no_common_eids <- UKBdata3[!(UKBdata3$eid %in% common_eids),]
# extract NMR10 samples without common_eids
NMR10_no_common_eids <- NMR10[!(NMR10$eid %in% common_eids),]
#####
UKBdata_no_common<-merge(UKBdata3_no_common_eids,NMR10_no_common_eids,by="eid",all.x = T)
##### 85669obs
UKBdata4<-merge(matched_data1,matched_data2,by="eid",all.x = T)
UKBdata4$energy<-UKBdata4$`100002_1`/4.184
UKBdata4$MIND_sum1_3group <- cut(UKBdata4$MIND_sum1, 
                                   breaks = quantile(UKBdata4$MIND_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata4$MEDAS_sum1_3group <- cut(UKBdata4$MEDAS_sum1, 
                                 breaks = quantile(UKBdata4$MEDAS_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata4$status<-ifelse(UKBdata4$pan_cancer==0,0,1)
colnames(UKBdata4)
#UKBdata4
library(tidyverse)
write_excel_csv(UKBdata4,"E:/deskbook/UKBdata4.csv")
UKBdata4<-fread("E:/deskbook/UKBdata4.csv",header=T)
#import UKBdata4#####
library(data.table)
UKBdata4<-fread("E:/deskbook/UKBdata4.csv",header=T)
colnames(UKBdata4)
vars<-colnames(UKBdata4[,c(4:25,33,26,28)])
catvars<-colnames(UKBdata4[,c(4:6,9,12:13,25)])
vars2<-colnames(UKBdata4[,c(8,10,11,14:24,33,26,28)])
library(tableone)
tableS12<-CreateTableOne(vars=vars,strata="status",data=UKBdata4,
                         factorVars =catvars,addOverall = T)
tableS12_1<-print(tableS12, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,
                  nonnormal=vars2)
write.csv(tableS12_1,"D:/deskbook/tableS12.csv")
#The association between diet and pancancer was analyzed
table(UKBdata4$MIND_sum1_3group)
table(UKBdata4$MIND_sum1_3group,UKBdata4$status)
table(UKBdata4$MEDAS_sum1_3group)
table(UKBdata4$MEDAS_sum1_3group,UKBdata4$status)
library(survival)
#Model 3
colnames(UKBdata4)
result<-c()
for (i in UKBdata4[,c(19,18)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata4)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata4[,c(32,31)]){
  UKBdata4$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata4)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#trend test
tapply(UKBdata4$MIND_sum1,UKBdata4$MIND_sum1_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})

UKBdata4$MIND_sum1_3group_p<-ifelse(UKBdata4$MIND_sum1_3group==1,4.5,ifelse(
  UKBdata4$MIND_sum1_3group==2,6.5,8))

tapply(UKBdata4$MEDAS_sum1,UKBdata4$MEDAS_sum1_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})
UKBdata4$MEDAS_sum1_3group_p<-ifelse(UKBdata4$MEDAS_sum1_3group==1,4,ifelse(
  UKBdata4$MEDAS_sum1_3group==2,5,7))
#model 3
result<-c()
for (i in UKBdata4[,41:42]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP,data=UKBdata4)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#export data
#FigureS5A
library(grid)
library("forestploter")
figure5<-read.csv("E:/deskbook/Figure5.csv",sep=",",header=T)
colnames(figure5)
figure5$Diet.Adherence<-ifelse(is.na(figure5$N),
                               figure5$Diet.Adherence,
                               paste0("  ",figure5$Diet.Adherence))
figure5$N<-ifelse(is.na(figure5$N),"",figure5$N)
figure5$Inicidence<-ifelse(is.na(figure5$Inicidence),"",figure5$Inicidence)
figure5$se<-(log(figure5$UCL)-log(figure5$HR))/1.96
figure5$se<-ifelse(figure5$se==0,NA,figure5$se)
figure5$` `<-paste(rep(' ',20),collapse = ' ')
figure5$`HR (95%CI)`<-ifelse(is.na(figure5$se),"",
                             paste0(figure5$HR," (",figure5$LCL,"-",figure5$UCL,")"))
p<-forest(figure5[,c(1:3,9:10)],
          est=figure5$HR,
          lower=figure5$LCL,
          upper = figure5$UCL,
          ci_column=4,
          ref_line = 1,xlim=c(0.7,1.1),
          ticks_at = c(0.7,0.8,0.9,1.0,1.1))
g<-edit_plot(p,row=c(1,6),
             gp=gpar(fontface="bold"))
#To make associations between metabolites and MIND and MEDAS####
colnames(UKBdata4)
result<-c()
for (i in UKBdata4[,21:30]){
  med.fiti<-lm(i~MIND_sum1+energy+sex+education+
                 famaliy_history+Townsend_deprivation_index2+household_income+
                 smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                 BMI+whr+SBP,data=UKBdata4)
  result<-cbind(result,c(summary(med.fiti)$coe[2,c(1:2,4)]))
}
#MEDAS
result1<-c()
for (i in UKBdata4[,21:30]){
  med.fiti<-lm(i~MEDAS_sum1+energy+sex+education+
                 famaliy_history+Townsend_deprivation_index2+household_income+
                 smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                 BMI+whr+SBP,data=UKBdata4)
  result1<-cbind(result1,c(summary(med.fiti)$coe[2,c(1:2,4)]))
}
write.csv(result,"E:/deskbook/result.csv",row.names = F)
write.csv(result1,"E:/deskbook/result1.csv",row.names = F)
#fIGURE5b(Online software)####
#Making metabolites correlates with outcome####
library(survival)
colnames(UKBdata4)
result<-c()
for (i in UKBdata4[,c(21:30)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata4)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
write.csv(result,"E:/deskbook/supplementtable13.csv")

#
library(dplyr)
supplementtable13<-read.csv("E:/deskbook/supplementtable13.csv")
#
supplementtable13<-supplementtable13[order(supplementtable13$P),]
supplementtable13$BH<-p.adjust(supplementtable13$P,method="BH")
#
write.csv(supplementtable13,"E:/deskbook/supplementtable13.csv")
#
library(grid)
library("forestploter")
figure5c<-read.csv("E:/deskbook/supplementtable13.csv",sep=",",header=T)
#
figure5c$` `<-paste(rep(' ',20),collapse = ' ')
#
figure5c$`HR (95%CI)`<-paste0(round(figure5c$HR,3)," (",round(figure5c$LCL,3),"-",round(figure5c$UCL,3),")")
p<-forest(figure5c[,c(1,7:8,5:6)],
          est=figure5c$HR,
          lower=figure5c$LCL,
          upper = figure5c$UCL,
          ci_column=2,
          ref_line = 1,xlim=c(0.8,1.1),
          ticks_at = c(0.8,0.85,0.90,0.95,1.0,1.1))
#mediation analysis####
library(data.table)
library(survival)
library(mma)
UKBdata4<-fread("E:/deskbook/UKBdata4.csv",header=T)
colnames(UKBdata4)
time1<-UKBdata4$recruitment_age
event1<-UKBdata4$status
y1<-Surv(UKBdata4$recruitment_age,event1)
#MIND
fit1<-coxph(Surv(recruitment_age,status)~factor(MIND_sum1_3group)+energy+factor(sex)+factor(education)+
              factor(famaliy_history)+factor(Townsend_deprivation_index2)+factor(household_income)+
              factor(smoking_status)+factor(alochol_drinking)+weekly_activity+sleep_duration+
              BMI+whr+SBP,data=UKBdata4)
summary(fit1)#energy,sex,Townsend_deprivation_index2,household_income,smoking_status,alochol_drinkin,weekly_activity,BMI,whr,SBP
fit2<-glm(MIND_sum1_3group~energy+factor(sex)+factor(education)+
            factor(famaliy_history)+factor(Townsend_deprivation_index2)+factor(household_income)+
            factor(smoking_status)+factor(alochol_drinking)+weekly_activity+sleep_duration+
            BMI+whr+SBP,data=UKBdata4)
summary(fit2)#
#MEDAS
fit3<-coxph(Surv(recruitment_age,status)~factor(MEDAS_sum1_3group)+energy+factor(sex)+factor(education)+
              factor(famaliy_history)+factor(Townsend_deprivation_index2)+factor(household_income)+
              factor(smoking_status)+factor(alochol_drinking)+weekly_activity+sleep_duration+
              BMI+whr+SBP,data=UKBdata4)
summary(fit3)#energy,sex,Townsend_deprivation_index2,household_income,smoking_status,alochol_drinkin,weekly_activity,BMI,whr,SBP
fit4<-glm(MEDAS_sum1_3group~energy+factor(sex)+factor(education)+
            factor(famaliy_history)+factor(Townsend_deprivation_index2)+factor(household_income)+
            factor(smoking_status)+factor(alochol_drinking)+weekly_activity+sleep_duration+
            BMI+whr+SBP,data=UKBdata4)
summary(fit4)#
colnames(UKBdata4)
covs<-subset(UKBdata4,select = c(4:6,8:17,31))
x1<-UKBdata4$MIND_sum1
x2<-UKBdata4$MIND_sum1_3group
x3<-UKBdata4$MEDAS_sum1
x4<-UKBdata4$MEDAS_sum1_3group
setwd("E:/deskbook/")
sink("mma_results_summary.txt")
for (i in  c(21:26,28:30)) {
  set.seed(55)
  metabolites <- subset(UKBdata4, select = i)
  m <- as.matrix(metabolites)
  colnames(m) <- paste0("m", c(1:ncol(m)))
  data_org<-data.org(m,y1,pred =x1,mediator = "m1",cova=covs)
  mma_result1 <- mma(m, y1, pred =x1, mediator = "m1", n = 10, n2 = 5, type = "lp",cova=covs)
  cat("Summary for mma_risk1_", i, "_MIND:\n")
  print(summary(mma_result1))
  data_org<-data.org(m,y1,pred =x2,mediator = "m1",cova=covs)
  mma_result2 <- mma(m, y1, pred =x2, mediator = "m1", n = 10, n2 = 5, type = "lp",cova=covs)
  cat("Summary for mma_risk2_", i, "_MIND:\n")
  print(summary(mma_result2))
}
for (i in c(21:26,28:30)) {
  set.seed(55)
  metabolites <- subset(UKBdata4, select = i)
  m <- as.matrix(metabolites)
  colnames(m) <- paste0("m", c(1:ncol(m)))
  data_org<-data.org(m,y1,pred =x3,mediator = "m1",cova=covs2)
  mma_result3 <- mma(m, y1, pred =x3, mediator = "m1", n = 10, n2 = 5, type = "lp",cova=covs)
  cat("Summary for mma_risk1_", i, "_MEDAS:\n")
  print(summary(mma_result1))
  data_org<-data.org(m,y1,pred =x4,mediator = "m1",cova=covs2)
  mma_result4 <- mma(m, y1, pred =x4, mediator = "m1", n = 10, n2 = 5, type = "lp",cova=covs)
  cat("Summary for mma_risk2_", i, "_MEDAS:\n")
  print(summary(mma_result4))
}
#multiple mediation
#MIND
sink("mulitiple_mma_results_summary.txt")
colnames(UKBdata4)
set.seed(55)
metabolites<-subset(UKBdata4,select = c(21:30))
m<-as.matrix(metabolites)
colnames(m)<-paste0("m",c(1:ncol(m)))
data_org<-data.org(m,y1,pred =x1,mediator =1:ncol(m),cova=covs1)
mma_risk_MIND1<-mma(m,y1,pred=x1,mediator=1:ncol(m),n=10,n2=5,type="lp",cova=covs)
summary(mma_risk_MIND1)
data_org<-data.org(m,y1,pred =x2,mediator =1:ncol(m),cova=covs1)
mma_risk_MIND2<-mma(m,y1,pred=x2,mediator=1:ncol(m),n=10,n2=5,type="lp",cova=covs)
summary(mma_risk_MIND2)
#MEDAS
metabolites<-subset(UKBdata4,select = c(21:30))
m<-as.matrix(metabolites)
colnames(m)<-paste0("m",c(1:ncol(m)))
data_org<-data.org(m,y1,pred =x3,mediator =1:ncol(m),cova=covs2)
mma_risk_MEDAS1<-mma(m,y1,pred=x3,mediator=1:ncol(m),n=10,n2=5,type="lp",cova=covs)
summary(mma_risk_MEDAS1)
data_org<-data.org(m,y1,pred =x4,mediator =1:ncol(m),cova=covs2)
mma_risk_MEDAS2<-mma(m,y1,pred=x4,mediator=1:ncol(m),n=10,n2=5,type="lp",cova=covs)
summary(mma_risk_MEDAS2)
sink()