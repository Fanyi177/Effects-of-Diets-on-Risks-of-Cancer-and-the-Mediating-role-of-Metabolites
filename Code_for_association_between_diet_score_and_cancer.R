
#import
write_excel_csv(UKBdata2,"E:/deskbook/UKBdata2.csv")
####
rm(list=ls())
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
#
ICD<-fread("E:/deskbook/ICD.csv",header=T)
ICD_suifang<-subset(ICD,select=c(1,35:41))
UKBdata_suifang<-merge(UKBdata2,ICD,all.x = T,by="eid")
table(UKBdata_suifang$`40005-0.0`)
#Supplement Table 1
table(UKBdata2$pan_cancer)
table(UKBdata2$various_cancer)
UKBdata2$various_cancer[UKBdata2$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
UKBdata2$various_cancer[UKBdata2$various_cancer == "throid"] <- "thyroid"
UKBdata2$various_cancer[UKBdata2$various_cancer == "cervix"|UKBdata2$various_cancer == "uterine"] <- "uterus_cervix"
#
table(UKBdata2$pan_cancer)/sum(table(UKBdata2$pan_cancer))*100
table(UKBdata2$various_cancer)/sum(table(UKBdata2$various_cancer))*100
#TABLE 1####
colnames(UKBdata2)
UKBdata2$energy<-UKBdata2$`100002_1`/4.184
vars<-colnames(UKBdata2[,c(9:22,59,168,267)])
catvars<-colnames(UKBdata2[,c(9:11,14,17:18,22)])
vars2<-colnames(UKBdata2[,c(13,15:16,19:21,59,168,267)])
library(tableone)
table1<-CreateTableOne(vars=vars,strata="status",data=UKBdata2,
                       factorVars =catvars,addOverall = T)
table1_1<-print(table1, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,
                nonnormal=vars2)
write.csv(table1_1,"E:/deskbook/table1.csv")
UKB1<-subset(UKBdata2,status==0)
UKB2<-subset(UKBdata2,status==1)
#Preliminary attempt
library(survival)
colnames(UKBdata2)
result<-c()
for (i in UKBdata2[,c(59,91,120,149,168,187,202,217)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+famaliy_history+
                smoking_status+alochol_drinking+Townsend_deprivation_index2,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
#Use the sum of the average 24hffq, sum1, and then try to use several groups
UKBdata2$MIND_sum1_3group <- cut(UKBdata2$MIND_sum1, 
                                 breaks = quantile(UKBdata2$MIND_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
UKBdata2$MIND_sum1_5group <- cut(UKBdata2$MIND_sum1, 
                                 breaks = quantile(UKBdata2$MIND_sum1, probs = c(0, 0.20,0.40,0.60, 0.80, 1)), include.lowest = TRUE, labels = FALSE)#五分位分组
UKBdata2$MIND_sum1_3group<-factor(UKBdata2$MIND_sum1_3group)
UKBdata2$MIND_sum1_5group<-factor(UKBdata2$MIND_sum1_5group)
fit1<-coxph(Surv(recruitment_age,status)~UKBdata2$MIND_sum1_3group+energy+sex+education+famaliy_history+
              smoking_status+alochol_drinking+Townsend_deprivation_index2,data=UKBdata2)
summary(fit1)
fit2<-coxph(Surv(recruitment_age,status)~UKBdata2$MIND_sum1_5group+energy+sex+education+famaliy_history+
              smoking_status+alochol_drinking+Townsend_deprivation_index2,data=UKBdata2)
summary(fit2)
#Supplement Table 5
vars<-colnames(UKBdata2[,c(9:22,59,168,267)])
catvars<-colnames(UKBdata2[,c(9:11,14,17:18,22)])
vars2<-colnames(UKBdata2[,c(13,15:16,19:21,59,168,267)])
Supplement_table5<-CreateTableOne(vars=vars,strata="MIND_sum1_3group",data=UKBdata2,
                                  factorVars =catvars,addOverall = T)
Supplement_table5_1<-print(Supplement_table5, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,
                           nonnormal=vars2)
write.csv(Supplement_table5_1,"E:/deskbook/Supplement_table5_1.csv")
#Supplement Table 6####
UKBdata2$MEDAS_sum1_3group <- cut(UKBdata2$MEDAS_sum1, 
                                  breaks = quantile(UKBdata2$MEDAS_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
Supplement_table6<-CreateTableOne(vars=vars,strata="MEDAS_sum1_3group",data=UKBdata2,
                                  factorVars =catvars,addOverall = T)
Supplement_table6_1<-print(Supplement_table6, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,
                           nonnormal=vars2)
write.csv(Supplement_table6_1,"E:/deskbook/Supplement_table6_1.csv")
#table 2####
table(UKBdata2$MIND_sum1_3group,UKBdata2$MIND_sum1)
table(UKBdata2$MEDAS_sum1_3group,UKBdata2$MEDAS_sum1)
table(UKBdata2$MIND_sum1_3group)
table(UKBdata2$MEDAS_sum1_3group)
table(UKBdata2$MIND_sum1_3group,UKBdata2$status)
table(UKBdata2$MEDAS_sum1_3group,UKBdata2$status)
library(survival)
#Model 1
result<-c()
colnames(UKBdata2)
for (i in UKBdata2[,c(59,168)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
write.csv(result,"E:/deskbook/table2_1.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(268,270)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_2.csv",row.names = F)
summary(fit1)
#Model 2
result<-c()
for (i in UKBdata2[,c(59,168)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
fit1<-coxph(Surv(recruitment_age,status)~MIND_sum1+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
summary(fit1)
fit1<-coxph(Surv(recruitment_age,status)~MEDAS_sum1+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
summary(fit1)
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(268,270)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3
result<-c()
for (i in UKBdata2[,c(59,168)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
fit1<-coxph(Surv(recruitment_age,status)~MIND_sum1+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income+
              smoking_status+alochol_drinking+weekly_activity+
              BMI+whr+SBP,data=UKBdata2)
summary(fit1)
fit1<-coxph(Surv(recruitment_age,status)~MEDAS_sum1+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income+
              smoking_status+alochol_drinking+weekly_activity+
              BMI+whr+SBP,data=UKBdata2)
summary(fit1)
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(268,270)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Trend test
tapply(UKBdata2$MIND_sum1,UKBdata2$MIND_sum1_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})

UKBdata2$MIND_sum1_3group_p<-ifelse(UKBdata2$MIND_sum1_3group==1,4.5,ifelse(
  UKBdata2$MIND_sum1_3group==2,6.5,8))

tapply(UKBdata2$MEDAS_sum1,UKBdata2$MEDAS_sum1_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})
UKBdata2$MEDAS_sum1_3group_p<-ifelse(UKBdata2$MEDAS_sum1_3group==1,4,ifelse(
  UKBdata2$MEDAS_sum1_3group==2,5,7))
#model 1
colnames(UKBdata2)
result<-c()
for (i in UKBdata2[,c(276:277)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#model 2
result<-c()
for (i in UKBdata2[,c(276:277)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#model 3
result<-c()
for (i in UKBdata2[,c(276:277)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#Figure1####
library(grid)
library("forestploter")
figure1<-read.csv("E:/deskbook/Figure1.csv",sep=",",header=T)
colnames(figure1)
figure1$Diet.Adherence<-ifelse(is.na(figure1$N),
                               figure1$Diet.Adherence,
                               paste0("  ",figure1$Diet.Adherence))
figure1$N<-ifelse(is.na(figure1$N),"",figure1$N)
figure1$Inicidence<-ifelse(is.na(figure1$Inicidence),"",figure1$Inicidence)

figure1$`Model 1 (HR [95%CI])`<-paste(rep(' ',20),collapse = ' ')
figure1$` `<-paste(rep(' ',3),collapse = ' ')
figure1$`Model 2 (HR [95%CI])`<-paste(rep(' ',20),collapse = ' ')
figure1$`  `<-paste(rep(' ',3),collapse = ' ')
figure1$`Model 3 (HR [95%CI])`<-paste(rep(' ',20),collapse = ' ')
colnames(figure1)
p<-forest(figure1[,c(1:3,13:17)],
          est=list(figure1$HR1,figure1$HR2,figure1$HR3),
          lower=list(figure1$LCL1,figure1$LCL2,figure1$LCL3),
          upper = list(figure1$UCL1,figure1$UCL2,figure1$UCL3),
          ci_column=c(4,6,8),
          ref_line = 1,xlim=c(0.75,1.04),
          ticks_at = c(0.75,0.8,0.85,0.9,0.95,1.0))
g<-edit_plot(p,row=c(1,6),
             gp=gpar(fontface="bold"))
#Doing various food groups was associated with scores and pancancer####
#MIND
colnames(UKBdata2)
library(survival)
var_list <- colnames(UKBdata2[,c(45:58)])
result<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+
               BMI+whr+SBP,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/Supplement_figure4.csv",row.names = F)
#MEDAS
var_list <- colnames(UKBdata2[,c(157,158,166,160,164,163,167,159,165,162,161)])
result<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+
               BMI+whr+SBP,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/Supplement_figure4.csv",row.names = F)
#Supplement Figure4a
library(grid)
library("forestploter")
Supplemet_figure4a<-read.csv("E:/deskbook/Supplemet_figure4a.csv",sep=",",header=T)
#
Supplemet_figure4a$` `<-paste(rep(' ',20),collapse = ' ')
#
Supplemet_figure4a$`HR (95%CI)`<-paste0(round(Supplemet_figure4a$HR,2)," (",round(Supplemet_figure4a$LCL,2),"-",
                                        round(Supplemet_figure4a$UCL,2),")")
p<-forest(Supplemet_figure4a[,c(1,6:7,5)],
          est=Supplemet_figure4a$HR,
          lower=Supplemet_figure4a$LCL,
          upper = Supplemet_figure4a$UCL,
          ci_column=2,
          ref_line = 1,xlim=c(0.8,1.13),
          ticks_at = c(0.80, 0.90,1.0,1.10))
#Supplement Figure4b
library(grid)
library("forestploter")
Supplemet_figure4b<-read.csv("E:/deskbook/Supplemet_figure4b.csv",sep=",",header=T)
#
Supplemet_figure4b$` `<-paste(rep(' ',20),collapse = ' ')
#
Supplemet_figure4b$`HR (95%CI)`<-paste0(round(Supplemet_figure4b$HR,2)," (",round(Supplemet_figure4b$LCL,2),"-",
                                        round(Supplemet_figure4b$UCL,2),")")
p<-forest(Supplemet_figure4b[,c(1,6:7,5)],
          est=Supplemet_figure4b$HR,
          lower=Supplemet_figure4b$LCL,
          upper = Supplemet_figure4b$UCL,
          ci_column=2,
          ref_line = 1,xlim=c(0.8,1.13),
          ticks_at = c(0.80, 0.90,1.0,1.10))
#Stratified forest map####
####overall cancer####
library(survival)
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
colnames(UKBdata2)
UKBdata2$various_cancer[UKBdata2$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
UKBdata2$various_cancer[UKBdata2$various_cancer == "throid"] <- "thyroid"
UKBdata2$various_cancer[UKBdata2$various_cancer == "cervix"|UKBdata2$various_cancer == "uterine"] <- "uterus_cervix"
UKBdata2$MIND_sum1_3group <- cut(UKBdata2$MIND_sum1, 
                                 breaks = quantile(UKBdata2$MIND_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
UKBdata2$MEDAS_sum1_3group <- cut(UKBdata2$MEDAS_sum1, 
                                 breaks = quantile(UKBdata2$MEDAS_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
UKBdata2$energy<-UKBdata2$`100002_1`/4.184
UKBdata2$smoking_status<-ifelse(UKBdata2$smoking_status=="Never",0,
                                ifelse(UKBdata2$smoking_status=="Former occasional smoker",1,
                                       ifelse(UKBdata2$smoking_status=="Former regular smoker",2,3)))
UKBdata2$alochol_drinking<-ifelse(UKBdata2$alochol_drinking=="Never",0,
                                ifelse(UKBdata2$alochol_drinking=="Occasional drinker",1,2))
#female
colnames(UKBdata2)
var_list<-colnames(UKBdata2[,c(268,267)])
result1_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset = (sex=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result1_1<-rbind(result1_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result1_1)<-c("MEDAS score","MIND score")
colnames(result1_1)<-c("OR","LCL","UCL","P")
#male
result1_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset = (sex=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result1_2<-rbind(result1_2, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result1_2)<-c("MEDAS score","MIND score")
result1<-rbind(result1_1,result1_2)
#no_education
result2_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset = (education=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result2_1<-rbind(result2_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result2_1)<-c("MEDAS score","MIND score")
colnames(result2_1)<-c("OR","LCL","UCL","P")
#education
result2_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset = (education=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result2_2<-rbind(result2_2,c(HR, CI_lower,CI_upper,p_value))
}
rownames(result2_2)<-c("MEDAS score","MIND score")
colnames(result2_2)<-c("OR","LCL","UCL","P")
result2<-rbind(result2_1,result2_2)
# family histroy=0
result4_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(famaliy_history=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result4_1<-rbind(result4_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result4_1)<-c("MEDAS score","MIND score")
colnames(result4_1)<-c("OR","LCL","UCL","P")
#family histor=1
result4_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(famaliy_history=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result4_2<-rbind(result4_2, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result4_2)<-c("MEDAS score","MIND score")
colnames(result4_2)<-c("OR","LCL","UCL","P")
result4<-rbind(result4_1,result4_2)
#smoking=0
UKBdata2$smoking_status2<-ifelse(UKBdata2$smoking_status=="Never",0,1)
result5_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(smoking_status2=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result5_1<-rbind(result5_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result5_1)<-c("MEDAS score","MIND score")
colnames(result5_1)<-c("OR","LCL","UCL","P")
#smoking=1
result5_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(smoking_status2=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result5_2<-rbind(result5_2, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result5_2)<-c("MEDAS score","MIND score")
colnames(result5_2)<-c("OR","LCL","UCL","P")
result5<-rbind(result5_1,result5_2)
#alcohol drinking=0
UKBdata2$alochol_drinking2<-ifelse(UKBdata2$alochol_drinking=="Never",0,1)
result6_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(alochol_drinking2=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result6_1<-rbind(result6_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result6_1)<-c("MEDAS score","MIND score")
colnames(result6_1)<-c("OR","LCL","UCL","P")
#alcohol drinking=1
result6_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2,subset=(alochol_drinking2=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result6_2<-rbind(result6_2, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result6_2)<-c("MEDAS score","MIND score")
colnames(result6_2)<-c("OR","LCL","UCL","P")
result6<-rbind(result6_1,result6_2)
#BMI=no fat
UKBdata2$BMIgroup<-ifelse(UKBdata2$BMI<=25,0,1)
result7_1<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMIgroup+whr+SBP,data=UKBdata2,subset=(BMIgroup=="0"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result7_1<-rbind(result7_1, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result7_1)<-c("MEDAS score","MIND score")
colnames(result7_1)<-c("OR","LCL","UCL","P")
#fat
result7_2<-c()
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMIgroup+whr+SBP,data=UKBdata2,subset=(BMIgroup=="1"))
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result7_2<-rbind(result7_2, c(HR, CI_lower, CI_upper, p_value))
}
rownames(result7_2)<-c("MEDAS score","MIND score")
colnames(result7_2)<-c("OR","LCL","UCL","P")
result7<-rbind(result7_1,result7_2)
#export
write.csv(result1,"E:/deskbook/result1.csv")
write.csv(result2,"E:/deskbook/result2.csv")
write.csv(result4,"E:/deskbook/result4.csv")
write.csv(result5,"E:/deskbook/result5.csv")
write.csv(result6,"E:/deskbook/result6.csv")
write.csv(result7,"E:/deskbook/result7.csv")
#inertaction P
#male
result1 <- data.frame() 
 for (var in var_list) {
    formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status + 
                               alochol_drinking + weekly_activity + sleep_duration+BMI + whr + SBP +", var, "*sex", sep = " "))
    fit <- coxph(formula, data = UKBdata2)
    interaction_term <- paste(var, "sex", sep = ":")
    p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
    result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
         }
print(result1)
#education
result1 <- data.frame() 
for (var in var_list) {
  formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status + 
                               alochol_drinking + weekly_activity + sleep_duration+BMI + whr + SBP +", var, "*education", sep = " "))
  fit <- coxph(formula, data = UKBdata2)
  interaction_term <- paste(var, "education", sep = ":")
  p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
  result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
}
print(result1)
#family history
result1 <- data.frame() 
for (var in var_list) {
  formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status + 
                               alochol_drinking + weekly_activity + sleep_duration+BMI + whr + SBP +", var, "*famaliy_history", sep = " "))
  fit <- coxph(formula, data = UKBdata2)
  interaction_term <- paste(var, "famaliy_history", sep = ":")
  p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
  result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
}
print(result1)
#smoking
result1 <- data.frame() 
for (var in var_list) {
  formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status2 + 
                               alochol_drinking + weekly_activity + sleep_duration+BMI + whr + SBP +", var, "*smoking_status2", sep = " "))
  fit <- coxph(formula, data = UKBdata2)
  interaction_term <- paste(var, "smoking_status2", sep = ":")
  p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
  result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
}
print(result1)
#alcohol drinking
colnames(UKBdata2)
result1 <- data.frame() 
for (var in var_list) {
  formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status+ 
                               alochol_drinking2+ weekly_activity + sleep_duration+BMI + whr + SBP +", var, "*alochol_drinking2", sep = " "))
  fit <- coxph(formula, data = UKBdata2)
  interaction_term <- paste(var, "alochol_drinking", sep = ":")
  p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
  result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
}
#BMI
result1 <- data.frame() 
for (var in var_list) {
  formula <- as.formula(paste("Surv(recruitment_age, status) ~", var, "+ energy + sex + education + famaliy_history + 
                              Townsend_deprivation_index2 + household_income + smoking_status+ 
                               alochol_drinking+ weekly_activity + sleep_duration+BMIgroup + whr + SBP +", var, "*BMIgroup", sep = " "))
  fit <- coxph(formula, data = UKBdata2)
  interaction_term <- paste(var, "BMIgroup", sep = ":")
  p_value <- ifelse(interaction_term %in% names(coef(fit)), summary(fit)$coefficients[interaction_term, "Pr(>|z|)"], NA)
  result1 <- rbind(result1, data.frame(Variable = var, P_Value = p_value))
}
print(result1)
colnames(UKBdata2)
table(UKBdata2$sex,UKBdata2$status)
table(UKBdata2$education,UKBdata2$status)
table(UKBdata2$famaliy_history,UKBdata2$status)
table(UKBdata2$smoking_status2)
table(UKBdata2$smoking_status2,UKBdata2$status)
table(UKBdata2$alochol_drinking2)
table(UKBdata2$alochol_drinking2,UKBdata2$status)
table(UKBdata2$BMIgroup)
table(UKBdata2$BMIgroup,UKBdata2$status)
#Figure S3####
library(grid)
library("forestploter")
figure2<-read.csv("E:/deskbook/Figure2.csv",sep=",",header=T)
figure2$Subgroup<-ifelse(is.na(figure2$N),
                         figure2$Subgroup,
                         paste0("  ",figure2$Subgroup))
figure2$N<-ifelse(is.na(figure2$N),"",figure2$N)
figure2$Incidence<-ifelse(is.na(figure2$Incidence),"",figure2$Incidence)
figure2$P.for.interaction<-ifelse(is.na(figure2$P.for.interaction),"",figure2$P.for.interaction)
figure2$se<-(log(figure2$UCL)-log(figure2$HR))/1.96
figure2$` `<-paste(rep(' ',20),collapse = ' ')
figure2$`HR (95%CI)`<-ifelse(is.na(figure2$se),"",
                             paste0(round(figure2$HR,2)," (",round(figure2$LCL,2),"-",round(figure2$UCL,2),")"))
ggsci::pal_aaas()
tm<-forest_theme(base_size=10,ci_pch = 15,
                 ci_col="black",ci_lty = 1,ci_lwd=2.0,ci_fill="blue")
colnames(figure2)
p<-forest(figure2[,c(1:5,10:11)],
          est=figure2$HR1,
          lower=figure2$LCL1,
          upper = figure2$UCL1,
          ci_column=6,
          ref_line = 1,xlim=c(0.8,1.05),
          ticks_at = c(0.8,0.85,0.9,0.95,1.0),theme = tm)
g<-edit_plot(p,row=c(1,6,11,16,21,26),
             gp=gpar(fontface="bold"))
####VARIOUS CANCER_Supplement Figure6####
colnames(UKBdata2)
library(data.table)
library(survival)
library(tidyverse)
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
colnames(UKBdata2)
####head and neck####
data_head<-subset(UKBdata2,various_cancer=="head_and_neck"|various_cancer==0)
#female
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#male
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("head"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("head"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("head"),.before=strate)
#吸烟史0
data_head$smoking_status2<-ifelse(data_head$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("head"),.before=strate)
#饮酒史为0
data_head$alochol_drinking2<-ifelse(data_head$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_head,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("head"),.before=strate)
#BMI为非肥胖
data_head$BMIgroup<-ifelse(data_head$BMI<=25,0,1)
result1_1<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_head,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_head,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_head[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_head)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("head"),.before=strate)
#合并
result_head<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####esophagus####
data_esophagus<-subset(UKBdata2,various_cancer=="esophagus"|various_cancer==0)
#female
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("esophagus"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("esophagus"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("esophagus"),.before=strate)
#吸烟史0
data_esophagus$smoking_status2<-ifelse(data_esophagus$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("esophagus"),.before=strate)
#饮酒史为0
data_esophagus$alochol_drinking2<-ifelse(data_esophagus$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_esophagus,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("esophagus"),.before=strate)
#BMI为非肥胖
data_esophagus$BMIgroup<-ifelse(data_esophagus$BMI<=25,0,1)
result1_1<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_esophagus,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_esophagus,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_esophagus[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_esophagus)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("esophagus"),.before=strate)
#合并
result_esophagus<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####stomach####
data_stomach<-subset(UKBdata2,various_cancer=="stomach"|various_cancer==0)
#female
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("stomach"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("stomach"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("stomach"),.before=strate)
#吸烟史0
data_stomach$smoking_status2<-ifelse(data_stomach$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("stomach"),.before=strate)
#饮酒史为0
data_stomach$alochol_drinking2<-ifelse(data_stomach$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("stomach"),.before=strate)
#BMI为非肥胖
data_stomach$BMIgroup<-ifelse(data_stomach$BMI<=25,0,1)
result1_1<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_stomach,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_stomach,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_stomach[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_stomach)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("stomach"),.before=strate)
#合并
result_stomach<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####colorectal####
data_colorectal<-subset(UKBdata2,various_cancer=="colorectal"|various_cancer==0)
#female
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("colorectal"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("colorectal"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("colorectal"),.before=strate)
#吸烟史0
data_colorectal$smoking_status2<-ifelse(data_colorectal$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("colorectal"),.before=strate)
#饮酒史为0
data_colorectal$alochol_drinking2<-ifelse(data_colorectal$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("colorectal"),.before=strate)
#BMI为非肥胖
data_colorectal$BMIgroup<-ifelse(data_colorectal$BMI<=25,0,1)
result1_1<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_colorectal,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_colorectal,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_colorectal[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_colorectal)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("colorectal"),.before=strate)
#合并
result_colorectal<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####liver####
data_liver<-subset(UKBdata2,various_cancer=="liver"|various_cancer==0)
#female
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("liver"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("liver"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("liver"),.before=strate)
#吸烟史0
data_liver$smoking_status2<-ifelse(data_liver$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("liver"),.before=strate)
#饮酒史为0
data_liver$alochol_drinking2<-ifelse(data_liver$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("liver"),.before=strate)
#BMI为非肥胖
data_liver$BMIgroup<-ifelse(data_liver$BMI<=25,0,1)
result1_1<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_liver,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_liver,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_liver[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_liver)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("liver"),.before=strate)
#合并
result_liver<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####gallbladder####
data_gallbladder<-subset(UKBdata2,various_cancer=="gallbladder"|various_cancer==0)
#female
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("gallbladder"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("gallbladder"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("gallbladder"),.before=strate)
#吸烟史0
data_gallbladder$smoking_status2<-ifelse(data_gallbladder$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("gallbladder"),.before=strate)
#饮酒史为0
data_gallbladder$alochol_drinking2<-ifelse(data_gallbladder$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("gallbladder"),.before=strate)
#BMI为非肥胖
data_gallbladder$BMIgroup<-ifelse(data_gallbladder$BMI<=25,0,1)
result1_1<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_gallbladder,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_gallbladder,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_gallbladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_gallbladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("gallbladder"),.before=strate)
#合并
result_gallbladder<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####pancreas####
data_pancreas<-subset(UKBdata2,various_cancer=="pancreas"|various_cancer==0)
#female
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("pancreas"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("pancreas"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("pancreas"),.before=strate)
#吸烟史0
data_pancreas$smoking_status2<-ifelse(data_pancreas$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("pancreas"),.before=strate)
#饮酒史为0
data_pancreas$alochol_drinking2<-ifelse(data_pancreas$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("pancreas"),.before=strate)
#BMI为非肥胖
data_pancreas$BMIgroup<-ifelse(data_pancreas$BMI<=25,0,1)
result1_1<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_pancreas,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_pancreas,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_pancreas[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_pancreas)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("pancreas"),.before=strate)
#合并
result_pancreas<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####lung####
data_lung<-subset(UKBdata2,various_cancer=="lung"|various_cancer==0)
#female
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("lung"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("lung"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("lung"),.before=strate)
#吸烟史0
data_lung$smoking_status2<-ifelse(data_lung$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("lung"),.before=strate)
#饮酒史为0
data_lung$alochol_drinking2<-ifelse(data_lung$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("lung"),.before=strate)
#BMI为非肥胖
data_lung$BMIgroup<-ifelse(data_lung$BMI<=25,0,1)
result1_1<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_lung,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_lung,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_lung[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_lung)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("lung"),.before=strate)
#合并
result_lung<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####malignant_melanoma####
data_malignant_melanoma<-subset(UKBdata2,various_cancer=="malignant_melanoma"|various_cancer==0)
#female
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#吸烟史0
data_malignant_melanoma$smoking_status2<-ifelse(data_malignant_melanoma$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#饮酒史为0
data_malignant_melanoma$alochol_drinking2<-ifelse(data_malignant_melanoma$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#BMI为非肥胖
data_malignant_melanoma$BMIgroup<-ifelse(data_malignant_melanoma$BMI<=25,0,1)
result1_1<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_malignant_melanoma,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_malignant_melanoma,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_malignant_melanoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("malignant_melanoma"),.before=strate)
#合并
result_malignant_melanoma<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####breast####
data_breast<-subset(UKBdata2,various_cancer=="breast"|various_cancer==0)
#female
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP+i*sex,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("breast"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("breast"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("breast"),.before=strate)
#吸烟史0
data_breast$smoking_status2<-ifelse(data_breast$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("breast"),.before=strate)
#饮酒史为0
data_breast$alochol_drinking2<-ifelse(data_breast$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("breast"),.before=strate)
#BMI为非肥胖
data_breast$BMIgroup<-ifelse(data_breast$BMI<=25,0,1)
result1_1<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_breast,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_breast,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_breast[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_breast)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("breast"),.before=strate)
#合并
result_breast<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####uterus_cervix####
data_uterus_cervix<-subset(UKBdata2,various_cancer=="uterus_cervix"|various_cancer==0)
#female
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#吸烟史0
data_uterus_cervix$smoking_status2<-ifelse(data_uterus_cervix$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#饮酒史为0
data_uterus_cervix$alochol_drinking2<-ifelse(data_uterus_cervix$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterus_cervix,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#BMI为非肥胖
data_uterus_cervix$BMIgroup<-ifelse(data_uterus_cervix$BMI<=25,0,1)
result1_1<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_uterus_cervix,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_uterus_cervix,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_uterus_cervix[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_uterus_cervix)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("uterus_cervix"),.before=strate)
#合并
result_uterus_cervix<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####ovary####
data_ovary<-subset(UKBdata2,various_cancer=="ovary"|various_cancer==0)
#female
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("ovary"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("ovary"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("ovary"),.before=strate)
#吸烟史0
data_ovary$smoking_status2<-ifelse(data_ovary$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("ovary"),.before=strate)
#饮酒史为0
data_ovary$alochol_drinking2<-ifelse(data_ovary$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("ovary"),.before=strate)
#BMI为非肥胖
data_ovary$BMIgroup<-ifelse(data_ovary$BMI<=25,0,1)
result1_1<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_ovary,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_ovary,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_ovary[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_ovary)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("ovary"),.before=strate)
#合并
result_ovary<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####prostate####
data_prostate<-subset(UKBdata2,various_cancer=="prostate"|various_cancer==0)
#female
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("prostate"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("prostate"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("prostate"),.before=strate)
#吸烟史0
data_prostate$smoking_status2<-ifelse(data_prostate$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("prostate"),.before=strate)
#饮酒史为0
data_prostate$alochol_drinking2<-ifelse(data_prostate$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("prostate"),.before=strate)
#BMI为非肥胖
data_prostate$BMIgroup<-ifelse(data_prostate$BMI<=25,0,1)
result1_1<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_prostate,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_prostate,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_prostate[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_prostate)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("prostate"),.before=strate)
#合并
result_prostate<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####testic####
data_testic<-subset(UKBdata2,various_cancer=="testic"|various_cancer==0)
#female
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("testic"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("testic"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("testic"),.before=strate)
#吸烟史0
data_testic$smoking_status2<-ifelse(data_testic$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("testic"),.before=strate)
#饮酒史为0
data_testic$alochol_drinking2<-ifelse(data_testic$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("testic"),.before=strate)
#BMI为非肥胖
data_testic$BMIgroup<-ifelse(data_testic$BMI<=25,0,1)
result1_1<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_testic,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_testic,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_testic[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_testic)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("testic"),.before=strate)
#合并
result_testic<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####kidney####
data_kidney<-subset(UKBdata2,various_cancer=="kidney"|various_cancer==0)
#female
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("kidney"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("kidney"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("kidney"),.before=strate)
#吸烟史0
data_kidney$smoking_status2<-ifelse(data_kidney$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("kidney"),.before=strate)
#饮酒史为0
data_kidney$alochol_drinking2<-ifelse(data_kidney$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("kidney"),.before=strate)
#BMI为非肥胖
data_kidney$BMIgroup<-ifelse(data_kidney$BMI<=25,0,1)
result1_1<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_kidney,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_kidney,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_kidney[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_kidney)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("kidney"),.before=strate)
#合并
result_kidney<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####bladder####
data_bladder<-subset(UKBdata2,various_cancer=="bladder"|various_cancer==0)
#female
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("bladder"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("bladder"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("bladder"),.before=strate)
#吸烟史0
data_bladder$smoking_status2<-ifelse(data_bladder$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("bladder"),.before=strate)
#饮酒史为0
data_bladder$alochol_drinking2<-ifelse(data_bladder$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("bladder"),.before=strate)
#BMI为非肥胖
data_bladder$BMIgroup<-ifelse(data_bladder$BMI<=25,0,1)
result1_1<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_bladder,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_bladder,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_bladder[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_bladder)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("bladder"),.before=strate)
#合并
result_bladder<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####brain####
data_brain<-subset(UKBdata2,various_cancer=="brain"|various_cancer==0)
#female
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("brain"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("brain"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("brain"),.before=strate)
#吸烟史0
data_brain$smoking_status2<-ifelse(data_brain$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("brain"),.before=strate)
#饮酒史为0
data_brain$alochol_drinking2<-ifelse(data_brain$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("brain"),.before=strate)
#BMI为非肥胖
data_brain$BMIgroup<-ifelse(data_brain$BMI<=25,0,1)
result1_1<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_brain,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_brain,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_brain[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_brain)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("brain"),.before=strate)
#合并
result_brain<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####thyroid####
data_thyroid<-subset(UKBdata2,various_cancer=="thyroid"|various_cancer==0)
#female
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("thyroid"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("thyroid"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("thyroid"),.before=strate)
#吸烟史0
data_thyroid$smoking_status2<-ifelse(data_thyroid$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("thyroid"),.before=strate)
#饮酒史为0
data_thyroid$alochol_drinking2<-ifelse(data_thyroid$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("thyroid"),.before=strate)
#BMI为非肥胖
data_thyroid$BMIgroup<-ifelse(data_thyroid$BMI<=25,0,1)
result1_1<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_thyroid,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_thyroid,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_thyroid[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_thyroid)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("thyroid"),.before=strate)
#合并
result_thyroid<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####Hodgkin_lymphoma####
data_Hodgkin_lymphoma<-subset(UKBdata2,various_cancer=="Hodgkin_lymphoma"|various_cancer==0)
#female
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#吸烟史0
data_Hodgkin_lymphoma$smoking_status2<-ifelse(data_Hodgkin_lymphoma$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#饮酒史为0
data_Hodgkin_lymphoma$alochol_drinking2<-ifelse(data_Hodgkin_lymphoma$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#BMI为非肥胖
data_Hodgkin_lymphoma$BMIgroup<-ifelse(data_Hodgkin_lymphoma$BMI<=25,0,1)
result1_1<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_Hodgkin_lymphoma,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_Hodgkin_lymphoma,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=strate)
#合并
result_Hodgkin_lymphoma<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)

####non_Hodgkin_lymphoma####
data_non_Hodgkin_lymphoma<-subset(UKBdata2,various_cancer=="non_Hodgkin_lymphoma"|various_cancer==0)
#female
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#吸烟史0
data_non_Hodgkin_lymphoma$smoking_status2<-ifelse(data_non_Hodgkin_lymphoma$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#饮酒史为0
data_non_Hodgkin_lymphoma$alochol_drinking2<-ifelse(data_non_Hodgkin_lymphoma$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#BMI为非肥胖
data_non_Hodgkin_lymphoma$BMIgroup<-ifelse(data_non_Hodgkin_lymphoma$BMI<=25,0,1)
result1_1<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_non_Hodgkin_lymphoma,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_non_Hodgkin_lymphoma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=strate)
#合并
result_non_Hodgkin_lymphoma<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####multiple_myeloma####
data_multiple_myeloma<-subset(UKBdata2,various_cancer=="multiple_myeloma"|various_cancer==0)
#female
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#吸烟史0
data_multiple_myeloma$smoking_status2<-ifelse(data_multiple_myeloma$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#饮酒史为0
data_multiple_myeloma$alochol_drinking2<-ifelse(data_multiple_myeloma$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#BMI为非肥胖
data_multiple_myeloma$BMIgroup<-ifelse(data_multiple_myeloma$BMI<=25,0,1)
result1_1<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_multiple_myeloma,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_multiple_myeloma,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_multiple_myeloma)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("multiple_myeloma"),.before=strate)
#合并
result_multiple_myeloma<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####leukemia####
data_leukemia<-subset(UKBdata2,various_cancer=="leukemia"|various_cancer==0)
#female
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (sex=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#性别为male
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (sex=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*sex,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_sex<-cbind(result1,new_result1_3)
result_sex<-result_sex%>% mutate(strate=c("female_MED","male_MED","Female_MIND","male_MIND"),.before=OR)
result_sex<-result_sex%>% mutate(cancer=c("leukemia"),.before=strate)
#无完成全职教育
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (education=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#有完成全职教育
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (education=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*education,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_education<-cbind(result1,new_result1_3)
result_education<-result_education%>% mutate(strate=c("noeducaiton_MED","educaiton_MED","noeducaiton_MIND","educaiton_MIND"),.before=OR)
result_education<-result_education%>% mutate(cancer=c("leukemia"),.before=strate)
#家族史为0
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (famaliy_history=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#家族史为1
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (famaliy_history=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*famaliy_history,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_family<-cbind(result1,new_result1_3)
result_family<-result_family%>% mutate(strate=c("nofamily_MED","family_MED","nofamily_MIND","family_MIND"),.before=OR)
result_family<-result_family%>% mutate(cancer=c("leukemia"),.before=strate)
#吸烟史0
data_leukemia$smoking_status2<-ifelse(data_leukemia$smoking_status=="0",0,1)
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (smoking_status2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#吸烟史为1
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (smoking_status2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status2+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*smoking_status2,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_smoking<-cbind(result1,new_result1_3)
result_smoking<-result_smoking%>% mutate(strate=c("nosmoking_MED","yes_MED","nosmoking_MIND","yes_MIND"),.before=OR)
result_smoking<-result_smoking%>% mutate(cancer=c("leukemia"),.before=strate)
#饮酒史为0
data_leukemia$alochol_drinking2<-ifelse(data_leukemia$alochol_drinking=="0",0,1)
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (alochol_drinking2=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#饮酒史为1
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia,subset = (alochol_drinking2=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking2+weekly_activity+sleep_duration+
                BMI+whr+SBP+i*alochol_drinking2,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_alcohol<-cbind(result1,new_result1_3)
result_alcohol<-result_alcohol%>% mutate(strate=c("noalcohol_MED","yes_MED","noalcohol_MIND","yes_MIND"),.before=OR)
result_alcohol<-result_alcohol%>% mutate(cancer=c("leukemia"),.before=strate)
#BMI为非肥胖
data_leukemia$BMIgroup<-ifelse(data_leukemia$BMI<=25,0,1)
result1_1<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_leukemia,subset = (BMIgroup=="0"))
  result1_1<-rbind(result1_1,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#肥胖
result1_2<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP,data=data_leukemia,subset = (BMIgroup=="1"))
  result1_2<-rbind(result1_2,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)]))
}
#交互作用
result1_3<-c()
for (i in data_leukemia[,c(268,267)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMIgroup+whr+SBP+i*BMIgroup,data=data_leukemia)
  result1_3<-rbind(result1_3,c(summary(fiti)$coefficients[14,5]))
}
result1_1 <- as.data.frame(result1_1)
result1_2 <- as.data.frame(result1_2)
result1 <- rbind(result1_1[1, ],result1_2[1, ],result1_1[2, ],result1_2[2, ])
colnames(result1)<-c("OR","LCL","UCL")
result1_3 <- as.data.frame(result1_3)
new_result1_3<-data.frame(Pinteraction = rep(NA, 4))
new_result1_3[c(1, 3), "Pinteraction"] <- result1_3[, 1]
result_BMI<-cbind(result1,new_result1_3)
result_BMI<-result_BMI%>% mutate(strate=c("nofat_MED","yes_MED","nofat_MIND","yes_MIND"),.before=OR)
result_BMI<-result_BMI%>% mutate(cancer=c("leukemia"),.before=strate)
#合并
result_leukemia<-rbind(result_sex,result_education,result_family,result_smoking,result_alcohol,result_BMI)


####合并
result_various_cancer<-rbind(result_head,result_esophagus,result_stomach,result_colorectal,
                             result_liver,result_gallbladder,result_pancreas,result_lung,result_malignant_melanoma,
                             result_breast,result_uterus_cervix,result_ovary,result_prostate,result_testic,
                             result_kidney,result_bladder,result_brain,result_thyroid,result_Hodgkin_lymphoma,
                             result_non_Hodgkin_lymphoma,result_multiple_myeloma,result_leukemia)
write.csv(result_various_cancer,"E:/deskbook/FigureS6.csv",row.names = F)




#Draw figure####
###画图####
rm(list=ls())
library(grid)
library("forestploter")
Supplement_figure6<-read.csv("E:/deskbook/Supplement_figure6.csv",sep=",",header=T)
Supplement_figure6$`Sex:Female`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$` `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Sex:Male`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`  `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Completed full time education:No`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`   `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Completed full time education:Yes`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`    `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Family history of cancer:No`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`     `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Family history of cancer:Yes`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`      `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Smoking status:No`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`1     `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Smoking status:Yes`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`1 `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Alcohol drinking:No`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`1  `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`Alcohol drinking:Yes`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`2 `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`BMI:<25`<-paste(rep(' ',5),collapse = ' ')
Supplement_figure6$`2   `<-paste(rep(' ',20),collapse = ' ')
Supplement_figure6$`BMI:>25`<-paste(rep(' ',5),collapse = ' ')
tm<-forest_theme(base_size=10,ci_pch = 15,
                 ci_col="black",ci_lty = 1,ci_lwd=1.4)
colnames(Supplement_figure6)
p<-forest(Supplement_figure6[,c(1,38:60)],
          est=list(Supplement_figure6$HR1_female,Supplement_figure6$HR2_male,Supplement_figure6$HR3_education_no,
                   Supplement_figure6$HR4_education_yes,Supplement_figure6$HR5_familyhistory_no,Supplement_figure6$HR6_familyhistory_yes,
                   Supplement_figure6$HR7_smoking_no,Supplement_figure6$HR8_smoking_yes,Supplement_figure6$HR9_alcohol_no,
                   Supplement_figure6$HR10_alcohol_yes,Supplement_figure6$HR11_bmi.25,Supplement_figure6$HR12_bmi.25),
          lower=list(Supplement_figure6$LCL1,Supplement_figure6$LCL2,Supplement_figure6$LCL3,
                     Supplement_figure6$LCL4,Supplement_figure6$LCL5,Supplement_figure6$LCL6,
                     Supplement_figure6$LCL7,Supplement_figure6$LCL8,Supplement_figure6$LCL9,
                     Supplement_figure6$LCL10,Supplement_figure6$LCL11,Supplement_figure6$LCL12),
          upper = list(Supplement_figure6$UCL1,Supplement_figure6$UCL2,Supplement_figure6$UCL3,
                       Supplement_figure6$UCL4,Supplement_figure6$UCL5,Supplement_figure6$UCL6,
                       Supplement_figure6$UCL7,Supplement_figure6$UCL8,Supplement_figure6$UCL9,
                       Supplement_figure6$UCL10,Supplement_figure6$UCL11,Supplement_figure6$UCL12),
          ci_column=c(2,4,6,8,10,12,14,16,18,20,22,24),xlim=c(0.7,1.03),ticks_at = c(0.6,0.8,1.0),
          ref_line = 1,theme = tm)
g<-edit_plot(p,row=c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59,62,65),
             col=c(2,4,6,8,10,12,14,16,18,20,22,24),which="ci",gp=gpar(col="#037255"))
g<-edit_plot(g,row=c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60,63,66),
             col=c(2,4,6,8,10,12,14,16,18,20,22,24),which="ci",gp=gpar(col="#f39800"))
g
#Do the association with various cancers####
library(survival)
colnames(UKBdata2)
table(UKBdata2$various_cancer)
data_headcancer<-subset(UKBdata2,various_cancer=="head_and_neck"|various_cancer==0)
table(data_headcancer$MIND_sum1_3group)
table(data_headcancer$MIND_sum1_3group,data_headcancer$status)
table(data_headcancer$MEDAS_sum1_3group)
table(data_headcancer$MEDAS_sum1_3group,data_headcancer$status)
UKBdata2$MIND_sum1_3group<-factor(UKBdata2$MIND_sum1_3group)
UKBdata2$MEDAS_sum1_3group<-factor(UKBdata2$MEDAS_sum1_3group)
result_head<-c()
for (i in data_headcancer[,c(268,267)]){
  data_headcancer$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_headcancer)
  result_head<-rbind(result_head,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_head)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_head<-data.frame(result_head)
result_head<-result_head%>% mutate(cancer=c("head"),.before=OR_1)
#Esophagus
data_Esophagus<-subset(UKBdata2,various_cancer=="esophagus"|various_cancer==0)
table(data_Esophagus$MIND_sum1_3group)
table(data_Esophagus$MIND_sum1_3group,data_Esophagus$status)
table(data_Esophagus$MEDAS_sum1_3group)
table(data_Esophagus$MEDAS_sum1_3group,data_Esophagus$status)
result_Esophagus<-c()
for (i in data_Esophagus[,c(268,267)]){
  data_Esophagus$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Esophagus)
  result_Esophagus<-rbind(result_Esophagus,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_Esophagus)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_Esophagus<-data.frame(result_Esophagus)
result_Esophagus<-result_Esophagus%>% mutate(cancer=c("Esophagus"),.before=OR_1)
#stomach
data_stomach<-subset(UKBdata2,various_cancer=="stomach"|various_cancer==0)
table(data_stomach$MIND_sum1_3group)
table(data_stomach$MIND_sum1_3group,data_stomach$status)
table(data_stomach$MEDAS_sum1_3group)
table(data_stomach$MEDAS_sum1_3group,data_stomach$status)
result_stomach<-c()
for (i in data_stomach[,c(268,267)]){
  data_stomach$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach)
  result_stomach<-rbind(result_stomach,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_stomach)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_stomach<-data.frame(result_stomach)
result_stomach<-result_stomach%>% mutate(cancer=c("stomach"),.before=OR_1)
#colorectal
data_colorectal<-subset(UKBdata2,various_cancer=="colorectal"|various_cancer==0)
table(data_colorectal$MIND_sum1_3group)
table(data_colorectal$MIND_sum1_3group,data_colorectal$status)
table(data_colorectal$MEDAS_sum1_3group)
table(data_colorectal$MEDAS_sum1_3group,data_colorectal$status)
result_colorectal<-c()
for (i in data_colorectal[,c(268,267)]){
  data_colorectal$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal)
  result_colorectal<-rbind(result_colorectal,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_colorectal)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_colorectal<-data.frame(result_colorectal)
result_colorectal<-result_colorectal%>% mutate(cancer=c("colorectal"),.before=OR_1)
#liver
data_liver<-subset(UKBdata2,various_cancer=="liver"|various_cancer==0)
table(data_liver$MIND_sum1_3group)
table(data_liver$MIND_sum1_3group,data_liver$status)
table(data_liver$MEDAS_sum1_3group)
table(data_liver$MEDAS_sum1_3group,data_liver$status)
result_liver<-c()
for (i in data_liver[,c(268,267)]){
  data_liver$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver)
  result_liver<-rbind(result_liver,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_liver)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_liver<-data.frame(result_liver)
result_liver<-result_liver%>% mutate(cancer=c("liver"),.before=OR_1)
#gallbladder
data_gallbladder<-subset(UKBdata2,various_cancer=="gallbladder"|various_cancer==0)
table(data_gallbladder$MIND_sum1_3group)
table(data_gallbladder$MIND_sum1_3group,data_gallbladder$status)
table(data_gallbladder$MEDAS_sum1_3group)
table(data_gallbladder$MEDAS_sum1_3group,data_gallbladder$status)
result_gallbladder<-c()
for (i in data_gallbladder[,c(268,267)]){
  data_gallbladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder)
  result_gallbladder<-rbind(result_gallbladder,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_gallbladder)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_gallbladder<-data.frame(result_gallbladder)
result_gallbladder<-result_gallbladder%>% mutate(cancer=c("gallbladder"),.before=OR_1)
#pancreas
table(UKBdata2$various_cancer)
data_pancreas<-subset(UKBdata2,various_cancer=="pancreas"|various_cancer==0)
table(data_pancreas$MIND_sum1_3group)
table(data_pancreas$MIND_sum1_3group,data_pancreas$status)
table(data_pancreas$MEDAS_sum1_3group)
table(data_pancreas$MEDAS_sum1_3group,data_pancreas$status)
result_pancreas<-c()
for (i in data_pancreas[,c(268,267)]){
  data_pancreas$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas)
  result_pancreas<-rbind(result_pancreas,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_pancreas)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_pancreas<-data.frame(result_pancreas)
result_pancreas<-result_pancreas%>% mutate(cancer=c("pancreas"),.before=OR_1)
#lung
data_lung<-subset(UKBdata2,various_cancer=="lung"|various_cancer==0)
table(data_lung$MIND_sum1_3group)
table(data_lung$MIND_sum1_3group,data_lung$status)
table(data_lung$MEDAS_sum1_3group)
table(data_lung$MEDAS_sum1_3group,data_lung$status)
result_lung<-c()
for (i in data_lung[,c(268,267)]){
  data_lung$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung)
  result_lung<-rbind(result_lung,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_lung)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_lung<-data.frame(result_lung)
result_lung<-result_lung%>% mutate(cancer=c("lung"),.before=OR_1)
#malignant_melanoma
data_malignant_melanoma<-subset(UKBdata2,various_cancer=="malignant_melanoma"|various_cancer==0)
table(data_malignant_melanoma$MIND_sum1_3group)
table(data_malignant_melanoma$MIND_sum1_3group,data_malignant_melanoma$status)
table(data_malignant_melanoma$MEDAS_sum1_3group)
table(data_malignant_melanoma$MEDAS_sum1_3group,data_malignant_melanoma$status)
result_malignant_melanoma<-c()
for (i in data_malignant_melanoma[,c(268,267)]){
  data_malignant_melanoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma)
  result_malignant_melanoma<-rbind(result_malignant_melanoma,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_malignant_melanoma)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_malignant_melanoma<-data.frame(result_malignant_melanoma)
result_malignant_melanoma<-result_malignant_melanoma%>% mutate(cancer=c("malignant_melanoma"),.before=OR_1)
#breast
data_breast<-subset(UKBdata2,various_cancer=="breast"|various_cancer==0)
table(data_breast$MIND_sum1_3group)
table(data_breast$MIND_sum1_3group,data_breast$status)
table(data_breast$MEDAS_sum1_3group)
table(data_breast$MEDAS_sum1_3group,data_breast$status)
result_breast<-c()
for (i in data_breast[,c(268,267)]){
  data_breast$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast)
  result_breast<-rbind(result_breast,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_breast)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_breast<-data.frame(result_breast)
result_breast<-result_breast%>% mutate(cancer=c("breast"),.before=OR_1)
#Uterus and cervix cancer
table(UKBdata2$various_cancer)
data_uterine<-subset(UKBdata2,various_cancer=="uterus_cervix"|various_cancer==0)
table(data_uterine$MIND_sum1_3group)
table(data_uterine$MIND_sum1_3group,data_uterine$status)
table(data_uterine$MEDAS_sum1_3group)
table(data_uterine$MEDAS_sum1_3group,data_uterine$status)
result_uterine<-c()
for (i in data_uterine[,c(268,267)]){
  data_uterine$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterine)
  result_uterine<-rbind(result_uterine,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_uterine)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_uterine<-data.frame(result_uterine)
result_uterine<-result_uterine%>% mutate(cancer=c("uterine"),.before=OR_1)
#ovary
data_ovary<-subset(UKBdata2,various_cancer=="ovary"|various_cancer==0)
table(data_ovary$MIND_sum1_3group)
table(data_ovary$MIND_sum1_3group,data_ovary$status)
table(data_ovary$MEDAS_sum1_3group)
table(data_ovary$MEDAS_sum1_3group,data_ovary$status)
result_ovary<-c()
for (i in data_ovary[,c(268,267)]){
  data_ovary$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary)
  result_ovary<-rbind(result_ovary,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_ovary)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_ovary<-data.frame(result_ovary)
result_ovary<-result_ovary%>% mutate(cancer=c("ovary"),.before=OR_1)
#prostate
data_prostate<-subset(UKBdata2,various_cancer=="prostate"|various_cancer==0)
table(data_prostate$MIND_sum1_3group)
table(data_prostate$MIND_sum1_3group,data_prostate$status)
table(data_prostate$MEDAS_sum1_3group)
table(data_prostate$MEDAS_sum1_3group,data_prostate$status)
result_prostate<-c()
for (i in data_prostate[,c(268,267)]){
  data_prostate$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate)
  result_prostate<-rbind(result_prostate,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_prostate)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_prostate<-data.frame(result_prostate)
result_prostate<-result_prostate%>% mutate(cancer=c("prostate"),.before=OR_1)
#testic
table(UKBdata2$various_cancer)
data_testic<-subset(UKBdata2,various_cancer=="testic"|various_cancer==0)
table(data_testic$MIND_sum1_3group)
table(data_testic$MIND_sum1_3group,data_testic$status)
table(data_testic$MEDAS_sum1_3group)
table(data_testic$MEDAS_sum1_3group,data_testic$status)
result_testic<-c()
for (i in data_testic[,c(268,267)]){
  data_testic$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic)
  result_testic<-rbind(result_testic,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_testic)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_testic<-data.frame(result_testic)
result_testic<-result_testic%>% mutate(cancer=c("testic"),.before=OR_1)
#kidney
data_kidney<-subset(UKBdata2,various_cancer=="kidney"|various_cancer==0)
table(data_kidney$MIND_sum1_3group)
table(data_kidney$MIND_sum1_3group,data_kidney$status)
table(data_kidney$MEDAS_sum1_3group)
table(data_kidney$MEDAS_sum1_3group,data_kidney$status)
result_kidney<-c()
for (i in data_kidney[,c(268,267)]){
  data_kidney$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney)
  result_kidney<-rbind(result_kidney,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_kidney)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_kidney<-data.frame(result_kidney)
result_kidney<-result_kidney%>% mutate(cancer=c("kidney"),.before=OR_1)
#bladder
data_bladder<-subset(UKBdata2,various_cancer=="bladder"|various_cancer==0)
table(data_bladder$MIND_sum1_3group)
table(data_bladder$MIND_sum1_3group,data_bladder$status)
table(data_bladder$MEDAS_sum1_3group)
table(data_bladder$MEDAS_sum1_3group,data_bladder$status)
result_bladder<-c()
for (i in data_bladder[,c(268,267)]){
  data_bladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder)
  result_bladder<-rbind(result_bladder,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_bladder)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_bladder<-data.frame(result_bladder)
result_bladder<-result_bladder%>% mutate(cancer=c("bladder"),.before=OR_1)
#brain
data_brain<-subset(UKBdata2,various_cancer=="brain"|various_cancer==0)
table(data_brain$MIND_sum1_3group)
table(data_brain$MIND_sum1_3group,data_brain$status)
table(data_brain$MEDAS_sum1_3group)
table(data_brain$MEDAS_sum1_3group,data_brain$status)
result_brain<-c()
for (i in data_brain[,c(268,267)]){
  data_brain$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain)
  result_brain<-rbind(result_brain,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_brain)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_brain<-data.frame(result_brain)
result_brain<-result_brain%>% mutate(cancer=c("brain"),.before=OR_1)
#thyroid
table(UKBdata2$various_cancer)
data_thyroid<-subset(UKBdata2,various_cancer=="thyroid"|various_cancer==0)
table(data_thyroid$MIND_sum1_3group)
table(data_thyroid$MIND_sum1_3group,data_thyroid$status)
table(data_thyroid$MEDAS_sum1_3group)
table(data_thyroid$MEDAS_sum1_3group,data_thyroid$status)
table(data_thyroid$various_cancer)
result_thyroid<-c()
for (i in data_thyroid[,c(268,267)]){
  data_thyroid$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid)
  result_thyroid<-rbind(result_thyroid,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_thyroid)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_thyroid<-data.frame(result_thyroid)
result_thyroid<-result_thyroid%>% mutate(cancer=c("thyroid"),.before=OR_1)
#Hodgkin_lymphoma
data_Hodgkin_lymphoma<-subset(UKBdata2,various_cancer=="Hodgkin_lymphoma"|various_cancer==0)
table(data_Hodgkin_lymphoma$MIND_sum1_3group)
table(data_Hodgkin_lymphoma$MIND_sum1_3group,data_Hodgkin_lymphoma$status)
table(data_Hodgkin_lymphoma$MEDAS_sum1_3group)
table(data_Hodgkin_lymphoma$MEDAS_sum1_3group,data_Hodgkin_lymphoma$status)
result_Hodgkin_lymphoma<-c()
for (i in data_Hodgkin_lymphoma[,c(268,267)]){
  data_Hodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma)
  result_Hodgkin_lymphoma<-rbind(result_Hodgkin_lymphoma,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_Hodgkin_lymphoma)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_Hodgkin_lymphoma<-data.frame(result_Hodgkin_lymphoma)
result_Hodgkin_lymphoma<-result_Hodgkin_lymphoma%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=OR_1)
#non_Hodgkin_lymphoma 
data_non_Hodgkin_lymphoma<-subset(UKBdata2,various_cancer=="non_Hodgkin_lymphoma"|various_cancer==0)
table(data_non_Hodgkin_lymphoma$MIND_sum1_3group)
table(data_non_Hodgkin_lymphoma$MIND_sum1_3group,data_non_Hodgkin_lymphoma$status)
table(data_non_Hodgkin_lymphoma$MEDAS_sum1_3group)
table(data_non_Hodgkin_lymphoma$MEDAS_sum1_3group,data_non_Hodgkin_lymphoma$status)
result_non_Hodgkin_lymphoma<-c()
for (i in data_non_Hodgkin_lymphoma[,c(268,267)]){
  data_non_Hodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma)
  result_non_Hodgkin_lymphoma<-rbind(result_non_Hodgkin_lymphoma,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_non_Hodgkin_lymphoma)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_non_Hodgkin_lymphoma<-data.frame(result_non_Hodgkin_lymphoma)
result_non_Hodgkin_lymphoma<-result_non_Hodgkin_lymphoma%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=OR_1)
#multiple_myeloma
rm(data_headcancer,data_Esophagus,data_testic,data_colorectal)
data_multiple_myeloma<-subset(UKBdata2,various_cancer=="multiple_myeloma"|various_cancer==0)
table(data_multiple_myeloma$MIND_sum1_3group)
table(data_multiple_myeloma$MIND_sum1_3group,data_multiple_myeloma$status)
table(data_multiple_myeloma$MEDAS_sum1_3group)
table(data_multiple_myeloma$MEDAS_sum1_3group,data_multiple_myeloma$status)
result_multiple_myeloma<-c()
for (i in data_multiple_myeloma[,c(268,267)]){
  data_multiple_myeloma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma)
  result_multiple_myeloma<-rbind(result_multiple_myeloma,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_multiple_myeloma)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_multiple_myeloma<-data.frame(result_multiple_myeloma)
result_multiple_myeloma<-result_multiple_myeloma%>% mutate(cancer=c("multiple_myeloma"),.before=OR_1)
#leukemia
data_leukemia<-subset(UKBdata2,various_cancer=="leukemia"|various_cancer==0)
table(data_leukemia$MIND_sum1_3group)
table(data_leukemia$MIND_sum1_3group,data_leukemia$status)
table(data_leukemia$MEDAS_sum1_3group)
table(data_leukemia$MEDAS_sum1_3group,data_leukemia$status)
rm(data_testic,data_thyroid,data_uterine)
result_leukemia<-c()
for (i in data_leukemia[,c(268,267)]){
  data_leukemia$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia)
  result_leukemia<-rbind(result_leukemia,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_leukemia)<-c("OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_leukemia<-data.frame(result_leukemia)
result_leukemia<-result_leukemia%>% mutate(cancer=c("leukemia"),.before=OR_1)
#merge
result_various_cancer<-rbind(result_head,result_Esophagus,result_stomach,result_colorectal,
                             result_liver,result_gallbladder,result_pancreas,result_lung,result_malignant_melanoma,
                             result_breast,result_uterine,result_ovary,result_prostate,result_testic,
                             result_kidney,result_bladder,result_brain,result_thyroid,result_Hodgkin_lymphoma,
                             result_non_Hodgkin_lymphoma,result_multiple_myeloma,result_leukemia)
write.csv(result_various_cancer,"E:/deskbook/various_cancer.csv",row.names = F)
colnames(UKBdata2)
#Figure2####
#head#
rm(list=ls())
library(readr)
figure3_head<-read_csv("E:/deskbook/figure3_head.csv")
colnames(figure3_head)
library(ggplot2)
figure3_head$X<-factor(figure3_head$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p1<-ggplot(figure3_head,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR (95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Head and neck cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Esophagus cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p2<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Esophagus cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Colorectal cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p4<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color = "black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Colorectal cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Lung cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p8<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color = "black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Lung cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Malignant melanoma
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p9<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Malignant melanoma")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Breast cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p10<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Breast cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Uterus and cervix cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p11<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Uterus and cervix cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Prostate cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p13<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Prostate cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Testis cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p14<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.2,1.7,0.3),limits = c(0.2,1.7))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Testis cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Kidney cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p15<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.29,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Kidney cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Bladder cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p16<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Bladder cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Brain cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p17<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.29,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Brain cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Thyroid cancer
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p18<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.2,1.6,0.2),limits = c(0.2,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Thyroid cancer")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Non–Hodgkin lymphoma
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p20<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Non–Hodgkin lymphoma")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Multiple myeloma
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"))
p21<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Multiple myeloma")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
#Leukemia
figure3_esophagus<-read_csv("E:/deskbook/figure3_esophagus.csv")
figure3_esophagus$X<-factor(figure3_esophagus$X,levels = c("MEDAS_tertile2","MEDAS_tertile3","MIND_tertile2","MIND_tertile3"),
                            labels = c("T21","T31","T22","T32"))
p22<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 16, face = "bold"))+#加粗文字
  ggtitle("Leukemia")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x =element_blank())
p23<-ggplot(figure3_esophagus,aes(x=X,y=Y))+
  geom_line(size=0.75)+
  geom_point(size=4)+
  geom_errorbar(aes(ymin=LCL,ymax=UCL),colour="black",width=0.06,size=0.75)+
  xlab("Diet Adherence")+ylab("HR(95%CI)")+theme_classic()+geom_hline(aes(yintercept=1),linetype="dashed")+#加一条虚线
  scale_y_continuous(breaks = seq(0.3,1.6,0.2),limits = c(0.3,1.6))+
  theme(axis.text.x = element_text(size = 16, face = "bold",angle=0,color="black"))+theme(axis.text.y = element_text(size = 16, face = "bold",color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 16, face = "bold"))+theme(axis.title.y = element_text(size = 20, face = "bold"))+#加粗文字
  ggtitle("Leukemia")+theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title=element_text(size=20,face="bold"),
        axis.title.x = element_blank())
#
library(gridExtra)
grid.arrange(p1, p2, p4, p8,nrow = 1)
grid.arrange(p9, p10, p11, p13,nrow = 1)
grid.arrange(p14, p15, p16, p17,nrow = 1)
grid.arrange(p18, p20, p21, p22,nrow = 1)