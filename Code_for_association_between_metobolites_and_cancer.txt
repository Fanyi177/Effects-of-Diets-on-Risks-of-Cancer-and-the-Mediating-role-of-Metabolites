#import NMR data####
rm(list=ls())
NMR<-fread("E:/deskbook/NMR.csv",header=T)
colnames(NMR)
NMR_subset1<-subset(NMR,select=c(2:337))
NMR_subset2<-subset(NMR,select=c(1))
colnames(NMR)
#Columns were screened for baseline follow-up
selected_columns <- which(seq_len(ncol(NMR_subset1)) %% 2 == 1)
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(NMR_subset1)), selected_columns))
new_data1 <- NMR_subset1[, ..sorted_columns]
colnames(new_data1)
NMR_subset1<-subset(new_data1 ,select=c(1:168))
NMR<-cbind(NMR_subset2,NMR_subset1)
colnames(NMR)
#Remaining: 274351
library(tidyverse)
NMR2<-NMR%>%filter(!(is.na(`23409-0.0`)&is.na(`23410-0.0`)&
                       is.na(`23578-0.0`)&is.na(`23552-0.0`)&
                       is.na(`23467-0.0`)&is.na(`23575-0.0`)))
ICD<-fread("E:/deskbook/ICD.csv",header=T)
colnames(ICD)
ICD<-subset(ICD,select=c(1,9,11,10,57))
NMR3<-merge(NMR2,ICD,by='eid',all.x = T)
#Excluding those with malignant tumors at baseline, 250,976 patients remained
NMR4<-filter(NMR3,Exclusion_1==0) 
#Delete baseline use of lipid-lowering agents, remaining 206904
medication<-fread("E:/deskbook/medication.csv",header=T)
colnames(medication)
table(medication$medication)
NMR5<-merge(NMR4,medication,by='eid',all.x = T)
NMR6<-filter(NMR5,medication==0)
education<-fread("E:/deskbook/education_family_history.csv",header=T)#3 variable
NMR7<-merge(NMR6,education,by='eid',all.x = T) #206904 obs 176 vars
NMR7$education[NMR7$education==3]<-NA
NMR7$famaliy_history[NMR7$famaliy_history==3]<-NA
#
cov<-fread("E:/deskbook/cov.csv",header=T)
#
cov$shifang<-cov$`190-0.0`
cov$whr<-cov$`48-0.0`/cov$`49-0.0`
cov$household_income<-ifelse(cov$`738-0.0`<=2,0,1)
cov$weekly_activity<-ifelse(cov$`884-0.0`>=0&cov$`904-0.0`>=0,
                            rowMeans((select(cov,c(`884-0.0`,`904-0.0`)))),
                            ifelse(cov$`884-0.0`<0&cov$`904-0.0`<0,NA,
                                   ifelse(cov$`884-0.0`<0,cov$`904-0.0`,
                                          ifelse(cov$`904-0.0`<0,cov$`884-0.0`,0))))
cov$`1160-0.0`[cov$`1160-0.0`<=0]<-NA
cov$sleep_duration<-cov$`1160-0.0`
cov$smoking_status<-ifelse(cov$`1239-0.0`==1|cov$`1239-0.0`==2,3,ifelse(
  cov$`1249-0.0`==1,2,ifelse(cov$`1249-0.0`==2,1,
                             ifelse(cov$`1249-0.0`==3,0,
                                    ifelse(cov$`1249-0.0`==4,0,ifelse(cov$`1239-0.0`==0,0,NA))))))
cov$smoking_status<-factor(cov$smoking_status,levels=c(0,1,2,3),labels = c("Never","Former occasional smoker","Former regular smoker","Current somker"))
cov$alochol_drinking<-ifelse(cov$`1558-0.0`<=3&cov$`1558-0.0`>=1,2,
                             ifelse(cov$`1558-0.0`>=4&cov$`1558-0.0`<=5,1,
                                    ifelse(cov$`1558-0.0`==6,0,NA)))
cov$alochol_drinking<-factor(cov$alochol_drinking,levels = c(0,1,2),labels = c("Never","Occasional drinker","Regular drinker"))
cov$SBP<-cov$`4080-0.0`
cov$BMI<-cov$`21001-0.0`
cov$recruitment_age<-cov$`21022-0.0`
cov$sex<-cov$`31-0.0`
cov$Townsend_deprivation_index<-cov$`189-0.0`
colnames(cov)
cov2<-subset(cov,select=c(1,533:544))
NMR8<-merge(NMR7,cov2,by="eid",all.x = T)
colnames(NMR8)
#Delete the missing people
NMR8<-NMR8%>%filter(is.na(shifang))
#
NMR9<-NMR8%>%filter(!(is.na(famaliy_history)|is.na(education)|
                        is.na(Townsend_deprivation_index)|is.na(whr)|
                        is.na(BMI)|is.na(smoking_status)|is.na(alochol_drinking)|
                        is.na(household_income)))
colnames(NMR9)
missing_vals<-colSums(is.na(NMR9))
print(missing_vals)
#Replace the missing NMR value with one half of the minimum value
NMR9_1<-subset(NMR9,select=c(2:169))
NMR9_2<-subset(NMR9,select=c(1,170,173,175:176,178:188))
for (i in 1:ncol(NMR9_1)) {
  col_min <- min(NMR9_1[[i]], na.rm = TRUE) # get the minimum value of the column
  NMR9_1[is.na(NMR9_1[[i]]), i] <- col_min / 2 # replace missing values with half of the minimum
}
summary(NMR9_1)
missing_vals<-colSums(is.na(NMR9_1))
print(missing_vals)
#
library(mice)
colnames(NMR9_2)
tianbu<-mice(NMR9_2[,2:16],m=5)
#
tianbu2<-complete(tianbu,action=2)
tianbu2$Townsend_deprivation_index2 <- cut(tianbu2$Townsend_deprivation_index, breaks = quantile(tianbu2$Townsend_deprivation_index, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
NMR9_2_2<-subset(NMR9_2,select=c(1))
NMR9_2<-cbind(NMR9_2_2,tianbu2)
#log(x+1) transformation and normalization of metabolites####
n<-nrow(NMR9_1)
log_NMR9_1<-data.frame(matrix(0,n,ncol(NMR9_1)))
for (i in 1:ncol(NMR9_1)){
  log_NMR9_1[[i]]<-log(NMR9_1[[i]]+1)
}
names(log_NMR9_1)<-names(NMR9_1)
log_NMR9_1<-scale(log_NMR9_1)
#
NMR9<-cbind(NMR9_2,log_NMR9_1)
colnames(NMR9)
write_excel_csv(NMR9,"E:/deskbook/NMR9.csv")
#Read the data of NMR9####
NMR9<-fread("E:/deskbook/NMR9.csv",header=T)
#Table S7####
table(NMR9$various_cancer)
NMR9$status<-ifelse(NMR9$pan_cancer==0,0,1)
NMR9$various_cancer[NMR9$various_cancer == ""] <- "0"
NMR9$various_cancer[NMR9$various_cancer == "#N/A"] <- "9"
NMR9$various_cancer[NMR9$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
NMR9$various_cancer[NMR9$various_cancer == "throid"] <- "thyroid"
NMR9$various_cancer[NMR9$various_cancer == "cervix"|NMR9$various_cancer == "uterine"] <- "uterus_cervix"
colnames(NMR9)
vars<-colnames(NMR9[,c(4:17)])
catvars<-colnames(NMR9[,c(4:5,7,10:11,17,15)])
vars2<-colnames(NMR9[,c(6,8:9,12:14)])
library(tableone)
tableS7<-CreateTableOne(vars=vars,strata="status",data=NMR9,
                        factorVars =catvars,addOverall = T)
tableS7_1<-print(tableS7, quote = FALSE, noSpaces = TRUE, printToggle = FALSE,showAllLevels = T,
                 nonnormal=vars2)
write.csv(tableS7_1,"E:/deskbook/tableS7_1.csv")
#####
library(data.table)
library(tidyverse)
library(survival)
NMR9<-fread("E:/deskbook/NMR9.csv",header=T)
NMR9$status<-ifelse(NMR9$pan_cancer==0,0,1)
NMR9$various_cancer[NMR9$various_cancer == ""] <- 0
NMR9$various_cancer[NMR9$various_cancer == "#N/A"] <- "9"
NMR9$various_cancer[NMR9$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
NMR9$various_cancer[NMR9$various_cancer == "throid"] <- "thyroid"
NMR9$various_cancer[NMR9$various_cancer == "cervix"|NMR9$various_cancer == "uterine"] <- "uterus_cervix"
table(NMR9$status)
table(NMR9$various_cancer)
#Divided into training set, test set####
library(glmnet)
library(caret)
colnames(NMR9)
set.seed(777)
training.samples <- NMR9$status %>%
  createDataPartition(p = 0.6, list = FALSE)
train_data<- NMR9[training.samples, ]
test_data<- NMR9[-training.samples, ]
table(train_data$status)#103106 control 18276cases
table(test_data$status)#68586 control 12335cases
table(test_data$various_cancer)
#proportion
table(test_data$pan_cancer)/sum(table(test_data$pan_cancer))*100
table(test_data$various_cancer)/sum(table(test_data$various_cancer))*100
#Screening training focuses on metabolites associated with outcome ####
library(survival)
colnames(train_data)
result<-c()
for (i in train_data[,18:185]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=train_data)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result)<-c("HR","LCL","UCL","P")
write.csv(result,"E:/deskbook/supplement_table_8.csv",row.names = F)
#FDR
library(dplyr)
supplement_table_8<-read.csv("E:/deskbook/supplement_table_8.csv")
#
supplement_table_8<-supplement_table_8[order(supplement_table_8$P),]
supplement_table_8$BH<-p.adjust(supplement_table_8$P,method="bonferroni")
#shuchu
write.csv(supplement_table_8,"E:/deskbook/supplement_table_8.csv")
#net screening for needed metabolites####

colnames(train_data)
metabolites<-subset(train_data,select=-c(1:17,24:26,48:50,67:68:75,78:79,82,87:108,115,122,157,
                                         172:176,178:186))
# Build the model using the training set
#
library(survival)
colnames(train_data)
train_data$recruitment_age<-as.double(train_data$recruitment_age)
train_data$status<-as.double(train_data$status)
y<-as.matrix(Surv(time=train_data$recruitment_age,event=train_data$status))
x<-as.matrix(metabolites)
rownames(x)<-paste0("S",c(1:nrow(x)))
#
set.seed(1000)
net.fit1<-cv.glmnet(x,y,family="cox",type.measure = "deviance",alpha=0.5,nfolds = 10)
net.fit2<-cv.glmnet(x,y,family="cox",type.measure = "deviance",alpha=0.75,nfolds = 10)
net.fit3<-cv.glmnet(x,y,family="cox",type.measure = "deviance",alpha=1,nfolds = 10)
net.fit5<-cv.glmnet(x,y,family="cox",type.measure = "deviance",alpha=0.25,nfolds = 10)
#
plot(net.fit5)
legend("topleft", legend ="alpha=0.25", 
       pch = 19, col = "#ffd401",x.intersp=0.6, y.intersp=0.8)
plot(net.fit1, col = "#00b0eb")
legend("topleft", legend ="alpha=0.50", 
       pch = 19, col = "#00b0eb",x.intersp=0.6, y.intersp=0.8)
plot(net.fit2, col = "#e20612")
legend("topleft", legend ="alpha=0.75", 
       pch = 19, col = "#e20612",x.intersp=0.6, y.intersp=0.8)
plot(net.fit3, col = "#21908CFF")
legend("topleft", legend ="alpha=1.00", 
       pch = 19, col = "#21908CFF",x.intersp=0.6, y.intersp=0.8)
#
net.fit_1<-glmnet(x,y,family="cox",alpha = 0.5,lambda =net.fit1$lambda.1se)
net.fit_1[["beta"]]
net.fit_2<-glmnet(x,y,family="cox",alpha = 0.75,lambda =net.fit2$lambda.1se)
net.fit_2[["beta"]]
net.fit_3<-glmnet(x,y,family="cox",alpha = 1.00,lambda =net.fit3$lambda.1se)
net.fit_3[["beta"]]
net.fit_4<-glmnet(x,y,family="cox",alpha = 0.00,lambda =net.fit4$lambda.1se)
net.fit_4[["beta"]]
net.fit_5<-glmnet(x,y,family="cox",alpha = 0.25,lambda =net.fit5$lambda.1se)
net.fit_5[["beta"]]
#Gradient Boosting Trees#####
colnames(train_data)
train_data_1<-subset(train_data,select = c(42,52,53,55,57,60,65,76,77,80:81,83:86,117,128,
                                           130,135,137,166))
train_data_2<-subset(train_data,select = c(14,186))
colnames(train_data_1)<-paste0("S",c(1:ncol(train_data_1)))
train_data_1<-cbind(train_data_1,train_data_2)
set.seed(111)
library(gbm)
response_variable <- Surv(train_data_1$recruitment_age,train_data_1$status)
predictors <- setdiff(names(train_data_1), response_variable)
gbm_model <- gbm(Surv(recruitment_age,status)~.,distribution = "coxph",data=train_data_1,
                 n.trees = 200,
                 shrinkage = 0.1,
                 interaction.depth = 5,
                 n.minobsinnode = 10,
                 cv.folds = 10)
summary(gbm_model)
#Do Veen diagrams in online software
#Lollipop chart
figureS5e<-read.csv("E:/deskbook/bangbangtangtu.csv")
library(ggsci)
colnames(figureS5e)
figureS5e$Metabolites<-factor(figureS5e$Metabolites,levels=c("Phosphoglycerides","Total Lipids in Small VLDL","Apolipoprotein A1","Acetone",
                                                             "Sphingomyelins","Linoleic Acid","Concentration of IDL Particles","Acetoacetate",
                                                             "3-Hydroxybutyrate","Concentration of Large LDL Particles","Free Cholesterol in Very Small VLDL",
                                                             "Total Lipids in VLDL","Total Cholines","Glucose","Tyrosine","Free Cholesterol in IDL",
                                                             "Creatinine","Total Lipids in Large HDL","Omega-3 Fatty Acids","Citrate","Albumin"))
ggplot(figureS5e, aes(x=Metabolites, y=Relative.influence)) +
  geom_segment( aes(x=Metabolites, xend=Metabolites, y=0, yend=Relative.influence),
                color="#ffd401") +
  geom_point(color="#21908CFF",size=4, alpha=0.6) +
  theme_light() +
  coord_flip() +theme_classic()+ylab("Relative influence")+
  theme(axis.text.x = element_text(size = 12,color="black"))+theme(axis.text.y = element_text(size = 12,color="black"))+#加粗刻度
  theme(axis.title.x = element_text(size = 12,color="black"))+theme(axis.title.y = element_text(size = 12,color="black"))
#Associations with individual cancers were made in the test set####
colnames(test_data)
result_all<-c()
for (i in test_data[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=test_data)
  result_all<-rbind(result_all,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_all)<-c("HR","LCL","UCL","P")
write.csv(result_all,"E:/deskbook/result_all.csv",row.names = F)
library(dplyr)#
result_all<-read.csv("E:/deskbook/result_all.csv")
#
result_all<-result_all[order(result_all$P),]
result_all$BH<-p.adjust(result_all$P,method="BH")
write.csv(result_all,"E:/deskbook/result_all.csv")
#head
data_headcancer<-subset(test_data,various_cancer=="head_and_neck"|various_cancer==0)
result_head<-c()
for (i in data_headcancer[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_headcancer)
  result_head<-rbind(result_head,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_head)<-c("HR","LCL","UCL","P")
result_head<-data.frame(result_head)
result_head<-result_head%>% mutate(cancer=c("head"),.before=HR)
#Esophagus
data_Esophagus<-subset(test_data,various_cancer=="esophagus"|various_cancer==0)
result_Esophagus<-c()
for (i in data_Esophagus[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Esophagus)
  result_Esophagus<-rbind(result_Esophagus,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_Esophagus)<-c("HR","LCL","UCL","P")
result_Esophagus<-data.frame(result_Esophagus)
result_Esophagus<-result_Esophagus%>% mutate(cancer=c("Esophagus"),.before=HR)
#stomach
data_stomach<-subset(test_data,various_cancer=="stomach"|various_cancer==0)
result_stomach<-c()
for (i in data_stomach[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_stomach)
  result_stomach<-rbind(result_stomach,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_stomach)<-c("HR","LCL","UCL","P")
result_stomach<-data.frame(result_stomach)
result_stomach<-result_stomach%>% mutate(cancer=c("stomach"),.before=HR)
#colorectal
data_colorectal<-subset(test_data,various_cancer=="colorectal"|various_cancer==0)
result_colorectal<-c()
for (i in data_colorectal[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_colorectal)
  result_colorectal<-rbind(result_colorectal,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_colorectal)<-c("HR","LCL","UCL","P")
result_colorectal<-data.frame(result_colorectal)
result_colorectal<-result_colorectal%>% mutate(cancer=c("colorectal"),.before=HR)
#liver
data_liver<-subset(test_data,various_cancer=="liver"|various_cancer==0)
result_liver<-c()
for (i in data_liver[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_liver)
  result_liver<-rbind(result_liver,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_liver)<-c("HR","LCL","UCL","P")
result_liver<-data.frame(result_liver)
result_liver<-result_liver%>% mutate(cancer=c("liver"),.before=HR)
#gallbladder
data_gallbladder<-subset(test_data,various_cancer=="gallbladder"|various_cancer==0)
result_gallbladder<-c()
for (i in data_gallbladder[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_gallbladder)
  result_gallbladder<-rbind(result_gallbladder,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_gallbladder)<-c("HR","LCL","UCL","P")
result_gallbladder<-data.frame(result_gallbladder)
result_gallbladder<-result_gallbladder%>% mutate(cancer=c("gallbladder"),.before=HR)
#pancreas
data_pancreas<-subset(test_data,various_cancer=="pancreas"|various_cancer==0)
result_pancreas<-c()
for (i in data_pancreas[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_pancreas)
  result_pancreas<-rbind(result_pancreas,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_pancreas)<-c("HR","LCL","UCL","P")
result_pancreas<-data.frame(result_pancreas)
result_pancreas<-result_pancreas%>% mutate(cancer=c("pancreas"),.before=HR)
#lung
data_lung<-subset(test_data,various_cancer=="lung"|various_cancer==0)
result_lung<-c()
for (i in data_lung[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_lung)
  result_lung<-rbind(result_lung,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_lung)<-c("HR","LCL","UCL","P")
result_lung<-data.frame(result_lung)
result_lung<-result_lung%>% mutate(cancer=c("lung"),.before=HR)
#malignant_melanoma
data_malignant_melanoma<-subset(test_data,various_cancer=="malignant_melanoma"|various_cancer==0)
result_malignant_melanoma<-c()
for (i in data_malignant_melanoma[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_malignant_melanoma)
  result_malignant_melanoma<-rbind(result_malignant_melanoma,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_malignant_melanoma)<-c("HR","LCL","UCL","P")
result_malignant_melanoma<-data.frame(result_malignant_melanoma)
result_malignant_melanoma<-result_malignant_melanoma%>% mutate(cancer=c("malignant_melanoma"),.before=HR)
#breast
data_breast<-subset(test_data,various_cancer=="breast"|various_cancer==0)
result_breast<-c()
for (i in data_breast[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_breast)
  result_breast<-rbind(result_breast,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_breast)<-c("HR","LCL","UCL","P")
result_breast<-data.frame(result_breast)
result_breast<-result_breast%>% mutate(cancer=c("breast"),.before=HR)
#Uterus and cervix cancer
table(test_data$various_cancer)
data_uterine<-subset(test_data,various_cancer=="uterus_cervix"|various_cancer==0)
result_uterine<-c()
for (i in data_uterine[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_uterine)
  result_uterine<-rbind(result_uterine,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_uterine)<-c("HR","LCL","UCL","P")
result_uterine<-data.frame(result_uterine)
result_uterine<-result_uterine%>% mutate(cancer=c("uterine"),.before=HR)
#ovary
data_ovary<-subset(test_data,various_cancer=="ovary"|various_cancer==0)
result_ovary<-c()
for (i in data_ovary[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_ovary)
  result_ovary<-rbind(result_ovary,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_ovary)<-c("HR","LCL","UCL","P")
result_ovary<-data.frame(result_ovary)
result_ovary<-result_ovary%>% mutate(cancer=c("ovary"),.before=HR)
#prostate
data_prostate<-subset(test_data,various_cancer=="prostate"|various_cancer==0)
result_prostate<-c()
for (i in data_prostate[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_prostate)
  result_prostate<-rbind(result_prostate,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_prostate)<-c("HR","LCL","UCL","P")
result_prostate<-data.frame(result_prostate)
result_prostate<-result_prostate%>% mutate(cancer=c("prostate"),.before=HR)
#testic
data_testic<-subset(test_data,various_cancer=="testic"|various_cancer==0)
result_testic<-c()
for (i in data_testic[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_testic)
  result_testic<-rbind(result_testic,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_testic)<-c("HR","LCL","UCL","P")
result_testic<-data.frame(result_testic)
result_testic<-result_testic%>% mutate(cancer=c("testic"),.before=HR)
#kidney
data_kidney<-subset(test_data,various_cancer=="kidney"|various_cancer==0)
result_kidney<-c()
for (i in data_kidney[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_kidney)
  result_kidney<-rbind(result_kidney,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_kidney)<-c("HR","LCL","UCL","P")
result_kidney<-data.frame(result_kidney)
result_kidney<-result_kidney%>% mutate(cancer=c("kidney"),.before=HR)
#bladder
data_bladder<-subset(test_data,various_cancer=="bladder"|various_cancer==0)
result_bladder<-c()
for (i in data_bladder[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_bladder)
  result_bladder<-rbind(result_bladder,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_bladder)<-c("HR","LCL","UCL","P")
result_bladder<-data.frame(result_bladder)
result_bladder<-result_bladder%>% mutate(cancer=c("bladder"),.before=HR)
#brain
data_brain<-subset(test_data,various_cancer=="brain"|various_cancer==0)
result_brain<-c()
for (i in data_brain[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_brain)
  result_brain<-rbind(result_brain,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_brain)<-c("HR","LCL","UCL","P")
result_brain<-data.frame(result_brain)
result_brain<-result_brain%>% mutate(cancer=c("brain"),.before=HR)
#thyroid
data_thyroid<-subset(test_data,various_cancer=="thyroid"|various_cancer==0)
result_thyroid<-c()
for (i in data_thyroid[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_thyroid)
  result_thyroid<-rbind(result_thyroid,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_thyroid)<-c("HR","LCL","UCL","P")
result_thyroid<-data.frame(result_thyroid)
result_thyroid<-result_thyroid%>% mutate(cancer=c("thyroid"),.before=HR)
#Hodgkin_lymphoma
data_Hodgkin_lymphoma<-subset(test_data,various_cancer=="Hodgkin_lymphoma"|various_cancer==0)
result_Hodgkin_lymphoma<-c()
for (i in data_Hodgkin_lymphoma[,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_Hodgkin_lymphoma)
  result_Hodgkin_lymphoma<-rbind(result_Hodgkin_lymphoma,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_Hodgkin_lymphoma)<-c("HR","LCL","UCL","P")
result_Hodgkin_lymphoma<-data.frame(result_Hodgkin_lymphoma)
result_Hodgkin_lymphoma<-result_Hodgkin_lymphoma%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=HR)
#non_Hodgkin_lymphoma 
table(test_data$various_cancer)
data_non_Hodgkin_lymphoma <-subset(test_data,various_cancer=="non_Hodgkin_lymphoma"|various_cancer==0)
result_non_Hodgkin_lymphoma <-c()
for (i in data_non_Hodgkin_lymphoma [,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_non_Hodgkin_lymphoma )
  result_non_Hodgkin_lymphoma <-rbind(result_non_Hodgkin_lymphoma ,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_non_Hodgkin_lymphoma )<-c("HR","LCL","UCL","P")
result_non_Hodgkin_lymphoma <-data.frame(result_non_Hodgkin_lymphoma )
result_non_Hodgkin_lymphoma <-result_non_Hodgkin_lymphoma %>% mutate(cancer=c("non_Hodgkin_lymphoma "),.before=HR)
#multiple_myeloma 
data_multiple_myeloma <-subset(test_data,various_cancer=="multiple_myeloma"|various_cancer==0)
result_multiple_myeloma <-c()
for (i in data_multiple_myeloma [,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_multiple_myeloma )
  result_multiple_myeloma <-rbind(result_multiple_myeloma ,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_multiple_myeloma )<-c("HR","LCL","UCL","P")
result_multiple_myeloma <-data.frame(result_multiple_myeloma )
result_multiple_myeloma <-result_multiple_myeloma %>% mutate(cancer=c("multiple_myeloma"),.before=HR)
#leukemia 
data_leukemia <-subset(test_data,various_cancer=="leukemia"|various_cancer==0)
result_leukemia <-c()
for (i in data_leukemia [,c(42,53,60,76,77,80,85,86,135,166)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_leukemia )
  result_leukemia <-rbind(result_leukemia ,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
colnames(result_leukemia )<-c("HR","LCL","UCL","P")
result_leukemia <-data.frame(result_leukemia )
result_leukemia <-result_leukemia %>% mutate(cancer=c("leukemia "),.before=HR)
#merge
NMR_various_cancer<-rbind(result_head,result_Esophagus,result_stomach,result_colorectal,
                          result_liver,result_gallbladder,result_pancreas,result_lung,result_malignant_melanoma,
                          result_breast,result_uterine,result_ovary,result_prostate,result_testic,
                          result_kidney,result_bladder,result_brain,result_thyroid,result_Hodgkin_lymphoma,
                          result_non_Hodgkin_lymphoma,result_multiple_myeloma,result_leukemia)
write.csv(NMR_various_cancer,"E:/deskbook/NMR_various_cancer.csv",row.names = F)
#Figure 3####
CI_data<-read_csv("E:/deskbook/chanshi.csv")
detail<-read_csv("E:/deskbook/NMR_various_cancer2.csv")
detail$Cancer<-factor(detail$Cancer,levels=c("Head and neck cancer","Esophagus cancer","Stomach cancer","Colorectal cancer",
                                             "Liver cancer","Gallbladder cancer","Pancreas cancer","Lung cancer","Malignant melanoma",
                                             "Breast cancer","Uterus and cervix cancer","Ovary cancer","Prostate cancer","Testis cancer",
                                             "Kidney cancer","Bladder cancer","Brain cancer","Thyroid cancer",
                                             "Hodgkin lymphoma","Non_Hodgkin lymphoma","Multiple myeloma","Leukemia"))

table(detail$Cancer)
label_data <- CI_data
number_of_bar <- nrow(label_data)
number_of_bar
attach(label_data)
label_data$id<-0
for(i in 1:nrow(label_data)){
  label_data$id[i]<-i
}
label_data$Va<-label_data$Zmetabolite
label_data$metabolite<-detail$Metabolites
angle <-  90 - 360 * (label_data$id-0.5)/number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)
label_data$HR<-as.numeric(label_data$HR)
label_data$UCL<-as.numeric(label_data$UCL)
label_data$LCL<-as.numeric(label_data$LCL)
label_data$group<-detail$Cancer
unique(label_data$group)
head(label_data)
write.csv(label_data,"E:/deskbook/label_data.csv")
##########################################Restart R

##########
library(tidyverse)
setwd("E:\\deskbook")######
label_data<-read.csv("label_data.csv",row.names = 1)
empty_bar <-5
dim(label_data)
to_add <- data.frame(matrix(NA, empty_bar*nlevels(label_data$group), ncol(label_data)))
colnames(to_add) <- colnames(label_data)
to_add$group <- rep(levels(label_data$group), each=empty_bar)
label_data <- rbind(label_data, to_add)
label_data$group<-factor(label_data$group,levels=c("Head and neck cancer","Esophagus cancer","Stomach cancer","Colorectal cancer",
                                                   "Liver cancer","Gallbladder cancer","Pancreas cancer","Lung cancer","Malignant melanoma",
                                                   "Breast cancer","Uterus and cervix cancer","Ovary cancer","Prostate cancer","Testis cancer",
                                                   "Kidney cancer","Bladder cancer","Brain cancer","Thyroid cancer",
                                                   "Hodgkin lymphoma","Non_Hodgkin lymphoma","Multiple myeloma","Leukemia"))

label_data <- label_data %>% arrange(group)
label_data$id <- seq(1, nrow(label_data))
dim(label_data)
#
new_label<-label_data
number_of_bar <- nrow(new_label)
angle <- 90-360 * (new_label$id-0.5)/number_of_bar   
new_label$hjust <- ifelse(angle < -90, 1, 0)
new_label$angle <- ifelse(angle < -90, angle+180, angle)
head(new_label)

base_data<-label_data %>% 
  group_by(group) %>% 
  summarize(start=min(id),end=max(id)-empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))
head(base_data)

grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data <- grid_data[-1,]
grid_data

##########
length(unique(base_data$group))==length(unique(label_data$group))
for(i in 1:length(unique(label_data$group))){
  if(unique(base_data$group)[i]==unique(label_data$group)[i]){
    print("TRUE")
  }else{
    print("FALSE")
  }
}

new_base_data<-base_data
new_base_data$length<-0
for(i in 1:length(unique(new_base_data$group))){
  sub_label_data<-subset(label_data,group==unique(label_data$group)[i])
  new_base_data$length[i]<-length(sub_label_data$group)
}
#write.csv(new_base_data,"new_base_data.csv") 
new_base_data$start[1]<-0
for(s in 1:length(unique(new_base_data$group))){
  if(s+1<23){
    new_base_data$start[s+1]<-new_base_data$start[s]+new_base_data$length[s]
    new_base_data$end[s]<-new_base_data$start[s+1]-0.2
    new_base_data$title[s]<-new_base_data$start[s]+(new_base_data$end[s]-new_base_data$start[s])/2
  }else{
    new_base_data$start[s]<-new_base_data$start[31]+new_base_data$length[31]+0.2
    new_base_data$end[s]<-new_base_data$start[s]+new_base_data$length[s]
    new_base_data$title[s]<-new_base_data$start[s]+(new_base_data$end[s]-new_base_data$start[s])/2
  }
}

empty_bar <-1
to_add <- data.frame(matrix(NA, empty_bar, ncol(new_base_data)))
colnames(to_add) <- colnames(new_base_data)
#to_add$group <- rep(levels(new_base_data$group), each=empty_bar)
new_base_data <- rbind(new_base_data, to_add)



number_of_bar <- nrow(new_base_data)
new_base_data$id <- seq(1, nrow(new_base_data))
angle <- 90 - 360 * (new_base_data$id-0.9)/number_of_bar  
new_base_data$hjust <- ifelse(angle < -90, 1, 0)
new_base_data$angle <- ifelse(angle < -90, angle+180, angle)

write.csv(new_base_data,"new_base_data.csv")




new_base_data<-read.csv("new_base_data.csv",row.names = 1)
############
p1<- ggplot(label_data, aes(x=Va, y=HR,fill=group)) +   
  geom_bar(stat="identity",width = 0.5,fill = ifelse(label_data$HR>1,'#e31a1cff','#287A22FF'),alpha=0.8) +
  geom_errorbar(aes(x = Va,ymin = LCL, ymax = UCL),
                width = 0.1,color = 'black') +
  scale_x_discrete(limits=label_data$Va)+
  geom_segment(data=grid_data, aes(x = 19, y = 2, xend = 1, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 19, y = 1.5, xend = 1, yend = 1.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 19, y = 1, xend = 1, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 19, y = 0.5, xend = 1, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 19, y = 0, xend = 1, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = -4, y = c(0, 0.5, 1, 1.5,2), label = c("0", "0.5", "1.0","1.5","2") , color="blue", size=3 , angle=0, fontface="bold", hjust=0) +
  
  geom_segment(data=grid_data, aes(x = 72, y = 2, xend = 53, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 72, y = 1.5, xend = 53, yend = 1.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 72, y = 1, xend = 53, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 72, y = 0.5, xend = 53, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 72, y = 0, xend = 53, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  geom_segment(data=grid_data, aes(x = 132, y = 2, xend = 113, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 132, y = 1.5, xend = 113, yend = 1.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 132, y = 1, xend = 113, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 132, y = 0.5, xend = 113, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 132, y = 0, xend = 113, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  geom_segment(data=grid_data, aes(x = 200, y = 2, xend = 180, yend = 2), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 200, y = 1.5, xend = 180, yend = 1.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 200, y = 1, xend = 180, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 200, y = 0.5, xend = 180, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = 200, y = 0, xend = 180, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #ylim(-1000,0.2)+
  theme_minimal() +
  coord_polar(start = 0)+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())+
  geom_segment(data=base_data, aes(x = start, y = -3.5, xend = end, yend = -3.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+
  geom_segment(data=new_base_data, aes(x = start, y = 7.5, xend = end, yend = 7.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )+#圈的位置
  geom_text(data=new_label, aes(x=id, y=3.4, label=metabolite, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.6, 
            angle=new_label$angle, inherit.aes = FALSE )+
  geom_text(data=new_base_data, aes(x = title, y = 10, label=group,hjust=hjust), colour = "black", alpha=0.8, size=4.3, angle=new_base_data$angle,fontface="bold", inherit.aes = FALSE)+#最外圈文字位置
  geom_text(data=new_label,aes(x=Va,y=2.5),label=ifelse(label_data$P_value<0.05,"*",""))
p1#12*12