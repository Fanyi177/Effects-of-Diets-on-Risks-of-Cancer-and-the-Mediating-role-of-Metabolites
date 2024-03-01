#sensitivity analysis####
#Do it with a baseline diet####
library(data.table)
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
colnames(UKBdata2)
UKBdata2$MIND_sum2_3group<- cut(UKBdata2$MIND_sum2, 
                                breaks = quantile(UKBdata2$MIND_sum2, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata2$MEDAS_sum2_3group<- cut(UKBdata2$MEDAS_sum2, 
                                 breaks = quantile(UKBdata2$MEDAS_sum2, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata2$various_cancer[UKBdata2$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
UKBdata2$various_cancer[UKBdata2$various_cancer == "throid"] <- "thyroid"
UKBdata2$various_cancer[UKBdata2$various_cancer == "cervix"|UKBdata2$various_cancer == "uterine"] <- "uterus_cervix"
UKBdata2$MIND_sum1_3group <- cut(UKBdata2$MIND_sum1, 
                                 breaks = quantile(UKBdata2$MIND_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata2$MEDAS_sum1_3group <- cut(UKBdata2$MEDAS_sum1, 
                                  breaks = quantile(UKBdata2$MEDAS_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)
UKBdata2$energy<-UKBdata2$`100002_1`/4.184
table(UKBdata2$MIND_sum2_3group)
table(UKBdata2$MEDAS_sum2_3group)
table(UKBdata2$status)
table(UKBdata2$MIND_sum2_3group,UKBdata2$status)
table(UKBdata2$MEDAS_sum2_3group,UKBdata2$status)
library(survival)
#Model 1
colnames(UKBdata2)
result<-c()
for (i in UKBdata2[,c(187,91)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1]),summary(fiti)$conf.int[1,c(3:4)],summary(fiti)$coef[1,5]))
}
write.csv(result,"E:/deskbook/table2_1.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(268,267)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_2.csv",row.names = F)
#Model 2
fit1<-coxph(Surv(recruitment_age,status)~MEDAS_sum2+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
summary(fit1)
fit2<-coxph(Surv(recruitment_age,status)~MIND_sum2+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
summary(fit2)
result<-c()
for (i in UKBdata2[,c(268,267)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3
fit1<-coxph(Surv(recruitment_age,status)~MEDAS_sum2+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income+
              smoking_status+alochol_drinking+weekly_activity+sleep_duration+
              BMI+whr+SBP,data=UKBdata2)
summary(fit1)
fit2<-coxph(Surv(recruitment_age,status)~MIND_sum2+energy+sex+education+
              famaliy_history+Townsend_deprivation_index2+household_income+
              smoking_status+alochol_drinking+weekly_activity+sleep_duration+
              BMI+whr+SBP,data=UKBdata2)
summary(fit2)
result<-c()
for (i in UKBdata2[,c(268,267)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#trend test
tapply(UKBdata2$MIND_sum2,UKBdata2$MIND_sum2_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})

UKBdata2$MIND_sum2_3group_p<-ifelse(UKBdata2$MIND_sum2_3group==1,4,ifelse(
  UKBdata2$MIND_sum1_3group==2,6,7.5))

tapply(UKBdata2$MEDAS_sum2,UKBdata2$MEDAS_sum2_3group,function(x) 
{return(quantile(x,probs=c(0.5)))})
UKBdata2$MEDAS_sum2_3group_p<-ifelse(UKBdata2$MEDAS_sum2_3group==1,3,ifelse(
  UKBdata2$MEDAS_sum2_3group==2,5,6))
#model 1
colnames(UKBdata2)
result<-c()
for (i in UKBdata2[,c(278:279)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#model 2
result<-c()
for (i in UKBdata2[,c(278:279)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#model 3
result<-c()
for (i in UKBdata2[,c(278:279)]){
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(summary(fiti)$coef[1,5]))
}
#2.Reconstruct the food score by excluding each food group####
#MIND#####
#MIND_sum_outgreen
colnames(UKBdata2)
UKBdata2[,MIND_sum_outgreen:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Other_Vegetables_sum1_score",
                                                                      "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                      "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                      "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outgreen_3group<- cut(UKBdata2$MIND_sum_outgreen, 
                                        breaks = quantile(UKBdata2$MIND_sum_outgreen, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outgreen_3group)
table(UKBdata2$MIND_sum_outgreen_3group,UKBdata2$status)
#
UKBdata2[,MIND_sum_outother:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score",
                                                                      "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                      "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                      "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outother_3group<- cut(UKBdata2$MIND_sum_outother, 
                                        breaks = quantile(UKBdata2$MIND_sum_outother, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outother_3group)
table(UKBdata2$MIND_sum_outother_3group,UKBdata2$status)
#
UKBdata2[,MIND_sum_outberries:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                        "Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                        "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                        "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outberries_3group<- cut(UKBdata2$MIND_sum_outberries, 
                                          breaks = quantile(UKBdata2$MIND_sum_outberries, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outberries_3group)
table(UKBdata2$MIND_sum_outberries_3group,UKBdata2$status)
#nut
UKBdata2[,MIND_sum_outnut:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                    "Berries_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                    "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                    "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outnut_3group<- cut(UKBdata2$MIND_sum_outnut, 
                                      breaks = quantile(UKBdata2$MIND_sum_outnut, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outnut_3group)
table(UKBdata2$MIND_sum_outnut_3group,UKBdata2$status)
#butter
UKBdata2[,MIND_sum_outbutter:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                       "Berries_sum1_score","Nuts_sum1_score","cheese_sum1_score",
                                                                       "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                       "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outbutter_3group<- cut(UKBdata2$MIND_sum_outbutter, 
                                         breaks = quantile(UKBdata2$MIND_sum_outbutter, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outbutter_3group)
table(UKBdata2$MIND_sum_outbutter_3group,UKBdata2$status)
#chess
UKBdata2[,MIND_sum_outchess:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                      "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score",
                                                                      "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                      "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outchess_3group<- cut(UKBdata2$MIND_sum_outchess, 
                                        breaks = quantile(UKBdata2$MIND_sum_outchess, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outchess_3group)
table(UKBdata2$MIND_sum_outchess_3group,UKBdata2$status)
#grain
UKBdata2[,MIND_sum_outgrain:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                      "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                      "fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                      "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outgrain_3group<- cut(UKBdata2$MIND_sum_outgrain, 
                                        breaks = quantile(UKBdata2$MIND_sum_outgrain, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outgrain_3group)
table(UKBdata2$MIND_sum_outgrain_3group,UKBdata2$status)
#排除sea food
UKBdata2[,MIND_sum_outsea:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                    "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                    "Whole_grains_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                    "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outsea_3group<- cut(UKBdata2$MIND_sum_outsea, 
                                      breaks = quantile(UKBdata2$MIND_sum_outsea, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outsea_3group)
table(UKBdata2$MIND_sum_outsea_3group,UKBdata2$status)
#排除Legumes
UKBdata2[,MIND_sum_outLegumes:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                        "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                        "Whole_grains_sum1_score","fish_sum1_score","poultry_sum1_score",
                                                                        "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outLegumes_3group<- cut(UKBdata2$MIND_sum_outLegumes, 
                                          breaks = quantile(UKBdata2$MIND_sum_outLegumes, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outLegumes_3group)
table(UKBdata2$MIND_sum_outLegumes_3group,UKBdata2$status)
#排除Poultry
UKBdata2[,MIND_sum_outPoultry:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                        "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                        "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score",
                                                                        "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outPoultry_3group<- cut(UKBdata2$MIND_sum_outPoultry, 
                                          breaks = quantile(UKBdata2$MIND_sum_outPoultry, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outPoultry_3group)
table(UKBdata2$MIND_sum_outPoultry_3group,UKBdata2$status)
#排除red meat
UKBdata2[,MIND_sum_outmeat:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                     "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                     "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                     "fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outmeat_3group<- cut(UKBdata2$MIND_sum_outmeat, 
                                       breaks = quantile(UKBdata2$MIND_sum_outmeat, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outmeat_3group)
table(UKBdata2$MIND_sum_outmeat_3group,UKBdata2$status)
#排除fast/fied fodd
UKBdata2[,MIND_sum_outfast:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                     "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                     "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                     "red_meat_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outfast_3group<- cut(UKBdata2$MIND_sum_outfast, 
                                       breaks = quantile(UKBdata2$MIND_sum_outfast, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outfast_3group)
table(UKBdata2$MIND_sum_outfast_3group,UKBdata2$status)
#排除Pastries
UKBdata2[,MIND_sum_outPastries:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                         "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                         "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                         "red_meat_sum1_score","fastfried_food_sum1_score","wine_sum1_score")]
UKBdata2$MIND_sum_outPastries_3group<- cut(UKBdata2$MIND_sum_outPastries, 
                                           breaks = quantile(UKBdata2$MIND_sum_outPastries, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outPastries_3group)
table(UKBdata2$MIND_sum_outPastries_3group,UKBdata2$status)
#排除wine
UKBdata2[,MIND_sum_outwine:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                                     "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                                     "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                                     "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score")]
UKBdata2$MIND_sum_outwine_3group<- cut(UKBdata2$MIND_sum_outwine, 
                                       breaks = quantile(UKBdata2$MIND_sum_outwine, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MIND_sum_outwine_3group)
table(UKBdata2$MIND_sum_outwine_3group,UKBdata2$status)
library(survival)
#Model 4进一步调整Total_Cholesterol,HDL,tri,CRP,monocyte_count,Albumin
colnames(UKBdata2)
var_list <- colnames(UKBdata2[,c(267,269,271,273,275,277,279,281,283,285,287,289,291,293)]) # 变量列表
result <- c() # 结果向量
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(268,270,272,274,276,278,280,282,284,286,288,290,292,294)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#MEDAS####
#MEDAS_sum_outvegetable
UKBdata2[,MEDAS_sum_outvegetable:= rowSums(.SD, na.rm = TRUE), .SDcols = c("fruit_sum1_score",
                                                                           "Mred_meat_sum1_score","MButter_sum1_score","MSweetened_sum1_score",
                                                                           "Mwine_sum1_score","Mbean_sum1_score","Mfish_sum1_score","MPastries_sweets_sum1_score",
                                                                           "MNuts_sum1_score","Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outvegetable_3group<- cut(UKBdata2$MEDAS_sum_outvegetable, 
                                             breaks = quantile(UKBdata2$MEDAS_sum_outvegetable, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outvegetable_3group)
table(UKBdata2$MEDAS_sum_outvegetable_3group,UKBdata2$status)
#MEDAS_sum_outFruits
UKBdata2[,MEDAS_sum_outFruits:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score",
                                                                        "Mred_meat_sum1_score","MButter_sum1_score","MSweetened_sum1_score",
                                                                        "Mwine_sum1_score","Mbean_sum1_score","Mfish_sum1_score","MPastries_sweets_sum1_score",
                                                                        "MNuts_sum1_score","Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outFruits_3group<- cut(UKBdata2$MEDAS_sum_outFruits, 
                                          breaks = quantile(UKBdata2$MEDAS_sum_outFruits, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outFruits_3group)
table(UKBdata2$MEDAS_sum_outFruits_3group,UKBdata2$status)
#MEDAS_sum_outNuts 
UKBdata2[,MEDAS_sum_outNuts:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score",
                                                                      "Mred_meat_sum1_score","MButter_sum1_score","MSweetened_sum1_score",
                                                                      "Mwine_sum1_score","Mbean_sum1_score","Mfish_sum1_score","MPastries_sweets_sum1_score",
                                                                      "Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outNuts_3group<- cut(UKBdata2$MEDAS_sum_outNuts, 
                                        breaks = quantile(UKBdata2$MEDAS_sum_outNuts, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outNuts_3group)
table(UKBdata2$MEDAS_sum_outNuts_3group,UKBdata2$status)
#MEDAS_sum_outButter 
UKBdata2[,MEDAS_sum_outButter:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score",
                                                                        "Mred_meat_sum1_score","MSweetened_sum1_score",
                                                                        "Mwine_sum1_score","Mbean_sum1_score","Mfish_sum1_score","MPastries_sweets_sum1_score",
                                                                        "Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outButter_3group<- cut(UKBdata2$MEDAS_sum_outButter, 
                                          breaks = quantile(UKBdata2$MEDAS_sum_outButter, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outButter_3group)
table(UKBdata2$MEDAS_sum_outButter_3group,UKBdata2$status)
#MEDAS_sum_outSeafood
UKBdata2[,MEDAS_sum_outSeafood:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                         "Mred_meat_sum1_score","MSweetened_sum1_score",
                                                                         "Mwine_sum1_score","Mbean_sum1_score","MPastries_sweets_sum1_score",
                                                                         "Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outSeafood_3group<- cut(UKBdata2$MEDAS_sum_outSeafood, 
                                           breaks = quantile(UKBdata2$MEDAS_sum_outSeafood, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outSeafood_3group)
table(UKBdata2$MEDAS_sum_outSeafood_3group,UKBdata2$status)
#MEDAS_sum_outLegumes
UKBdata2[,MEDAS_sum_outLegumes:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                         "Mfish_sum1_score","Mred_meat_sum1_score","MSweetened_sum1_score",
                                                                         "Mwine_sum1_score","MPastries_sweets_sum1_score",
                                                                         "Mpoultry_sum1_score")]
UKBdata2$MEDAS_sum_outLegumes_3group<- cut(UKBdata2$MEDAS_sum_outLegumes, 
                                           breaks = quantile(UKBdata2$MEDAS_sum_outLegumes, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outLegumes_3group)
table(UKBdata2$MEDAS_sum_outLegumes_3group,UKBdata2$status)
#MEDAS_sum_outPoultry
UKBdata2[,MEDAS_sum_outPoultry:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                         "Mfish_sum1_score","Mbean_sum1_score","Mred_meat_sum1_score","MSweetened_sum1_score",
                                                                         "Mwine_sum1_score","MPastries_sweets_sum1_score")]
UKBdata2$MEDAS_sum_outPoultry_3group<- cut(UKBdata2$MEDAS_sum_outPoultry, 
                                           breaks = quantile(UKBdata2$MEDAS_sum_outPoultry, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outPoultry_3group)
table(UKBdata2$MEDAS_sum_outPoultry_3group,UKBdata2$status)
#MEDAS_sum_outred
UKBdata2[,MEDAS_sum_outred:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                     "Mfish_sum1_score","Mbean_sum1_score","Mpoultry_sum1_score","MSweetened_sum1_score",
                                                                     "Mwine_sum1_score","MPastries_sweets_sum1_score")]
UKBdata2$MEDAS_sum_outred_3group<- cut(UKBdata2$MEDAS_sum_outred, 
                                       breaks = quantile(UKBdata2$MEDAS_sum_outred, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outred_3group)
table(UKBdata2$MEDAS_sum_outred_3group,UKBdata2$status)
#MEDAS_sum_outPastries
UKBdata2[,MEDAS_sum_outPastries:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                          "Mfish_sum1_score","Mbean_sum1_score","Mpoultry_sum1_score","MSweetened_sum1_score",
                                                                          "Mwine_sum1_score","Mred_meat_sum1_score")]
UKBdata2$MEDAS_sum_outPastries_3group<- cut(UKBdata2$MEDAS_sum_outPastries, 
                                            breaks = quantile(UKBdata2$MEDAS_sum_outPastries, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outPastries_3group)
table(UKBdata2$MEDAS_sum_outPastries_3group,UKBdata2$status)
#MEDAS_sum_outWine
UKBdata2[,MEDAS_sum_outWine:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                      "Mfish_sum1_score","Mbean_sum1_score","Mpoultry_sum1_score","MSweetened_sum1_score",
                                                                      "Mred_meat_sum1_score","MPastries_sweets_sum1_score")]
UKBdata2$MEDAS_sum_outWine_3group<- cut(UKBdata2$MEDAS_sum_outWine, 
                                        breaks = quantile(UKBdata2$MEDAS_sum_outWine, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outWine_3group)
table(UKBdata2$MEDAS_sum_outWine_3group,UKBdata2$status)
#MEDAS_sum_outsweetened
UKBdata2[,MEDAS_sum_outsweetened:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score","MNuts_sum1_score","MButter_sum1_score",
                                                                           "Mfish_sum1_score","Mbean_sum1_score","Mpoultry_sum1_score","Mwine_sum1_score",
                                                                           "Mred_meat_sum1_score","MPastries_sweets_sum1_score")]
UKBdata2$MEDAS_sum_outsweetened_3group<- cut(UKBdata2$MEDAS_sum_outsweetened, 
                                             breaks = quantile(UKBdata2$MEDAS_sum_outsweetened, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
table(UKBdata2$MEDAS_sum_outsweetened_3group)
table(UKBdata2$MEDAS_sum_outsweetened_3group,UKBdata2$status)
#Model 4进一步调整Total_Cholesterol,HDL,tri,CRP,monocyte_count,Albumin
colnames(UKBdata2)
var_list <- colnames(UKBdata2[,c(302,304,306,308,310,312,314,316,318,320,322)]) # 变量列表
result <- c() # 结果向量
for (var in var_list) {
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(303,305,307,309,311,313,315,317,319,321,323)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)

#4.排除肿瘤发生在前两年的个体####
table(UKBdata2$paichuqianliangnian)
library(tidyverse)
UKBdata3<-UKBdata2%>%filter((paichuqianliangnian!=1)%>% replace_na(TRUE))
#
table(UKBdata3$MIND_sum1_3group)
table(UKBdata3$MEDAS_sum1_3group)
table(UKBdata3$MIND_sum1_3group,UKBdata3$status)
table(UKBdata3$MEDAS_sum1_3group,UKBdata3$status)
library(survival)
#Model 1仅进行能量调整
colnames(UKBdata3)
result<-c()
var_list<-colnames(UKBdata3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy,data=UKBdata3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_1.csv",row.names = F)
result<-c()
for (i in UKBdata3[,c(298,297)]){
  UKBdata3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_2.csv",row.names = F)
#Model 2进一步调整sex,education,income, index, family history
result<-c()
var_list<-colnames(UKBdata3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata3[,c(298,297)]){
  UKBdata3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3进一步调整Smoking, alcohol, physical activity, BMI, WHR, SBP
result<-c()
var_list<-colnames(UKBdata3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata3[,c(298,297)]){
  UKBdata3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#5.进一步调整营养素####
#重新读取数据####
UKBdata2<-fread("D:/deskbook/UKBdata2.csv",header=T)
colnames(UKBdata2)
library(survival)
#Model 5
result<-c()
var_list<-colnames(UKBdata3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+
                `100003_1`+`100004_1`+`100005_1`,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(298,297)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+
                `100003_1`+`100004_1`+`100005_1`,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#model 6
result<-c()
var_list<-colnames(UKBdata3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+
                `100003_1`+`100004_1`+`100005_1`+`100009_1`,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(298,297)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+
                `100003_1`+`100004_1`+`100005_1`+`100009_1`,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#6.排除每种肿瘤####
#nohead#
table(UKBdata2$various_cancer)
data_nohead<-subset(UKBdata2,various_cancer!="head_and_neck")
table(data_nohead$MEDAS_sum1_3group)
result_nohead<-c()
for (i in data_nohead[,c(298,297)]){
  data_nohead$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nohead)
  result_nohead<-rbind(result_nohead,c(table(data_nohead$i),table(data_nohead$i,data_nohead$status)[,2],
                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nohead)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nohead<-data.frame(result_nohead)
result_nohead<-result_nohead%>% mutate(cancer=c("nohead"),.before=x1)
#noesophagus#
data_noesophagus<-subset(UKBdata2,various_cancer!="esophagus")
result_noesophagus<-c()
for (i in data_noesophagus[,c(298,297)]){
  data_noesophagus$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noesophagus)
  result_noesophagus<-rbind(result_noesophagus,c(table(data_noesophagus$i),table(data_noesophagus$i,data_noesophagus$status)[,2],
                                                 exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noesophagus)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noesophagus<-data.frame(result_noesophagus)
result_noesophagus<-result_noesophagus%>% mutate(cancer=c("noesophagus"),.before=x1)
#nostomach#
data_nostomach<-subset(UKBdata2,various_cancer!="stomach")
result_nostomach<-c()
for (i in data_nostomach[,c(298,297)]){
  data_nostomach$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nostomach)
  result_nostomach<-rbind(result_nostomach,c(table(data_nostomach$i),table(data_nostomach$i,data_nostomach$status)[,2],
                                             exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nostomach)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nostomach<-data.frame(result_nostomach)
result_nostomach<-result_nostomach%>% mutate(cancer=c("nostomach"),.before=x1)
#nocolorectal#
data_nocolorectal<-subset(UKBdata2,various_cancer!="colorectal")
result_nocolorectal<-c()
for (i in data_nocolorectal[,c(298,297)]){
  data_nocolorectal$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nocolorectal)
  result_nocolorectal<-rbind(result_nocolorectal,c(table(data_nocolorectal$i),table(data_nocolorectal$i,data_nocolorectal$status)[,2],
                                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nocolorectal)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nocolorectal<-data.frame(result_nocolorectal)
result_nocolorectal<-result_nocolorectal%>% mutate(cancer=c("nocolorectal"),.before=x1)
#noliver#
data_noliver<-subset(UKBdata2,various_cancer!="liver")
result_noliver<-c()
for (i in data_noliver[,c(298,297)]){
  data_noliver$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noliver)
  result_noliver<-rbind(result_noliver,c(table(data_noliver$i),table(data_noliver$i,data_noliver$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noliver)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noliver<-data.frame(result_noliver)
result_noliver<-result_noliver%>% mutate(cancer=c("noliver"),.before=x1)
#nogallbladder#
data_nogallbladder<-subset(UKBdata2,various_cancer!="gallbladder")
result_nogallbladder<-c()
for (i in data_nogallbladder[,c(298,297)]){
  data_nogallbladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nogallbladder)
  result_nogallbladder<-rbind(result_nogallbladder,c(table(data_nogallbladder$i),table(data_nogallbladder$i,data_nogallbladder$status)[,2],
                                                     exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nogallbladder)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nogallbladder<-data.frame(result_nogallbladder)
result_nogallbladder<-result_nogallbladder%>% mutate(cancer=c("nogallbladder"),.before=x1)
#nopancreas#
data_nopancreas<-subset(UKBdata2,various_cancer!="pancreas")
result_nopancreas<-c()
for (i in data_nopancreas[,c(298,297)]){
  data_nopancreas$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nopancreas)
  result_nopancreas<-rbind(result_nopancreas,c(table(data_nopancreas$i),table(data_nopancreas$i,data_nopancreas$status)[,2],
                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nopancreas)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nopancreas<-data.frame(result_nopancreas)
result_nopancreas<-result_nopancreas%>% mutate(cancer=c("nopancreas"),.before=x1)
#nolung#
data_nolung<-subset(UKBdata2,various_cancer!="lung")
result_nolung<-c()
for (i in data_nolung[,c(298,297)]){
  data_nolung$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nolung)
  result_nolung<-rbind(result_nolung,c(table(data_nolung$i),table(data_nolung$i,data_nolung$status)[,2],
                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nolung)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nolung<-data.frame(result_nolung)
result_nolung<-result_nolung%>% mutate(cancer=c("nolung"),.before=x1)
#nomalignant_melanoma#
data_nomalignant_melanoma<-subset(UKBdata2,various_cancer!="malignant_melanoma")
result_nomalignant_melanoma<-c()
for (i in data_nomalignant_melanoma[,c(298,297)]){
  data_nomalignant_melanoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nomalignant_melanoma)
  result_nomalignant_melanoma<-rbind(result_nomalignant_melanoma,c(table(data_nomalignant_melanoma$i),table(data_nomalignant_melanoma$i,data_nomalignant_melanoma$status)[,2],
                                                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nomalignant_melanoma)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nomalignant_melanoma<-data.frame(result_nomalignant_melanoma)
result_nomalignant_melanoma<-result_nomalignant_melanoma%>% mutate(cancer=c("nomalignant_melanoma"),.before=x1)
#nobreast#
data_nobreast<-subset(UKBdata2,various_cancer!="breast")
result_nobreast<-c()
for (i in data_nobreast[,c(298,297)]){
  data_nobreast$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nobreast)
  result_nobreast<-rbind(result_nobreast,c(table(data_nobreast$i),table(data_nobreast$i,data_nobreast$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nobreast)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nobreast<-data.frame(result_nobreast)
result_nobreast<-result_nobreast%>% mutate(cancer=c("nobreast"),.before=x1)
#nouterine#
table(UKBdata2$various_cancer)
data_nouterine<-subset(UKBdata2,various_cancer!="uterus_cervix")
table(data_nouterine$various_cancer)
result_nouterine<-c()
for (i in data_nouterine[,c(298,297)]){
  data_nouterine$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nouterine)
  result_nouterine<-rbind(result_nouterine,c(table(data_nouterine$i),table(data_nouterine$i,data_nouterine$status)[,2],
                                             exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nouterine)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nouterine<-data.frame(result_nouterine)
result_nouterine<-result_nouterine%>% mutate(cancer=c("nouterine"),.before=x1)
#noovary#
data_noovary<-subset(UKBdata2,various_cancer!="ovary")
result_noovary<-c()
for (i in data_noovary[,c(298,297)]){
  data_noovary$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noovary)
  result_noovary<-rbind(result_noovary,c(table(data_noovary$i),table(data_noovary$i,data_noovary$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noovary)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noovary<-data.frame(result_noovary)
result_noovary<-result_noovary%>% mutate(cancer=c("noovary"),.before=x1)
#noprostate#
data_noprostate<-subset(UKBdata2,various_cancer!="prostate")
result_noprostate<-c()
for (i in data_noprostate[,c(298,297)]){
  data_noprostate$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noprostate)
  result_noprostate<-rbind(result_noprostate,c(table(data_noprostate$i),table(data_noprostate$i,data_noprostate$status)[,2],
                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noprostate)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noprostate<-data.frame(result_noprostate)
result_noprostate<-result_noprostate%>% mutate(cancer=c("noprostate"),.before=x1)
#notestic#
data_notestic<-subset(UKBdata2,various_cancer!="testic")
result_notestic<-c()
for (i in data_notestic[,c(298,297)]){
  data_notestic$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_notestic)
  result_notestic<-rbind(result_notestic,c(table(data_notestic$i),table(data_notestic$i,data_notestic$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_notestic)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_notestic<-data.frame(result_notestic)
result_notestic<-result_notestic%>% mutate(cancer=c("notestic"),.before=x1)
#nokidney#
data_nokidney<-subset(UKBdata2,various_cancer!="kidney")
result_nokidney<-c()
for (i in data_nokidney[,c(298,297)]){
  print(table(data_nokidney$various_cancer))
  data_nokidney$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nokidney)
  result_nokidney<-rbind(result_nokidney,c(table(data_nokidney$i),table(data_nokidney$i,data_nokidney$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nokidney)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nokidney<-data.frame(result_nokidney)
result_nokidney<-result_nokidney%>% mutate(cancer=c("nokidney"),.before=x1)
#nobladder#
data_nobladder<-subset(UKBdata2,various_cancer!="bladder")
result_nobladder<-c()
for (i in data_nobladder[,c(298,297)]){
  print(table(data_nobladder$various_cancer))
  data_nobladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nobladder)
  result_nobladder<-rbind(result_nobladder,c(table(data_nobladder$i),table(data_nobladder$i,data_nobladder$status)[,2],
                                             exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nobladder)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nobladder<-data.frame(result_nobladder)
result_nobladder<-result_nobladder%>% mutate(cancer=c("nobladder"),.before=x1)
#nobrain#
data_nobrain<-subset(UKBdata2,various_cancer!="brain")
result_nobrain<-c()
for (i in data_nobrain[,c(298,297)]){
  print(table(data_nobrain$various_cancer))
  data_nobrain$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nobrain)
  result_nobrain<-rbind(result_nobrain,c(table(data_nobrain$i),table(data_nobrain$i,data_nobrain$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nobrain)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nobrain<-data.frame(result_nobrain)
result_nobrain<-result_nobrain%>% mutate(cancer=c("nobrain"),.before=x1)
#nothroid#
data_nothroid<-subset(UKBdata2,various_cancer!="thyroid")
result_nothroid<-c()
for (i in data_nothroid[,c(298,297)]){
  print(table(data_nothroid$various_cancer))
  data_nothroid$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nothroid)
  result_nothroid<-rbind(result_nothroid,c(table(data_nothroid$i),table(data_nothroid$i,data_nothroid$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nothroid)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nothroid<-data.frame(result_nothroid)
result_nothroid<-result_nothroid%>% mutate(cancer=c("nothroid"),.before=x1)
#noHodgkin_lymphoma#
data_noHodgkin_lymphoma<-subset(UKBdata2,various_cancer!="Hodgkin_lymphoma")
result_noHodgkin_lymphoma<-c()
for (i in data_noHodgkin_lymphoma[,c(298,297)]){
  print(table(data_noHodgkin_lymphoma$various_cancer))
  data_noHodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noHodgkin_lymphoma)
  result_noHodgkin_lymphoma<-rbind(result_noHodgkin_lymphoma,c(table(data_noHodgkin_lymphoma$i),table(data_noHodgkin_lymphoma$i,data_noHodgkin_lymphoma$status)[,2],
                                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noHodgkin_lymphoma)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noHodgkin_lymphoma<-data.frame(result_noHodgkin_lymphoma)
result_noHodgkin_lymphoma<-result_noHodgkin_lymphoma%>% mutate(cancer=c("noHodgkin_lymphoma"),.before=x1)
#nonon_Hodgkin_lymphoma#
data_nonon_Hodgkin_lymphoma<-subset(UKBdata2,various_cancer!="non_Hodgkin_lymphoma")
result_nonon_Hodgkin_lymphoma<-c()
for (i in data_nonon_Hodgkin_lymphoma[,c(298,297)]){
  print(table(data_nonon_Hodgkin_lymphoma$various_cancer))
  data_nonon_Hodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nonon_Hodgkin_lymphoma)
  result_nonon_Hodgkin_lymphoma<-rbind(result_nonon_Hodgkin_lymphoma,c(table(data_nonon_Hodgkin_lymphoma$i),table(data_nonon_Hodgkin_lymphoma$i,data_nonon_Hodgkin_lymphoma$status)[,2],
                                                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nonon_Hodgkin_lymphoma)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nonon_Hodgkin_lymphoma<-data.frame(result_nonon_Hodgkin_lymphoma)
result_nonon_Hodgkin_lymphoma<-result_nonon_Hodgkin_lymphoma%>% mutate(cancer=c("nonon_Hodgkin_lymphoma"),.before=x1)
#nomultiple_myeloma#
data_nomultiple_myeloma<-subset(UKBdata2,various_cancer!="multiple_myeloma")
result_nomultiple_myeloma<-c()
for (i in data_nomultiple_myeloma[,c(298,297)]){
  print(table(data_nomultiple_myeloma$various_cancer))
  data_nomultiple_myeloma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_nomultiple_myeloma)
  result_nomultiple_myeloma<-rbind(result_nomultiple_myeloma,c(table(data_nomultiple_myeloma$i),table(data_nomultiple_myeloma$i,data_nomultiple_myeloma$status)[,2],
                                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_nomultiple_myeloma)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_nomultiple_myeloma<-data.frame(result_nomultiple_myeloma)
result_nomultiple_myeloma<-result_nomultiple_myeloma%>% mutate(cancer=c("nomultiple_myeloma"),.before=x1)
#noleukemia#
data_noleukemia<-subset(UKBdata2,various_cancer!="leukemia")
result_noleukemia<-c()
for (i in data_noleukemia[,c(298,297)]){
  print(table(data_noleukemia$various_cancer))
  data_noleukemia$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=data_noleukemia)
  result_noleukemia<-rbind(result_noleukemia,c(table(data_noleukemia$i),table(data_noleukemia$i,data_noleukemia$status)[,2],
                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_noleukemia)<-c("x1","x2","x3","x11","x12","x13","HR_1","HR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_noleukemia<-data.frame(result_noleukemia)
result_noleukemia<-result_noleukemia%>% mutate(cancer=c("noleukemia"),.before=x1)
#合并起来
result_nopancancer<-rbind(result_nohead,result_noesophagus,result_nostomach,result_nocolorectal,
                          result_noliver,result_nogallbladder,result_nopancreas,result_nolung,
                          result_nomalignant_melanoma,result_nobreast,result_nouterine,
                          result_noovary,result_noprostate,result_notestic,result_nokidney,
                          result_nobladder,result_nobrain,result_nothroid,result_noHodgkin_lymphoma,
                          result_nonon_Hodgkin_lymphoma,result_nomultiple_myeloma,result_noleukemia)
write.csv(result_nopancancer,"E:/deskbook/result_nopancancer.csv",row.names = F)
#7.排除自评幸福感差的人群####
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header=T)
happiness<-fread("E:/deskbook/happiness.csv",header=T)
happiness$rating<-happiness[["2178-0.0"]]
summary(happiness$rating)
happiness2<-subset(happiness,select=c(1,6))
UKBdata2<-merge(UKBdata2,happiness2,by="eid",all.x = T)
table(UKBdata2$rating)
UKBdata2_3<-UKBdata2%>%filter(UKBdata2$rating==1|UKBdata2$rating==2)
table(UKBdata2_3$MIND_sum1_3group)
table(UKBdata2_3$MIND_sum1_3group,UKBdata2_3$status)
table(UKBdata2_3$MEDAS_sum1_3group)
table(UKBdata2_3$MEDAS_sum1_3group,UKBdata2_3$status)
library(survival)
colnames(UKBdata2_3)
#model 1
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 2进一步调整sex,education,income, index, family history
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3进一步调整Smoking, alcohol, physical activity, BMI, WHR, SBP
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)




#9.仅针对基线时无恶性肿瘤和良性肿瘤####
colnames(UKBdata2)
table(UKBdata2$Exclusion_2)
UKBdata2_3<-UKBdata2%>%filter(UKBdata2$Exclusion_2!=1)
table(UKBdata2_3$MIND_sum1_3group)
table(UKBdata2_3$MIND_sum1_3group,UKBdata2_3$status)
table(UKBdata2_3$MEDAS_sum1_3group)
table(UKBdata2_3$MEDAS_sum1_3group,UKBdata2_3$status)
library(survival)
colnames(UKBdata2_3)
#model 1
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 2进一步调整sex,education,income, index, family history
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3进一步调整Smoking, alcohol, physical activity, BMI, WHR, SBP
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)

#10.仅针对进行过两次以上的膳食调查的人群做分析####
ICD<-fread("E:/deskbook/ICD.csv",header = T)
colnames(ICD)
table(ICD$Endpoint1)
ICD$Endpoint1[is.na(ICD$Endpoint1)] <- 0
filtered_rows <- apply(ICD, 1, function(row) {
  endpoint <- as.numeric(row["Endpoint1"])
  
  if (!is.na(endpoint)) {
    if (endpoint == 0) {
      sum(!is.na(row[80:85])) >= 2
    } else if (endpoint == 1) {
      sum(!is.na(row[80:85]) & row[80:85] < row[13]) >= 2
    } else if (endpoint == 2) {
      sum(!is.na(row[80:85]) & row[80:85] < row[14]) >= 2
    } else if (endpoint == 3) {
      sum(!is.na(row[80:85]) & row[80:85] < row[15]) >= 2
    } else if (endpoint == 4) {
      sum(!is.na(row[80:85]) & row[80:85] < row[16]) >= 2
    } else if (endpoint == 5) {
      sum(!is.na(row[80:85]) & row[80:85] < row[17]) >= 2
    } else if (endpoint == 6) {
      sum(!is.na(row[80:85]) & row[80:85] < row[18]) >= 2
    } else if (endpoint == 7) {
      sum(!is.na(row[80:85]) & row[80:85] < row[19]) >= 2
    } else {
      FALSE
    }
  } else {
    FALSE
  }
})
result_df <- ICD[filtered_rows, ]
colnames(result_df)
result_df <-subset(result_df,select = 1)
UKBdata2_3<-merge(UKBdata2,result_df)
#
table(UKBdata2_3$MIND_sum1_3group)
table(UKBdata2_3$MIND_sum1_3group,UKBdata2_3$status)
table(UKBdata2_3$MEDAS_sum1_3group)
table(UKBdata2_3$MEDAS_sum1_3group,UKBdata2_3$status)
library(survival)
colnames(UKBdata2_3)
#model 1
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 2进一步调整sex,education,income, index, family history
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3进一步调整Smoking, alcohol, physical activity, BMI, WHR, SBP
result<-c()
var_list<-colnames(UKBdata2_3[,c(168,59)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2_3)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2_3[,c(298,297)]){
  UKBdata2_3$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata2_3)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#11.进一步调整吸烟和饮酒强度####
library(data.table)
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header = T)
UKBdata2$various_cancer[UKBdata2$various_cancer == "non-Hodgkin_lymphoma"] <- "non_Hodgkin_lymphoma"
UKBdata2$various_cancer[UKBdata2$various_cancer == "throid"] <- "thyroid"
UKBdata2$various_cancer[UKBdata2$various_cancer == "cervix"|UKBdata2$various_cancer == "uterine"] <- "uterus_cervix"
UKBdata2$energy<-UKBdata2$`100002_1`/4.184
smoking_intensity<-fread("E:/deskbook/smoking_intensity.csv",header = T)
UKBdata_sen11<-merge(UKBdata2,smoking_intensity,by="eid",all.x = T)
colnames(UKBdata_sen11)
missing_vals <- colSums(is.na(UKBdata_sen11[,268:321]))
print(missing_vals)
UKBdata_sen11$alcohol_frequency<-UKBdata_sen11$`1558-0.0`
summary(UKBdata_sen11$alcohol_frequency)
summary(UKBdata_sen11$`2887-0.0`)
table(UKBdata_sen11$`2887-0.0`)
UKBdata_sen11$smoking_intensity<-UKBdata_sen11$`20161-0.0`
library(tidyverse)
UKBdata_sen<-UKBdata_sen11%>%filter(!(is.na(smoking_intensity)))
table(UKBdata_sen$status)#52723obs,44021controls,8702 cases
UKBdata_sen$alcohol_frequency<-factor(UKBdata_sen$alcohol_frequency)
UKBdata_sen$MIND_sum1_3group <- cut(UKBdata_sen$MIND_sum1, 
                                 breaks = quantile(UKBdata_sen$MIND_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
UKBdata_sen$MEDAS_sum1_3group <- cut(UKBdata_sen$MEDAS_sum1, 
                                  breaks = quantile(UKBdata_sen$MEDAS_sum1, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组                                    
#做与总癌与各个癌症的关联####
library(survival)
colnames(UKBdata_sen)
table(UKBdata_sen$various_cancer)
#总癌
UKBdata_sen_overall<-UKBdata_sen
result_overall<-c()
for (i in UKBdata_sen_overall[,c(325,324)]){
  UKBdata_sen_overall$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alocohol_frequency,data=UKBdata_sen_overall)
  result_overall<-rbind(result_overall,c(table(UKBdata_sen_overall$i),table(UKBdata_sen_overall$i,UKBdata_sen_overall$status)[,2],
                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_overall)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_overall<-data.frame(result_overall)
result_overall<-result_overall%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_overall<-result_overall%>% mutate(cancer=c("overall"),.before=diet)
#head
UKBdata_sen_head<-subset(UKBdata_sen,various_cancer=="head_and_neck"|various_cancer==0)
result_head<-c()
for (i in UKBdata_sen_head[,c(325,324)]){
  UKBdata_sen_head$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity_alcohol_frequency,data=UKBdata_sen_head)
  result_head<-rbind(result_head,c(table(UKBdata_sen_head$i),table(UKBdata_sen_head$i,UKBdata_sen_head$status)[,2],
                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_head)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_head<-data.frame(result_head)
result_head<-result_head%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_head<-result_head%>% mutate(cancer=c("head"),.before=diet)

#esophagus
UKBdata_sen_esophagus<-subset(UKBdata_sen,various_cancer=="esophagus"|various_cancer==0)
result_esophagus<-c()
for (i in UKBdata_sen_esophagus[,c(325,324)]){
  UKBdata_sen_esophagus$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_esophagus)
  result_esophagus<-rbind(result_esophagus,c(table(UKBdata_sen_esophagus$i),table(UKBdata_sen_esophagus$i,UKBdata_sen_esophagus$status)[,2],
                                             exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_esophagus)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_esophagus<-data.frame(result_esophagus)
result_esophagus<-result_esophagus%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_esophagus<-result_esophagus%>% mutate(cancer=c("esophagus"),.before=diet)

#stomach
UKBdata_sen_stomach<-subset(UKBdata_sen,various_cancer=="stomach"|various_cancer==0)
result_stomach<-c()
for (i in UKBdata_sen_stomach[,c(325,324)]){
  UKBdata_sen_stomach$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_stomach)
  result_stomach<-rbind(result_stomach,c(table(UKBdata_sen_stomach$i),table(UKBdata_sen_stomach$i,UKBdata_sen_stomach$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_stomach)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_stomach<-data.frame(result_stomach)
result_stomach<-result_stomach%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_stomach<-result_stomach%>% mutate(cancer=c("stomach"),.before=diet)

#colorectal
UKBdata_sen_colorectal<-subset(UKBdata_sen,various_cancer=="colorectal"|various_cancer==0)
result_colorectal<-c()
for (i in UKBdata_sen_colorectal[,c(325,324)]){
  UKBdata_sen_colorectal$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_colorectal)
  result_colorectal<-rbind(result_colorectal,c(table(UKBdata_sen_colorectal$i),table(UKBdata_sen_colorectal$i,UKBdata_sen_colorectal$status)[,2],
                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_colorectal)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_colorectal<-data.frame(result_colorectal)
result_colorectal<-result_colorectal%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_colorectal<-result_colorectal%>% mutate(cancer=c("colorectal"),.before=diet)

#liver
UKBdata_sen_liver<-subset(UKBdata_sen,various_cancer=="liver"|various_cancer==0)
result_liver<-c()
for (i in UKBdata_sen_liver[,c(325,324)]){
  UKBdata_sen_liver$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_liver)
  result_liver<-rbind(result_liver,c(table(UKBdata_sen_liver$i),table(UKBdata_sen_liver$i,UKBdata_sen_liver$status)[,2],
                                     exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_liver)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_liver<-data.frame(result_liver)
result_liver<-result_liver%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_liver<-result_liver%>% mutate(cancer=c("liver"),.before=diet)

#gallbladder
UKBdata_sen_gallbladder<-subset(UKBdata_sen,various_cancer=="gallbladder"|various_cancer==0)
result_gallbladder<-c()
for (i in UKBdata_sen_gallbladder[,c(325,324)]){
  UKBdata_sen_gallbladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_gallbladder)
  result_gallbladder<-rbind(result_gallbladder,c(table(UKBdata_sen_gallbladder$i),table(UKBdata_sen_gallbladder$i,UKBdata_sen_gallbladder$status)[,2],
                                                 exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_gallbladder)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_gallbladder<-data.frame(result_gallbladder)
result_gallbladder<-result_gallbladder%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_gallbladder<-result_gallbladder%>% mutate(cancer=c("gallbladder"),.before=diet)

#pancreas
UKBdata_sen_pancreas<-subset(UKBdata_sen,various_cancer=="pancreas"|various_cancer==0)
result_pancreas<-c()
for (i in UKBdata_sen_pancreas[,c(325,324)]){
  UKBdata_sen_pancreas$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_pancreas)
  result_pancreas<-rbind(result_pancreas,c(table(UKBdata_sen_pancreas$i),table(UKBdata_sen_pancreas$i,UKBdata_sen_pancreas$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_pancreas)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_pancreas<-data.frame(result_pancreas)
result_pancreas<-result_pancreas%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_pancreas<-result_pancreas%>% mutate(cancer=c("pancreas"),.before=diet)

#lung
UKBdata_sen_lung<-subset(UKBdata_sen,various_cancer=="lung"|various_cancer==0)
result_lung<-c()
for (i in UKBdata_sen_lung[,c(325,324)]){
  UKBdata_sen_lung$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_lung)
  result_lung<-rbind(result_lung,c(table(UKBdata_sen_lung$i),table(UKBdata_sen_lung$i,UKBdata_sen_lung$status)[,2],
                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_lung)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_lung<-data.frame(result_lung)
result_lung<-result_lung%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_lung<-result_lung%>% mutate(cancer=c("lung"),.before=diet)

#malignant_melanoma
UKBdata_sen_malignant_melanoma<-subset(UKBdata_sen,various_cancer=="malignant_melanoma"|various_cancer==0)
result_malignant_melanoma<-c()
for (i in UKBdata_sen_malignant_melanoma[,c(325,324)]){
  UKBdata_sen_malignant_melanoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_malignant_melanoma)
  result_malignant_melanoma<-rbind(result_malignant_melanoma,c(table(UKBdata_sen_malignant_melanoma$i),table(UKBdata_sen_malignant_melanoma$i,UKBdata_sen_malignant_melanoma$status)[,2],
                                                               exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_malignant_melanoma)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_malignant_melanoma<-data.frame(result_malignant_melanoma)
result_malignant_melanoma<-result_malignant_melanoma%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_malignant_melanoma<-result_malignant_melanoma%>% mutate(cancer=c("malignant_melanoma"),.before=diet)

#breast
UKBdata_sen_breast<-subset(UKBdata_sen,various_cancer=="breast"|various_cancer==0)
result_breast<-c()
for (i in UKBdata_sen_breast[,c(325,324)]){
  UKBdata_sen_breast$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_breast)
  result_breast<-rbind(result_breast,c(table(UKBdata_sen_breast$i),table(UKBdata_sen_breast$i,UKBdata_sen_breast$status)[,2],
                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_breast)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_breast<-data.frame(result_breast)
result_breast<-result_breast%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_breast<-result_breast%>% mutate(cancer=c("breast"),.before=diet)

#uterine
UKBdata_sen_uterine<-subset(UKBdata_sen,various_cancer=="uterus_cervix"|various_cancer==0)
result_uterine<-c()
for (i in UKBdata_sen_uterine[,c(325,324)]){
  UKBdata_sen_uterine$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_uterine)
  result_uterine<-rbind(result_uterine,c(table(UKBdata_sen_uterine$i),table(UKBdata_sen_uterine$i,UKBdata_sen_uterine$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_uterine)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_uterine<-data.frame(result_uterine)
result_uterine<-result_uterine%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_uterine<-result_uterine%>% mutate(cancer=c("uterine"),.before=diet)

#ovary
UKBdata_sen_ovary<-subset(UKBdata_sen,various_cancer=="ovary"|various_cancer==0)
result_ovary<-c()
for (i in UKBdata_sen_ovary[,c(325,324)]){
  UKBdata_sen_ovary$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_ovary)
  result_ovary<-rbind(result_ovary,c(table(UKBdata_sen_ovary$i),table(UKBdata_sen_ovary$i,UKBdata_sen_ovary$status)[,2],
                                     exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_ovary)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_ovary<-data.frame(result_ovary)
result_ovary<-result_ovary%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_ovary<-result_ovary%>% mutate(cancer=c("ovary"),.before=diet)


#prostate
UKBdata_sen_prostate<-subset(UKBdata_sen,various_cancer=="prostate"|various_cancer==0)
result_prostate<-c()
for (i in UKBdata_sen_prostate[,c(325,324)]){
  UKBdata_sen_prostate$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_prostate)
  result_prostate<-rbind(result_prostate,c(table(UKBdata_sen_prostate$i),table(UKBdata_sen_prostate$i,UKBdata_sen_prostate$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_prostate)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_prostate<-data.frame(result_prostate)
result_prostate<-result_prostate%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_prostate<-result_prostate%>% mutate(cancer=c("prostate"),.before=diet)


#testic
UKBdata_sen_testic<-subset(UKBdata_sen,various_cancer=="testic"|various_cancer==0)
result_testic<-c()
for (i in UKBdata_sen_testic[,c(325,324)]){
  UKBdata_sen_testic$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_testic)
  result_testic<-rbind(result_testic,c(table(UKBdata_sen_testic$i),table(UKBdata_sen_testic$i,UKBdata_sen_testic$status)[,2],
                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_testic)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_testic<-data.frame(result_testic)
result_testic<-result_testic%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_testic<-result_testic%>% mutate(cancer=c("testic"),.before=diet)


#kidney
UKBdata_sen_kidney<-subset(UKBdata_sen,various_cancer=="kidney"|various_cancer==0)
result_kidney<-c()
for (i in UKBdata_sen_kidney[,c(325,324)]){
  UKBdata_sen_kidney$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_kidney)
  result_kidney<-rbind(result_kidney,c(table(UKBdata_sen_kidney$i),table(UKBdata_sen_kidney$i,UKBdata_sen_kidney$status)[,2],
                                       exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_kidney)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_kidney<-data.frame(result_kidney)
result_kidney<-result_kidney%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_kidney<-result_kidney%>% mutate(cancer=c("kidney"),.before=diet)


#bladder
UKBdata_sen_bladder<-subset(UKBdata_sen,various_cancer=="bladder"|various_cancer==0)
result_bladder<-c()
for (i in UKBdata_sen_bladder[,c(325,324)]){
  UKBdata_sen_bladder$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_bladder)
  result_bladder<-rbind(result_bladder,c(table(UKBdata_sen_bladder$i),table(UKBdata_sen_bladder$i,UKBdata_sen_bladder$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_bladder)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_bladder<-data.frame(result_bladder)
result_bladder<-result_bladder%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_bladder<-result_bladder%>% mutate(cancer=c("bladder"),.before=diet)


#brain
UKBdata_sen_brain<-subset(UKBdata_sen,various_cancer=="brain"|various_cancer==0)
result_brain<-c()
for (i in UKBdata_sen_brain[,c(325,324)]){
  UKBdata_sen_brain$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_brain)
  result_brain<-rbind(result_brain,c(table(UKBdata_sen_brain$i),table(UKBdata_sen_brain$i,UKBdata_sen_brain$status)[,2],
                                     exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_brain)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_brain<-data.frame(result_brain)
result_brain<-result_brain%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_brain<-result_brain%>% mutate(cancer=c("brain"),.before=diet)


#thyroid
UKBdata_sen_thyroid<-subset(UKBdata_sen,various_cancer=="thyroid"|various_cancer==0)
result_thyroid<-c()
for (i in UKBdata_sen_thyroid[,c(325,324)]){
  UKBdata_sen_thyroid$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_thyroid)
  result_thyroid<-rbind(result_thyroid,c(table(UKBdata_sen_thyroid$i),table(UKBdata_sen_thyroid$i,UKBdata_sen_thyroid$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_thyroid)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_thyroid<-data.frame(result_thyroid)
result_thyroid<-result_thyroid%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_thyroid<-result_thyroid%>% mutate(cancer=c("thyroid"),.before=diet)


#Hodgkin_lymphoma
UKBdata_sen_Hodgkin_lymphoma<-subset(UKBdata_sen,various_cancer=="Hodgkin_lymphoma"|various_cancer==0)
result_Hodgkin_lymphoma<-c()
for (i in UKBdata_sen_Hodgkin_lymphoma[,c(325,324)]){
  UKBdata_sen_Hodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_Hodgkin_lymphoma)
  result_Hodgkin_lymphoma<-rbind(result_Hodgkin_lymphoma,c(table(UKBdata_sen_Hodgkin_lymphoma$i),table(UKBdata_sen_Hodgkin_lymphoma$i,UKBdata_sen_Hodgkin_lymphoma$status)[,2],
                                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_Hodgkin_lymphoma)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_Hodgkin_lymphoma<-data.frame(result_Hodgkin_lymphoma)
result_Hodgkin_lymphoma<-result_Hodgkin_lymphoma%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_Hodgkin_lymphoma<-result_Hodgkin_lymphoma%>% mutate(cancer=c("Hodgkin_lymphoma"),.before=diet)

#non_Hodgkin_lymphoma
UKBdata_sen_non_Hodgkin_lymphoma<-subset(UKBdata_sen,various_cancer=="non_Hodgkin_lymphoma"|various_cancer==0)
result_non_Hodgkin_lymphoma<-c()
for (i in UKBdata_sen_non_Hodgkin_lymphoma[,c(325,324)]){
  UKBdata_sen_non_Hodgkin_lymphoma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_non_Hodgkin_lymphoma)
  result_non_Hodgkin_lymphoma<-rbind(result_non_Hodgkin_lymphoma,c(table(UKBdata_sen_non_Hodgkin_lymphoma$i),table(UKBdata_sen_non_Hodgkin_lymphoma$i,UKBdata_sen_non_Hodgkin_lymphoma$status)[,2],
                                                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_non_Hodgkin_lymphoma)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_non_Hodgkin_lymphoma<-data.frame(result_non_Hodgkin_lymphoma)
result_non_Hodgkin_lymphoma<-result_non_Hodgkin_lymphoma%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_non_Hodgkin_lymphoma<-result_non_Hodgkin_lymphoma%>% mutate(cancer=c("non_Hodgkin_lymphoma"),.before=diet)

#multiple_myeloma
UKBdata_sen_multiple_myeloma<-subset(UKBdata_sen,various_cancer=="multiple_myeloma"|various_cancer==0)
result_multiple_myeloma<-c()
for (i in UKBdata_sen_multiple_myeloma[,c(325,324)]){
  UKBdata_sen_multiple_myeloma$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_multiple_myeloma)
  result_multiple_myeloma<-rbind(result_multiple_myeloma,c(table(UKBdata_sen_multiple_myeloma$i),table(UKBdata_sen_multiple_myeloma$i,UKBdata_sen_multiple_myeloma$status)[,2],
                                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_multiple_myeloma)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_multiple_myeloma<-data.frame(result_multiple_myeloma)
result_multiple_myeloma<-result_multiple_myeloma%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_multiple_myeloma<-result_multiple_myeloma%>% mutate(cancer=c("multiple_myeloma"),.before=diet)

#leukemia
UKBdata_sen_leukemia<-subset(UKBdata_sen,various_cancer=="leukemia"|various_cancer==0)
result_leukemia<-c()
for (i in UKBdata_sen_leukemia[,c(325,324)]){
  UKBdata_sen_leukemia$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP+smoking_intensity+alcohol_frequency,data=UKBdata_sen_leukemia)
  result_leukemia<-rbind(result_leukemia,c(table(UKBdata_sen_leukemia$i),table(UKBdata_sen_leukemia$i,UKBdata_sen_leukemia$status)[,2],
                                           exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_leukemia)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_leukemia<-data.frame(result_leukemia)
result_leukemia<-result_leukemia%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_leukemia<-result_leukemia%>% mutate(cancer=c("leukemia"),.before=diet)


#合并
result_sen_various_cancer<-rbind(result_overall,result_head,result_esophagus,result_stomach,result_colorectal,
                             result_liver,result_gallbladder,result_pancreas,result_lung,result_malignant_melanoma,
                             result_breast,result_uterine,result_ovary,result_prostate,result_testic,
                             result_kidney,result_bladder,result_brain,result_thyroid,result_Hodgkin_lymphoma,
                             result_non_Hodgkin_lymphoma,result_multiple_myeloma,result_leukemia)
write.csv(result_sen_various_cancer,"E:/deskbook/result_sen_various_cancer.csv",row.names = F)
#不调整
#总癌
UKBdata_sen_overall<-UKBdata_sen
result_overall<-c()
for (i in UKBdata_sen_overall[,c(325,324)]){
  UKBdata_sen_overall$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata_sen_overall)
  result_overall<-rbind(result_overall,c(table(UKBdata_sen_overall$i),table(UKBdata_sen_overall$i,UKBdata_sen_overall$status)[,2],
                                         exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_overall)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_overall<-data.frame(result_overall)
result_overall<-result_overall%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_overall<-result_overall%>% mutate(cancer=c("overall"),.before=diet)
#head
UKBdata_sen_head<-subset(UKBdata_sen,various_cancer=="head_and_neck"|various_cancer==0)
result_head<-c()
for (i in UKBdata_sen_head[,c(325,324)]){
  UKBdata_sen_head$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata_sen_head)
  result_head<-rbind(result_head,c(table(UKBdata_sen_head$i),table(UKBdata_sen_head$i,UKBdata_sen_head$status)[,2],
                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_head)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_head<-data.frame(result_head)
result_head<-result_head%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_head<-result_head%>% mutate(cancer=c("head"),.before=diet)

#esophagus
UKBdata_sen_esophagus<-subset(UKBdata_sen,various_cancer=="esophagus"|various_cancer==0)
result_esophagus<-c()
for (i in UKBdata_sen_esophagus[,c(325,324)]){
  UKBdata_sen_esophagus$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata_sen_esophagus)
  result_esophagus<-rbind(result_esophagus,c(table(UKBdata_sen_esophagus$i),table(UKBdata_sen_esophagus$i,UKBdata_sen_esophagus$status)[,2],
                                             exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_esophagus)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_esophagus<-data.frame(result_esophagus)
result_esophagus<-result_esophagus%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_esophagus<-result_esophagus%>% mutate(cancer=c("esophagus"),.before=diet)
#lung
UKBdata_sen_lung<-subset(UKBdata_sen,various_cancer=="lung"|various_cancer==0)
result_lung<-c()
for (i in UKBdata_sen_lung[,c(325,324)]){
  UKBdata_sen_lung$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+sleep_duration+
                BMI+whr+SBP,data=UKBdata_sen_lung)
  result_lung<-rbind(result_lung,c(table(UKBdata_sen_lung$i),table(UKBdata_sen_lung$i,UKBdata_sen_lung$status)[,2],
                                   exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
colnames(result_lung)<-c("x1","x2","x3","x4","x5","x6","OR_1","OR_2","LCL_1","LCL_2","UCL_1","UCL_2","P1","P2")
result_lung<-data.frame(result_lung)
result_lung<-result_lung%>% mutate(diet=c("MEDAS","MIND"),.before=x1)
result_lung<-result_lung%>% mutate(cancer=c("lung"),.before=diet)
#合并
result_sen_various_cancer<-rbind(result_overall,result_head,result_esophagus,result_lung)
write.csv(result_sen_various_cancer,"E:/deskbook/result_sen_various_cancer.csv",row.names = F)
#12.用不同地中海评分####
library(data.table)
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
ffqdata<-fread("E:/deskbook/ffqdata.csv",header=T)
colnames(ffqdata)
ffqdata2_1<-subset(ffqdata,select=c(1))
ffqdata2_2<-subset(ffqdata,select=c(199))
colnames(ffqdata2_2) <- paste0("a", gsub(" ", "_", colnames(ffqdata2_2)))
ffqdata2<-cbind(ffqdata2_1,ffqdata2_2)
UKBdata2<-merge(UKBdata2,ffqdata2,all.x = T,by="eid")
colnames(UKBdata2)
####MEDAS_continous####
#Vegetables_sum1
UKBdata2$Vegetables_sum1_score1<-ifelse(UKBdata2$Vegetables_sum1>=2,1,ifelse(UKBdata2$a104090_1>=1,1,
                                                                             ifelse(UKBdata2$Vegetables_sum1==0,0,
                                                                                    ifelse(UKBdata2$a104090_1==0,0,NA))))#大于2份蔬菜或者大于一份沙拉
UKBdata2$Vegetables_sum1_score1[is.na(UKBdata2$Vegetables_sum1_score1)]<-0
#fruit
UKBdata2$fruit_sum1_score1<-ifelse(UKBdata2$fruit_sum1>=3,1,ifelse(UKBdata2$fruit_sum1_score1==0,0,NA))
summary(UKBdata2$fruit_sum1_score1)
UKBdata2$fruit_sum1_score1[is.na(UKBdata2$fruit_sum1_score1)]<-0
#red meat
UKBdata2$Mred_meat_sum1_score1<-ifelse(UKBdata2$red_meat_sum1>=2,0,
                                       ifelse(UKBdata2$red_meat_sum1<1,1,NA))
table(UKBdata2$Mred_meat_sum1_score1)
summary(UKBdata2$Mred_meat_sum1_score1)
fit1<-lm(Mred_meat_sum1_score1~red_meat_sum1,data=UKBdata2)
summary(fit1)$coe[2,1]
UKBdata2$Mred_meat_sum1_score1<-ifelse(UKBdata2$red_meat_sum1<1,1,
                                       ifelse(UKBdata2$red_meat_sum1>=2,0,
                                              UKBdata2$red_meat_sum1*summary(fit1)$coe[2,1]))
UKBdata2$Mred_meat_sum1_score1[UKBdata2$Mred_meat_sum1_score1<=0]<-0                                      
#butter
UKBdata2$MButter_sum1_score1<-ifelse(UKBdata2$Butter_sum1>=2,0,
                                    ifelse(UKBdata2$Butter_sum1<1,1,NA))
summary(UKBdata2$MButter_sum1_score1)
UKBdata2$MButter_sum1_score1[is.na(UKBdata2$fruit_sum1_score1)]<-0
#sweetend
UKBdata2$MSweetened_sum1_score1<-ifelse(UKBdata2$Sweetened_sum1>=2,0,
                                        ifelse(UKBdata2$Sweetened_sum1<1,1,NA))
summary(UKBdata2$MSweetened_sum1_score1)
fit2<-lm(MSweetened_sum1_score1~Sweetened_sum1,data=UKBdata2)
UKBdata2$MSweetened_sum1_score1<-ifelse(UKBdata2$Sweetened_sum1>=2,0,
                                        ifelse(UKBdata2$Sweetened_sum1<1,1,
                                       UKBdata2$Sweetened_sum1*summary(fit2)$coe[2,1]))
UKBdata2$MSweetened_sum1_score1[UKBdata2$MSweetened_sum1_score1<=0]<-0
#wine
UKBdata2$Mwine_sum1_score1<-ifelse(UKBdata2$wine_sum1>=1,1,0)
#legumes
3/7
UKBdata2$Mbean_sum1_score1<-ifelse(UKBdata2$bean_sum1>=0.428571,1,
                                  ifelse(UKBdata2$bean_sum1==0,0,NA))
str(UKBdata2$Mbean_sum1_score1)
fit3<-lm(Mbean_sum1_score1~bean_sum1,data=UKBdata2)
UKBdata2$Mbean_sum1_score1<-ifelse(UKBdata2$bean_sum1>=0.428571,1,
                                   ifelse(UKBdata2$bean_sum1==0,0,
                                          UKBdata2$bean_sum1*summary(fit3)$coe[2,1]))
#seafood
UKBdata2$Mfish_sum1_score1<-ifelse(UKBdata2$fish_sum1>=0.428571,1,
                                   ifelse(UKBdata2$fish_sum1==0,0,NA))
summary(UKBdata2$Mfish_sum1_score1)
fit4<-lm(Mfish_sum1_score1~fish_sum1,data=UKBdata2)
UKBdata2$Mfish_sum1_score1<-ifelse(UKBdata2$fish_sum1>=0.428571,1,
                                   ifelse(UKBdata2$fish_sum1==0,0,
                                          UKBdata2$fish_sum1*summary(fit4)$coe[2,1]))
#sweets
2/7,4/7
UKBdata2$MPastries_sweets_sum1_score1<-ifelse(UKBdata2$Pastries_sweets_sum1>=0.5714286,0,
                                              ifelse(UKBdata2$Pastries_sweets_sum1<0.2857143,1,NA))
summary(UKBdata2$MPastries_sweets_sum1_score1)
fit5<-lm(MPastries_sweets_sum1_score1~Pastries_sweets_sum1,data=UKBdata2)
UKBdata2$MPastries_sweets_sum1_score1<-ifelse(UKBdata2$Pastries_sweets_sum1>=0.5714286,0,
                                              ifelse(UKBdata2$Pastries_sweets_sum1<0.2857143,1,
                                                     UKBdata2$Pastries_sweets_sum1*summary(fit5)$coe[2,1]))
UKBdata2$MPastries_sweets_sum1_score1[UKBdata2$MPastries_sweets_sum1_score1<=0]<-0 
#nuts
3/7
UKBdata2$MNuts_sum1_score1<-ifelse(UKBdata2$Nuts_sum1>=0.428571,1,
                                   ifelse(UKBdata2$Nuts_sum1==0,0,NA))
summary(UKBdata2$MNuts_sum1_score1)
fit6<-lm(MNuts_sum1_score1~Nuts_sum1,data=UKBdata2)
UKBdata2$MNuts_sum1_score1<-ifelse(UKBdata2$Nuts_sum1>=0.428571,1,
                                   ifelse(UKBdata2$Nuts_sum1==0,0,
                                          UKBdata2$Nuts_sum1*summary(fit6)$coe[2,1]))
#white meat
UKBdata2$Mpoultry_sum1_score1<-ifelse(UKBdata2$poultry_sum1>UKBdata2$red_meat_sum1,1,0)
#总分
UKBdata2[,MEDAS_sum1_continous:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score1","fruit_sum1_score1",
                                                               "Mred_meat_sum1_score1","MButter_sum1_score1","MSweetened_sum1_score1",
                                                               "Mwine_sum1_score1","Mbean_sum1_score1","Mfish_sum1_score1","MPastries_sweets_sum1_score1",
                                                               "MNuts_sum1_score1","Mpoultry_sum1_score1")]
table(UKBdata2[,MEDAS_sum1_continous])
UKBdata2$MEDAS_sum1_continous_3group <- cut(UKBdata2$MEDAS_sum1_continous, 
                                  breaks = quantile(UKBdata2$MEDAS_sum1_continous, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
#Model 1仅进行能量调整
table(UKBdata2$MEDAS_sum1_continous_3group)
table(UKBdata2$MEDAS_sum1_continous_3group,UKBdata2$status)
table(UKBdata2$MEDAS_sum1_continous_3group,UKBdata2$MEDAS_sum1_continous)
colnames(UKBdata2)
result<-c()
var_list<-colnames(UKBdata2[,c(282)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_1.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(287)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_2.csv",row.names = F)
#Model 2进一步调整sex,education,income, index, family history
result<-c()
var_list<-colnames(UKBdata2[,c(282)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(287)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#Model 3进一步调整Smoking, alcohol, physical activity, BMI, WHR, SBP
result<-c()
var_list<-colnames(UKBdata2[,c(282)])
for (var in var_list){
  fit<-coxph(Surv(recruitment_age,status)~eval(parse(text = var))+energy+sex+education+
               famaliy_history+Townsend_deprivation_index2+household_income+
               smoking_status+alochol_drinking+weekly_activity+sleep_duration+
               BMI+whr+SBP,data=UKBdata2)
  HR <- exp(fit$coefficients[1])
  CI_lower <- summary(fit)$conf.int[1, 3]
  CI_upper <- summary(fit)$conf.int[1, 4]
  p_value <- summary(fit)$coef[1, 5]
  result <- rbind(result, c(HR, CI_lower, CI_upper, p_value))
}
write.csv(result,"E:/deskbook/table2_3.csv",row.names = F)
result<-c()
for (i in UKBdata2[,c(287)]){
  UKBdata2$i<-factor(i)
  fiti<-coxph(Surv(recruitment_age,status)~i+energy+sex+education+
                famaliy_history+Townsend_deprivation_index2+household_income+
                smoking_status+alochol_drinking+weekly_activity+
                BMI+whr+SBP,data=UKBdata2)
  result<-rbind(result,c(exp(fiti$coefficients[1:2]),summary(fiti)$conf.int[1:2,3:4],summary(fiti)$coef[1:2,5]))
}
write.csv(result,"E:/deskbook/table2_4.csv",row.names = F)
#计算中位随访时间####
library(data.table)
ICD<-fread("E:/deskbook/ICD.csv",header = T)
colnames(ICD)
table(ICD$Endpoint1)
ICD$Endpoint1[is.na(ICD$Endpoint1)] <- 0
#转换成时间编码
ICD$`53-0.0` <- as.POSIXct(ICD$`53-0.0`, format="%Y/%m/%d")
ICD$`40005-0.0` <- as.POSIXct(ICD$`40005-0.0`, format="%Y/%m/%d")
ICD$`40005-1.0` <- as.POSIXct(ICD$`40005-1.0`, format="%Y/%m/%d")
ICD$`40005-2.0` <- as.POSIXct(ICD$`40005-2.0`, format="%Y/%m/%d")
ICD$`40005-3.0` <- as.POSIXct(ICD$`40005-3.0`, format="%Y/%m/%d")
ICD$`40005-4.0` <- as.POSIXct(ICD$`40005-4.0`, format="%Y/%m/%d")
ICD$`40005-5.0` <- as.POSIXct(ICD$`40005-5.0`, format="%Y/%m/%d")
ICD$`40005-6.0` <- as.POSIXct(ICD$`40005-6.0`, format="%Y/%m/%d")
#计算
ICD$follow_up_time<-0
ICD$follow_up_time2 <- as.POSIXct("2022/08/31", format="%Y/%m/%d")
ICD$follow_up_time2<- as.Date(ICD$follow_up_time2)
ICD$follow_up_time <- ifelse(ICD$Endpoint1==0,(difftime(ICD$follow_up_time2, ICD$`53-0.0`, units = "weeks"))/52,
                             ifelse(ICD$Endpoint1==1,(difftime(ICD$`40005-0.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                    ifelse(ICD$Endpoint1==2,(difftime(ICD$`40005-1.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                           ifelse(ICD$Endpoint1==3,(difftime(ICD$`40005-2.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                                  ifelse(ICD$Endpoint1==4,(difftime(ICD$`40005-3.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                                         ifelse(ICD$Endpoint1==5,(difftime(ICD$`40005-4.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                                                ifelse(ICD$Endpoint1==6,(difftime(ICD$`40005-5.0`, ICD$`53-0.0`, units = "weeks"))/52,
                                                                       ifelse(ICD$Endpoint1==7,(difftime(ICD$`40005-6.0`, ICD$`53-0.0`, units = "weeks"))/52,NA))))))))
ICD_follow_up<-subset(ICD,select = c(1,85))
UKBdata2<-fread("E:/deskbook/UKBdata2.csv",header = T)
UKBdata_follow<-merge(UKBdata2,ICD_follow_up,all.x = T,by="eid")
summary(UKBdata_follow$follow_up_time)
colnames(UKBdata_follow)
#计算中位随访年龄
library(survival)
fit<-survfit(Surv(follow_up_time,status==0)~1,data=UKBdata_follow)
fit