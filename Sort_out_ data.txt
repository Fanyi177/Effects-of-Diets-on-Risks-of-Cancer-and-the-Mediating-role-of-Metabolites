#
memory.limit(999999999999)
library(data.table)
data<-fread("E:/deskbook/food.csv",header=T) # 502357 obs 1408 variables
colnames(data)
#
colSums(is.na(data)[,1399:1403])/nrow(data)
#
library(tidyverse)
data2<-data%>%filter(!(is.na(`100002-0.0`)&is.na(`100002-1.0`)&is.na(`100002-2.0`)&is.na(`100002-3.0`)&is.na(`100002-4.0`)))# 排除291,400obs
#
options(max.print = 1000000)
colnames(data2)
data3<-subset(data2,select=c(1))
colnames(data3)
data4<-subset(data2,select=c(24:1378))
colnames(data4)
data_cov<-subset(data2,select=c(1,2:23,1379:1408))
n<-nrow(data4)
#
data_105010<-subset(data,select=c(1,1399:1403))
######
#Columns were screened for baseline follow-up
selected_columns <- which(seq_len(ncol(data4)) %% 5 == 1)
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(data4)), selected_columns))
# 
new_data1<- data4[, ..sorted_columns]
colnames(new_data1)
new_data1_1<-subset(new_data1,select=c(1:24))
new_data1_2<-subset(new_data1,select=c(25:271))
colnames(new_data1_1)
# The columns for the first follow-up were screened
selected_columns <- which(seq_len(ncol(data4)) %% 5 == 2)
# 
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(data4)), selected_columns))
# 
new_data2 <- data4[, ..sorted_columns]
colnames(new_data2)
new_data2_1<-subset(new_data2,select=c(1:24))
new_data2_2<-subset(new_data2,select=c(25:271))
# The columns for the second follow-up were screened
selected_columns <- which(seq_len(ncol(data4)) %% 5 == 3)
# 
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(data4)), selected_columns))
#
new_data3 <- data4[, ..sorted_columns]
new_data3_1<-subset(new_data3,select=c(1:24))
new_data3_2<-subset(new_data3,select=c(25:271))
#The columns for the third follow-up were screened
selected_columns <- which(seq_len(ncol(data4)) %% 5 == 4)
# 
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(data4)), selected_columns))
#
new_data4 <- data4[, ..sorted_columns]
new_data4_1<-subset(new_data4,select=c(1:24))
new_data4_2<-subset(new_data4,select=c(25:271))
#The columns for the forth follow-up were screened
selected_columns <- which(seq_len(ncol(data4)) %% 5 == 0)
# 
sorted_columns <- c(selected_columns, setdiff(seq_len(ncol(data4)), selected_columns))
#
new_data5 <- data4[, ..sorted_columns]
new_data5_1<-subset(new_data5,select=c(1:24))
new_data5_2<-subset(new_data5,select=c(25:271))
#
#adjusted for energy
new_data11<-data.frame(matrix(0,n,ncol(new_data1_1)))
for (i in 2:ncol(new_data1_1)){
  new_data11[,1]<-new_data1_1[,1]
  colnames(new_data11)[1]<-colnames(new_data1_1)[1]
  new_data11[,i]<-new_data1_1[, get(colnames(new_data1_1)[i])]* data4$`100002-0.0`/ 2000
  colnames(new_data11)[i]<-colnames(new_data1_1)[i]
}
#
new_data21<-data.frame(matrix(0,n,ncol(new_data2_1)))
for (i in 2:ncol(new_data2_1)){
  new_data21[,1]<-new_data2_1[,1]
  colnames(new_data21)[1]<-colnames(new_data2_1)[1]
  new_data21[,i]<-new_data2_1[, get(colnames(new_data2_1)[i])]* data4$`100002-1.0`/ 2000
  colnames(new_data21)[i]<-colnames(new_data2_1)[i]
}
#
new_data31<-data.frame(matrix(0,n,ncol(new_data3_1)))
for (i in 2:ncol(new_data3_1)){
  new_data31[,1]<-new_data3_1[,1]
  colnames(new_data31)[1]<-colnames(new_data3_1)[1]
  new_data31[,i]<-new_data3_1[, get(colnames(new_data3_1)[i])]* data4$`100002-2.0`/ 2000
  colnames(new_data31)[i]<-colnames(new_data3_1)[i]
}
#
new_data41<-data.frame(matrix(0,n,ncol(new_data4_1)))
for (i in 2:ncol(new_data4_1)){
  new_data41[,1]<-new_data4_1[,1]
  colnames(new_data41)[1]<-colnames(new_data4_1)[1]
  new_data41[,i]<-new_data4_1[, get(colnames(new_data4_1)[i])]* data4$`100002-3.0`/ 2000
  colnames(new_data41)[i]<-colnames(new_data4_1)[i]
}
#
new_data51<-data.frame(matrix(0,n,ncol(new_data5_1)))
for (i in 2:ncol(new_data5_1)){
  new_data51[,1]<-new_data5_1[,1]
  colnames(new_data51)[1]<-colnames(new_data5_1)[1]
  new_data51[,i]<-new_data5_1[, get(colnames(new_data5_1)[i])]* data4$`100002-4.0`/ 2000
  colnames(new_data51)[i]<-colnames(new_data5_1)[i]
}
#
data5<-cbind(data3,new_data11,new_data21,new_data31,new_data41,new_data51)
colnames(data5)
write.csv(data5,"E:/deskbook/data5.csv")
data5_2<-cbind(data3,new_data1_2,new_data2_2,new_data3_2,new_data4_2,new_data5_2)
colnames(data5_2)
#Merge ICD data 
ICD<-fread("E:/deskbook/ICD.csv",header=T)
ICD<-merge(ICD,data_105010,by='eid',all.x = T)
write_excel_csv(ICD,"E:/deskbook/ICD.csv")
#
ICD<-fread("E:/deskbook/ICD.csv",header=T)
colnames(ICD)
ICD<-subset(ICD,select=c(1:20,57,80:84))
data6<-merge(data5,ICD,by='eid',all.x = T)
data6_2<-merge(data5_2,ICD,by='eid',all.x = T)
#Excluded patients with malignant tumors at baseline
colnames(data6)
data6<-filter(data6,Exclusion_1==0) 
data6_2<-filter(data6_2,Exclusion_1==0)#193424 obs
#Exclude 24hFFQ collected prior to cancer occurrence
data7<-data6%>%filter(paichufood!=1) #剩余190097例
data7_2<-data6_2%>%filter(paichufood!=1)#剩余190097例
#
colnames(data7)
data_zhengli<-subset(data7,select=c(1:121,133:137,142:146,130))
colnames(data_zhengli)
data_zhengli[is.na(data_zhengli$Endpoint1), "Endpoint1"] <- 0
data_zhengli$`105010-0.0`<-as.Date(data_zhengli$`105010-0.0`, format="%Y/%m/%d")
data_zhengli$`105010-0.0`<-format(data_zhengli$`105010-0.0`, "%Y/%m/%d")
data_zhengli$`105010-1.0`<-as.Date(data_zhengli$`105010-1.0`, format="%Y/%m/%d")
data_zhengli$`105010-1.0`<-format(data_zhengli$`105010-1.0`, "%Y/%m/%d")
data_zhengli$`105010-2.0`<-as.Date(data_zhengli$`105010-2.0`, format="%Y/%m/%d")
data_zhengli$`105010-2.0`<-format(data_zhengli$`105010-2.0`, "%Y/%m/%d")
data_zhengli$`105010-3.0`<-as.Date(data_zhengli$`105010-3.0`, format="%Y/%m/%d")
data_zhengli$`105010-3.0`<-format(data_zhengli$`105010-3.0`, "%Y/%m/%d")
data_zhengli$`105010-4.0`<-as.Date(data_zhengli$`105010-4.0`, format="%Y/%m/%d")
data_zhengli$`105010-4.0`<-format(data_zhengli$`105010-4.0`, "%Y/%m/%d")
#derive
setwd("E:/deskbook/data_subset")
n_rows <- nrow(data_zhengli)
n_groups <- ceiling(n_rows/20000)

for (i in 1:n_groups) {
  start_row <- (i-1) * 20000 + 1
  end_row <- min(i * 20000, n_rows)
  data_subset1 <- data_zhengli[start_row:end_row, ]
  filename <- paste0("data1_subset_", i, ".csv")
  write_excel_csv(data_subset1, file = filename)
}

data_zhengli_2<-subset(data7_2,select=c(1,2:1236,1248:1252,1257:1261,1245))
data_zhengli_2[is.na(data_zhengli_2$Endpoint1), "Endpoint1"] <- 0
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 555] <- 0.5
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 444] <- 0.25
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 200] <- 2.5
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 300] <- 3.5
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 400] <- 4.5
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 500] <- 5.5
data_zhengli_2[, 2:1236][data_zhengli_2[, 2:1236] == 600] <- 6.5
colnames(data_zhengli_2)
data_zhengli_2$`105010-0.0`<-as.Date(data_zhengli_2$`105010-0.0`, format="%Y/%m/%d")
data_zhengli_2$`105010-0.0`<-format(data_zhengli_2$`105010-0.0`, "%Y/%m/%d")
data_zhengli_2$`105010-1.0`<-as.Date(data_zhengli_2$`105010-1.0`, format="%Y/%m/%d")
data_zhengli_2$`105010-1.0`<-format(data_zhengli_2$`105010-1.0`, "%Y/%m/%d")
data_zhengli_2$`105010-2.0`<-as.Date(data_zhengli_2$`105010-2.0`, format="%Y/%m/%d")
data_zhengli_2$`105010-2.0`<-format(data_zhengli_2$`105010-2.0`, "%Y/%m/%d")
data_zhengli_2$`105010-3.0`<-as.Date(data_zhengli_2$`105010-3.0`, format="%Y/%m/%d")
data_zhengli_2$`105010-3.0`<-format(data_zhengli_2$`105010-3.0`, "%Y/%m/%d")
data_zhengli_2$`105010-4.0`<-as.Date(data_zhengli_2$`105010-4.0`, format="%Y/%m/%d")
data_zhengli_2$`105010-4.0`<-format(data_zhengli_2$`105010-4.0`, "%Y/%m/%d")
write_excel_csv(data_zhengli_2,"E:/deskbook/data_zhengli_2.csv")
n_rows <- nrow(data_zhengli_2)
n_groups <- ceiling(n_rows/20000)
# 
for (i in 1:n_groups) {
  start_row <- (i-1) * 20000 + 1
  end_row <- min(i * 20000, n_rows)
  data_subset <- data_zhengli_2[start_row:end_row, ]
  filename <- paste0("data_subset_", i, ".csv")
  write_excel_csv(data_subset, file = filename)
}
#Merge other variable information####
colnames(data7)
data8<-subset(data7,select=c(1,122,124,129:132,141))
colnames(data8)
setwd("E:/deskbook")
cov<-fread("E:/deskbook/cov.csv",header=T)
cov3<-fread("E:/deskbook/UKB/cov.csv",header=T)
cov3<-subset(cov3,select=c(1,127))
cov<-merge(cov,cov3,all.x = T,by="eid")
write_excel_csv(cov,"E:/deskbook/cov.csv")
colnames(cov)
cov2<-subset(cov,select = c(1,11,3,7,13,20,24,28,32,36,40,350,354,48,532))
colnames(cov2)
education_family_history<-subset(cov,select=c(1,17:19,218:349))
write_excel_csv(education_family_history,"E:/deskbook/education_family_history.csv")
education<-fread("E:/deskbook/education_family_history.csv",header=T)#3 variable
data9<-merge(data8,education,by='eid',all.x = T) #190097 obs 10 vars
data9$education[data9$education==3]<-NA
data9$famaliy_history[data9$famaliy_history==3]<-NA
data9<-merge(data9,cov2,by='eid',all.x = T)
colnames(data9)
data9$sex<-data9$`31-0.0`
data9$Townsend_deprivation_index<-data9$`189-0.0`
data9$shifang<-data9$`190-0.0`
data9$whr<-data9$`48-0.0`/data9$`49-0.0`
data9$household_income<-ifelse(data9$`738-0.0`<=2,0,1)
data9$weekly_activity<-ifelse(data9$`884-0.0`>=0&data9$`904-0.0`>=0,
                              rowMeans((select(data9,c(`884-0.0`,`904-0.0`)))),
                              ifelse(data9$`884-0.0`<0&data9$`904-0.0`<0,NA,
                                     ifelse(data9$`884-0.0`<0,data9$`904-0.0`,
                                            ifelse(data9$`904-0.0`<0,data9$`884-0.0`,0))))
data9$`1160-0.0`[data9$`1160-0.0`<=0]<-NA
data9$sleep_duration<-data9$`1160-0.0`
data9$smoking_status<-ifelse(data9$`1239-0.0`==1|data9$`1239-0.0`==2,3,ifelse(
  data9$`1249-0.0`==1,2,ifelse(data9$`1249-0.0`==2,1,
                               ifelse(data9$`1249-0.0`==3,0,
                                      ifelse(data9$`1249-0.0`==4,0,ifelse(data9$`1239-0.0`==0,0,NA))))))
data9$smoking_status<-factor(data9$smoking_status,levels=c(0,1,2,3),labels = c("Never","Former occasional smoker","Former regular smoker","Current somker"))
data9$alochol_drinking<-ifelse(data9$`1558-0.0`<=3&data9$`1558-0.0`>=1,2,
                               ifelse(data9$`1558-0.0`>=4&data9$`1558-0.0`<=5,1,
                                      ifelse(data9$`1558-0.0`==6,0,NA)))
data9$alochol_drinking<-factor(data9$alochol_drinking,levels = c(0,1,2),labels = c("Never","Occasional drinker","Regular drinker"))
data9$SBP<-data9$`4080-0.0`
data9$BMI<-data9$`21001-0.0`
data9$recruitment_age<-data9$`21022-0.0`
data11<-data9%>%filter(is.na(shifang))# 189571obs
colnames(data11)
data11 <- subset(data11, select = -c(3,11:24))
#Remaining:187495 obs
data12<-data11%>%filter(!(is.na(famaliy_history)|is.na(education)|
                            is.na(Townsend_deprivation_index)|is.na(whr)|
                            is.na(BMI)|is.na(smoking_status)|is.na(alochol_drinking)|
                            is.na(household_income)))
#Interpolation method to fill the data
library(mice)
colnames(data12)
summary(data12)
tianbu<-mice(data12[,c(3,7:11,13:21)],m=5)
tianbu2<-complete(tianbu,action=2)
tianbu2$Townsend_deprivation_index2<- cut(tianbu2$Townsend_deprivation_index, breaks = quantile(tianbu2$Townsend_deprivation_index, probs = c(0, 0.33, 0.66, 1)), include.lowest = TRUE, labels = FALSE)#三分位分组
data13<-subset(data12,select=-c(3,7:11,13:21))
colnames(tianbu2)
UKBdata<-cbind(data13,tianbu2)
colnames(UKBdata)
table(UKBdata$various_cancer)
UKBdata[, 8][UKBdata[, 8] == "#N/A"] <- 9
UKBdata[, 8][UKBdata[, 8] == ""] <- 0
#Export UKB data#####
setwd("E:/deskbook")
n_rows <- nrow(UKBdata)
n_groups <- ceiling(n_rows/20000)
for (i in 1:n_groups) {
  start_row <- (i-1) * 20000 + 1
  end_row <- min(i * 20000, n_rows)
  UKBdata_subset <- UKBdata[start_row:end_row, ]
  filename <- paste0("UKBdata_subset_", i, ".csv")
  write_excel_csv(UKBdata_subset, file = filename)
}