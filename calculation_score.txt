#Reading python completes an average 24hFFQ message###
ffqdata<-fread("D:/deskbook/ffqdata.csv",header=T)
colnames(ffqdata)
ffqdata_1<-subset(ffqdata,select=c(1))
ffqdata_2<-subset(ffqdata,select=-c(1))
colnames(ffqdata_2) <- paste0("a", gsub(" ", "_", colnames(ffqdata_2)))
ffqdata<-cbind(ffqdata_1,ffqdata_2)
ffq<-fread("E:/deskbook/FFQ.csv",header=T) 
ffqdata2<-merge(ffqdata,ffq,by="eid",all.x = T)
colnames(ffqdata2)
ffqdata3<-ffqdata2[,-c(736:750,771:825)]
names(ffqdata3) <- gsub("\\.x", "", names(ffqdata3))
colnames(ffqdata3)
table(ffqdata3$Canned_soup_vegetables)
#Calcualation of food score
#MIND_sum1: Calculated using average 24hFFQ and total intake####
ffqdata3$a104090_1_0.5<-ifelse(is.na(ffqdata3[, a104090_1]), NA, 0.5*ffqdata3[, a104090_1])
ffqdata3$a104060_1_0.5<-ifelse(is.na(ffqdata3[, a104060_1]), NA, 0.5*ffqdata3[, a104060_1])
ffqdata3$a104070_1_0.5<-ifelse(is.na(ffqdata3[, a104070_1]), NA, 0.5*ffqdata3[, a104070_1])
ffqdata3[, Green_Leafy_Vegetables_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104160_1", "a104300_1", "a104240_1","a104370_1", "a104090_1_0.5",
                                                                                 "a104360_1", "a104180_1", "a104080_1", "a104060_1_0.5", "a104070_1_0.5")]
table(ffqdata3$Green_Leafy_Vegetables_sum1)
#other vegetables
ffqdata3$Canned_soup_vegetables<-ifelse(
  is.na(ffqdata3$`20108-0.0`)&is.na(ffqdata3$`20108-0.1`)&is.na(ffqdata3$`20108-0.2`), 0,ifelse(
    ffqdata3$`20108-0.0` == 5| ffqdata3$`20108-0.1` ==5|ffqdata3$`20108-0.2`==5,0.25, 
    0))
ffqdata3$homemade_soup_vegetables<-ifelse(
  is.na(ffqdata3$`20109-0.0`)&is.na(ffqdata3$`20109-0.1`)&is.na(ffqdata3$`20109-0.2`), 0,ifelse(
    ffqdata3$`20109-0.0`== 5| ffqdata3$`20109-0.1`==5|ffqdata3$`20109-0.2`==5,0.25, 
    0))
ffqdata3[, Other_Vegetables_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104190_1", "a104170_1", "a104140_1","a104340_1", "a104350_1","a104320_1","a104060_1_0.5", "a104070_1_0.5","a104090_1_0.5",
                                                                           "a104130_1", "a104150_1", "a104200_1","a104210_1","a104220_1","a104230_1", 
                                                                           "a104250_1","a104260_1","a104270_1","a104290_1","a104310_1","a104330_1","a104380_1",'Canned_soup_vegetables',"homemade_soup_vegetables")]
table(ffqdata3$Other_Vegetables_sum1)
#Berries
ffqdata3[, Berries_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104470_1")]
table(ffqdata3$Berries_sum1)
summary(ffqdata3$Berries_sum1)
#Nuts
ffqdata3[, Nuts_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a102430_1","a102410_1","a102450_1","a102440_1",
                                                               "a102420_1")]
table(ffqdata3$Nuts_sum1)
#Butter/margarine
ffqdata3$Butter_sum1<-ifelse(ffqdata3$a104040_1==1|ffqdata3$a101300_1==1|ffqdata3$a104040_1==1,1,0)
ffqdata3$Butter_sum1[is.na(ffqdata3$Butter_sum1)]<-0
table(ffqdata3$Butter_sum1)
#Whole_grains
ffqdata3[, Whole_grains_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100770_1","a100800_1","a100840_1","a100850_1",
                                                                       "a102720_1","a102740_1")]
table(ffqdata3$Whole_grains_sum1)
#Cheese
ffqdata3[, cheese_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a102820_1","a102830_1","a102840_1","a102860_1",
                                                                 "a102870_1","a102880_1","a102890_1","a102900_1","a102910_1")]
table(ffqdata3$cheese_sum1)
#fish
ffqdata3$Canned_soup_fish<-ifelse(ffqdata3$`20108-0.0`==1|ffqdata3$`20108-0.1`==1|ffqdata3$`20108-0.2`==1|ffqdata3$`20108-0.3`==1,0.25,0)
ffqdata3$homemade_soup_fish<-ifelse(ffqdata3$`20109-0.0`==1|ffqdata3$`20109-0.1`==1|ffqdata3$`20109-0.2`==1|ffqdata3$`20109-0.3`==1,0.25,0)
summary(ffqdata3$Canned_soup_fish)
ffqdata3[,fish_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103150_1", "a103160_1","a103190_1", "a103200_1","a103210_1",
                                                              "a103220_1","a103230_1",'Canned_soup_fish',"homemade_soup_fish")]
summary(ffqdata3$fish_sum1)
#Beans
ffqdata3$Canned_soup_bean<-ifelse(ffqdata3$`20108-0.0`==4|ffqdata3$`20108-0.1`==4|ffqdata3$`20108-0.2`==4|ffqdata3$`20108-0.3`==4,0.25,0)
ffqdata3$homemade_soup_bean<-ifelse(ffqdata3$`20109-0.0`==4|ffqdata3$`20109-0.1`==4|ffqdata3$`20109-0.2`==4|ffqdata3$`20109-0.3`==4,0.25,0)
ffqdata3[,bean_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104000_1", "a104010_1","a104110_1", "a104120_1","a104280_1",
                                                              'Canned_soup_bean',"homemade_soup_bean","a103270_1")]
table(ffqdata3$bean_sum1)
#poultry
ffqdata3$Canned_soup_meat<-ifelse(ffqdata3$`20108-0.0`==2|ffqdata3$`20108-0.1`==2|ffqdata3$`20108-0.2`==2|ffqdata3$`20108-0.3`==2,0.25,0)
ffqdata3$homemade_soup_meat<-ifelse(ffqdata3$`20109-0.0`==2|ffqdata3$`20109-0.1`==2|ffqdata3$`20109-0.2`==2|ffqdata3$`20109-0.3`==2,0.25,0)
ffqdata3[,poultry_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103060_1",
                                                                 'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$poultry_sum1)
#red meat
ffqdata3[,red_meat_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103010_1","a103020_1","a103030_1","a103040_1","a103070_1",
                                                                  "a103080_1",'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$red_meat_sum1)
#Fast/fried foods 
ffqdata3[,fastfried_food_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103050_1","a103170_1","a103180_1","a104020_1","a102460_1")]
table(ffqdata3$fastfried_food_sum1)
#Pastries and sweets 
ffqdata3[,Pastries_sweets_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a101970_1","a101980_1","a101990_1","a102010_1","a102020_1",
                                                                         "a102030_1","a102050_1","a102060_1",'a102070_1',
                                                                         "a102120_1","a102140_1",'a102150_1','a102170_1','a102180_1',
                                                                         "a102190_1","a102200_1","a102210_1","a102220_1","a102230_1",
                                                                         "a102260_1","a102270_1","a102280_1","a102290_1","a102300_1","a102310_1",
                                                                         "a102320_1","a102320_1",'a102330_1','a102340_1','a102350_1',
                                                                         "a102360_1",'a102370_1',"a102380_1")]
table(ffqdata3$Pastries_sweets_sum1)
#Wine
ffqdata3[,wine_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100590_1","a100630_1","a100670_1","a100720_1")]
table(ffqdata3$wine_sum1)
colnames(ffqdata3)
missing_vals <- colSums(is.na(ffqdata3[,769:790]))
print(missing_vals)
ffqdata3$Green_Leafy_Vegetables_sum1_score<-ifelse(ffqdata3$Green_Leafy_Vegetables_sum1<=0.285714,0,ifelse(
  ffqdata3$Green_Leafy_Vegetables_sum1<0.857143,0.5,1))
table(ffqdata3$Green_Leafy_Vegetables_sum1_score)
ffqdata3$Other_Vegetables_sum1_score<-ifelse(ffqdata3$Other_Vegetables_sum1<0.714285,0,ifelse(
  ffqdata3$Other_Vegetables_sum1<1,0.5,1))
ffqdata3$Berries_sum1_score<-ifelse(ffqdata3$Berries_sum1<0.142857,0,ifelse(
  ffqdata3$Berries_sum1<0.285714,0.5,1))
ffqdata3$Nuts_sum1_score<-ifelse(ffqdata3$Nuts_sum1<0.071429,0,ifelse(
  ffqdata3$Nuts_sum1<=0.7142864,0.5,1))
ffqdata3$Butter_sum1_score<-ifelse(ffqdata3$Butter_sum1==1,1,0)
ffqdata3$cheese_sum1_score<-ifelse(ffqdata3$cheese_sum1>=1,0,ifelse(
  ffqdata3$cheese_sum1>0.142857,0.5,1))
ffqdata3$Whole_grains_sum1_score<-ifelse(ffqdata3$Whole_grains_sum1<1,0,ifelse(
  ffqdata3$cheese_sum1<=2,0.5,1))
ffqdata3$fish_sum1_score<-ifelse(ffqdata3$fish_sum1<0.03333,0,ifelse(
  ffqdata3$fish_sum1<=0.142857,0.5,1))
ffqdata3$bean_sum1_score<-ifelse(ffqdata3$bean_sum1<0.142857,0,ifelse(
  ffqdata3$bean_sum1<=0.428571,0.5,1))
ffqdata3$poultry_sum1_score<-ifelse(ffqdata3$poultry_sum1<0.142857,0,ifelse(
  ffqdata3$poultry_sum1<=0.428571,0.5,1))
ffqdata3$red_meat_sum1_score<-ifelse(ffqdata3$red_meat_sum1>0.857143,0,ifelse(
  ffqdata3$red_meat_sum1>=0.571429,0.5,1))
ffqdata3$fastfried_food_sum1_score<-ifelse(ffqdata3$fastfried_food_sum1>0.428571,0,ifelse(
  ffqdata3$fastfried_food_sum1>=0.142857,0.5,1))
ffqdata3$Pastries_sweets_sum1_score<-ifelse(ffqdata3$Pastries_sweets_sum1>1,0,ifelse(
  ffqdata3$Pastries_sweets_sum1>0.714286,0.5,1))
ffqdata3$wine_sum1_score<-ifelse(ffqdata3$wine_sum1<0.033333,0,ifelse(
  ffqdata3$wine_sum1<0.285714,0.5,1))
ffqdata3[,MIND_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum1_score","Other_Vegetables_sum1_score",
                                                              "Berries_sum1_score","Nuts_sum1_score","Butter_sum1_score","cheese_sum1_score",
                                                              "Whole_grains_sum1_score","fish_sum1_score","bean_sum1_score","poultry_sum1_score",
                                                              "red_meat_sum1_score","fastfried_food_sum1_score","Pastries_sweets_sum1_score","wine_sum1_score")]
table(ffqdata3[,MIND_sum1])
#MIND_sum2: Calculated using baseline 24hFFQ and total intake####
ffqdata3$a104090_2_0.5<-ifelse(is.na(ffqdata3[, a104090_2]), NA, 0.5*ffqdata3[, a104090_2])
ffqdata3$a104060_2_0.5<-ifelse(is.na(ffqdata3[, a104060_2]), NA, 0.5*ffqdata3[, a104060_2])
ffqdata3$a104070_2_0.5<-ifelse(is.na(ffqdata3[, a104070_2]), NA, 0.5*ffqdata3[, a104070_2])
colnames(ffqdata3)
ffqdata3[, Green_Leafy_Vegetables_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104160_2", "a104300_2", "a104240_2","a104370_2", "a104090_2_0.5",
                                                                                 "a104360_2", "a104180_2", "a104080_2", "a104060_2_0.5", "a104070_2_0.5")]
table(ffqdata3$Green_Leafy_Vegetables_sum2)
#other vegetables
ffqdata3[, Other_Vegetables_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104190_2", "a104170_2", "a104140_2","a104340_2", "a104350_2","a104320_2","a104060_2_0.5", "a104070_2_0.5","a104090_2_0.5",
                                                                           "a104130_2", "a104150_2", "a104200_2","a104210_2","a104220_2","a104230_2", 
                                                                           "a104250_2","a104260_2","a104270_2","a104290_2","a104310_2","a104330_2","a104380_2",'Canned_soup_vegetables',"homemade_soup_vegetables")]
table(ffqdata3$Other_Vegetables_sum2)
#Berries
ffqdata3[, Berries_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104470_2")]
table(ffqdata3$Berries_sum2)
#Nuts
ffqdata3[, Nuts_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a102430_2","a102410_2","a102450_2","a102440_2",
                                                               "a102420_2")]
table(ffqdata3$Nuts_sum2)
#Butter/margarine
ffqdata3$Butter_sum2<-ifelse(ffqdata3$a104040_2==1|ffqdata3$a101300_2==1|ffqdata3$a104040_2==1,1,0)
ffqdata3$Butter_sum2[is.na(ffqdata3$Butter_sum2)]<-0
table(ffqdata3$Butter_sum2)
#Whole_grains
ffqdata3[, Whole_grains_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100770_2","a100800_2","a100840_2","a100850_2",
                                                                       "a102720_2","a102740_2")]
table(ffqdata3$Whole_grains_sum2)

#Cheese
ffqdata3[, cheese_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a102820_2","a102830_2","a102840_2","a102860_2",
                                                                 "a102870_2","a102880_2","a102890_2","a102900_2","a102910_2")]
table(ffqdata3$cheese_sum2)
#fish
ffqdata3[,fish_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103150_2", "a103160_2","a103190_2", "a103200_2","a103210_2",
                                                              "a103220_2","a103230_2",'Canned_soup_fish',"homemade_soup_fish")]
table(ffqdata3$fish_sum2)
#Beans
ffqdata3[,bean_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104000_2", "a104010_2","a104110_2", "a104120_2","a104280_2",
                                                              'Canned_soup_bean',"homemade_soup_bean","a103270_2")]
table(ffqdata3$bean_sum2)
#poultry
ffqdata3[,poultry_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103060_2",
                                                                 'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$poultry_sum2)
#red meat
ffqdata3[,red_meat_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103010_2","a103020_2","a103030_2","a103040_2","a103070_2",
                                                                  "a103080_2",'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$red_meat_sum2)
#Fast/fried foods 
ffqdata3[,fastfried_food_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a103050_2","a103170_2","a103180_2","a104020_2","a102460_2")]
table(ffqdata3$fastfried_food_sum2)
#Pastries and sweets 
ffqdata3[,Pastries_sweets_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a101970_2","a101980_2","a101990_2","a102010_2","a102020_2",
                                                                         "a102030_2","a102050_2","a102060_2",'a102070_2',
                                                                         "a102120_2","a102140_2",'a102150_2','a102170_2','a102180_2',
                                                                         "a102190_2","a102200_2","a102210_2","a102220_2","a102230_2",
                                                                         "a102260_2","a102270_2","a102280_2","a102290_2","a102300_2","a102310_2",
                                                                         "a102320_2","a102320_2",'a102330_2','a102340_2','a102350_2',
                                                                         "a102360_2",'a102370_2',"a102380_2")]
table(ffqdata3$Pastries_sweets_sum2)
#Wine
ffqdata3[,wine_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100590_2","a100630_2","a100670_2","a100720_2")]
table(ffqdata3$wine_sum2)
colnames(ffqdata3)
missing_vals <- colSums(is.na(ffqdata3[,809:814]))
print(missing_vals)
ffqdata3$Green_Leafy_Vegetables_sum2_score<-ifelse(ffqdata3$Green_Leafy_Vegetables_sum2<=0.285714,0,ifelse(
  ffqdata3$Green_Leafy_Vegetables_sum2<0.857143,0.5,1))
table(ffqdata3$Green_Leafy_Vegetables_sum2_score)
ffqdata3$Other_Vegetables_sum2_score<-ifelse(ffqdata3$Other_Vegetables_sum2<0.714285,0,ifelse(
  ffqdata3$Other_Vegetables_sum2<1,0.5,1))
ffqdata3$Berries_sum2_score<-ifelse(ffqdata3$Berries_sum2<0.142857,0,ifelse(
  ffqdata3$Berries_sum2<0.285714,0.5,1))
ffqdata3$Nuts_sum2_score<-ifelse(ffqdata3$Nuts_sum2<0.071429,0,ifelse(
  ffqdata3$Nuts_sum2<=0.7142864,0.5,1))
ffqdata3$Butter_sum2_score<-ifelse(ffqdata3$Butter_sum2==1,1,0)
ffqdata3$cheese_sum2_score<-ifelse(ffqdata3$cheese_sum2>=1,0,ifelse(
  ffqdata3$cheese_sum2>0.142857,0.5,1))
ffqdata3$Whole_grains_sum2_score<-ifelse(ffqdata3$Whole_grains_sum2<1,0,ifelse(
  ffqdata3$cheese_sum2<=2,0.5,1))
ffqdata3$fish_sum2_score<-ifelse(ffqdata3$fish_sum2<0.03333,0,ifelse(
  ffqdata3$fish_sum2<=0.142857,0.5,1))
ffqdata3$bean_sum2_score<-ifelse(ffqdata3$bean_sum2<0.142857,0,ifelse(
  ffqdata3$bean_sum2<=0.428571,0.5,1))
ffqdata3$poultry_sum2_score<-ifelse(ffqdata3$poultry_sum2<0.142857,0,ifelse(
  ffqdata3$poultry_sum2<=0.428571,0.5,1))
ffqdata3$red_meat_sum2_score<-ifelse(ffqdata3$red_meat_sum2>0.857143,0,ifelse(
  ffqdata3$red_meat_sum2>=0.571429,0.5,1))
ffqdata3$fastfried_food_sum2_score<-ifelse(ffqdata3$fastfried_food_sum2>0.428571,0,ifelse(
  ffqdata3$fastfried_food_sum2>=0.142857,0.5,1))
ffqdata3$Pastries_sweets_sum2_score<-ifelse(ffqdata3$Pastries_sweets_sum2>1,0,ifelse(
  ffqdata3$Pastries_sweets_sum2>0.714286,0.5,1))
ffqdata3$wine_sum2_score<-ifelse(ffqdata3$wine_sum2<0.033333,0,ifelse(
  ffqdata3$wine_sum2<0.285714,0.5,1))
ffqdata3[,MIND_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_sum2_score","Other_Vegetables_sum2_score",
                                                              "Berries_sum2_score","Nuts_sum2_score","Butter_sum2_score","cheese_sum2_score",
                                                              "Whole_grains_sum2_score","fish_sum2_score","bean_sum2_score","poultry_sum2_score",
                                                              "red_meat_sum2_score","fastfried_food_sum2_score","Pastries_sweets_sum2_score","wine_sum2_score")]
table(ffqdata3[,MIND_sum2])
#MIND_average1: Calculated using average 24hFFQ and average intake####
ffqdata3[, Green_Leafy_Vegetables_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104160_1", "a104300_1", "a104240_1","a104370_1", "a104090_1_0.5",
                                                                                      "a104360_1", "a104180_1", "a104080_1", "a104060_1_0.5", "a104070_1_0.5")]
table(ffqdata3$Green_Leafy_Vegetables_average1)
ffqdata3$Green_Leafy_Vegetables_average1[is.na(ffqdata3$Green_Leafy_Vegetables_average1)]<-0
#other vegetables
ffqdata3[, Other_Vegetables_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104190_1", "a104170_1", "a104140_1","a104340_1", "a104350_1","a104320_1","a104060_1_0.5", "a104070_1_0.5","a104090_1_0.5",
                                                                                "a104130_1", "a104150_1", "a104200_1","a104210_1","a104220_1","a104230_1", 
                                                                                "a104250_1","a104260_1","a104270_1","a104290_1","a104310_1","a104330_1","a104380_1",'Canned_soup_vegetables',"homemade_soup_vegetables")]
table(ffqdata3$Other_Vegetables_average1)
ffqdata3$Other_Vegetables_average1[is.na(ffqdata3$Other_Vegetables_average1)]<-0
#Berries
ffqdata3[, Berries_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104470_1")]
table(ffqdata3$Berries_average1)
ffqdata3$Berries_average1[is.na(ffqdata3$Berries_average1)]<-0
#Nuts
ffqdata3[, Nuts_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a102430_1","a102410_1","a102450_1","a102440_1",
                                                                    "a102420_1")]
table(ffqdata3$Nuts_average1)
ffqdata3$Nuts_average1[is.na(ffqdata3$Nuts_average1)]<-0
#Butter/margarine
ffqdata3$Butter_average1<-ifelse(ffqdata3$a104040_1==1|ffqdata3$a101300_1==1|ffqdata3$a104040_1==1,1,0)
ffqdata3$Butter_average1[is.na(ffqdata3$Butter_average1)]<-0
table(ffqdata3$Butter_average1)
#Whole_grains
ffqdata3[, Whole_grains_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100770_1","a100800_1","a100840_1","a100850_1",
                                                                            "a102720_1","a102740_1")]
table(ffqdata3$Whole_grains_average1)
ffqdata3$Whole_grains_average1[is.na(ffqdata3$Whole_grains_average1)]<-0
#Cheese
ffqdata3[, cheese_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a102820_1","a102830_1","a102840_1","a102860_1",
                                                                      "a102870_1","a102880_1","a102890_1","a102900_1","a102910_1")]
table(ffqdata3$cheese_average1)
ffqdata3$cheese_average1[is.na(ffqdata3$cheese_average1)]<-0
#fish
ffqdata3[,fish_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103150_1", "a103160_1","a103190_1", "a103200_1","a103210_1",
                                                                   "a103220_1","a103230_1",'Canned_soup_fish',"homemade_soup_fish")]
table(ffqdata3$fish_average1)
ffqdata3$fish_average1[is.na(ffqdata3$fish_average1)]<-0
#Beans
ffqdata3[,bean_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104000_1", "a104010_1","a104110_1", "a104120_1","a104280_1",
                                                                   'Canned_soup_bean',"homemade_soup_bean","a103270_1")]
table(ffqdata3$bean_average1)
ffqdata3$bean_average1[is.na(ffqdata3$bean_average1)]<-0
#poultry
ffqdata3[,poultry_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103060_1",
                                                                      'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$poultry_average1)
ffqdata3$poultry_average1[is.na(ffqdata3$poultry_average1)]<-0
#red meat
ffqdata3[,red_meat_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103010_1","a103020_1","a103030_1","a103040_1","a103070_1",
                                                                       "a103080_1",'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$red_meat_average1)
ffqdata3$red_meat_average1[is.na(ffqdata3$red_meat_average1)]<-0
#Fast/fried foods 
ffqdata3[,fastfried_food_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103050_1","a103170_1","a103180_1","a104020_1","a102460_1")]
table(ffqdata3$fastfried_food_average1)
ffqdata3$fastfried_food_average1[is.na(ffqdata3$fastfried_food_average1)]<-0
#Pastries and sweets 
ffqdata3[,Pastries_sweets_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a101970_1","a101980_1","a101990_1","a102010_1","a102020_1",
                                                                              "a102030_1","a102050_1","a102060_1",'a102070_1',
                                                                              "a102120_1","a102140_1",'a102150_1','a102170_1','a102180_1',
                                                                              "a102190_1","a102200_1","a102210_1","a102220_1","a102230_1",
                                                                              "a102260_1","a102270_1","a102280_1","a102290_1","a102300_1","a102310_1",
                                                                              "a102320_1","a102320_1",'a102330_1','a102340_1','a102350_1',
                                                                              "a102360_1",'a102370_1',"a102380_1")]
table(ffqdata3$Pastries_sweets_average1)
ffqdata3$Pastries_sweets_average1[is.na(ffqdata3$Pastries_sweets_average1)]<-0
#Wine
ffqdata3[,wine_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100590_1","a100630_1","a100670_1","a100720_1")]
table(ffqdata3$wine_average1)
ffqdata3$wine_average1[is.na(ffqdata3$wine_average1)]<-0
colnames(ffqdata3)

missing_vals <- colSums(is.na(ffqdata3[,838:851]))
print(missing_vals)
ffqdata3$Green_Leafy_Vegetables_average1_score<-ifelse(ffqdata3$Green_Leafy_Vegetables_average1<=0.285714,0,ifelse(
  ffqdata3$Green_Leafy_Vegetables_average1<0.857143,0.5,1))
table(ffqdata3$Green_Leafy_Vegetables_average1_score)
ffqdata3$Other_Vegetables_average1_score<-ifelse(ffqdata3$Other_Vegetables_average1<0.714285,0,ifelse(
  ffqdata3$Other_Vegetables_average1<1,0.5,1))
ffqdata3$Berries_average1_score<-ifelse(ffqdata3$Berries_average1<0.142857,0,ifelse(
  ffqdata3$Berries_average1<0.285714,0.5,1))
ffqdata3$Nuts_average1_score<-ifelse(ffqdata3$Nuts_average1<0.071429,0,ifelse(
  ffqdata3$Nuts_average1<=0.7142864,0.5,1))
ffqdata3$Butter_average1_score<-ifelse(ffqdata3$Butter_average1==1,1,0)
ffqdata3$cheese_average1_score<-ifelse(ffqdata3$cheese_average1>=1,0,ifelse(
  ffqdata3$cheese_average1>0.142857,0.5,1))
ffqdata3$Whole_grains_average1_score<-ifelse(ffqdata3$Whole_grains_average1<1,0,ifelse(
  ffqdata3$cheese_average1<=2,0.5,1))
ffqdata3$fish_average1_score<-ifelse(ffqdata3$fish_average1<0.03333,0,ifelse(
  ffqdata3$fish_average1<=0.142857,0.5,1))
ffqdata3$bean_average1_score<-ifelse(ffqdata3$bean_average1<0.142857,0,ifelse(
  ffqdata3$bean_average1<=0.428571,0.5,1))
ffqdata3$poultry_average1_score<-ifelse(ffqdata3$poultry_average1<0.142857,0,ifelse(
  ffqdata3$poultry_average1<=0.428571,0.5,1))
ffqdata3$red_meat_average1_score<-ifelse(ffqdata3$red_meat_average1>0.857143,0,ifelse(
  ffqdata3$red_meat_average1>=0.571429,0.5,1))
ffqdata3$fastfried_food_average1_score<-ifelse(ffqdata3$fastfried_food_average1>0.428571,0,ifelse(
  ffqdata3$fastfried_food_average1>=0.142857,0.5,1))
ffqdata3$Pastries_sweets_average1_score<-ifelse(ffqdata3$Pastries_sweets_average1>1,0,ifelse(
  ffqdata3$Pastries_sweets_average1>0.714286,0.5,1))
ffqdata3$wine_average1_score<-ifelse(ffqdata3$wine_average1<0.033333,0,ifelse(
  ffqdata3$wine_average1<0.285714,0.5,1))
ffqdata3[,MIND_average1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_average1_score","Other_Vegetables_average1_score",
                                                                  "Berries_average1_score","Nuts_average1_score","Butter_average1_score","cheese_average1_score",
                                                                  "Whole_grains_average1_score","fish_average1_score","bean_average1_score","poultry_average1_score",
                                                                  "red_meat_average1_score","fastfried_food_average1_score","Pastries_sweets_average1_score","wine_average1_score")]
table(ffqdata3[,MIND_average1])
#MIND_average2: Calculated using baseline 24hFFQ and average intake####
ffqdata3[, Green_Leafy_Vegetables_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104160_2", "a104300_2", "a104240_2","a104370_2", "a104090_2_0.5",
                                                                                      "a104360_2", "a104180_2", "a104080_2", "a104060_2_0.5", "a104070_2_0.5")]
table(ffqdata3$Green_Leafy_Vegetables_average2)
ffqdata3$Green_Leafy_Vegetables_average2[is.na(ffqdata3$Green_Leafy_Vegetables_average2)]<-0
#other vegetables
ffqdata3[, Other_Vegetables_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104190_2", "a104170_2", "a104140_2","a104340_2", "a104350_2","a104320_2","a104060_2_0.5", "a104070_2_0.5","a104090_2_0.5",
                                                                                "a104130_2", "a104150_2", "a104200_2","a104210_2","a104220_2","a104230_2", 
                                                                                "a104250_2","a104260_2","a104270_2","a104290_2","a104310_2","a104330_2","a104380_2",'Canned_soup_vegetables',"homemade_soup_vegetables")]
table(ffqdata3$Other_Vegetables_average2)
ffqdata3$Other_Vegetables_average2[is.na(ffqdata3$Other_Vegetables_average2)]<-0
#Berries
ffqdata3[, Berries_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104470_2")]
table(ffqdata3$Berries_average2)
ffqdata3$Berries_average2[is.na(ffqdata3$Berries_average2)]<-0
#Nuts
ffqdata3[, Nuts_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a102430_2","a102410_2","a102450_2","a102440_2",
                                                                    "a102420_2")]
table(ffqdata3$Nuts_average2)
ffqdata3$Nuts_average2[is.na(ffqdata3$Nuts_average2)]<-0
#Butter/margarine
ffqdata3$Butter_average2<-ifelse(ffqdata3$a104040_2==1|ffqdata3$a101300_2==1|ffqdata3$a104040_2==1,1,0)
ffqdata3$Butter_average2[is.na(ffqdata3$Butter_average2)]<-0
table(ffqdata3$Butter_average2)
#Whole_grains
ffqdata3[, Whole_grains_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100770_2","a100800_2","a100840_2","a100850_2",
                                                                            "a102720_2","a102740_2")]
table(ffqdata3$Whole_grains_average2)
ffqdata3$Whole_grains_average2[is.na(ffqdata3$Whole_grains_average2)]<-0
#Cheese
ffqdata3[, cheese_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a102820_2","a102830_2","a102840_2","a102860_2",
                                                                      "a102870_2","a102880_2","a102890_2","a102900_2","a102910_2")]
table(ffqdata3$cheese_average2)
ffqdata3$cheese_average2[is.na(ffqdata3$cheese_average2)]<-0
#fish
ffqdata3[,fish_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103150_2", "a103160_2","a103190_2", "a103200_2","a103210_2",
                                                                   "a103220_2","a103230_2",'Canned_soup_fish',"homemade_soup_fish")]
table(ffqdata3$fish_average2)
ffqdata3$fish_average2[is.na(ffqdata3$fish_average2)]<-0
#Beans
ffqdata3[,bean_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104000_2", "a104010_2","a104110_2", "a104120_2","a104280_2",
                                                                   'Canned_soup_bean',"homemade_soup_bean","a103270_2")]
table(ffqdata3$bean_average2)
ffqdata3$bean_average2[is.na(ffqdata3$bean_average2)]<-0
#poultry
ffqdata3[,poultry_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103060_2",
                                                                      'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$poultry_average2)
ffqdata3$poultry_average2[is.na(ffqdata3$poultry_average2)]<-0
#red meat
ffqdata3[,red_meat_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103010_2","a103020_2","a103030_2","a103040_2","a103070_2",
                                                                       "a103080_2",'Canned_soup_meat',"homemade_soup_meat")]
table(ffqdata3$red_meat_average2)
ffqdata3$red_meat_average2[is.na(ffqdata3$red_meat_average2)]<-0
#Fast/fried foods 
ffqdata3[,fastfried_food_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a103050_2","a103170_2","a103180_2","a104020_2","a102460_2")]
table(ffqdata3$fastfried_food_average2)
ffqdata3$fastfried_food_average2[is.na(ffqdata3$fastfried_food_average2)]<-0
#Pastries and sweets 
ffqdata3[,Pastries_sweets_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a101970_2","a101980_2","a101990_2","a102010_2","a102020_2",
                                                                              "a102030_2","a102050_2","a102060_2",'a102070_2',
                                                                              "a102120_2","a102140_2",'a102150_2','a102170_2','a102180_2',
                                                                              "a102190_2","a102200_2","a102210_2","a102220_2","a102230_2",
                                                                              "a102260_2","a102270_2","a102280_2","a102290_2","a102300_2","a102310_2",
                                                                              "a102320_2","a102320_2",'a102330_2','a102340_2','a102350_2',
                                                                              "a102360_2",'a102370_2',"a102380_2")]
table(ffqdata3$Pastries_sweets_average2)
ffqdata3$Pastries_sweets_average2[is.na(ffqdata3$Pastries_sweets_average2)]<-0
#Wine
ffqdata3[,wine_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100590_2","a100630_2","a100670_2","a100720_2")]
table(ffqdata3$wine_average2)
ffqdata3$wine_average2[is.na(ffqdata3$wine_average2)]<-0
colnames(ffqdata3)
missing_vals <- colSums(is.na(ffqdata3[,867:879]))
print(missing_vals)
ffqdata3$Green_Leafy_Vegetables_average2_score<-ifelse(ffqdata3$Green_Leafy_Vegetables_average2<=0.285714,0,ifelse(
  ffqdata3$Green_Leafy_Vegetables_average2<0.857143,0.5,1))
table(ffqdata3$Green_Leafy_Vegetables_average2_score)
ffqdata3$Other_Vegetables_average2_score<-ifelse(ffqdata3$Other_Vegetables_average2<0.714285,0,ifelse(
  ffqdata3$Other_Vegetables_average2<1,0.5,1))
ffqdata3$Berries_average2_score<-ifelse(ffqdata3$Berries_average2<0.142857,0,ifelse(
  ffqdata3$Berries_average2<0.285714,0.5,1))
ffqdata3$Nuts_average2_score<-ifelse(ffqdata3$Nuts_average2<0.071429,0,ifelse(
  ffqdata3$Nuts_average2<=0.7142864,0.5,1))
ffqdata3$Butter_average2_score<-ifelse(ffqdata3$Butter_average2==1,1,0)
ffqdata3$cheese_average2_score<-ifelse(ffqdata3$cheese_average2>=1,0,ifelse(
  ffqdata3$cheese_average2>0.142857,0.5,1))
ffqdata3$Whole_grains_average2_score<-ifelse(ffqdata3$Whole_grains_average2<1,0,ifelse(
  ffqdata3$cheese_average2<=2,0.5,1))
ffqdata3$fish_average2_score<-ifelse(ffqdata3$fish_average2<0.03333,0,ifelse(
  ffqdata3$fish_average2<=0.142857,0.5,1))
ffqdata3$bean_average2_score<-ifelse(ffqdata3$bean_average2<0.142857,0,ifelse(
  ffqdata3$bean_average2<=0.428571,0.5,1))
ffqdata3$poultry_average2_score<-ifelse(ffqdata3$poultry_average2<0.142857,0,ifelse(
  ffqdata3$poultry_average2<=0.428571,0.5,1))
ffqdata3$red_meat_average2_score<-ifelse(ffqdata3$red_meat_average2>0.857143,0,ifelse(
  ffqdata3$red_meat_average2>=0.571429,0.5,1))
ffqdata3$fastfried_food_average2_score<-ifelse(ffqdata3$fastfried_food_average2>0.428571,0,ifelse(
  ffqdata3$fastfried_food_average2>=0.142857,0.5,1))
ffqdata3$Pastries_sweets_average2_score<-ifelse(ffqdata3$Pastries_sweets_average2>1,0,ifelse(
  ffqdata3$Pastries_sweets_average2>0.714286,0.5,1))
ffqdata3$wine_average2_score<-ifelse(ffqdata3$wine_average2<0.033333,0,ifelse(
  ffqdata3$wine_average2<0.285714,0.5,1))
ffqdata3[,MIND_average2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Green_Leafy_Vegetables_average2_score","Other_Vegetables_average2_score",
                                                                  "Berries_average2_score","Nuts_average2_score","Butter_average2_score","cheese_average2_score",
                                                                  "Whole_grains_average2_score","fish_average2_score","bean_average2_score","poultry_average2_score",
                                                                  "red_meat_average2_score","fastfried_food_average2_score","Pastries_sweets_average2_score","wine_average2_score")]
table(ffqdata3[,MIND_average2])
#MEDAS_sum1: Calculated using average 24hFFQ and total intake####
#Vegetables_sum1
ffqdata3[, Vegetables_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104160_1", "a104300_1", "a104240_1","a104370_1", "a104360_1",
                                                                     "a104180_1", "a104080_1", "a104190_1", "a104170_1","a104140_1","a104340_1",
                                                                     "a104350_1","a104320_1","a104060_1","a104070_1","a104130_1","a104150_1",
                                                                     "a104200_1","a104210_1","a104220_1","a104230_1","a104250_1","a104260_1",
                                                                     "a104270_1","a104290_1","a104310_1","a104330_1","a104380_1")]
table(ffqdata3$Green_Leafy_Vegetables_sum1)
#fruit_sum1
ffqdata3$a100190_1_0.25<-ifelse(is.na(ffqdata3[, a100190_1]), NA, 0.25*ffqdata3[, a100190_1])
ffqdata3$a100200_1_0.25<-ifelse(is.na(ffqdata3[, a100200_1]), NA, 0.25*ffqdata3[, a100200_1])
ffqdata3$a100210_1_0.5<-ifelse(is.na(ffqdata3[, a100210_1]), NA, 0.5*ffqdata3[, a100210_1])
ffqdata3$a100220_1_0.25<-ifelse(is.na(ffqdata3[, a100220_1]), NA, 0.25*ffqdata3[, a100220_1])
ffqdata3[, fruit_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104410_1", "a104420_1", "a104430_1","a104440_1", "a104450_1",
                                                                "a104460_1", "a104470_1", "a104480_1", "a104490_1","a104500_1","a104510_1",
                                                                "a104520_1","a104530_1","a104540_1","a104550_1","a104560_1","a104570_1",
                                                                "a104580_1","a104590_1","a100190_1_0.25","a100200_1_0.25",
                                                                "a100210_1_0.5","a100220_1_0.25")]
table(ffqdata3$fruit_sum1)
#Red meat =red_meat_sum1
#Butter,margarine or cream =ffqdata3$Butter_sum1
#Sweetened or carbonated drinks
ffqdata3[, Sweetened_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100160_1", "a100170_1", "a100180_1")]
table(ffqdata3[, Sweetened_sum1])
#Wine =wine_sum1
#Legumes =bean_sum1
#Seafood =fish_sum1
#Sweets or pastries =Pastries_sweets_sum1
#Nuts =Nuts_sum1
#white meat =poultry_sum1
colnames(ffqdata3)
missing_vals <- colSums(is.na(ffqdata3[,896:902]))
print(missing_vals)
colnames(ffqdata3)
ffqdata3$Vegetables_sum1_score<-ifelse(ffqdata3$Vegetables_sum1>=2,1,ifelse(ffqdata3$a104090_1>=1,1,0))#大于2份蔬菜或者大于一份沙拉
ffqdata3$Vegetables_sum1_score[is.na(ffqdata3$Vegetables_sum1_score)]<-0
ffqdata3$fruit_sum1_score<-ifelse(ffqdata3$fruit_sum1>=3,1,0)
ffqdata3$Mred_meat_sum1_score<-ifelse(ffqdata3$red_meat_sum1>=1,0,1)
ffqdata3$MButter_sum1_score<-ifelse(ffqdata3$Butter_sum1>=1,0,1)
ffqdata3$MSweetened_sum1_score<-ifelse(ffqdata3$Sweetened_sum1>=1,0,1)
ffqdata3$Mwine_sum1_score<-ifelse(ffqdata3$wine_sum1>=1,1,0)
ffqdata3$Mbean_sum1_score<-ifelse(ffqdata3$bean_sum1>=0.428571,1,0)
ffqdata3$Mfish_sum1_score<-ifelse(ffqdata3$fish_sum1>=0.428571,1,0)
ffqdata3$MPastries_sweets_sum1_score<-ifelse(ffqdata3$Pastries_sweets_sum1>=0.285714,0,1)
ffqdata3$MNuts_sum1_score<-ifelse(ffqdata3$Nuts_sum1>=0.428571,1,0)
ffqdata3$Mpoultry_sum1_score<-ifelse(ffqdata3$poultry_sum1>ffqdata3$red_meat_sum1,1,0)
ffqdata3[,MEDAS_sum1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum1_score","fruit_sum1_score",
                                                               "Mred_meat_sum1_score","MButter_sum1_score","MSweetened_sum1_score",
                                                               "Mwine_sum1_score","Mbean_sum1_score","Mfish_sum1_score","MPastries_sweets_sum1_score",
                                                               "MNuts_sum1_score","Mpoultry_sum1_score")]
table(ffqdata3[,MEDAS_sum1])
##MEDAS_sum2: Calculated using baseline 24hFFQ and total intake####
#Vegetables_sum2
ffqdata3[, Vegetables_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104160_2", "a104300_2", "a104240_2","a104370_2", "a104360_2",
                                                                     "a104180_2", "a104080_2", "a104190_2", "a104170_2","a104140_2","a104340_2",
                                                                     "a104350_2","a104320_2","a104060_2","a104070_2","a104130_2","a104150_2",
                                                                     "a104200_2","a104210_2","a104220_2","a104230_2","a104250_2","a104260_2",
                                                                     "a104270_2","a104290_2","a104310_2","a104330_2","a104380_2")]
table(ffqdata3$Green_Leafy_Vegetables_sum2)
#fruit_sum2
ffqdata3$a100190_2_0.25<-ifelse(is.na(ffqdata3[, a100190_2]), NA, 0.25*ffqdata3[, a100190_2])
ffqdata3$a100200_2_0.25<-ifelse(is.na(ffqdata3[, a100200_2]), NA, 0.25*ffqdata3[, a100200_2])
ffqdata3$a100210_2_0.5<-ifelse(is.na(ffqdata3[, a100210_2]), NA, 0.5*ffqdata3[, a100210_2])
ffqdata3$a100220_2_0.25<-ifelse(is.na(ffqdata3[, a100220_2]), NA, 0.25*ffqdata3[, a100220_2])
ffqdata3[, fruit_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a104410_2", "a104420_2", "a104430_2","a104440_2", "a104450_2",
                                                                "a104460_2", "a104470_2", "a104480_2", "a104490_2","a104500_2","a104510_2",
                                                                "a104520_2","a104530_2","a104540_2","a104550_2","a104560_2","a104570_2",
                                                                "a104580_2","a104590_2","a100190_2_0.25","a100200_2_0.25",
                                                                "a100210_2_0.5","a100220_2_0.25")]
table(ffqdata3$fruit_sum2)
#Red meat=red_meat_sum2
#Butter,margarine or cream=ffqdata3$Butter_sum2
#Sweetened or carbonated drinks
ffqdata3[, Sweetened_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("a100160_2", "a100170_2", "a100180_2")]
table(ffqdata3[, Sweetened_sum2])
#Wine=wine_sum2
#Legumes =bean_sum2
#Seafood=fish_sum2
#Sweets or pastries=Pastries_sweets_sum2
#Nuts=Nuts_sum2
#white meat=poultry_sum2
colnames(ffqdata3)
missing_vals <- colSums(is.na(ffqdata3[,916:921]))
print(missing_vals)
colnames(ffqdata3)
ffqdata3$Vegetables_sum2_score<-ifelse(ffqdata3$Vegetables_sum2>=2,1,ifelse(ffqdata3$a104090_2>=1,1,0))#大于2份蔬菜或者大于一份沙拉
ffqdata3$Vegetables_sum2_score[is.na(ffqdata3$Vegetables_sum2_score)]<-0
ffqdata3$fruit_sum2_score<-ifelse(ffqdata3$fruit_sum2>=3,1,0)
ffqdata3$Mred_meat_sum2_score<-ifelse(ffqdata3$red_meat_sum2>=1,0,1)
ffqdata3$MButter_sum2_score<-ifelse(ffqdata3$Butter_sum2>=1,0,1)
ffqdata3$MSweetened_sum2_score<-ifelse(ffqdata3$Sweetened_sum2>=1,0,1)
ffqdata3$Mwine_sum2_score<-ifelse(ffqdata3$wine_sum2>=1,1,0)
ffqdata3$Mbean_sum2_score<-ifelse(ffqdata3$bean_sum2>=0.428571,1,0)
ffqdata3$Mfish_sum2_score<-ifelse(ffqdata3$fish_sum2>=0.428571,1,0)
ffqdata3$MPastries_sweets_sum2_score<-ifelse(ffqdata3$Pastries_sweets_sum2>=0.285714,0,1)
ffqdata3$MNuts_sum2_score<-ifelse(ffqdata3$Nuts_sum2>=0.428571,1,0)
ffqdata3$Mpoultry_sum2_score<-ifelse(ffqdata3$poultry_sum2>ffqdata3$red_meat_sum2,1,0)
ffqdata3[,MEDAS_sum2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_sum2_score","fruit_sum2_score",
                                                               "Mred_meat_sum2_score","MButter_sum2_score","MSweetened_sum2_score",
                                                               "Mwine_sum2_score","Mbean_sum2_score","Mfish_sum2_score","MPastries_sweets_sum2_score",
                                                               "MNuts_sum2_score","Mpoultry_sum2_score")]
table(ffqdata3[,MEDAS_sum2])
#MEDAS_average1: Calculated using average 24hFFQ and average intake####
#Vegetables_average1
ffqdata3[, Vegetables_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104160_1", "a104300_1", "a104240_1","a104370_1", "a104360_1",
                                                                          "a104180_1", "a104080_1", "a104190_1", "a104170_1","a104140_1","a104340_1",
                                                                          "a104350_1","a104320_1","a104060_1","a104070_1","a104130_1","a104150_1",
                                                                          "a104200_1","a104210_1","a104220_1","a104230_1","a104250_1","a104260_1",
                                                                          "a104270_1","a104290_1","a104310_1","a104330_1","a104380_1")]
table(ffqdata3$Green_Leafy_Vegetables_average1)
#fruit_average1
ffqdata3[, fruit_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104410_1", "a104420_1", "a104430_1","a104440_1", "a104450_1",
                                                                     "a104460_1", "a104470_1", "a104480_1", "a104490_1","a104500_1","a104510_1",
                                                                     "a104520_1","a104530_1","a104540_1","a104550_1","a104560_1","a104570_1",
                                                                     "a104580_1","a104590_1","a100190_1_0.25","a100200_1_0.25",
                                                                     "a100210_1_0.5","a100220_1_0.25")]
table(ffqdata3$fruit_average1)
#Red meat =red_meat_average1
#Butter,margarine or cream =ffqdata3$Butter_average1
#Sweetened or carbonated drinks
ffqdata3[, Sweetened_average1:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100160_1", "a100170_1", "a100180_1")]
table(ffqdata3[, Sweetened_average1])
#Wine =wine_average1
#Legumes =bean_average1
#Seafood =fish_average1
#Sweets or pastries =Pastries_sweets_average1
#Nuts =Nuts_average1
#white meat =poultry_average1
ffqdata3$Vegetables_average1_score<-ifelse(ffqdata3$Vegetables_average1>=2,1,ifelse(ffqdata3$a104090_1>=1,1,0))#大于2份蔬菜或者大于一份沙拉
ffqdata3$Vegetables_average1_score[is.na(ffqdata3$Vegetables_average1_score)]<-0
ffqdata3$fruit_average1_score<-ifelse(ffqdata3$fruit_average1>=3,1,0)
ffqdata3$Mred_meat_average1_score<-ifelse(ffqdata3$red_meat_average1>=1,0,1)
ffqdata3$MButter_average1_score<-ifelse(ffqdata3$Butter_average1>=1,0,1)
ffqdata3$MSweetened_average1_score<-ifelse(ffqdata3$Sweetened_average1>=1,0,1)
ffqdata3$Mwine_average1_score<-ifelse(ffqdata3$wine_average1>=1,1,0)
ffqdata3$Mbean_average1_score<-ifelse(ffqdata3$bean_average1>=0.428571,1,0)
ffqdata3$Mfish_average1_score<-ifelse(ffqdata3$fish_average1>=0.428571,1,0)
ffqdata3$MPastries_sweets_average1_score<-ifelse(ffqdata3$Pastries_sweets_average1>=0.285714,0,1)
ffqdata3$MNuts_average1_score<-ifelse(ffqdata3$Nuts_average1>=0.428571,1,0)
ffqdata3$Mpoultry_average1_score<-ifelse(ffqdata3$poultry_average1>ffqdata3$red_meat_average1,1,0)
ffqdata3[,MEDAS_average1:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_average1_score","fruit_average1_score",
                                                                   "Mred_meat_average1_score","MButter_average1_score","MSweetened_average1_score",
                                                                   "Mwine_average1_score","Mbean_average1_score","Mfish_average1_score","MPastries_sweets_average1_score",
                                                                   "MNuts_average1_score","Mpoultry_average1_score")]
table(ffqdata3[,MEDAS_average1])
#MEDAS_average2: Calculated using baseline 24hFFQ and average intake####
#Vegetables_average2
ffqdata3[, Vegetables_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104160_2", "a104300_2", "a104240_2","a104370_2", "a104360_2",
                                                                          "a104180_2", "a104080_2", "a104190_2", "a104170_2","a104140_2","a104340_2",
                                                                          "a104350_2","a104320_2","a104060_2","a104070_2","a104130_2","a104150_2",
                                                                          "a104200_2","a104210_2","a104220_2","a104230_2","a104250_2","a104260_2",
                                                                          "a104270_2","a104290_2","a104310_2","a104330_2","a104380_2")]
table(ffqdata3$Green_Leafy_Vegetables_average2)
#fruit_average2
ffqdata3[, fruit_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a104410_2", "a104420_2", "a104430_2","a104440_2", "a104450_2",
                                                                     "a104460_2", "a104470_2", "a104480_2", "a104490_2","a104500_2","a104510_2",
                                                                     "a104520_2","a104530_2","a104540_2","a104550_2","a104560_2","a104570_2",
                                                                     "a104580_2","a104590_2","a100190_2_0.25","a100200_2_0.25",
                                                                     "a100210_2_0.5","a100220_2_0.25")]
table(ffqdata3$fruit_average2)
#Red meat=red_meat_average2
#Butter,margarine or cream=ffqdata3$Butter_average2
#Sweetened or carbonated drinks
ffqdata3[, Sweetened_average2:= rowMeans(.SD, na.rm = TRUE), .SDcols = c("a100160_2", "a100170_2", "a100180_2")]
table(ffqdata3[, Sweetened_average2])
#Wine=wine_average2
#Legumes =bean_average2
#Seafood =fish_average2
#Sweets or pastries =Pastries_sweets_average2
#Nuts =Nuts_average2
#white meat =poultry_average2
ffqdata3$Vegetables_average2_score<-ifelse(ffqdata3$Vegetables_average2>=2,1,ifelse(ffqdata3$a104090_2>=1,1,0))#大于2份蔬菜或者大于一份沙拉
ffqdata3$Vegetables_average2_score[is.na(ffqdata3$Vegetables_average2_score)]<-0
ffqdata3$fruit_average2_score<-ifelse(ffqdata3$fruit_average2>=3,1,0)
ffqdata3$Mred_meat_average2_score<-ifelse(ffqdata3$red_meat_average2>=1,0,1)
ffqdata3$MButter_average2_score<-ifelse(ffqdata3$Butter_average2>=1,0,1)
ffqdata3$MSweetened_average2_score<-ifelse(ffqdata3$Sweetened_average2>=1,0,1)
ffqdata3$Mwine_average2_score<-ifelse(ffqdata3$wine_average2>=1,1,0)
ffqdata3$Mbean_average2_score<-ifelse(ffqdata3$bean_average2>=0.428571,1,0)
ffqdata3$Mfish_average2_score<-ifelse(ffqdata3$fish_average2>=0.428571,1,0)
ffqdata3$MPastries_sweets_average2_score<-ifelse(ffqdata3$Pastries_sweets_average2>=0.285714,0,1)
ffqdata3$MNuts_average2_score<-ifelse(ffqdata3$Nuts_average2>=0.428571,1,0)
ffqdata3$Mpoultry_average2_score<-ifelse(ffqdata3$poultry_average2>ffqdata3$red_meat_average2,1,0)
ffqdata3[,MEDAS_average2:= rowSums(.SD, na.rm = TRUE), .SDcols = c("Vegetables_average2_score","fruit_average2_score",
                                                                   "Mred_meat_average2_score","MButter_average2_score","MSweetened_average2_score",
                                                                   "Mwine_average2_score","Mbean_average2_score","Mfish_average2_score","MPastries_sweets_average2_score",
                                                                   "MNuts_average2_score","Mpoultry_average2_score")]
table(ffqdata3[,MEDAS_average2])
summary(ffqdata3[,MEDAS_average2])
#Import energy and dietary element data####
setwd("E:/deskbook/data_subset")
file_names <- list.files(pattern = "^1pan_cancer")
ffqenergy <- list()
for (file_name in file_names) {
  data <- fread(file_name,header=T,select=c(2:51))
  ffqenergy[[file_name]] <- data
}
ffqenergy_df <- do.call(rbind, ffqenergy)# 190095 obs
colnames(ffqenergy_df)
#merge
ffqdata4<-merge(ffqdata3,ffqenergy_df,by="eid",all.x = T)
#ffqdata4####
write_excel_csv(ffqdata4,"E:/deskbook/ffqdata4.csv")
#Basic demographic data were combined####
colnames(ffqdata4)
ffq<-subset(ffqdata4,select=c(1,769:1011))
setwd("E:/deskbook/data_subset")
file_names <- list.files(pattern = "^UKBdata")
all_data <- list()
for (file_name in file_names) {
  data <- fread(file_name,header=T)
  all_data[[file_name]] <- data
}
UKBdata <- do.call(rbind, all_data)# 187495 obs
#common eid
common_eids <- intersect(UKBdata$eid, ffq$eid)
matched_data1 <- UKBdata[UKBdata$eid %in% common_eids,]
matched_data2 <- ffq[ffq$eid %in% common_eids,]
####
UKBdata2<-merge(matched_data1,matched_data2,by="eid",all.x = T)
UKBdata2$status<-ifelse(UKBdata2$pan_cancer==0,0,1)
colnames(UKBdata2)
missing_vals <- colSums(is.na(UKBdata2[,218:265]))
table(UKBdata2$status)
print(missing_vals)
#Interpolation method to fill the data
library(mice)
names<-colnames(UKBdata2[,218:265])
new_col_names <- paste("x", 1:48, sep = "")
names(UKBdata2)[218:265] <- new_col_names
tianbu<-mice(UKBdata2[,218:265],m=3)

tianbu2<-complete(tianbu,action=2)
colnames(UKBdata2)
UKBdata2_2<-subset(UKBdata2,select=-c(218:265))
names(tianbu2)<- names
UKBdata2<-cbind(UKBdata2_2,tianbu2)# 187485 obs
#import
write_excel_csv(UKBdata2,"E:/deskbook/UKBdata2.csv")