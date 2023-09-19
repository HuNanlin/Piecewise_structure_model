install.packages("piecewiseSEM")
install.packages("mediation")
library(mediation)
library(piecewiseSEM) 
library(openxlsx)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggplot2)
library(lme4)
library(lavaan)
library(semPlot)
library(vegan)
# library(sem)
# detach("package:sem",unload=TRUE)

data <- read_xls("O:\\china_biomass\\参数\\soilgrids_new.xls",sheet=1)
data <- data[,c(1:25,37,43,48,53,58)]
colnames(data)[27:30] <- c("soc","PH","ocd","N")
weatherdata <- read_xls("o:\\china_biomass\\参数\\weather_new.xls",sheet=1)
weatherdata <- weatherdata[,c(3,43,48,53,58)]
colnames(weatherdata)[2:5] <- c("temp_std","temp_mean","pre_std","pre_mean")
SEMdata <- left_join(data,weatherdata,by="ID")
# write.csv(SEMdata,"J:\\china_biomass\\结构方程\\SEMdata_cv.csv")
SEMdata <- subset(SEMdata,SEMdata$cv>0)
#处理复合变量
####水位####
unique(SEMdata$积水状况_x)   #watertype
#处理水位类型
SEMdata$积水状况_x[SEMdata$积水状况_x=="不积水"] = "暂时性"
SEMdata$积水状况_x[SEMdata$积水状况_x=="长期积水"] = "永久性"
SEMdata$积水状况_x[SEMdata$积水状况_x=="常年积水"] = "永久性"


# SEMdata$积水状况_x[SEMdata$积水状况_x=="不积水"] = "aa"
SEMdata$积水状况_x[SEMdata$积水状况_x=="暂时性"] = "aa"
SEMdata$积水状况_x[SEMdata$积水状况_x=="季节性"] = "bb"
# SEMdata$积水状况_x[SEMdata$积水状况_x=="长期积水"] = "dd"
# SEMdata$积水状况_x[SEMdata$积水状况_x=="常年积水"] = "ee"
SEMdata$积水状况_x[SEMdata$积水状况_x=="永久性"] = "cc"
# test <- subset(SEMdata,SEMdata$积水状况_x != "aa" & SEMdata$积水状况_x != "bb"
#                & SEMdata$积水状况_x != "cc")
colnames(SEMdata)[10] <- "watertype"
#将因子数据转化为0/1数据
waterType <- model.matrix(~ watertype - 1, data=SEMdata)
head(waterType)
#用转化为0/1的数据矩阵赋值替换原数据中的因子变量
SEMdata$wType <-waterType
head(SEMdata)
#输出数据转化后的“csv”格式到本地
write.csv(SEMdata,file="O:\\china_biomass\\结构方程\\SEMdata_wtype.csv",row.names = FALSE)
#读取上一步保存的数据文件夹
SEMdata <- read.csv("O:\\china_biomass\\结构方程\\SEMdata_wtype.csv",header=T)
SEMdata <- read_xls("O:\\china_biomass\\结构方程\\SEMdata_wtype.xls",sheet=1)

colnames(SEMdata)[c(35:37)] <- c("wType_aa","wType_bb","wType_cc")
#构造复合变量XX4,拟合线性模型XX4_model
XX4_model <- lm(cv ~ wType_aa + wType_bb + wType_cc,SEMdata)
summary(XX4_model)
summary(XX4_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX4_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX4_model)$coefficients[3, 1]
# beta_Type.Typecc <-  summary(XX4_model)$coefficients[4, 1]
# beta_Type.Typedd <-  summary(XX4_model)$coefficients[5, 1]
# beta_Type.Typeee <-  summary(XX4_model)$coefficients[6, 1]
# beta_Type.Typeff <-  summary(XX4_model)$coefficients[7, 1] #na

XX1 <- beta_Type.Typeaa*SEMdata$wType_aa + beta_Type.Typebb*SEMdata$wType_bb
SEMdata$waterxx1 <- XX1
summary(lm(cv ~ waterxx1, SEMdata))
coefs(lm(cv ~ waterxx1, SEMdata))
####soil type####
unique(SEMdata$土壤_x)   #soiltype
SEMdata$土壤_x[SEMdata$土壤_x=="沼泽土"] = "aa"
SEMdata$土壤_x[SEMdata$土壤_x=="潮土-潮盐土"] = "bb"
SEMdata$土壤_x[SEMdata$土壤_x=="潮盐土"] = "cc"
SEMdata$土壤_x[SEMdata$土壤_x=="潮土"] = "dd"
SEMdata$土壤_x[SEMdata$土壤_x=="棕壤土"] = "ee"
SEMdata$土壤_x[SEMdata$土壤_x=="沙壤土"] = "ff"
SEMdata$土壤_x[SEMdata$土壤_x=="海泥土"] = "gg"
SEMdata$土壤_x[SEMdata$土壤_x=="海泥沙土"] = "hh"
SEMdata$土壤_x[SEMdata$土壤_x=="冲积土"] = "ii"
SEMdata$土壤_x[SEMdata$土壤_x=="冲积沙土"] = "jj"
SEMdata$土壤_x[SEMdata$土壤_x=="底质"] = "kk"
SEMdata$土壤_x[SEMdata$土壤_x=="砂石"] = "ll"
SEMdata$土壤_x[SEMdata$土壤_x=="沉积土"] = "mm"
SEMdata$土壤_x[SEMdata$土壤_x=="砂土"] = "nn"
SEMdata$土壤_x[SEMdata$土壤_x=="红黄壤"] = "oo"
SEMdata$土壤_x[SEMdata$土壤_x=="沉积土（红壤）"] = "pp"
SEMdata$土壤_x[SEMdata$土壤_x=="红壤"] = "qq"
SEMdata$土壤_x[SEMdata$土壤_x=="泥炭土"] = "rr"
SEMdata$土壤_x[SEMdata$土壤_x=="草甸土"] = "ss"
SEMdata$土壤_x[SEMdata$土壤_x=="盐土"] = "tt"
SEMdata$土壤_x[SEMdata$土壤_x=="腐殖土"] = "uu"
SEMdata$土壤_x[SEMdata$土壤_x=="腐殖地"] = "vv"
SEMdata$土壤_x[SEMdata$土壤_x=="盐碱地"] = "ww"
SEMdata$土壤_x[SEMdata$土壤_x=="盐碱"] = "xx"
SEMdata$土壤_x[SEMdata$土壤_x=="沙土"] = "yy"
SEMdata$土壤_x[SEMdata$土壤_x=="荒漠土"] = "zz"

colnames(SEMdata)[11] <- "soiltype"
#将因子数据转化为0/1数据
soiltype <- model.matrix(~ soiltype - 1, data=SEMdata)
head(waterType)
#用转化为0/1的数据矩阵赋值替换原数据中的因子变量
SEMdata$sType <-soiltype
head(SEMdata)
#输出数据转化后的“csv”格式到本地
write.csv(SEMdata,file="J:\\china_biomass\\结构方程\\SEMdata_stype.csv",row.names = FALSE)
#读取上一步保存的数据文件夹
SEMdata <- read.csv("J:\\china_biomass\\结构方程\\SEMdata_stype.csv",header=T)
colnames(SEMdata)[c(39:64)] <- c("sType_aa","sType_bb","sType_cc","sType_dd",
         "sType_ee","sType_ff","sType_gg","sType_hh","sType_ii","sType_jj",
         "sType_kk","sType_ll","sType_mm","sType_nn","sType_oo","sType_pp",
         "sType_qq","sType_rr","sType_ss","sType_tt","sType_uu","sType_vv",
         "sType_ww","sType_xx","sType_yy","sType_zz")
#构造复合变量XX2,拟合线性模型XX2_model  
XX2_model <- lm(cv ~ sType_aa + sType_bb + sType_cc + sType_dd
                + sType_ee + sType_ff + sType_gg + sType_hh + sType_ii + sType_jj +
                sType_kk + sType_ll + sType_mm + sType_nn + sType_oo + sType_pp +
                sType_qq + sType_rr + sType_ss + sType_tt + sType_uu + sType_vv + 
                sType_ww + sType_xx + sType_yy + sType_zz, SEMdata)
summary(XX2_model)
summary(XX2_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX2_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX2_model)$coefficients[3, 1]
beta_Type.Typecc <-  summary(XX2_model)$coefficients[4, 1]
beta_Type.Typedd <-  summary(XX2_model)$coefficients[5, 1]
beta_Type.Typeee <-  summary(XX2_model)$coefficients[6, 1]
beta_Type.Typeff <-  summary(XX2_model)$coefficients[7, 1]
beta_Type.Typegg <-  summary(XX2_model)$coefficients[8, 1]
beta_Type.Typehh <-  summary(XX2_model)$coefficients[9, 1]
beta_Type.Typeii <-  summary(XX2_model)$coefficients[10, 1]
beta_Type.Typejj <-  summary(XX2_model)$coefficients[11, 1]
beta_Type.Typekk <-  summary(XX2_model)$coefficients[12, 1]
beta_Type.Typell <-  summary(XX2_model)$coefficients[13, 1]
beta_Type.Typemm <-  summary(XX2_model)$coefficients[14, 1]
beta_Type.Typenn <-  summary(XX2_model)$coefficients[15, 1]
beta_Type.Typeoo <-  summary(XX2_model)$coefficients[16, 1]
beta_Type.Typepp <-  summary(XX2_model)$coefficients[17, 1]
beta_Type.Typeqq <-  summary(XX2_model)$coefficients[18, 1]
beta_Type.Typerr <-  summary(XX2_model)$coefficients[19, 1]
beta_Type.Typess <-  summary(XX2_model)$coefficients[20, 1]
beta_Type.Typett <-  summary(XX2_model)$coefficients[21, 1]
beta_Type.Typeuu <-  summary(XX2_model)$coefficients[22, 1]
beta_Type.Typevv <-  summary(XX2_model)$coefficients[23, 1]
beta_Type.Typeww <-  summary(XX2_model)$coefficients[24, 1]
beta_Type.Typexx <-  summary(XX2_model)$coefficients[25, 1]
beta_Type.Typeyy <-  summary(XX2_model)$coefficients[26, 1]
# beta_Type.Typezz <-  summary(XX2_model)$coefficients[27, 1]


XX2 <- beta_Type.Typeaa*SEMdata$sType_aa + beta_Type.Typebb*SEMdata$sType_bb +
  beta_Type.Typecc*SEMdata$sType_cc + beta_Type.Typedd*SEMdata$sType_dd + 
  beta_Type.Typeee*SEMdata$sType_ee + beta_Type.Typeff*SEMdata$sType_ff +
  beta_Type.Typegg*SEMdata$sType_gg + beta_Type.Typehh*SEMdata$sType_hh +
  beta_Type.Typeii*SEMdata$sType_ii + beta_Type.Typejj*SEMdata$sType_jj +
  beta_Type.Typekk*SEMdata$sType_kk + beta_Type.Typell*SEMdata$sType_ll + 
  beta_Type.Typemm*SEMdata$sType_mm + beta_Type.Typenn*SEMdata$sType_nn +
  beta_Type.Typeoo*SEMdata$sType_oo + beta_Type.Typepp*SEMdata$sType_pp + 
  beta_Type.Typeqq*SEMdata$sType_qq + beta_Type.Typerr*SEMdata$sType_rr + 
  beta_Type.Typess*SEMdata$sType_ss + beta_Type.Typett*SEMdata$sType_tt + 
  beta_Type.Typeuu*SEMdata$sType_uu + beta_Type.Typevv*SEMdata$sType_vv + 
  beta_Type.Typeww*SEMdata$sType_ww + beta_Type.Typexx*SEMdata$sType_xx + 
  beta_Type.Typeyy*SEMdata$sType_yy #+ beta_Type.Typezz*SEMdata$sType_zz + 
  
SEMdata$soilxx2 <- XX2
summary(lm(cv ~ soilxx2, SEMdata))
coefs(lm(cv ~ soilxx2, SEMdata))

#提出复合变量
SEMdata <- SEMdata[,c(1:34,38,65)]
SEMdatabf <- SEMdata
SEMdata <- SEMdatabf
####plant type####
unique(SEMdata$type)   #plant type
SEMdata$type[SEMdata$type!="互花米草" & SEMdata$type!="芦苇" &
               SEMdata$type!="藨草" & SEMdata$type!="薹草" &
               SEMdata$type!="荻" & SEMdata$type!="小叶章" &
               SEMdata$type!="披碱草" & SEMdata$type!="华扁穗草" &
               SEMdata$type!="碱蓬"] = "jj"
SEMdata$type[SEMdata$type=="互花米草"] = "aa"
SEMdata$type[SEMdata$type=="芦苇"] = "bb"
SEMdata$type[SEMdata$type=="藨草"] = "cc"
SEMdata$type[SEMdata$type=="薹草"] = "dd"
SEMdata$type[SEMdata$type=="荻"] = "ee"
SEMdata$type[SEMdata$type=="小叶章"] = "ff"
SEMdata$type[SEMdata$type=="披碱草"] = "gg"
SEMdata$type[SEMdata$type=="华扁穗草"] = "hh"
SEMdata$type[SEMdata$type=="碱蓬"] = "ii"

colnames(SEMdata)[9] <- "planttype"
#将因子数据转化为0/1数据
planttype <- model.matrix(~ planttype - 1, data=SEMdata)
head(planttype)
#用转化为0/1的数据矩阵赋值替换原数据中的因子变量
SEMdata$pType <-planttype
head(SEMdata)
#输出数据转化后的“csv”格式到本地
write.csv(SEMdata,file="J:\\china_biomass\\结构方程\\SEMdata_ptype.csv",row.names = FALSE)
#读取上一步保存的数据文件夹
SEMdata <- read.csv("J:\\china_biomass\\结构方程\\SEMdata_ptype.csv",header=T)
colnames(SEMdata)[c(37:46)] <- c("pType_aa","pType_bb","pType_cc",
                                 "pType_dd","pType_ee","pType_ff",
                                 "pType_gg","pType_hh","pType_ii",
                                 "pType_jj")
#构造复合变量XX3,拟合线性模型XX3_model
XX3_model <- lm(cv ~ pType_aa + pType_bb + pType_cc + pType_dd
                + pType_ee + pType_ff + pType_gg + pType_hh + pType_ii +
                pType_jj, SEMdata)
summary(XX3_model)
summary(XX3_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX3_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX3_model)$coefficients[3, 1]
beta_Type.Typecc <-  summary(XX3_model)$coefficients[4, 1]
beta_Type.Typedd <-  summary(XX3_model)$coefficients[5, 1]
beta_Type.Typeee <-  summary(XX3_model)$coefficients[6, 1]
beta_Type.Typeff <-  summary(XX3_model)$coefficients[7, 1]
beta_Type.Typegg <-  summary(XX3_model)$coefficients[8, 1]
beta_Type.Typehh <-  summary(XX3_model)$coefficients[9, 1]
beta_Type.Typeii <-  summary(XX3_model)$coefficients[10, 1]
# beta_Type.Typejj <-  summary(XX3_model)$coefficients[11, 1]

XX3 <- beta_Type.Typeaa*SEMdata$pType_aa + beta_Type.Typebb*SEMdata$pType_bb +
  beta_Type.Typecc*SEMdata$pType_cc + beta_Type.Typedd*SEMdata$pType_dd + 
  beta_Type.Typeee*SEMdata$pType_ee + beta_Type.Typeff*SEMdata$pType_ff + 
  beta_Type.Typegg*SEMdata$pType_gg + beta_Type.Typehh*SEMdata$pType_hh + 
  beta_Type.Typeii*SEMdata$pType_ii
  
SEMdata$plantxx3 <- XX3
summary(lm(cv ~ plantxx3, SEMdata))
coefs(lm(cv ~ plantxx3, SEMdata))

SEMdata <- SEMdata[,c(1:36,47)]
SEMdatabf <- SEMdata
####
#构造地理复合变量XX4  lon + lat  海拔
XX4_model <- lm(cv ~ lat + altitude, SEMdata)
summary(XX4_model)
# table1 <- summary(XX4_model)$coefficients
# write.csv(table1,file="O:\\china_biomass\\表格\\table1_geography.csv",row.names = FALSE)
summary(XX4_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX4_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX4_model)$coefficients[3, 1]
# beta_Type.Typecc <-  summary(XX4_model)$coefficients[4, 1]
#beta_Type.Typebb*SEMdata$lon +
XX4 <- beta_Type.Typeaa*SEMdata$lat + 
  beta_Type.Typebb*SEMdata$altitude
  # beta_Type.Typecc*SEMdata$lon

SEMdata$geoxx4 <- XX4
summary(lm(cv ~ lat, SEMdata))
# summary(lm(cv ~ lon, SEMdata))
summary(lm(cv ~ altitude, SEMdata))
#构造土壤复合变量XX5
# SEMdata <- subset(SEMdata,SEMdata$soc>0)
# SEMdata <- subset(SEMdata,SEMdata$PH>0)
# +soc ocd +
XX5_model <- lm(cv ~ PH+ N+soc , SEMdata)
summary(XX5_model)
summary(XX5_model)$coefficients
# table2 <- summary(XX5_model)$coefficients
# write.csv(table2,file="O:\\china_biomass\\表格\\table2_soil.csv",row.names = FALSE)

#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX5_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX5_model)$coefficients[3, 1]
beta_Type.Typecc <-  summary(XX5_model)$coefficients[4, 1]
# beta_Type.Typedd <-  summary(XX5_model)$coefficients[5, 1]

#beta_Type.Typecc*SEMdata$ocd + +beta_Type.Typecc*SEMdata$PH
XX5 <- beta_Type.Typeaa*SEMdata$PH + beta_Type.Typebb*SEMdata$N+
  beta_Type.Typecc*SEMdata$soc
SEMdata$soilXX5 <- XX5
summary(lm(cv ~ PH, SEMdata))
summary(lm(cv ~ N, SEMdata))
summary(lm(cv ~ soc, SEMdata))

#构造气候复合变量XX6
# SEMdata <- subset(SEMdata,SEMdata$temp_std>0)
# SEMdata <- subset(SEMdata,SEMdata$pre_mean>0)
# SEMdata[,42] <- SEMdata$temp_mean/SEMdata$temp_std
# SEMdata[,43] <- SEMdata$pre_mean/SEMdata$pre_std
# colnames(SEMdata)[c(42,43)] <- c("temp_sta","pre_sta")
#temp_mean + + pre_mean   temp_std + pre_std   +temp_std
XX6_model <- lm(cv ~ temp_mean +pre_mean, SEMdata)
# XX6_model <- lm(cv ~ temp_sta +pre_sta, SEMdata)
summary(XX6_model)
summary(XX6_model)$coefficients
# table3 <- summary(XX6_model)$coefficients
# write.csv(table3,file="O:\\china_biomass\\表格\\table3_climate.csv",row.names = FALSE)

#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX6_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX6_model)$coefficients[3, 1]
# beta_Type.Typecc <-  summary(XX6_model)$coefficients[4, 1]
# beta_Type.Typedd <-  summary(XX6_model)$coefficients[5, 1]

#beta_Type.Typecc*SEMdata$pre_std + beta_Type.Typedd*SEMdata$pre_mean
XX6 <- beta_Type.Typeaa*SEMdata$temp_mean + beta_Type.Typebb*SEMdata$pre_mean
  # beta_Type.Typecc*SEMdata$temp_std
# XX6 <- beta_Type.Typeaa*SEMdata$temp_sta + beta_Type.Typebb*SEMdata$pre_sta
SEMdata$weatherXX6 <- XX6
summary(lm(cv ~ temp_mean, SEMdata))
summary(lm(cv ~ pre_mean, SEMdata))
# summary(lm(cv ~ temp_std, SEMdata))
# summary(lm(cv ~ pre_std, SEMdata))
# write.csv(SEMdata,file="J:\\china_biomass\\结构方程\\SEMdata_finaldata.csv",row.names = FALSE)
# # ##############
#构造气候变异复合变量XX8
#temp_mean + + pre_mean   temp_std + pre_std   +temp_std
XX8_model <- lm(cv ~ temp_std+pre_std, SEMdata)
summary(XX8_model)
summary(XX8_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX6_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX6_model)$coefficients[3, 1]
#beta_Type.Typecc*SEMdata$pre_std + beta_Type.Typedd*SEMdata$pre_mean
XX8 <- beta_Type.Typeaa*SEMdata$temp_std + beta_Type.Typebb*SEMdata$pre_std
SEMdata$weather_sdXX8 <- XX8
summary(lm(cv ~ temp_std, SEMdata))
summary(lm(cv ~ pre_std, SEMdata))
#读标准化数据####
SEMdata <- read.csv("O:\\china_biomass\\结构方程\\SEMdata_finaldata.csv")
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
#加入汇水指数
huiwater <- read.xlsx("O:\\china_biomass\\二次订正-土壤类型.xlsx",sheet = 2)
huiwater <- huiwater[,c(1,39)]
colnames(huiwater)[1] <- "ID"
SEMdata <- left_join(SEMdata,huiwater,by="ID")
SEMdata$汇水指数[SEMdata$汇水指数<0] = NA
#加入gracewater
gracewater <- read.csv("O:\\china_biomass\\gracewater_2013_2017.csv")
colnames(gracewater)[2] <- 'gracewater'
SEMdata <- left_join(SEMdata,gracewater,by="ID")

#构造水复合变量XX7 
XX7_model <- lm(cv ~ waterxx1 + 汇水指数, SEMdata)
summary(XX7_model)
summary(XX7_model)$coefficients
#提取系数以生成因子分数，来预测目标变量
beta_Type.Typeaa<-  summary(XX7_model)$coefficients[2, 1]
beta_Type.Typebb<-  summary(XX7_model)$coefficients[3, 1]
XX7 <- beta_Type.Typeaa*SEMdata$waterxx1 + 
  beta_Type.Typebb*SEMdata$汇水指数

SEMdata$mwaterxx7 <- XX7
summary(lm(cv ~ waterxx1, SEMdata))
summary(lm(cv ~ 汇水指数, SEMdata))
####构建SEM####
#new water level
SEMdata$watertype[SEMdata$watertype=="aa"&SEMdata$湿地区!="东部滨海区"] = "bb"
# SEMdata <- subset(SEMdata,SEMdata$soc>0)
# SEMdata <- subset(SEMdata,SEMdata$N>0)
# SEMdata <- subset(SEMdata,SEMdata$temp_std>0)
# SEMdata <- subset(SEMdata,SEMdata$pre_mean>0)

# #用lavaan试试  (1|湿地区_x) 
# model <- '
#         Zcv ~ ZBiodiversi + Zplantxx3 ++Zwaterxx1+Zgeoxx4 + ZsoilXX5
#         ZBiodiversi ~ Zplantxx3 +Zwaterxx1+Zgeoxx4 + ZsoilXX5
#         # ZsoilXX5 ~ Zgeoxx4
#         # Zwaterxx1 ~Zgeoxx4
#         Zplantxx3 ~Zwaterxx1+Zgeoxx4 + ZsoilXX5
#         # # 两边相同，表示该变量的方差，不同的话表示两者的协方差
#         Zwaterxx1 ~~ ZsoilXX5+Zgeoxx4'
# # 然后拟合cfa函数，第一个参数是模型，第二个参数是数据集
# fit <- cfa(model, data = SEMdata)
# # 再通过summary函数给出结果
# summary(fit, fit.measure = TRUE)
# fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
# p2 = semPaths(fit, what = "stand", layout = "tree2", fade=F,style = "lisrel",
#               nCharNodes = 0,edge.label.cex = 1,esize  = 3,nDigits = 3)

#glm lmer +  family="gaussian",  AGB +   waterxx1 +   (1|watertype) + 
# +ZweatherXX6 (1|湿地区_x) +
# SEMdata1 <- subset(SEMdata,SEMdata$watertype=="aa")
# NG.piecewise <- psem(  #有水文
#   glm(cv ~ Biodiversi+plantxx3 +soilXX5+
#          geoxx4,family="gaussian",data = SEMdata),
#   glm(Biodiversi ~ plantxx3+geoxx4+waterxx1,
#       family="gaussian",data = SEMdata),
#   glm(plantxx3 ~waterxx1+geoxx4+soilXX5,
#       family="gaussian",data = SEMdata),
#   glm(waterxx1 ~ weatherXX6+geoxx4,
#       family="gaussian",data = SEMdata),
#   glm(soilXX5 ~ geoxx4+waterxx1,
#       family="gaussian",data = SEMdata)
# )
SEMdatabf <- SEMdata
SEMdata <- SEMdatabf
#全部变量log
SEMdata <- SEMdata[,c(20:23,26:28,30:35,37:42)]
SEMdata1 <- decostand(SEMdata[,c(1:12,18)], "log")
SEMdata <- cbind(SEMdata[,c(13:17,19)],SEMdata1)
#复合变量log
SEMdata <- SEMdata[,c(20,26,35,37:42)]
SEMdata1 <- decostand(SEMdata[,c(1,2,9)], "hellinger")#hellinger  log
SEMdata <- cbind(SEMdata[,c(3:7,9)],SEMdata1)
#所有
SEMdatabf1 <- SEMdata
SEMdata <- SEMdatabf1
SEMdata <- SEMdata[,c(20,26,35,37:42)]
SEMdata <- decostand(SEMdata, "log")#hellinger  log
# SEMdata <- cbind(SEMdata[,c(3:7,9)],SEMdata1)

# NG.piecewise <- psem( #拟合成功 三个气象因子
#   glm(cv ~ Biodiversi +soilXX5+gracewater+weatherXX6+geoxx4,
#         family="gaussian",data = SEMdata),
#   glm(Biodiversi ~ plantxx3+gracewater+geoxx4,
#       family="gaussian",data = SEMdata),
#   glm(plantxx3 ~ soilXX5+weatherXX6+geoxx4,
#       family="gaussian",data = SEMdata),
#   glm(gracewater ~ weatherXX6,
#       family="gaussian",data = SEMdata),
#   glm(soilXX5 ~ geoxx4+weatherXX6+gracewater,
#       family="gaussian",data = SEMdata),
#   glm(weatherXX6 ~ geoxx4,
#       family="gaussian",data = SEMdata)
# )
# summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
# plot(NG.piecewise)

NG.piecewise <- psem( #考虑气候变异
  glm(cv ~ Biodiversi+plantxx3 +soilXX5+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata),
  glm(Biodiversi ~ plantxx3+weatherXX6+geoxx4+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata),
  glm(plantxx3 ~ soilXX5+weatherXX6+weather_sdXX8+geoxx4,
      family="gaussian",data = SEMdata),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata),
  glm(soilXX5 ~ geoxx4+weatherXX6+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata)
  # glm(weather_sdXX8 ~ geoxx4,
  #     family="gaussian",data = SEMdata),
  # glm(weatherXX6 ~ geoxx4+weather_sdXX8,
  #     family="gaussian",data = SEMdata)
)
# cerror(weather_sdXX8 %~~% weatherXX6, NG.piecewise, SEMdata)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)
#temp_sta
NG.piecewise <- psem( #没有水文
  glm(cv ~ Biodiversi+plantxx3 +soilXX5+geoxx4,
      family="gaussian",data = SEMdata),
  glm(Biodiversi ~ plantxx3+weatherXX6+soilXX5,
      family="gaussian",data = SEMdata),
  glm(plantxx3 ~ soilXX5+weatherXX6,
      family="gaussian",data = SEMdata),
  glm(mwaterxx7 ~ weatherXX6,
      family="gaussian",data = SEMdata),
  glm(soilXX5 ~ geoxx4+weatherXX6+mwaterxx7,
      family="gaussian",data = SEMdata),
  glm(weatherXX6 ~ geoxx4,
      family="gaussian",data = SEMdata)
)
# cerror(weather_sdXX8 %~~% weatherXX6, NG.piecewise, SEMdata)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)
#log
NG.piecewise <- psem( #考虑气候变异
  glm(cv ~ Biodiversi+plantxx3 +soilXX5+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata),
  glm(Biodiversi ~ plantxx3+weatherXX6+geoxx4+gracewater+weather_sdXX8+soilXX5,
      family="gaussian",data = SEMdata),
  glm(plantxx3 ~ soilXX5+weatherXX6+geoxx4,
      family="gaussian",data = SEMdata),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata),
  glm(soilXX5 ~ geoxx4+weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata)
  # glm(weather_sdXX8 ~ geoxx4,
  #     family="gaussian",data = SEMdata),
  # glm(weatherXX6 ~ geoxx4+weather_sdXX8,
  #     family="gaussian",data = SEMdata)
)
# cerror(weather_sdXX8 %~~% weatherXX6, NG.piecewise, SEMdata)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)
#降水比率
NG.piecewise <- psem( 
  glm(cv ~ Biodiversi+plantxx3 +soilXX5+gracewater+weatherXX6,
      family="gaussian",data = SEMdata),
  glm(Biodiversi ~ plantxx3+weatherXX6+gracewater+soilXX5,
      family="gaussian",data = SEMdata),
  glm(plantxx3 ~ soilXX5+weatherXX6+gracewater,
      family="gaussian",data = SEMdata),
  glm(gracewater ~ geoxx4+weatherXX6,
      family="gaussian",data = SEMdata),
  glm(soilXX5 ~ geoxx4+weatherXX6+gracewater,
      family="gaussian",data = SEMdata),
  glm(weatherXX6 ~ geoxx4,
      family="gaussian",data = SEMdata)
)
# cerror(weather_sdXX8 %~~% weatherXX6, NG.piecewise, SEMdata)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)
# #汇水指数
# NG.piecewise <- psem( #没有水文
#   glm(cv ~ Biodiversi+plantxx3 +soilXX5+
#         geoxx4,family="gaussian",data = SEMdata),
#   glm(Biodiversi ~ plantxx3+weatherXX6+geoxx4,
#       family="gaussian",data = SEMdata),
#   glm(plantxx3 ~ soilXX5+waterxx1+weatherXX6,
#       family="gaussian",data = SEMdata),
#   glm(waterxx1 ~ geoxx4,
#       family="gaussian",data = SEMdata),
#   glm(汇水指数 ~ weatherXX6+waterxx1,
#       family="gaussian",data = SEMdata),
#   glm(soilXX5 ~ geoxx4+weatherXX6+waterxx1,
#       family="gaussian",data = SEMdata),
#   glm(weatherXX6 ~ geoxx4,
#       family="gaussian",data = SEMdata)
# )
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
# #全部指数
# NG.piecewise <- psem( #没有水文
#   glm(cv ~ Biodiversi+plantxx3+soc+PH+N+temp_std+temp_mean+pre_std+pre_mean+
#       lat+altitude+waterxx1,family="gaussian",data = SEMdata),
#   glm(Biodiversi ~ plantxx3+soc+PH+N+temp_std+temp_mean+pre_std+pre_mean+
#         lat+altitude+waterxx1,family="gaussian",data = SEMdata),
#   glm(plantxx3 ~ soc+PH+N+waterxx1+temp_std+temp_mean+
#         pre_std+pre_mean+lat+altitude,
#       family="gaussian",data = SEMdata),
#   glm(waterxx1 ~ temp_std+temp_mean+
#         pre_std+pre_mean+lat+altitude,
#       family="gaussian",data = SEMdata),
#   # glm(汇水指数 ~ weatherXX6+waterxx1,
#   #     family="gaussian",data = SEMdata),
#   glm(soc ~ temp_std+temp_mean+pre_std+pre_mean+lat+altitude+waterxx1,
#       family="gaussian",data = SEMdata),
#   glm(PH ~ temp_std+temp_mean+pre_std+pre_mean+lat+altitude+waterxx1,
#       family="gaussian",data = SEMdata),
#   glm(N ~ temp_std+temp_mean+pre_std+pre_mean+lat+altitude+waterxx1,
#       family="gaussian",data = SEMdata)
#   # glm(weatherXX6 ~ lat+altitude,
#   #     family="gaussian",data = SEMdata)
# )
# cerror(weatherXX6 %~~% geoxx4, NG.piecewise, SEMdata)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
summary(NG.piecewise, test.statistic = "T",test.type= "III") #显示标准化参数
summary(NG.piecewise, std.nox = TRUE, rsquare = TRUE)
piecewiseSEM::AIC_psem(NG.piecewise)
piecewiseSEM::stdCoefs(NG.piecewise)
piecewiseSEM::dSep(NG.piecewise)

codata <- summary(NG.piecewise,fit.measures=TRUE) #.progressBar=FLASE
codata <- codata$coefficients
write.xlsx(codata,file="O:\\china_biomass\\结构方程\\SEMdata_coefficients516.xlsx")

summarySEM(NG.piecewise)
coefs(NG.piecewise)
plot(NG.piecewise)
plot(NG.piecewise, node_attrs = list(shape = "rectangle", color = "black",
                                     fillcolor = "orange"))
####lavvan####
lavmod <- '
cv~Biodiversi+plantxx3 +soilXX5+geoxx4
Biodiversi ~ plantxx3+geoxx4+weatherXX6
plantxx3 ~ soilXX5+waterxx1+weatherXX6
waterxx1 ~ geoxx4+weatherXX6
soilXX5 ~ geoxx4+weatherXX6+waterxx1
weatherXX6 ~ geoxx4
'
lavtest <- lavaan::sem(lavmod,data=SEMdata)
summary(lavtest,fit.measures=T)
inspect(lavtest,"r2")
standardizedsolution(lavtest)
####路径效应####
# effectdata <- read.xlsx("O:\\china_biomass\\结构方程\\路径效应_sum.xlsx",sheet=1)
effectdata <- read.xlsx("O:\\china_biomass\\结构方程\\路径效应变异814.xlsx",sheet=2)
brewer.pal.info
sfcolor <- brewer.pal(name="RdYlGn",11)#PiYG  RdBu
sfcolor <- c("#BC80BD","#FCCDE5","#80B1D3","#BEBADA","#8DD3C7","#8DD3C7",
             "#FDB462","#B3DE69","#CCEBC5","#A6D96A","#FB8072" )
# imporplot[,1] <- imporplot[,1]*100
p <- ggplot(imporplot) +　
  aes(x = X1, y = importance, fill = Name) +
  geom_col() +
  xlab("Variables") +
  ylab("Relative importance (%)") +
  theme_gray()+
  # scale_x_continuous("Explains (%)",breaks=c(0,10)) +
  # scale_fill_hue(direction = 1) +
  scale_fill_manual(values = sfcolor)+
  coord_flip() +
  guides(shape=guide_legend(title=NULL),color=guide_legend(title=NULL),
         fill=F)+
  geom_text(aes(y = importance, label = sprintf("%.1f",importance)),
            vjust = 0.5,hjust=2, family = "serif",size = 4.5)+
  theme(axis.title.x = element_text(family = "serif",size = 20),
        axis.text.x = element_text(family = "serif",size = 15),
        axis.title.y = element_text(family = "serif",size = 20),
        axis.text.y = element_text(family = "serif",size = 15))+
  annotate("text", x = 1.5, y = 10, family="serif",size = 5.5,
           label = "Variance explained: 29.2%",colour="black")
p

library(esquisse)
esquisser(effectdata)

####分气候区####
###温带湿润半湿润区####
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
unique(SEMdata$湿地区)
SEMdata1 <- subset(SEMdata,SEMdata$湿地区=='温带湿润半湿润区')
NG.piecewise <- psem( #考虑气候变异
  glm(cv ~ soilXX5,
      family="gaussian",data = SEMdata1),
  glm(Biodiversi ~ plantxx3+weather_sdXX8,
      family="gaussian",data = SEMdata1),
  glm(plantxx3 ~ soilXX5+weatherXX6+geoxx4,
      family="gaussian",data = SEMdata1),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata1),
  glm(soilXX5 ~ weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata1)
)
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)
# NG.piecewise <- psem( #没有水文
#   glm(cv ~ Biodiversi+plantxx3 +soilXX5+
#         geoxx4,family="gaussian",data = SEMdata1),
#   glm(Biodiversi ~ plantxx3+geoxx4+weatherXX6,
#       family="gaussian",data = SEMdata1),
#   glm(plantxx3 ~ soilXX5+waterxx1+weatherXX6,
#       family="gaussian",data = SEMdata1),
#   glm(waterxx1 ~ geoxx4+weatherXX6,
#       family="gaussian",data = SEMdata1),
#   glm(soilXX5 ~ geoxx4+weatherXX6+waterxx1,
#       family="gaussian",data = SEMdata1),
#   glm(weatherXX6 ~ geoxx4,
#       family="gaussian",data = SEMdata1)
# )
summary(NG.piecewise,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise)

summary(lm(cv ~ lat, SEMdata1))
summary(lm(cv ~ 海拔, SEMdata1))
summary(lm(cv ~ PH, SEMdata1))
summary(lm(cv ~ N, SEMdata1))
summary(lm(cv ~ soc, SEMdata1))
summary(lm(cv ~ temp_mean, SEMdata1))
summary(lm(cv ~ pre_mean, SEMdata1))
summary(lm(cv ~ pre_std, SEMdata1))
summary(lm(cv ~ temp_std, SEMdata1))
###东部滨海区####
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
unique(SEMdata$湿地区)
SEMdata2 <- subset(SEMdata,SEMdata$湿地区=='东部滨海区')
NG.piecewise2 <- psem( #考虑气候变异
  glm(cv ~ Biodiversi+plantxx3 +soilXX5+geoxx4+
        gracewater+weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata2),
  glm(Biodiversi ~ plantxx3 +soilXX5+geoxx4+
        gracewater+weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata2),
  glm(plantxx3 ~ soilXX5+weatherXX6+geoxx4,
      family="gaussian",data = SEMdata2),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata2),
  glm(soilXX5 ~ weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata2)
)
summary(NG.piecewise2,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise2)

###热带亚热带湿润区####
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
unique(SEMdata$湿地区)
SEMdata3 <- subset(SEMdata,SEMdata$湿地区=='热带亚热带湿润区')
NG.piecewise3 <- psem( #没有水文
  glm(cv ~ Biodiversi+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata3),
  glm(Biodiversi ~ gracewater+weatherXX6+weather_sdXX8,
      family="gaussian",data = SEMdata3),
  glm(plantxx3 ~ geoxx4+gracewater,
      family="gaussian",data = SEMdata3),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata3),
  glm(soilXX5 ~ weatherXX6+gracewater,
      family="gaussian",data = SEMdata3)
)
summary(NG.piecewise3,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise3)
summary(lm(cv ~ lat, SEMdata3))
summary(lm(cv ~ 海拔, SEMdata3))
summary(lm(cv ~ PH, SEMdata3))
summary(lm(cv ~ N, SEMdata3))
summary(lm(cv ~ soc, SEMdata3))
summary(lm(cv ~ temp_mean, SEMdata3))
summary(lm(cv ~ pre_mean, SEMdata3))
summary(lm(cv ~ temp_std, SEMdata3))
summary(lm(cv ~ pre_std, SEMdata3))
###温带干旱半干旱区####
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
unique(SEMdata$湿地区)
SEMdata4 <- subset(SEMdata,SEMdata$湿地区=='温带干旱半干旱区')
NG.piecewise4 <- psem( #没有水文
  glm(cv ~ soilXX5,
      family="gaussian",data = SEMdata4),
  glm(Biodiversi ~ gracewater+weatherXX6+plantxx3,
      family="gaussian",data = SEMdata4),
  glm(plantxx3 ~ geoxx4+gracewater+soilXX5,
      family="gaussian",data = SEMdata4),
  glm(gracewater ~ geoxx4+weatherXX6,
      family="gaussian",data = SEMdata4),
  glm(soilXX5 ~ weatherXX6+gracewater+weather_sdXX8,
      family="gaussian",data = SEMdata4)
)
summary(NG.piecewise4,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise4)

summary(lm(cv ~ lat, SEMdata4))
summary(lm(cv ~ 海拔, SEMdata4))
summary(lm(cv ~ PH, SEMdata4))
summary(lm(cv ~ N, SEMdata4))
summary(lm(cv ~ soc, SEMdata4))
summary(lm(cv ~ temp_mean, SEMdata4))
summary(lm(cv ~ pre_mean, SEMdata4))
summary(lm(cv ~ temp_std, SEMdata4))
summary(lm(cv ~ pre_std, SEMdata4))

###西南高原区####
SEMdata <- read.xlsx("O:\\china_biomass\\SEMdata_finaldata425.xlsx",sheet=1)
unique(SEMdata$湿地区)
SEMdata5 <- subset(SEMdata,SEMdata$湿地区=='西南高原区')
NG.piecewise5 <- psem( #没有水文
  glm(cv ~ Biodiversi+gracewater+weather_sdXX8+soilXX5,
      family="gaussian",data = SEMdata5),
  glm(Biodiversi ~ gracewater+weatherXX6+soilXX5,
      family="gaussian",data = SEMdata5),
  glm(plantxx3 ~ geoxx4+gracewater+soilXX5+weather_sdXX8,
      family="gaussian",data = SEMdata5),
  glm(gracewater ~ geoxx4+weather_sdXX8+weatherXX6,
      family="gaussian",data = SEMdata5),
  glm(soilXX5 ~ weatherXX6+gracewater+weather_sdXX8+geoxx4,
      family="gaussian",data = SEMdata5)
)
summary(NG.piecewise5,fit.measures=T) #.progressBar=FLASE
plot(NG.piecewise5)

summary(lm(cv ~ lat, SEMdata5))
summary(lm(cv ~ 海拔, SEMdata5))
summary(lm(cv ~ PH, SEMdata5))
summary(lm(cv ~ N, SEMdata5))
summary(lm(cv ~ soc, SEMdata5))
summary(lm(cv ~ temp_mean, SEMdata5))
summary(lm(cv ~ pre_mean, SEMdata5))
summary(lm(cv ~ temp_std, SEMdata5))
summary(lm(cv ~ pre_std, SEMdata5))



