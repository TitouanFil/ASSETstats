### 1. Packages and data preparation
##  1.1. Packages
library(plyr)
library(dplyr)
library(rstatix)
library(stringr)
library(fmsb)
library(labelled)
library(ggplot2)
library(survey)

##  1.2. Data import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#Cambodia database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HomegardenCambodia_2C <- readRDS("HomegardenCambodia_2.rds")
#Laos database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")
ClowlandLaos_2C <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2C <- readRDS("CuplandLaos_2.rds")
#Vietnam database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")

#We create a data.frame with country, province and village information
for (i in c(1,14,15,18)){
  HouseholdCambodia_2C[,i] <- as.character(HouseholdCambodia_2C[,i])
}
for (i in c(1,14,15,17)){
  HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
}
for (i in c(1,20,23,32)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
}

x <- HouseholdCambodia_2C[,c(1,14,15,18)]
y <- HouseholdLaos_2C[,c(1,14,15,17)]
z <- HouseholdVietnam_2C[,c(1,20,23,32)]
colnames(z) <- c("o9","o4","o5","o8")

TableAEP <- rbind(x,y,z)


##  1.3. Crop area Table preparation

#First we turn upland, lowland and homegarden crops data to wide format (Duplicate alert = Normal,
#we had no time to take care of it yet)
#Lowland
dlowcC <- ClowlandCambodia_2C[,c(2:3,12)]
colnames(dlowcC)[2] <- "o9"
dlowcL <- ClowlandLaos_2C[,c(1:2,11)]
colnames(dlowcL)[2] <- "o9"
dlowcV <- ClowlandVietnam_2C[,c(1:2,11)]
colnames(dlowcV)[2] <- "o9"
dlowc <- rbind(dlowcC, dlowcL, dlowcV)
dlowcwide <- reshape(dlowc, direction = "wide", timevar = "crop1_now", idvar = "o9")
for (i in 2:ncol(dlowcwide)){
  dlowcwide[,i] <- as.numeric(dlowcwide[,i])
}
id <- rbind(HouseholdCambodia_2C[,c(1,14:15)], HouseholdLaos_2C[,c(1,14:15)], HouseholdVietnam_2C[,c(1,14:15)])
id$o4 <- str_replace(id$o4, "1", "Vietnam")
id$o5 <- str_replace(id$o5, "11", "Dien Bien")
id$o5 <- str_replace(id$o5, "14", "Son La")
dlowc <- join(id, dlowcwide, by = "o9")
#upland
dupcC <- CuplandCambodia_2C[,c(2:3,11)]
colnames(dupcC)[2] <- "o9"
dupcL <- CuplandLaos_2C[,c(1:2,10)]
colnames(dupcL)[2] <- "o9"
dupcV <- CuplandVietnam_2C[,c(1:2,10)]
colnames(dupcV)[2] <- "o9"
dupc <- rbind(dupcC, dupcL, dupcV)
dupcwide <- reshape(dupc, direction = "wide", timevar = "crop2_now", idvar = "o9")
for (i in 2:ncol(dupcwide)){
  dupcwide[,i] <- as.numeric(dupcwide[,i])
}
dupc <- join(id, dupcwide, by = "o9")
#We merge the tables
dcrops <- merge(dlowc[,c(1:8)],dupc[,c(1,4:9)], by.x = "o9", all = T)
#We add a column with the total cultivated area for each household
dcrops$TotalArea <- rowSums(dcrops[,4:14], na.rm = T)


### 2. AE scores

##  2.1. Recycling
#Score from 0 to 4

#2.1.1. Do they apply any of the following water conservation practices
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#8 Practices
#0 = Score of 0
#1 = Score =<0.125 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.125 (equivalent to more than 1 practice on all area)
#d12
HouseholdCambodia_2Cf <- HouseholdCambodia_2C[,-c(832:834,849:851,866:868,883:885,900:902,
                                                  917:919,934:936,951:953)]

d12subS <- TableAEP[,c(1:4)]
for (i in c(821,835,849,863,877,891,905,919)){
  d12 <- rbind(HouseholdCambodia_2Cf[,c(1,i:(i+10))],HouseholdLaos_2C[,c(1,(i+10):(i+20))],
               HouseholdVietnam_2C[,c(1,(i+56):(i+66))])
  d12 <- join(dcrops,d12, by = "o9")
  for (k in 4:14){
    d12[,(k+12)] <- as.character(d12[,(k+12)])
    d12[,(k+12)] <- as.numeric(d12[,(k+12)])
    for (j in 1:nrow(d12)){
      d12[j,k+12] <- d12[j,k+12]*d12[j,k]
    }
  }
  d12$AreaPract <- rowSums(d12[,16:26], na.rm = T)
  d12$PractPer100 <- ifelse(d12$TotalArea > 0, d12$AreaPract / d12$TotalArea, 0)
  d12subS <- join(d12subS,d12[,c(1,28)], by = "o9")
}
colnames(d12subS)[5:12] <- c("Rainwater collection/conservation","Greywater recycling",
                             "Ponds (for water conservation)","Terraces building",
                             "Swales digging","Land levelling","Mulching","Other")
#Checking table
for (i in 5:12){
  print(sum(d12subS[,i] > 0))
}
#Checking table
Dumm <- rbind(HouseholdCambodia_2Cf[,c(809:816)], HouseholdLaos_2C[,c(819:826)],
              HouseholdVietnam_2C[,c(865:872)])
for (i in 1:8){
  Dumm[,i] <- as.character(Dumm[,i])
  Dumm[,i] <- as.numeric(Dumm[,i])
  print(sum(Dumm[,i], na.rm = T))
}
#Final % calculation
d12subS$FinalPer <- rowSums(d12subS[,c(5:12)]) / ncol(d12subS[,c(5:12)])
summary(d12subS$FinalPer)
d12subS$Score <- ifelse(d12subS$FinalPer > 0.125, 2, ifelse(d12subS$FinalPer > 0, 1, 0)) 
TableAEP <- join(TableAEP,d12subS[,c(1,14)], by = "o9")
colnames(TableAEP)[5] <- "V1_1"

#2.1.2. Do they apply any of the following maintaining/enhancing soil fertility practices: 
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score =<0.083 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.083 (equivalent to more than 1 practice on all area)
#d17
HouseholdCambodia_2Cc <- HouseholdCambodia_2C[,-c(1131:1133,1148:1150,1165:1167,1182:1184,
                                                  1199:1201,1216:1218,1233:1235,1250:1252,
                                                  1267:1269,1284:1286,1301:1303,1318:1320)]
colnames(HouseholdLaos_2C)[1222] <- "d18_1111"
colnames(HouseholdVietnam_2C)[1268:1278] <- c("d18_1111","d18_1112","d18_1113","d18_1114",
                                              "d18_1115","d18_1116","d18_1117","d18_1118",
                                              "d18_1119","d18_11110","d18_11111")
colnames(HouseholdCambodia_2Cc)[1260:1270] <- c("d18_1111","d18_1112","d18_1113","d18_1114",
                                                "d18_1115","d18_1116","d18_1117","d18_1118",
                                                "d18_1119","d18_11110","d18_11111")

d17subS <- TableAEP[,c(1:4)]
for (i in c(1120,1134,1148,1162,1176,1190,1204,1218,1232,1246,1260,1274)){
  d17 <- rbind(HouseholdCambodia_2Cc[,c(1,i:(i+10))],HouseholdLaos_2C[,c(1,(i-38):(i-28))],
               HouseholdVietnam_2C[,c(1,(i+8):(i+18))])
  d17 <- join(dcrops,d17, by = "o9")
  for (k in 4:14){
    d17[,(k+12)] <- as.character(d17[,(k+12)])
    d17[,(k+12)] <- as.numeric(d17[,(k+12)])
    for (j in 1:nrow(d17)){
      d17[j,k+12] <- d17[j,k+12]*d17[j,k]
    }
  }
  d17$AreaPract <- rowSums(d17[,16:26], na.rm = T)
  d17$PractPer100 <- ifelse(d17$TotalArea > 0, d17$AreaPract / d17$TotalArea, 0)
  d17subS <- join(d17subS,d17[,c(1,28)], by = "o9")
}
colnames(d17subS)[5:16] <- c("Animal manure","Compost (heap)","Bokashi (fermented organic matter)",
                             "Legume-based green manure","Pulses in association and/or rotation with main crop",
                             "Cover crops in association and/or rotation with main crop","Biochar",
                             "Crop residue maintenance","Recycling crop waste","Ramial Wood Chip (RWC) or other wood chips",
                             "Organic agro-industrial waste","Other methods")
#Checking table
for (i in 5:16){
  print(sum(d17subS[,i] > 0))
}
#Final % calculation
d17subS$FinalPer <- rowSums(d17subS[,c(5:16)]) / ncol(d17subS[,c(5:16)])
summary(d17subS$FinalPer)
d17subS$Score <- ifelse(d17subS$FinalPer > 0.083, 2, ifelse(d17subS$FinalPer > 0, 1, 0)) 
TableAEP <- join(TableAEP,d17subS[,c(1,18)], by = "o9")
colnames(TableAEP)[6] <- "V1_2"

#We create the score for the 1st principle
TableAEP$V1 <- TableAEP$V1_1 + TableAEP$V1_2
summary(as.factor(TableAEP$V1))


##  2.2. Input reduction
#Score from 0 to 7

# 2.2.1. Do you use any of the following practices to control weeds in your fields?
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score =<0.066 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.066 (equivalent to more than 1 practice on all area)
#d20
HouseholdCambodia_2Cd <- HouseholdCambodia_2C[,-c(1354:1356,1371:1373,1388:1390,1405:1407,
                                                  1422:1424,1439:1441,1456:1458,1473:1475,
                                                  1490:1492,1507:1509,1524:1526,1541:1543,
                                                  1558:1560,1575:1577,1592:1594)]

d20subS <- TableAEP[,c(1:4)]
d20 <- rbind(HouseholdCambodia_2Cd[,c(1,1343:1353)],HouseholdLaos_2C[,c(1,(1269:1279))])
d20 <- join(dcrops,d20, by = "o9")
for (k in 4:14){
  d20[,(k+12)] <- as.character(d20[,(k+12)])
  d20[,(k+12)] <- as.numeric(d20[,(k+12)])
  for (j in 1:nrow(d20)){
    d20[j,k+12] <- d20[j,k+12]*d20[j,k]
  }
}
d20$AreaPract <- rowSums(d20[,16:26], na.rm = T)
d20$PractPer100 <- ifelse(d20$TotalArea > 0, d20$AreaPract / d20$TotalArea, 0)
d20subS <- join(d20subS,d20[,c(1,28)], by = "o9")
for (i in c(1357,1371,1385,1399,1413,1427,1441,1455,1469,1483,1497,1514,1525,1539)){
  d20 <- rbind(HouseholdCambodia_2Cd[,c(1,i:(i+10))],HouseholdLaos_2C[,c(1,(i-74):(i-64))],
               HouseholdVietnam_2C[,c(1,(i-41):(i-31))])
  d20 <- join(dcrops,d20, by = "o9")
  for (k in 4:14){
    d20[,(k+12)] <- as.character(d20[,(k+12)])
    d20[,(k+12)] <- as.numeric(d20[,(k+12)])
    for (j in 1:nrow(d20)){
      d20[j,k+12] <- d20[j,k+12]*d20[j,k]
    }
  }
  d20$AreaPract <- rowSums(d20[,16:26], na.rm = T)
  d20$PractPer100 <- ifelse(d20$TotalArea > 0, d20$AreaPract / d20$TotalArea, 0)
  d20subS <- join(d20subS,d20[,c(1,28)], by = "o9")
}
colnames(d20subS)[5:19] <- c("Crop rotation / intercropping","Cover crops","Mulching / shading",
                             "Sowing date / rate / depth","Crop spatial arrangement","Seed cleaning before sowing",
                             "Cultivar choice","Crop mixtures","Nutrient placement","Patch/ban spraying",
                             "Bioherbicide","Mowing / slashing","Grazing",
                             "Post harvest weed seed destruction in field","Any other methods")
#Checking table
for (i in 5:16){
  print(sum(d20subS[,i] > 0))
}
#Checking table
Dumm <- rbind(HouseholdCambodia_2C[,c(1326:1340)], HouseholdLaos_2C[,c(1252:1266)],
              HouseholdVietnam_2C[,c(1298:1312)])
for (i in 1:15){
  Dumm[,i] <- as.character(Dumm[,i])
  Dumm[,i] <- as.numeric(Dumm[,i])
  print(sum(Dumm[,i], na.rm = T))
}
#Final % calculation
d20subS$FinalPer <- rowSums(d20subS[,c(5:19)]) / ncol(d20subS[,c(5:19)])
summary(d20subS$FinalPer)
d20subS$Score <- ifelse(d20subS$FinalPer > 0.066, 2, ifelse(d20subS$FinalPer > 0, 1, 0)) 
TableAEP <- join(TableAEP,d20subS[,c(1,21)], by = "o9")
colnames(TableAEP)[8] <- "V2_1"


# 2.2.2. Do you use any of the following practices to control pests and disease in your fields?
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score =<0.066 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.066 (equivalent to more than 1 practice on all area)
#d27
HouseholdCambodia_2Ce <- HouseholdCambodia_2C[,-c(1616,1632:1634,1649:1651,1666:1668,
                                                  1683:1685,1700:1702,1717:1719,1734:1736,
                                                  1751:1753,1768:1770,1785:1787,1802:1804,
                                                  1819:1821,1836:1838,1841:1857,1870:1872,
                                                  1887:1889)]
colnames(HouseholdCambodia_2Ce)[1760:1770] <- c("d27_1111","d27_1112","d27_1113","d27_1114",
                                                "d27_1115","d27_1116","d27_1117","d27_1118",
                                                "d27_1119","d27_11110","d27_11111")
colnames(HouseholdCambodia_2Ce)[1802:1812] <- c("d27_1411","d27_1412","d27_1413","d27_1414",
                                                "d27_1415","d27_1416","d27_1417","d27_1418",
                                                "d27_1419","d27_14110","d27_14111")
colnames(HouseholdLaos_2C)[1641] <- c("d27_1111")
colnames(HouseholdVietnam_2C)[1674] <- c("d27_1111")
d27subS <- TableAEP[,c(1:4)]
for (i in c(1620,1634,1648,1662,1676,1690,1704,1718,1732,1746,1760,1774,1788,1802,
            1816)){
  d27 <- rbind(HouseholdCambodia_2Ce[,c(1,i:(i+10))],HouseholdLaos_2C[,c(1,(i-119):(i-109))],
               HouseholdVietnam_2C[,c(1,(i-86):(i-76))])
  d27 <- join(dcrops,d27, by = "o9")
  for (k in 4:14){
    d27[,(k+12)] <- as.character(d27[,(k+12)])
    d27[,(k+12)] <- as.numeric(d27[,(k+12)])
    for (j in 1:nrow(d27)){
      d27[j,k+12] <- d27[j,k+12]*d27[j,k]
    }
  }
  d27$AreaPract <- rowSums(d27[,16:26], na.rm = T)
  d27$PractPer100 <- ifelse(d27$TotalArea > 0, d27$AreaPract / d27$TotalArea, 0)
  d27subS <- join(d27subS,d27[,c(1,28)], by = "o9")
}
colnames(d27subS)[5:19] <- c("Crop rotation / intercropping","Flower strips","Hedgerows",
                             "Soil health maintenance/improvement","Sanitation practices (removal of damaged/infected plants and fruits)",
                             "Planting date","Water and nutrient management", "Cultivar choice (tolerant/resistant) / cultivar mixture",
                             "Biopesticide / organic pesticide", "Commercial biological control agents (BCAs)",
                             "Home-made efficient microorganism (EM)","Commercial efficient microorganism (EM)",
                             "Pheromone traps","Protein baits","Any other methods")
#Checking table
for (i in 5:19){
  print(sum(d27subS[,i] > 0))
}
#Checking table
Dumm <- rbind(HouseholdCambodia_2Ce[,c(1603:1615)], HouseholdLaos_2C[,c(1484:1496)],
              HouseholdVietnam_2C[,c(1517:1529)])
for (i in 1:13){
  Dumm[,i] <- as.character(Dumm[,i])
  Dumm[,i] <- as.numeric(Dumm[,i])
  print(sum(Dumm[,i], na.rm = T))
}
#Final % calculation
d27subS$FinalPer <- rowSums(d27subS[,c(5:19)]) / ncol(d27subS[,c(5:19)])
summary(d27subS$FinalPer)
d27subS$Score <- ifelse(d27subS$FinalPer > 0.066, 2, ifelse(d27subS$FinalPer > 0, 1, 0)) 
TableAEP <- join(TableAEP,d27subS[,c(1,21)], by = "o9")
colnames(TableAEP)[9] <- "V2_2"


# 2.2.3. Do they apply any of the following maintaining/enhancing soil fertility practices: 
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score =<0.083 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.083 (equivalent to more than 1 practice on all area)
#
TableAEP <- join(TableAEP,d17subS[,c(1,18)], by = "o9")
colnames(TableAEP)[10] <- "V2_3"

# 2.2.4. Do you supply any of your animals with concentrate from the farm (maize meal, paddy rice bran) 
#e25_11 - e39_11 - e53_11 
#Yes = 1 / No = 0 for each animal
Supply <- rbind(HouseholdCambodia_2C[,c(1,2190,2257,2324)], HouseholdLaos_2C[,c(1,1984,2048,2107)],
                HouseholdVietnam_2C[,c(1,2016,2077,2138)])
for (i in 2:ncol(Supply)){
  Supply[,i] <- as.character(Supply[,i])
  Supply[,i] <- as.numeric(Supply[,i])
}
Supply$Score <- rowSums(Supply[2:4], na.rm = T)
TableAEP <- join(TableAEP,Supply[,c(1,5)], by = "o9")
colnames(TableAEP)[11] <- "V2_4"

#We create the score for the 2nd principle
TableAEP$V2 <- TableAEP$V2_1 + TableAEP$V2_2 + TableAEP$V2_3 + TableAEP$V2_4
summary(as.factor(TableAEP$V2))


## 2.3. Soil heath
#Score from 0 to 4

#2.3.1. Do you use any of the following soil conservation practices ? 
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score <0.125 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.125 (equivalent to equal or less than 1 practice on all area)
#d14
HouseholdCambodia_2Cb <- HouseholdCambodia_2C[,-c(979:981,996:998,1013:1015,1030:1032,1047:1049,
                                                  1064:1066,1081:1083,1098:1100)]
d14subS <- TableAEP[,c(1:4)]
for (i in c(968,982,996,1010,1024,1038,1052,1066)){
  d14 <- rbind(HouseholdCambodia_2Cb[,c(1,i:(i+10))],HouseholdLaos_2C[,c(1,(i-14):(i-4))],
               HouseholdVietnam_2C[,c(1,(i+32):(i+42))])
  d14 <- join(dcrops,d14, by = "o9")
  for (k in 4:14){
    d14[,(k+12)] <- as.character(d14[,(k+12)])
    d14[,(k+12)] <- as.numeric(d14[,(k+12)])
    for (j in 1:nrow(d14)){
      d14[j,k+12] <- d14[j,k+12]*d14[j,k]
    }
  }
  d14$AreaPract <- rowSums(d14[,16:26], na.rm = T)
  d14$PractPer100 <- ifelse(d14$TotalArea > 0, d14$AreaPract / d14$TotalArea, 0)
  d14subS <- join(d14subS,d14[,c(1,28)], by = "o9")
}
colnames(d14subS)[5:12] <- c("Sowing in contour lines", "Natural or planted grass strips",
                             "Trees conservation in agricultural plots","Agroforestry (trees + crops)",
                             "Crop residues maintained to cover the soil","Use of cover crops",
                             "Reduced to no-tillage","Other")
for (i in 5:12){
  print(sum(d14subS[,i] > 0))
}
#Checking table
Dumm <- rbind(HouseholdCambodia_2C[,c(958:965)], HouseholdLaos_2C[,c(944:951)],
              HouseholdVietnam_2C[,c(990:997)])
for (i in 1:8){
  Dumm[,i] <- as.character(Dumm[,i])
  Dumm[,i] <- as.numeric(Dumm[,i])
  print(sum(Dumm[,i], na.rm = T))
}
#Final % calculation
d14subS$FinalPer <- rowSums(d14subS[,c(5:12)]) / ncol(d14subS[,c(5:12)])
summary(d14subS$FinalPer)
d14subS$Score <- ifelse(d14subS$FinalPer > 0.125, 2, ifelse(d14subS$FinalPer > 0, 1, 0)) 
TableAEP <- join(TableAEP,d14subS[,c(1,14)], by = "o9")
colnames(TableAEP)[13] <- "V3_1"


#2.3.2. Do they apply any of the following maintaining/enhancing soil fertility practices: 
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#0 = Score of 0
#1 = Score =<0.083 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.083 (equivalent to more than 1 practice on all area)
#d17
TableAEP <- join(TableAEP,d17subS[,c(1,18)], by = "o9")
colnames(TableAEP)[14] <- "V3_2"

#We create the score for the 3rd principle
TableAEP$V3 <- TableAEP$V3_1 + TableAEP$V3_2
summary(as.factor(TableAEP$V3))


## 2.4. Animal Health
#Score from 0 to 3

#2.4.1. Is there any month in the year when there is a lack of feed for the animals?  
#Yes = 0 / No = 1
#e59
LackFeed <- rbind(HouseholdCambodia_2C[,c(1,2363)],
                  HouseholdLaos_2C[,c(1,2145)],
                  HouseholdVietnam_2C[,c(1,2179)])
LackFeed$e59 <- str_replace_all(LackFeed$e59,"Yes","1")
LackFeed$e59 <- str_replace_all(LackFeed$e59,"No","0")
LackFeed$e59 <- as.numeric(LackFeed$e59)
LackFeed$Answer <- ifelse(LackFeed$e59 == 1, 0, 1)
TableAEP <- join(TableAEP,LackFeed[c(1,3)], by = "o9")
colnames(TableAEP)[16] <- "V4_1"

#2.4.2. Do the animals have access to water? 
#Yes = 1 / No = 0
#e60
WaterAn <- rbind(HouseholdCambodia_2C[,c(1,2377)],
                 HouseholdLaos_2C[,c(1,2159)],
                 HouseholdVietnam_2C[,c(1,2193)])
WaterAn$e60 <- str_replace_all(WaterAn$e60,"Yes","1")
WaterAn$e60 <- str_replace_all(WaterAn$e60,"No","0")
WaterAn$e60 <- as.numeric(WaterAn$e60)
TableAEP <- join(TableAEP,WaterAn, by = "o9")
colnames(TableAEP)[17] <- "V4_2"

#2.4.3. Do you have problems with Cattle/Buffalo (any) parasites and if so do you
#treat them with traditional treatments? 
#Yes = 1 / No = 0
#e27
TradiTreat <- rbind(HouseholdCambodia_2C[,c(1,2198)],
                    HouseholdLaos_2C[,c(1,1997)],
                    HouseholdVietnam_2C[,c(1,2025)])
TradiTreat$e271 <- as.numeric(TradiTreat$e271)
TradiTreat$e271 <- ifelse(TradiTreat$e271 == 2, 1, 0)
TableAEP <- join(TableAEP,TradiTreat, by = "o9")
colnames(TableAEP)[18] <- "V4_3"

#We create the score for the 4th principle
TableAEP$V4 <- rowSums(TableAEP[,c(16:18)], na.rm = T)
summary(as.factor(TableAEP$V4))

##  2.5. Biodiversity
#Score from 0 to 8

#2.5.1. Index based on the number of NTFP collected
# 0 = 0 / 1 to 3 = 1 / 4 or more = 2
#d7
x <- HouseholdCambodia_2C[,c(1,710:724)]
for (i in 2:16){
  x[,i] <- as.numeric(x[,i])
  x[,i] <- ifelse(x[,i] == 2, 1, 0)
}
x$Sum <- rowSums(x[2:16])
y <- HouseholdLaos_2C[,c(1,720:735)]
for (i in 2:17){
  y[,i] <- as.numeric(y[,i])
  y[,i] <- ifelse(y[,i] == 2, 1, 0)
}
y$Sum <- rowSums(y[2:17])
z <- HouseholdVietnam_2C[,c(1,763:778)]
for (i in 2:17){
  z[,i] <- as.numeric(z[,i])
  z[,i] <- ifelse(z[,i] == 2, 1, 0)
}
z$Sum <- rowSums(z[2:17])
NTFP <- rbind(x[,c(1,17)], y[,c(1,18)], z[,c(1,18)])
NTFP$Score <- ifelse(NTFP$Sum < 1, 0, ifelse(NTFP$Sum < 4, 1, 2))
TableAEP <- join(TableAEP,NTFP[c(1,3)], by = "o9")
colnames(TableAEP)[20] <- "V5_1"

#2.5.2. Index by terciles for the sum of plant species that the household raised
# 0-1 = 0 ; 2-3 = 1 ; 4 or more = 2
#First tercile = low biodiversity ((= 0 or 1 crop incl. all the different types of crops) 
#Third tercile = high 
dlowc2 <- dlowc
for (i in 4:ncol(dlowc2)){
  dlowc2[,i] <- ifelse(dlowc2[,i] > 1,1, dlowc2[,i])
}
dlowc2$Sum <- rowSums(dlowc2[4:14], na.rm = T)
dupc2 <- dupc
for (i in 4:ncol(dupc2)){
  dupc2[,i] <- ifelse(dupc2[,i] > 1,1, dupc2[,i])
}
dupc2$Sum <- rowSums(dupc2[4:11], na.rm = T)
Cspecies <- join(dlowc2[,c(1,15)], dupc2[,c(1,12)], by = "o9")
Cspecies$SumT <- rowSums(Cspecies[2:3], na.rm = T)
Cspecies$Score <- ifelse(Cspecies$SumT < 2, 0, ifelse(Cspecies$SumT < 4, 1, 2))
TableAEP <- join(TableAEP,Cspecies[c(1,5)], by = "o9")
colnames(TableAEP)[21] <- "V5_2"

#2.5.3. If they have animals: Generate an index by quartiles for the sum of animal species
#that the household raised in the last 12 months
#e2
AnimalN <- rbind(HouseholdCambodia_2C[,c(1,2032:2044)],HouseholdLaos_2C[,c(1,1839:1851)],
                 HouseholdVietnam_2C[,c(1,1871:1883)])
for (i in 2:ncol(AnimalN)){
  AnimalN[,i] <- as.numeric(AnimalN[,i])
  AnimalN[,i] <- ifelse(AnimalN[,i] == 2, 1, 0)
}
AnimalN$Species <- rowSums(AnimalN[,c(2:14)], na.rm = T)
AnimalN$Score <- ifelse(AnimalN$Species < 2,0,ifelse(AnimalN$Species < 4, 1, 2))
TableAEP <- join(TableAEP,AnimalN[c(1,16)], by = "o9")
colnames(TableAEP)[22] <- "V5_3"

#2.5.4. Do you conserve and use traditional/local seeds or any local animal breed? 
# Yes = 1 / No = 00, 0 to 2
#d32 - e5_c - e6_c - e7_c
LocalS <- rbind(HouseholdCambodia_2C[,c(1,1942,2071,2101,2122)],
                HouseholdLaos_2C[,c(1,1751,1879,1907,1926)],
                HouseholdVietnam_2C[,c(1,1784,1911,1939,1958)])
for (i in 2:ncol(LocalS)){
  LocalS[,i] <- as.character(LocalS[,i])
  LocalS[,i] <- ifelse(LocalS[,i] == "Yes", "1", LocalS[,i])
  LocalS[,i] <- ifelse(LocalS[,i] == "No", "0", LocalS[,i])
  LocalS[,i] <- as.numeric(LocalS[,i])
}
LocalS$Sum <- ifelse(rowSums(LocalS[,c(3:5)], na.rm = T) > 0, 1, 0) + LocalS$d32
TableAEP <- join(TableAEP,LocalS[c(1,6)], by = "o9")
colnames(TableAEP)[23] <- "V5_4"


#We create the score for the 5th principle
TableAEP$V5 <- rowSums(TableAEP[,c(20:23)], na.rm = T)
summary(as.factor(TableAEP$V5))                             

##  2.6. Synergy
#Score from 0 to 2

#2.6.1. Use the answer of practices from different questions (control of weeds, pest and soil fertility) 
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice divided by the number of practices
#% score where 0 = no practices and 1 = all practices on all cropping area
#7 Practices
#0 = Score of 0
#1 = Score =<0.143 (equivalent to equal or less than 1 practice on all area)
#2 = Score >0.143 (equivalent to more than 1 practice on all area)
#d17-d20-d27
HouseholdCambodia_2Cg <- HouseholdCambodia_2C[,-c(832:834,849:851,866:868,883:885,900:902,
                                                  917:919,934:936,951:953)]

Synergy <- join(d17subS[,c(1:5,9:10,14)], d20subS[,c(1,5,12)], by = "o9")
Synergy <- join(Synergy, d27subS[,c(1,5)], by = "o9")

#Final % calculation
Synergy$FinalPer <- rowSums(Synergy[,c(5:11)]) / ncol(Synergy[,c(5:11)])
summary(d12subS$FinalPer)
Synergy$Score <- ifelse(Synergy$FinalPer > 0.143, 2, ifelse(Synergy$FinalPer > 0, 1, 0))
Synergy$Score <- Synergy$Score / 2
TableAEP <- join(TableAEP,Synergy[,c(1,13)], by = "o9")
colnames(TableAEP)[25] <- "V6_1"


#2.6.2. Sources of animal feed in dry and rainy season (only consider ruminants):  
# yes = 1 / no = 0
#Options considered: own crop residues, cutting and carry (except when from public areas) 
#e19: "Cutting and carry natural grass / vegetables from own area",
#     "Cutting and carry of forage", "Own crop residues (rice straw, maize stem...) grazing after harvest"
#     "Own crop residues (rice straw, maize stem…) collected and stored"
#e24: "Cutting and carry natural grass / vegetables from own area",
#     "Cutting and carry of forage", "Own crop residues (rice straw, maize stem...) grazing after harvest"
#     "Own crop residues (rice straw, maize stem…) collected and stored"
AniFeed <- rbind(HouseholdCambodia_2C[,c(1,2155:2158,2175:2178)],
                 HouseholdLaos_2C[,c(1,1957:1960,1973:1976)],
                 HouseholdVietnam_2C[,c(1,1989:1992,2005:2008)])
for (i in 2:ncol(AniFeed)){
  AniFeed[,i] <- as.numeric(AniFeed[,i])
  AniFeed[,i] <- ifelse(AniFeed[,i] == 2, 1, 0)
}
AniFeed$Score <- ifelse(rowSums(AniFeed[2:9], na.rm = T) > 0, 1, 0)
TableAEP <- join(TableAEP,AniFeed[c(1,10)], by = "o9")
colnames(TableAEP)[26] <- "V6_2"

#We create the score for the 6th principle
TableAEP$V6 <- TableAEP$V6_1 + TableAEP$V6_2
summary(as.factor(TableAEP$V6))

##  2.7. Economic diversification
#Score from 0 to 6

#2.7.1. Index based on the sum of farm income generating activities
#(Need to create an indicator including all the product household sell,
#animal and crops)
#Less than 2 activities = 0 / Less than 5 = 1 and 5 or more = 2
#1st for lowland
colnames(ClowlandCambodia_2C)[20] <- "d2_136"
Clowland <- rbind(ClowlandCambodia_2C[,c(2:3,20)], ClowlandLaos_2C[,c(1:2,15)], ClowlandVietnam_2C[,c(1:2,15)])
colnames(Clowland)[2] <- "o9"
LowlandSold <- Clowland %>% filter(Clowland$d2_136 == 0)
ncrop_soldl <- LowlandSold %>%
  dplyr::count(`o9`)
#2nd for upland
colnames(CuplandCambodia_2C)[19] <- "d2_236"
Cupland <- rbind(CuplandCambodia_2C[,c(2:3,19)], CuplandLaos_2C[,c(1:2,14)], CuplandVietnam_2C[,c(1:2,14)])
colnames(Cupland)[2] <- "o9"
uplandSold <- Cupland %>% filter(Cupland$d2_236 == 0)
ncrop_soldu <- uplandSold %>%
  dplyr::count(`o9`)
#3rd for the 3 main animals
AniSold <- rbind(HouseholdCambodia_2C[,c(1,2078,2108,2129)],
                 HouseholdLaos_2C[,c(1,1886,1914,1933)],
                 HouseholdVietnam_2C[,c(1,1918,1946,1965)])
for (i in 2:4){
  AniSold[,i] <- ifelse(AniSold[,i] > 0, '1', '0')
  AniSold[,i] <- as.numeric(AniSold[,i])
}
AniSold$Nani <- rowSums(AniSold[2:4], na.rm = T)
#Now we merge these 3 tables together
Dummy <- join(ncrop_soldl, ncrop_soldu, by = "o9")
colnames(Dummy)[2:3] <- c("lowland","upland")
SoldProd <- join(AniSold[,c(1,5)],Dummy, by = "o9")
SoldProd$SoldProdTot <- as.numeric(ifelse(!is.na(SoldProd$Nani), SoldProd$Nani,'0'))+ 
  as.numeric(ifelse(!is.na(SoldProd$lowland), SoldProd$lowland,'0'))+
  as.numeric(ifelse(!is.na(SoldProd$upland), SoldProd$upland,'0'))
SoldProd <- SoldProd[,c(1,5)]
#3rd for derived products
#b2_1 & b2_2
Derived <- rbind(HouseholdCambodia_2C[,c(1,83:86,89:91,93)],HouseholdLaos_2C[,c(1,73:76,79:82)],
                 HouseholdVietnam_2C[,c(1,83:85,98,102:104,113)])
for (i in 2:ncol(Derived)){
  Derived[,i] <- as.numeric(Derived[,i])
  Derived[,i] <- ifelse(Derived[,i] == 2, 1, 0)
}
Derived$Sum <- rowSums(Derived[2:9], na.rm = T)
SoldProd <- join(SoldProd, Derived[,c(1,10)], by = "o9")
#NTFP collection
#d7
NTFP <- rbind(HouseholdCambodia_2C[,c(1,709)],HouseholdLaos_2C[,c(1,719)],
              HouseholdVietnam_2C[,c(1,762)])
NTFP$NTFP <- ifelse(NTFP$d70 == '1', 0, 1)
SoldProd <- join(SoldProd, NTFP[,c(1,3)], by = "o9")
SoldProd$Tot <- rowSums(SoldProd[2:4], na.rm = T)
table(SoldProd$Tot)
SoldProd$Score <- ifelse(SoldProd$Tot < 2, 0, ifelse(SoldProd$Tot < 5, 1, 2))
TableAEP <- join(TableAEP,SoldProd[c(1,6)], by = "o9")
colnames(TableAEP)[28] <- "V7_1"

#2.7.2.  % of income from the 2 main sources of incomes
#More than 75% for 1st source = 0 / 40-70% = 1 / Less than 40% = 2
#b5_1, b5_2, b5_3
Income <- rbind(HouseholdCambodia_2C[,c(1,126,128)],HouseholdLaos_2C[,c(1,113,115)],
                HouseholdVietnam_2C[,c(1,150,152)])
Income$b5_1 <- ifelse(Income$b5_1 > 100, 100, Income$b5_1)
Income$Sum <- rowSums(Income[2:3], na.rm = T)
Income$Score <- ifelse(Income$b5_1 > 74, 0, ifelse(Income$b5_1 > 39, 1, 2))
TableAEP <- join(TableAEP,Income[c(1,5)], by = "o9")
colnames(TableAEP)[29] <- "V7_2"

#2.7.3. Sum of all HH mb income generating activities (on farm/ off farm/ salary/ business)  
#a6. & a7.
#If 0 = 0, 1 or 2 = 1 and 3 or 4 = 2
HouseholdM <- rbind(HouMemberCambodia_2C[,c(2,3,13,15)],HouMemberLaos_2C[,c(1,2,12,14)],
                    HouMemberVietnam_2C[,c(1,2,12,14)])
for (i in 3:ncol(HouseholdM)){
  HouseholdM[,i] <- as.character(HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "1", "On-farm",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Agricultural work on their own farm (including livestock management)", "On-farm", HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "2", "Other farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Permanent salaried employment on someone else’s farm (including livestock management", "Other farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "3", "Other farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Seasonal salaried employment on someone else’s farm (including livestock management)", "Other farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "4", "Off-farm salaried", HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Government", "Off-farm salaried", HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "5", "Business owner",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Business (owner)", "Business owner",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "6", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Salaried employment (non-agricultural work)", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "7", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Junk collector", "Off-farm salaried", HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "8", NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Study/training", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "9", NA ,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Housewife/ caregiver", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "10", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Unable to work", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "88", NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Other", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "99", NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Do not know", NA, HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Agricultural service provision (please specify)", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Agricultural service provision", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Business (owner) (please specify)", "Business owner",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Daily wage sale of labor", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "No main Occupation",NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Other non-agricultural work (please specify)", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Salaried employment with company", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Salaried employment with government", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Salaried employment with NGOs", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Student/Study at university/training", NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "No secondary occupation", NA,HouseholdM[,i])
  HouseholdM[,i] <- ifelse(HouseholdM[,i] == "Other non-agricultural work", "Off-farm salaried",HouseholdM[,i])
  HouseholdM[,i] <- as.factor(HouseholdM[,i])
}
HouMemberOcc1 <- table(HouseholdM[c('hhid_re1', 'a6')])
HouMemberOcc2 <- table(HouseholdM[c('hhid_re1', 'a7')]) 
HouMemberOcc <- as.data.frame(cbind(HouMemberOcc1, HouMemberOcc2))
HouMemberOcc <- HouMemberOcc[,-c(1,6:7)]
HouMemberOcc$Score <- ifelse(HouMemberOcc$`Business owner` > 0 | HouMemberOcc$`Business owner.1` > 0, 1, 0)
HouMemberOcc$Score <- ifelse(HouMemberOcc$`Off-farm salaried` > 0 | HouMemberOcc$`Off-farm salaried.1` > 0, HouMemberOcc$Score + 1, HouMemberOcc$Score)
HouMemberOcc$Score <- ifelse(HouMemberOcc$`On-farm` > 0 | HouMemberOcc$`On-farm.1` > 0, HouMemberOcc$Score + 1, HouMemberOcc$Score)
HouMemberOcc$Score <- ifelse(HouMemberOcc$`Other farm salaried` > 0 | HouMemberOcc$`Other farm salaried.1` > 0, HouMemberOcc$Score + 1, HouMemberOcc$Score)
HouMemberOcc$o9 <- rownames(HouMemberOcc)
HouMemberOcc$Score2 <- ifelse(HouMemberOcc$Score > 2, 2, ifelse(HouMemberOcc$Score > 0, 1, 0))
TableAEP <- join(TableAEP,HouMemberOcc[,c(10,11)], by = "o9")
colnames(TableAEP)[30] <- "V7_3"

#We create the score for the 7th principle
TableAEP$V7 <- rowSums(TableAEP[,c(28:30)], na.rm = T)


## 2.8. Co-creation of knowledge
#Score from 0 to 4

#2.8.1. Agricultural products, equipment or animals exchanging with other farmers?
#Yes = 1 ; No = 0
#c14.
c14 <- rbind(HouseholdCambodia_2C[,c(1,578)],HouseholdLaos_2C[,c(1,614)],HouseholdVietnam_2C[,c(1,658)])
summary(c14$c14)
c14$c14 <- str_replace_all(c14$c14,"Yes","1")
c14$c14 <- str_replace_all(c14$c14,"No","0")
c14$c14 <- as.numeric(c14$c14)
TableAEP <- join(TableAEP,c14, by = "o9")
colnames(TableAEP)[32] <- "V8_1"

#2.8.2. Appreciation of time availability to acquire new knowledge and improve skills
#f4
#0 (1-2) ; 1 (3) ; 2 (4-5)
f4 <- rbind(HouseholdCambodia_2C[,c(1,2381)],HouseholdLaos_2C[,c(1,2163)],HouseholdVietnam_2C[,c(1,2197)])
summary(f4$f4)
f4$f4 <- str_replace_all(f4$f4,"No time","1")
f4$f4 <- str_replace_all(f4$f4,"Very little time","2")
f4$f4 <- str_replace_all(f4$f4,"Moderate amount of time","3")
f4$f4 <- str_replace_all(f4$f4,"Almost enough time","4")
f4$f4 <- str_replace_all(f4$f4,"Sufficient amount of time","5")
f4$f4 <- str_replace_all(f4$f4,"Do not know","88")
f4$f4 <- as.numeric(f4$f4)
f4$Score <- ifelse(f4$f4 == 1 | f4$f4 == 2, 0, ifelse(f4$f4 == 3, 1,
                                                      ifelse(f4$f4 == 4 | f4$f4 == 5, 2, NA))) / 2
TableAEP <- join(TableAEP,f4[c(1,3)], by = "o9")
colnames(TableAEP)[33] <- "V8_2"

#2.8.3. Collaboration with other people for different activities
#c13.
#Yes = 1 / No = 0
#c13.
colnames(HouseholdVietnam_2C)[646] <- "c13"
c13 <- rbind(HouseholdCambodia_2C[,c(1,568)],HouseholdLaos_2C[,c(1,604)],HouseholdVietnam_2C[,c(1,646)])
summary(c13$c13)
c13$c13 <- str_replace_all(c13$c13,"No collaboration with other people on these issues","0")
c13$c13 <- ifelse(c13$c13 != 0, 1, c13$c13)
c13$c13 <- as.numeric(c13$c13)
TableAEP <- join(TableAEP,c13, by = "o9")
colnames(TableAEP)[34] <- "V8_3"

#We create the score for the 8th principle
TableAEP$V8 <- rowSums(TableAEP[,c(32:34)], na.rm = T)
summary(as.factor(TableAEP$V8))

##  2.9. Social values and diets
#Score from 0 to 9

#2.9.1. Generate a tercile index based on the proportion of the food (rice, vegetable, animal products, etc.) consumed by your family that comes from your own farm or home garden 
#Binary variable:  
#Below 50% = 0 ; Above 50% = 1
#i1.
i1 <- rbind(HouseholdCambodia_2C[,c(1,2406)],HouseholdLaos_2C[,c(1,2189)],HouseholdVietnam_2C[,c(1,2222)])
summary(i1$i1)
i1$i1 <- str_replace_all(i1$i1,"Less than 25%","1")
i1$i1 <- str_replace_all(i1$i1,"25-50%","2")
i1$i1 <- str_replace_all(i1$i1,"50-75%","3")
i1$i1 <- str_replace_all(i1$i1,"Over 75%","4")
i1$i1 <- as.numeric(i1$i1)
i1$i1b <- ifelse(i1$i1 == 3 | i1$i1 == 4, 1, 0)
TableAEP <- join(TableAEP,i1[,c(1,3)], by = "o9")
colnames(TableAEP)[36] <- "V9_1"

#2.9.2. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed? 
#Yes = 1 ; No = 0
#g5.
g5 <- rbind(HouseholdCambodia_2C[,c(1,2387)],HouseholdLaos_2C[,c(1,2169)],HouseholdVietnam_2C[,c(1,2203)])
summary(g5$g5)
g5$g5 <- str_replace_all(g5$g5,"Yes","1")
g5$g5 <- str_replace_all(g5$g5,"No","0")
g5$g5 <- as.numeric(g5$g5)
TableAEP <- join(TableAEP,g5, by = "o9")
colnames(TableAEP)[37] <- "V9_2"

#2.9.3. Were there months, in the past 12 months, in which you did not have enough food to meet your family’s needs? 
#Yes = 0 ; No = 1
#i2.
i2 <- rbind(HouseholdCambodia_2C[,c(1,2407)],HouseholdLaos_2C[,c(1,2190)],HouseholdVietnam_2C[,c(1,2223)])
summary(i2$i2)
i2$i2 <- str_replace_all(i2$i2,"Yes","1")
i2$i2 <- str_replace_all(i2$i2,"No","0")
i2$i2 <- as.numeric(i2$i2)
i2$Lack <- ifelse(i2$i2 == 1, 0, 1)
TableAEP <- join(TableAEP,i2[,c(1,3)], by = "o9")
colnames(TableAEP)[38] <- "V9_3"

#2.9.4. Are the decisions about what (e.g. crops, animals) and how to produce, purchasing,
#selling, or transferring major household assets (land, cattle, equipment), borrowing or lending money, about how the household income is used made in consultation with spouse/other family members?  
#Myself alone = 1 / Me in consultation with spouse/other family members = 2 /
#My spouse/other family members = 3
#g1, g2, g3, g4
g1234 <- rbind(HouseholdCambodia_2C[,c(1,2383:2386)],HouseholdLaos_2C[,c(1,2165:2168)],HouseholdVietnam_2C[,c(1,2199:2202)])
summary(g1234$g1)
for (i in 2:ncol(g1234)){
  g1234[,i] <- str_replace_all(g1234[,i],"Myself alone","1")
  g1234[,i] <- str_replace_all(g1234[,i],"Me in consultation with spouse/other family members","2")
  g1234[,i] <- str_replace_all(g1234[,i],"My spouse/other family members","3")
  g1234[,i] <- str_replace_all(g1234[,i],"Do not know","0")
  g1234[,i] <- str_replace_all(g1234[,i],"88","0")
  g1234[,i] <- as.numeric(g1234[,i])
}
g1234$gSum <- g1234$g1 + g1234$g2 + g1234$g3 + g1234$g4
g1234$Score <- ifelse(g1234$gSum >10, 3, ifelse(g1234$gSum >7, 2,
                                                ifelse(g1234$gSum >4, 1, 0))) / 3
summary(as.factor(g1234$Score))
TableAEP <- join(TableAEP,g1234[,c(1,7)], by = "o9")
colnames(TableAEP)[39] <- "V9_4"

#2.9.5. What is the proportion of products from crops, vegetables or fruits that is sold raw (not processed/not transformed)? 
#b11. 
#Less than 25% = 3 ; 25-50% = 2 ; 50-75% = 1 ; Over 75% = 0 
b11 <- rbind(HouseholdCambodia_2C[,c(1,221)],HouseholdLaos_2C[,c(1,202)],HouseholdVietnam_2C[,c(1,239)])
summary(b11$b11)
b11$b11 <- str_replace_all(b11$b11,"Less than 25%","1")
b11$b11 <- str_replace_all(b11$b11,"25-50%","2")
b11$b11 <- str_replace_all(b11$b11,"50-75%","3")
b11$b11 <- str_replace_all(b11$b11,"Over 75%","4")
b11$b11 <- ifelse(b11$b11 == "Do not know",NA, b11$b11)
b11$b11 <- ifelse(b11$b11 == "88",NA, b11$b11)
b11$b11 <- ifelse(b11$b11 == "",NA, b11$b11)
b11$b11b <- (as.numeric(b11$b11) - 1) / 3
TableAEP <- join(TableAEP,b11[,c(1,3)], by = "o9")
colnames(TableAEP)[40] <- "V9_5"

#We create the score for the 9th principle
TableAEP$V9 <- rowSums(TableAEP[,c(36:40)], na.rm = T)
summary(as.factor(TableAEP$V9))

## 2.10. Fairness
#Score from 0 to 2
# Removed 

#2.10.1. Do you regularly communicate with buyers to get their feedback on the product? 
#b28.
#Yes = 1 ; No = 0
#b28 <- rbind(HouseholdCambodia_2C[,c(1,477)],HouseholdLaos_2C[,c(1,538)],HouseholdVietnam_2C[,c(1,576)])
#summary(b28$b28)
#b28$b28 <- str_replace_all(b28$b28,"No","0")
#b28$b28 <- str_replace_all(b28$b28,"Yes","1")
#b28$b28 <- as.numeric(b28$b28)
#TableAEP <- join(TableAEP,b28, by = "o9")
#colnames(TableAEP)[42] <- "V10_1"

#2.10.2. In the past year/12 months, what were the two main outlets or buyers for crop/vegetables/fruit or for livestock in terms of income? 
#Cooperative of which you are a member = 1 
#b12_1 & b12_2
#b12 <- rbind(HouseholdCambodia_2C[,c(1,634,666)],HouseholdLaos_2C[,c(1,654,657)],HouseholdVietnam_2C[,c(1,697,700)])
#summary(b12$b12_1)
#b12$b12b <- ifelse(b12$b12_1 == 6 | b12$b12_1 == "Cooperative of which you are a member" |
#                     b12$b12_2 == 6 | b12$b12_2 == "Cooperative of which you are a member", 1, 0)
#b12$b12b <- ifelse(b12$b12_1 == '' | 	
#                     b12$b12_1 == "Do not know/ No selling/ No the second outlet", NA, b12$b12b)
#TableAEP <- join(TableAEP, b12[,c(1,4)], by = "o9")
#colnames(TableAEP)[43] <- "V10_2"

#We create the score for the 10th principle
#TableAEP$V10 <- rowSums(TableAEP[,c(42:43)], na.rm = T)
#summary(as.factor(TableAEP$V10))

## 2.11. Connectivity 
#Score from 0 to 4

#2.11.1. & 2. If you sell crops/vegetables/fruits or processed  or livestock products, do you know what is their main final destination? 
#2 Binary variables: 
#Crops…:  Know = 1  ; Don’t know = 0 
#b10.
b10 <- rbind(HouseholdCambodia_2C[,c(1,211)],HouseholdLaos_2C[,c(1,193)],HouseholdVietnam_2C[,c(1,230)])
summary(b10$b10)
b10$b10b <- ifelse(b10$b10 == "Do not know", 0, b10$b10)
b10$b10b <- ifelse(b10$b10 == "88", 0, b10$b10b)
b10$b10b <- ifelse(b10$b10 == '', NA, b10$b10b)
b10$b10b <- ifelse(b10$b10b != 0 , 1, b10$b10b)
b10$b10b <- as.numeric(b10$b10b)
TableAEP <- join(TableAEP, b10[,c(1,3)], by = "o9")
colnames(TableAEP)[42] <- "V11_1"

#Livestock: Know = 1 ; Don’t know = 0
#b16. 
b16 <- rbind(HouseholdCambodia_2C[,c(1,222)],HouseholdLaos_2C[,c(1,203)],HouseholdVietnam_2C[,c(1,240)])
b16$b16b <- ifelse(b16$b16 == "Do not know", 0, b16$b16)
b16$b16b <- ifelse(b16$b16 == "88", 0, b16$b16b)
b16$b16b <- ifelse(b16$b16 == '', NA, b16$b16b)
b16$b16b <- ifelse(b16$b16b != 0 , 1, b16$b16b)
b16$b16b <- as.numeric(b16$b16b)
TableAEP <- join(TableAEP, b16[,c(1,3)], by = "o9")
colnames(TableAEP)[43] <- "V11_2"

#2.11.3. Does any of your buyers (crops, animals, other) provide any of the following:  
#0 = 0 ; 1 or more = 1
#Considering the following answers:  
#Technical advises
#Market information
#Inputs (sold)
#Regular sales
#b14_1. & b14_2.
x <- HouseholdCambodia_2C[,c(1,656,659:661,688,691:693)]
y <- HouseholdLaos_2C[,c(1,675,678:680,699,702:704)]
colnames(y)[6:9] <- c("b14_21","b14_24","b14_25","b14_26")
z <- HouseholdVietnam_2C[,c(1,718,721:723,743,746:748)]
b16 <- rbind(x,y,z)
for (i in 2:ncol(b16)){
  b16[,i] <- as.numeric(b16[,i])
  b16[,i] <- ifelse(!is.na(b16[,i]), b16[,i], 0)
}
b16$Sum <- b16$b14_14 + b16$b14_14 + b16$b14_15 + b16$b14_16 + b16$b14_21 + b16$b14_24 +
  b16$b14_25 + b16$b14_26
b16$Score <- ifelse(b16$Sum > 0, 1, b16$Sum)
TableAEP <- join(TableAEP, b16[,c(1,11)], by = "o9")
colnames(TableAEP)[44] <- "V11_3"

#We create the score for the 11th principle
TableAEP$V11 <- rowSums(TableAEP[,c(42:44)], na.rm = T)
summary(as.factor(TableAEP$V11))

## 2.12. Land and natural resource governance 
#/

## 2.13. Participation
#Score from 0 to 4

#2.13.1. Are you a member of one or more farmer group/cooperative/organization or village organization (e.g. crops/fruits/livestock/honey/ water/Forest etc.)? 
#c2.
#Yes More than 1 = 2 ; Yes, one = 1 ; No = 0
c2 <- rbind(HouseholdCambodia_2C[,c(1,498)],HouseholdLaos_2C[,c(1,552)],HouseholdVietnam_2C[,c(1,591)])
summary(c2$c2)
c2$c2 <- str_replace_all(c2$c2,"No","0")
c2$c2 <- str_replace_all(c2$c2,"Yes, one","1")
c2$c2 <- str_replace_all(c2$c2,"Yes, more than one","2")
c2$c2 <- as.numeric(c2$c2)
TableAEP <- join(TableAEP, c2, by = "o9")
colnames(TableAEP)[46] <- "V13_1"

#2.13.2. Are you or anyone in your household active in any of the following? 
#c1.
#Yes = 1
#No = 0
c1 <- rbind(HouseholdCambodia_2C[,c(1,481)],HouseholdLaos_2C[,c(1,542)],HouseholdVietnam_2C[,c(1,580)])
c1$c1b <- str_replace_all(c1$c1,"None","0")
c1$c1b <- ifelse(c1$c1b == "I do not know", NA, c1$c1b)
c1$c1b <- ifelse(c1$c1 == "88", NA, c1$c1b)
c1$c1b <- ifelse(c1$c1b == "0" | is.na(c1$c1b), c1$c1b, 1)
c1$c1b <- as.numeric(c1$c1b)
TableAEP <- join(TableAEP, c1[,c(1,3)], by = "o9")
colnames(TableAEP)[47] <- "V13_2"

#2.13.3. Are you or anyone in your household involved in any advocacy work?
#c15.
#Yes = 1
#No = 0
c15 <- rbind(HouseholdCambodia_2C[,c(1,579)],HouseholdLaos_2C[,c(1,615)],HouseholdVietnam_2C[,c(1,659)])
summary(c15$c15)
c15$c15 <- str_replace_all(c15$c15,"No","0")
c15$c15 <- str_replace_all(c15$c15,"Yes","1")
c15$c15 <- as.numeric(c15$c15)
TableAEP <- join(TableAEP, c15, by = "o9")
colnames(TableAEP)[48] <- "V13_3"

#We create the score for the 13th principle
TableAEP$V13 <- rowSums(TableAEP[,c(46:48)], na.rm = T)

## 3. Final score calculation and average values
Scores <- TableAEP[,c(1:4,7,12,15,19,24,27,31,35,41,45,49)]
Scores$V1 <- Scores$V1 / 4
Scores$V2 <- Scores$V2 / 9
Scores$V3 <- Scores$V3 / 4
Scores$V4 <- Scores$V4 / 3
Scores$V5 <- Scores$V5 / 8
Scores$V6 <- Scores$V6 / 2
Scores$V7 <- Scores$V7 / 6
Scores$V8 <- Scores$V8 / 3
Scores$V9 <- Scores$V9 / 5
Scores$V11 <- Scores$V11 / 3
Scores$V13 <- Scores$V13 / 4

## 3.1 Plotting (no sampling weight)
#We calculate average score value for each principle - Plot Study area
AvScores <- aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13) ~ o5, data = Scores, FUN = mean, na.rm = TRUE)
colnames(AvScores) <- c("Area","1.Recycling","2.Input reduction","3.Soil Health",
                        "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                        "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                        "13.Participation")
rownames(AvScores) <- c("Vietnam-DB","Cambodia","Vietnam-SL","Laos")
 
#Plotting
AvScores<- rbind(rep(1,4) , rep(0,4) , AvScores)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot - Area
radarchart(AvScores[,c(2:12)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScores[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#Cluster id loading
ClusterID <- read.csv2("ClusterID.csv", sep = ";")
colnames(ClusterID)[2] <- "o9"
Scores <- join(Scores, ClusterID[,c(2,3)], by = "o9")

# plot - Cluster Cambodia
#We calculate average score value for each principle
ScoresC <- filter(Scores, o4 == "Cambodia")
AvScoresC <- aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13) ~ clust, data = ScoresC, FUN = mean, na.rm = TRUE)
colnames(AvScoresC) <- c("Clusters","1.Recycling","2.Input reduction","3.Soil Health",
                        "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                        "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                        "13.Participation")
rownames(AvScoresC) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")

#Plotting
AvScoresC<- rbind(rep(1,13) , rep(0,13) , AvScoresC)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresC[,c(2:12)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y= 1, legend = rownames(AvScoresC[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Laos
#We calculate average score value for each principle
ScoresL <- filter(Scores, o4 == "Lao")
AvScoresL <- aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13) ~ clust, data = ScoresL, FUN = mean, na.rm = TRUE)
colnames(AvScoresL) <- c("Clusters","1.Recycling","2.Input reduction","3.Soil Health",
                         "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                         "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                         "13.Participation")
rownames(AvScoresL) <- c("Cluster 1","Cluster 2","Cluster 3")

#Plotting
AvScoresL<- rbind(rep(1,13) , rep(0,13) , AvScoresL)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresL[,c(2:12)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresL[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Vietnam MC
#We calculate average score value for each principle
ScoresVSL <- filter(Scores, o5 == "Son La province")
AvScoresVSL <- aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13) ~ clust, data = ScoresVSL, FUN = mean, na.rm = TRUE)
colnames(AvScoresVSL) <- c("Clusters","1.Recycling","2.Input reduction","3.Soil Health",
                         "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                         "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                         "13.Participation")
rownames(AvScoresVSL) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")

#Plotting
AvScoresVSL<- rbind(rep(1,13) , rep(0,13) , AvScoresVSL)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresVSL[,c(2:12)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresVSL[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Vietnam DB
#We calculate average score value for each principle
ScoresVDB <- filter(Scores, o5 == "Dien Bien province")
AvScoresVDB <- aggregate(cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13) ~ clust, data = ScoresVDB, FUN = mean, na.rm = TRUE)
colnames(AvScoresVDB) <- c("Clusters","1.Recycling","2.Input reduction","3.Soil Health",
                           "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                           "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                           "13.Participation")
rownames(AvScoresVDB) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")

#Plotting
AvScoresVDB<- rbind(rep(1,13) , rep(0,13) , AvScoresVDB)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresVDB[,c(2:12)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresVDB[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



## 3.2 Plotting (Sampling weight)
#Cambodia
x <- as.data.frame(summary(HouseholdCambodia_2C$village_eng_preload))
#Population of villages from an external file
x$fpc <- c(141,228,130,94,286,118,260,90,37,124,198,78,95,165,233,140)
x$Weight <- x$fpc/x$`summary(HouseholdCambodia_2C$village_eng_preload)`
x$village_eng_preload <- rownames(x)
x$village_eng_preload <- str_replace(x$village_eng_preload, "Kk Pon", "Kok Poun")
colnames(x)[1] <- "SurH"
#Laos
#First we add a column in which we add the sampling weight for each household
y <- as.data.frame(summary(HouseholdLaos_2C$village_eng_preload))
#Population of villages from an external file
y$fpc <- c(1,119,23,178,126,64,163,82,71,143,34,80,211,98,93,123,167)
y <- y[c(2:17),]
y$Weight <- y$fpc/y$`summary(HouseholdLaos_2C$village_eng_preload)`
y$village_eng_preload <- rownames(y)
y$village_eng_preload <- str_replace(y$village_eng_preload, "Xay-nadu ", "Xay-nadu")
y$village_eng_preload <- str_replace(y$village_eng_preload, "KuayMor", "Kuay")
y$village_eng_preload <- str_replace(y$village_eng_preload, "Tin-Nua", "Ton-Nua")
colnames(y)[1] <- "SurH"
#Vietnam
#First we add a column in which we add the sampling weight for each household
z <- as.data.frame(summary(as.factor(HouseholdVietnam_2C$village_eng_preload)))
z$`summary(as.factor(HouseholdVietnam_2C$village_eng_preload))` <- as.numeric(z$`summary(as.factor(HouseholdVietnam_2C$village_eng_preload))`)
#Population of villages from an external file
z$fpc <- c(66,56,89,121,95,75,51,139,121,94,180,252,176,
           123,94,109,83,46,95,87)
z$Weight <- z$fpc/z$`summary(as.factor(HouseholdVietnam_2C$village_eng_preload))`
z$village_eng_preload <- rownames(z)
colnames(z)[1] <- "SurH"

#Merging
SWeight <- rbind(x,y,z)
colnames(SWeight)[4] <- "o8"
Scores <- join(Scores,SWeight, by = "o8")
Scores$o5 <- str_replace(Scores$o5, "Preah Vihear", "Cambodia")
Scores$o5 <- str_replace(Scores$o5, "Xeingkhouang", "Laos")
Scores$o5 <- str_replace(Scores$o5, "Dien Bien province", "Vietnam - DB")
Scores$o5 <- str_replace(Scores$o5, "Son La province", "Vietnam - SL")
dstratC <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = Scores[Scores$o5 == "Cambodia",],
  fpc = ~fpc)
dstratL <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = Scores[Scores$o5 == "Laos",],
  fpc = ~fpc)
dstratVDB <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = Scores[Scores$o5 == "Vietnam - DB",],
  fpc = ~fpc)
dstratVSL <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = Scores[Scores$o5 == "Vietnam - SL",],
  fpc = ~fpc)


#We calculate average score value for each principle - Plot Study area
Cambodia <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstratC, na.rm = TRUE)
Laos <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstratL, na.rm = TRUE)
VDB <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstratVDB, na.rm = TRUE)
VSL <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstratVSL, na.rm = TRUE)
AvScores <- rbind(Cambodia,Laos,VDB,VSL)
AvScores <- as.data.frame(AvScores)
AvScores$Area <- rownames(AvScores)
colnames(AvScores) <- c("1.Recycling","2.Input reduction","3.Soil Health",
                        "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                        "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                        "13.Participation","Area")
rownames(AvScores) <- c("Cambodia","Laos","Vietnam-DB","Vietnam-SL")

#Plotting
AvScores<- rbind(rep(1,4) , rep(0,4) , AvScores)

# Color vector
colors_border=c(rgb(0.8,0.2,0.5,0.9),rgb(0.4,0.2,0.9,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.7,0.5,0.1,0.9))
colors_in=c(rgb(0.8,0.2,0.5,0.9),rgb(0.4,0.2,0.9,0.9),rgb(0.2,0.5,0.5,0.9),rgb(0.7,0.5,0.1,0.9))

# plot - Area
radarchart(AvScores[,c(1:11)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScores[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

#Cluster id loading
ClusterID <- read.csv2("ClusterID.csv", sep = " ")
colnames(ClusterID)[1] <- "o9"
Scores <- join(Scores, ClusterID, by = "o9")

# plot - Cluster Cambodia
#We calculate average score value for each principle
ScoresC <- Scores[Scores$o4 == "Cambodia",]
dstrat1 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresC[ScoresC$clust == 1,],
  fpc = ~fpc)
dstrat2 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresC[ScoresC$clust == 2,],
  fpc = ~fpc)
dstrat3 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresC[ScoresC$clust == 3,],
  fpc = ~fpc)
dstrat4 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresC[ScoresC$clust == 4,],
  fpc = ~fpc)


#We calculate average score value for each principle - Plot Study area
Cluster1 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat1, na.rm = TRUE)
Cluster2 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat2, na.rm = TRUE)
Cluster3 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat3, na.rm = TRUE)
Cluster4 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat4, na.rm = TRUE)
AvScoresC <- rbind(Cluster1,Cluster2,Cluster3,Cluster4)
AvScoresC <- as.data.frame(AvScoresC)
AvScoresC$Area <- rownames(AvScoresC)
colnames(AvScoresC) <- c("1.Recycling","2.Input reduction","3.Soil Health",
                        "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                        "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                        "13.Participation","Area")
rownames(AvScoresC) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")


#Plotting
AvScoresC<- rbind(rep(1,13) , rep(0,13) , AvScoresC)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresC[,c(1:11)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresC[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Laos
#We calculate average score value for each principle
ScoresL <- Scores[Scores$o4 == "Lao",]
ScoresL <- ScoresL[!is.na(ScoresL$clust),]
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
dstrat1 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresL[ScoresL$clust == 1,],
  fpc = ~fpc)
dstrat2 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresL[ScoresL$clust == 2,],
  fpc = ~fpc)
dstrat3 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresL[ScoresL$clust == 3,],
  fpc = ~fpc)


#We calculate average score value for each principle - Plot Study area
Cluster1 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat1, na.rm = TRUE)
Cluster2 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat2, na.rm = TRUE)
Cluster3 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat3, na.rm = TRUE)
AvScoresL <- rbind(Cluster1,Cluster2,Cluster3)
AvScoresL <- as.data.frame(AvScoresL)
AvScoresL$Area <- rownames(AvScoresL)
colnames(AvScoresL) <- c("1.Recycling","2.Input reduction","3.Soil Health",
                         "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                         "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                         "13.Participation","Area")
rownames(AvScoresL) <- c("Cluster 1","Cluster 2","Cluster 3")

#Plotting
AvScoresL<- rbind(rep(1,13) , rep(0,13) , AvScoresL)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresL[,c(1:11)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresL[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Vietnam MC
#We calculate average score value for each principle
ScoresVSL <- Scores[Scores$o5 == "Vietnam - SL",]
ScoresVSL <- ScoresVSL[!is.na(ScoresVSL$clust),]
dstrat1 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVSL[ScoresVSL$clust == 1,],
  fpc = ~fpc)
dstrat2 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVSL[ScoresVSL$clust == 2,],
  fpc = ~fpc)
dstrat3 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVSL[ScoresVSL$clust == 3,],
  fpc = ~fpc)
dstrat4 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVSL[ScoresVSL$clust == 4,],
  fpc = ~fpc)


#We calculate average score value for each principle - Plot Study area
Cluster1 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat1, na.rm = TRUE)
Cluster2 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat2, na.rm = TRUE)
Cluster3 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat3, na.rm = TRUE)
Cluster4 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat4, na.rm = TRUE)
AvScoresVSL <- rbind(Cluster1,Cluster2,Cluster3,Cluster4)
AvScoresVSL <- as.data.frame(AvScoresVSL)
AvScoresVSL$Area <- rownames(AvScoresVSL)
colnames(AvScoresVSL) <- c("1.Recycling","2.Input reduction","3.Soil Health",
                         "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                         "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                         "13.Participation","Area")
rownames(AvScoresVSL) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")

#Plotting
AvScoresVSL<- rbind(rep(1,13) , rep(0,13) , AvScoresVSL)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresVSL[,c(1:11)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresVSL[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)


# plot - Cluster Vietnam DB
#We calculate average score value for each principle
ScoresVDB <- Scores[Scores$o5 == "Vietnam - DB",]
dstrat1 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVDB[ScoresVDB$clust == 1,],
  fpc = ~fpc)
dstrat2 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVDB[ScoresVDB$clust == 2,],
  fpc = ~fpc)
dstrat3 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVDB[ScoresVDB$clust == 3,],
  fpc = ~fpc)
dstrat4 <- svydesign(
  id = ~1, strata = ~o8,
  weights = ~Weight, data = ScoresVDB[ScoresVDB$clust == 4,],
  fpc = ~fpc)


#We calculate average score value for each principle - Plot Study area
Cluster1 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat1, na.rm = TRUE)
Cluster2 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat2, na.rm = TRUE)
Cluster3 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat3, na.rm = TRUE)
Cluster4 <- svymean(~cbind(V1,V2,V3,V4,V5,V6,V7,V8,V9,V11,V13),dstrat4, na.rm = TRUE)
AvScoresVDB <- rbind(Cluster1,Cluster2,Cluster3,Cluster4)
AvScoresVDB <- as.data.frame(AvScoresVDB)
AvScoresVDB$Area <- rownames(AvScoresVDB)
colnames(AvScoresVDB) <- c("1.Recycling","2.Input reduction","3.Soil Health",
                           "4.Animal health","5.Biodiversity","6.Synergy","7.Economic diversification",
                           "8.Co-creation of knowledge","9.Social values and diet","11.Connectivity",
                           "13.Participation","Area")
rownames(AvScoresVDB) <- c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")

#Plotting
AvScoresVDB<- rbind(rep(1,13) , rep(0,13) , AvScoresVDB)

# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9), rgb(0.4,0.2,0.9,0.9) )

# plot
radarchart(AvScoresVDB[,c(1:11)], axistype=1 ,
           #custom polygon
           pcol=colors_border , plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,0.25), cglwd=0.8,
           #custom labels
           vlcex=0.8 
)
# Add a legend
legend(x= 1.5, y=1, legend = rownames(AvScoresVDB[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)




## 4. Plotting of all components and subcomponents -

#4.1. GENERAL
TableAEP$o5 <- str_replace(TableAEP$o5, "Preah Vihear", "Cambodia")
TableAEP$o5 <- str_replace(TableAEP$o5, "Xeingkhouang", "Laos")
TableAEP$o5 <- str_replace(TableAEP$o5, "Dien Bien province", "Vietnam - Dien Bien")
TableAEP$o5 <- str_replace(TableAEP$o5, "Son La province", "Vietnam - Son La")
i <- 27
for (i in 5:ncol(TableAEP)){
  #T1 - Title of Variable
  TableAEP[,i] <- as.character(TableAEP[,i])
  TableAEP[,i] <- ifelse(is.na(TableAEP[,i]), "NA", TableAEP[,i])
  TableAEP[,i] <- as.factor(TableAEP[,i])
  x <- ggplot(TableAEP, aes(x= TableAEP[,i])) +
    geom_bar()+
    ggtitle(paste(colnames(TableAEP)[i]," /  N = ", nrow(TableAEP)))+
    geom_text(aes(label = ..count..), stat = "count", vjust = 1, colour = "white")+
    xlab("number")
  print(x)
  TableAEP[,i] <- as.numeric(TableAEP[,i])
  y <- ggplot(TableAEP, aes(x= TableAEP[,i], fill = o5)) +
    geom_histogram(position = 'dodge',binwidth = max(TableAEP[,i],na.rm = TRUE)/10)+
    ggtitle(paste(colnames(TableAEP)[i]," /  N = ", nrow(TableAEP)))+
    xlab("number")
  print(y)
}

#4.2. Cambodia clusters
TableAEPC <- filter(TableAEP, o5 == "Preah Vihear")
TableAEPC <- join(TableAEPC, ClusterID, by = "o9")
TableAEPC$clust <- as.factor(TableAEPC$clust)
for (i in 5:ncol(TableAEPC)){
  y <- ggplot(TableAEPC, aes(x= TableAEPC[,i], fill = clust)) +
    geom_histogram(position = 'dodge',binwidth = max(TableAEPC[,i],na.rm = TRUE)/10)+
    ggtitle(paste(colnames(TableAEPC)[i]," /  N = ", nrow(TableAEPC)))+
    xlab("number")
  print(y)
}

Anim <- TableAEPC[,c(16,17,18,51)]
summary(Anim[Anim$clust == 4,])

#4.3. Laos clusters
TableAEPL <- filter(TableAEP, o5 == "Xeingkhouang")
TableAEPL <- join(TableAEPL, ClusterID, by = "o9")
TableAEPL$clust <- as.factor(TableAEPL$clust)
for (i in 5:ncol(TableAEPL)){
  y <- ggplot(TableAEPL, aes(x= TableAEPL[,i], fill = clust)) +
    geom_histogram(position = 'dodge',binwidth = max(TableAEPL[,i],na.rm = TRUE)/10)+
    ggtitle(paste(colnames(TableAEPL)[i]," /  N = ", nrow(TableAEPL)))+
    xlab("number")
  print(y)
}

Anim <- TableAEPL[,c(16,17,18,51)]
summary(Anim[Anim$clust == 4,])

#4.3. Vietnam SL clusters
TableAEPSL <- filter(TableAEP, o5 == "Son La province")
TableAEPSL <- join(TableAEPSL, ClusterID, by = "o9")
TableAEPSL$clust <- as.factor(TableAEPSL$clust)
for (i in 5:ncol(TableAEPSL)){
  y <- ggplot(TableAEPSL, aes(x= TableAEPSL[,i], fill = clust)) +
    geom_histogram(position = 'dodge',binwidth = max(TableAEPSL[,i],na.rm = TRUE)/10)+
    ggtitle(paste(colnames(TableAEPSL)[i]," /  N = ", nrow(TableAEPSL)))+
    xlab("number")
  print(y)
}

Anim <- TableAEPSL[,c(16,17,18,51)]
summary(Anim[Anim$clust == 4,])

#4.4. Vietnam DB clusters
TableAEPDB <- filter(TableAEP, o5 == "Dien Bien province")
TableAEPDB <- join(TableAEPDB, ClusterID, by = "o9")
TableAEPDB$clust <- as.factor(TableAEPDB$clust)
for (i in 5:ncol(TableAEPDB)){
  y <- ggplot(TableAEPDB, aes(x= TableAEPDB[,i], fill = clust)) +
    geom_histogram(position = 'dodge',binwidth = max(TableAEPDB[,i],na.rm = TRUE)/10)+
    ggtitle(paste(colnames(TableAEPDB)[i]," /  N = ", nrow(TableAEPDB)))+
    xlab("number")
  print(y)
}

Anim <- TableAEPDB[,c(16,17,18,51)]
summary(Anim[Anim$clust == 4,])
