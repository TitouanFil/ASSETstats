### 1. Libraries loading
library(FactoMineR)
library(factoextra)
library(corrplot)
library(missMDA)
library(tidyr)
library(plyr)
library(dplyr)
library(ExcelFunctionsR)
library(corrplot)

#WARNING: NA To be considered

### 2. Data import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")

### 3. Data preparation

#3.1 Forest area (ha): possiblement peu de données
#OK Nothing to do - Column 586

#3.2 Access to non-timber forest products: village data
#We prepare a column with the total nb of days dedicated to Natural resources collection
  HouseholdCambodia_2C$Sum <- rowSums(HouseholdCambodia_2C[,c(726,729,732,735,738,741,744,747,750,753,
                                                              756,759,762,765,768)], na.rm=TRUE)
#Limits: does not really represent access to NTFP as maybe longer for some households
  #Column 2575

#Script for all crops 
{
  #Lowland part
  #First we turn upland and lowland data to wide format (Duplicate alert = Normal,
  #we had no time to take care of it yet)
  dlowc2 <- ClowlandCambodia_2C[,c(2,3,4,10,12)]
  dlowc2$d2_132 <- as.character(dlowc2$d2_132)
  dlowc2$d2_132 <- as.numeric(dlowc2$d2_132)
  colnames(dlowc2)[2] <- "o9"
  dlowc2wide <- reshape(dlowc2, direction = "wide", idvar = "pid", timevar = "d2_13e",v.names= c("d2_132"))
  lowlandC <- dlowc2wide %>%
    group_by(o9) %>%
    summarize(across(3:18, sum, na.rm = T))
  #Upland part
  dupc2 <- CuplandCambodia_2C[,c(2,3,4,9,11)]
  dupc2$d2_232 <- as.character(dupc2$d2_232)
  dupc2$d2_232 <- as.numeric(dupc2$d2_232)
  colnames(dupc2)[2] <- "o9"
  dupc2wide <- reshape(dupc2, direction = "wide", idvar = "pid", timevar = "d2_23e",v.names= c("d2_232"))
  uplandC <- dupc2wide %>%
    group_by(o9) %>%
    summarize(across(3:19, sum, na.rm = T))
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, lowlandC, by = "o9")
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, uplandC, by = "o9")
  #3.3 Paddy area (ha)
  HouseholdCambodia_2C$Paddy <- rowSums(HouseholdCambodia_2C[,c(2577,2578)], na.rm = T)
  #3.4 Soybean area (ha)
  HouseholdCambodia_2C$Soybean <- rowSums(HouseholdCambodia_2C[,c(2598,2600,2589)], na.rm = T)
  #3.5 Cassava area (ha)
  HouseholdCambodia_2C$Cassava <- rowSums(HouseholdCambodia_2C[,c(2591,2593,2594,2602)], na.rm = T)
  #3.6 Upland rice area (ha)
  HouseholdCambodia_2C$UpRice <- rowSums(HouseholdCambodia_2C[,c(2579,2595,2607)], na.rm = T)
  #3.7 Fruit area (ha): total area of fruits
  HouseholdCambodia_2C$Fruit <- rowSums(HouseholdCambodia_2C[,c(2596,2599,2601,2603,2605,2606,2608)], na.rm = T)
  #3.8 Vegetables area (ha): total area
  HouseholdCambodia_2C$Vegetables <- rowSums(HouseholdCambodia_2C[,c(2580:2585,2587,2590)], na.rm = T)
  #3.9 Cashew area (ha)
  HouseholdCambodia_2C$Tea <- rowSums(HouseholdCambodia_2C[,c(2588,2591,2592,2594,2602,2604,2607)], na.rm = T)
  }
  
#Columns 2609:2615
#3.13 Nb of cattle heads
  HouseholdCambodia_2C$Calf <- HouseholdCambodia_2C[,2050]/2
  HouseholdCambodia_2C$Cattle <- rowSums(HouseholdCambodia_2C[,c(2049,2616)], na.rm = T)
  #Column 2617
#3.14 Nb of buffalo heads 
  HouseholdCambodia_2C$CalfBuff <- HouseholdCambodia_2C[,2048]/2
  HouseholdCambodia_2C$Buffalo <- rowSums(HouseholdCambodia_2C[,c(2047,2618)], na.rm = T)
  #Column 2619
#3.16 Nb of pig heads
  HouseholdCambodia_2C$Piglet <- HouseholdCambodia_2C[,2052]/2
  HouseholdCambodia_2C$Pig <- rowSums(HouseholdCambodia_2C[,c(2051,2620)], na.rm = T)
  #Column 2621
#3.17 Nb of poultry
  HouseholdCambodia_2C$Poultry <- rowSums(HouseholdCambodia_2C[,c(2059:2061)], na.rm = T)
  #Column 2622
#3.18 Nr of land uses: Lowland, Upland, Pasture, Fallow, Forest (?), Aquaculture land, Home garden
  for (i in 582:589){
    HouseholdCambodia_2C[,i] <- as.character(HouseholdCambodia_2C[,i])
    HouseholdCambodia_2C[,i] <- as.numeric(HouseholdCambodia_2C[,i])
  }
  HouseholdCambodia_2C$LU <- rowSums(HouseholdCambodia_2C[,c(582:589)], na.rm = T)
  #Column 2623

#3.19 Water pump system
  #Column 2482
  
#3.20 Irrigation system
  #Column 2483

  
  ----
  
#3.21 Distance of village from administrative centre: village data (proxy for remoteness and market access)
  Gps <- HouseholdCambodia_2C[,c(26,32,2342)]
  #Distance to Dien Bien Phu roundabout (21.392366, 103.015877) (Source: Google maps)
  #Doi 13 - Ban Men: 21.4315598 103.0068549: 8 km 
  #Doi 16 - Na Lom: 21.4224517 103.011962: 7km
  #Doi 22 - Phieng Ban: 21.4234433 102.9927883: 8km
  #Thon 4: 21.2919858 103.0111826: 12 km
  #Ban Na Ten: 21.2631914 102.9936858: 16 km
  #Ban Pa Nam: 21.2790777 102.9968519: 14 km
  #Ban Na Co: 21.2753262 102.9939047: 14 km
  #Ban Na Sang 1: 21.2649134 103.0954206: 25 km
  #Ban Ten Nua: 21.2449066 103.0599013: 21 km
  #Ban Huoi Hua: 21.2156664 103.0756501: 26 km
  
  #Distance to Moc Chau city (20.844233, 104.641171) (Source: Google maps)
  #Ban Po Nang: 21.01436 104.6006581: 39 km
  #Ban Ca Dac: 21.0088103 104.5681153: 41 km
  #Ban Sao Tua: 21.0104575 104.6815896: 51 km
  #Ban Tat Ngoang: 20.8860141 104.5726224: 16 km
  #Ban Pieng Lan: 20.8914476 104.555704: 15 km
  #Ban Ta Niet: 20.90093 104.5178967: 19 km
  #Ban Ta So 1: 20.9323353 104.5522915: 18 km
  #Ban Phieng Tien: 20.811099 104.7368085: 13 km
  #Ban Xom Lom: 20.8331891 104.7349254: 12 km
  #Ban Pieng Sang: 20.853961 104.75346: 18 km
list <- data.frame(village_eng_preload=c("Doi 13 - Ban Men","Doi 16 - Na Lom","Doi 22 - Phieng Ban","Thon 4","Ban Na Ten",
                             "Ban Pa Nam","Ban Na Co","Ban Na Sang 1","Ban Ten Nua","Ban Huoi Hua","Ban Po Nang",
                             "Ban Ca Dac","Ban Sao Tua","Ban Tat Ngoang","Ban Pieng Lan","Ban Ta Niet","Ban Ta So 1",
                             "Ban Phieng Tien","Ban Xom Lom","Ban Pieng Sang"),
                   "distance (km)"=c(8,7,8,12,16,14,14,25,21,26,39,41,51,16,15,19,18,13,12,18))
HouseholdCambodia_2C <- join(HouseholdCambodia_2C, list, by = "village_eng_preload")

#Column 2843

#3.22 Number of active household members
  Active <- HouMemberCambodia_2C %>% dplyr::filter(a6 == 1 | a6 == 2 | a6 == 3 |
                                                 a6 == 4 | a6 == 5 | a6 == 6 |
                                                  a6 == 7 | a6 == 9 | a6 == 11)
  Active <- Active %>% dplyr::count(hhid_re1)
  colnames(Active)[1] <- "o9"
  colnames(Active)[2] <- "Active"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, Active, by = "o9")
  HouseholdCambodia_2C[,2844] <- ifelse(is.na(HouseholdCambodia_2C[,2844]),'0',HouseholdCambodia_2C[,2844])
  
#3.22 Number of household members working on the farm
  FarmW <- HouMemberCambodia_2C %>% dplyr::filter(a6 == 1)
  FarmW <- FarmW %>% dplyr::count(hhid_re1)
  colnames(FarmW)[1] <- "o9"
  colnames(FarmW)[2] <- "FarmW"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, FarmW, by = "o9")
  HouseholdCambodia_2C[,2845] <- ifelse(is.na(HouseholdCambodia_2C[,2845]),'0',HouseholdCambodia_2C[,2845])
  
  #Column 2845

#3.23 Total number of household members
  #Column 59

#3.24 Nr of hired labor
  #Column 1855

#3.25 Off-farm income: % over total income/ main source of income of the households
  x <- HouseholdCambodia_2C[,c(1,144,150)]
  y <- HouseholdCambodia_2C[,c(1,146,152)]
  z <- HouseholdCambodia_2C[,c(1,148,154)]
x <- x %>% dplyr::filter(b4_1 == 12 | b4_1 == 13 | b4_1 == 14)
y <- y %>% dplyr::filter(b4_2 == 12 | b4_2 == 13 | b4_2 == 14)
z <- z %>% dplyr::filter(b4_3 == 12 | b4_3 == 13 | b4_3 == 14)
All <- merge(x,y, by = "o9", all = TRUE)
All <- merge(All,z, by = "o9", all = TRUE)
All$SumNF <- ifelse(!is.na(All$b5_1),All$b5_1,0) + ifelse(!is.na(All$b5_2),All$b5_2,0) + ifelse(!is.na(All$b5_3),All$b5_3,0)
All <- All[,c(1,8)]
HouseholdCambodia_2C <- join(HouseholdCambodia_2C, All, by = "o9")
x <- ifelse(!is.na(HouseholdCambodia_2C$b5_1),HouseholdCambodia_2C$b5_1,0)+
  ifelse(!is.na(HouseholdCambodia_2C$b5_2),HouseholdCambodia_2C$b5_2,0)+
  ifelse(!is.na(HouseholdCambodia_2C$b5_3),HouseholdCambodia_2C$b5_3,0)
#WARNING: It is possible that some households earn non-farm income but not as one of the 
#first source of revenue, it is then not considered here

#2846                

#3.26 % of hh members that migrated
  Migr <- HouMemberCambodia_2C %>% dplyr::filter(a10 == 1)
  Migr <- Migr %>% dplyr::count(hhid_re1)
  colnames(Migr)[1] <- "o9"
  colnames(Migr)[2] <- "Migr"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, Migr, by = "o9")
  HouseholdCambodia_2C$Migr <- as.numeric(HouseholdCambodia_2C$Migr)
  HouseholdCambodia_2C$Migr <- HouseholdCambodia_2C$Migr / HouseholdCambodia_2C$a0
  HouseholdCambodia_2C[,2847] <- ifelse(is.na(HouseholdCambodia_2C[,2847]),'0',HouseholdCambodia_2C[,2847])
  #Column 2847
  
#3.27 Final Table creation
  VarPCA <- HouseholdCambodia_2C[,c(23,32,59,758,1855,2278,2279,2731,2826:2832,2834,2836,2838,2840:2847)]
  colnames(VarPCA)[3:8] <- c("Hmembers","ForestArea","HiringW","WaterPump","Irrigation","NTFP")
  for (i in 3:26){
        VarPCA[,i] <- ifelse(is.na(VarPCA[,i]),'0',VarPCA[,i])
        VarPCA[,i] <- as.numeric(VarPCA[,i])
  }
  VarPCA$Irrigation <- ifelse(VarPCA$Irrigation == 2, 1, 0)
  VarPCA$WaterPump <- ifelse(VarPCA$WaterPump == 2, 1, 0)
#We split the table depending on the province
PCADB <- VarPCA[VarPCA$province_eng_preload == 'Dien Bien province',]
PCAMC <- VarPCA[VarPCA$province_eng_preload == 'Son La province',]


#3.28 Descriptive statistics
{
#Son La province
#Summary statistics
summary(PCAMC[,3:26])
#Histogramm for each variable
for (i in 3:26){
  x <- ggplot(PCAMC, aes(x= PCAMC[,i])) + geom_histogram(binwidth = max(PCAMC[,i],na.rm = TRUE)/20)+
    ggtitle(colnames(PCAMC)[i])+
    xlab("number")
  print(x)
}
#Matrix of correlation
res <- cor(PCAMC[,3:26])
round(res, 2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Dien Bien province
#Summary statistics
summary(PCADB[,3:26])
#We remove Macadamia and Tea
PCADB <- PCADB[,c(1:14,17:26)]
#Histogramm for each variable
for (i in 3:24){
  x <- ggplot(PCADB, aes(x= PCADB[,i])) + geom_histogram(binwidth = max(PCADB[,i],na.rm = TRUE)/20)+
    ggtitle(colnames(PCADB)[i])+
    xlab("number")
  print(x)
}
#Matrix of correlation
res <- cor(PCADB[,3:24])
round(res, 2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
}
  
## 4. PCA
#PCA - Moc Chau
PCAMC <- PCAMC[,-c(4:8,12,17,18,24)]
res.pca <- PCA(PCAMC[,c(3:17)], scale.unit = T, ncp = 12, graph = TRUE)
# Scale.unit: une valeur logique. Si TRUE, les donn?es sont standardis?es/normalis?es avant l'analyse.
# Ncp: nombre de dimensions conserv?es dans les r?sultats finaux.
# Graph: une valeur logique. Si TRUE un graphique est affich?.
#Get contributions
contrib <- as.data.frame(res.pca$var$contrib)
#Get eigen values
eig.val <- as.data.frame(get_eigenvalue(res.pca))
eig.val <- eig.val[c(1:16),]
#Merge it
contrib <- rbind(contrib,eig.val$eigenvalue)
contrib$contribTot <- 1
for (i in 1:(nrow(contrib)-1)){
  contrib[i,17] <- contrib[i,1]*contrib[24,1]+contrib[i,2]*contrib[24,2]+
    contrib[i,3]*contrib[24,3]+contrib[i,4]*contrib[24,4]+
    contrib[i,5]*contrib[24,5]+contrib[i,6]*contrib[24,6]+
    contrib[i,7]*contrib[24,7]+contrib[i,8]*contrib[24,8]+
    contrib[i,9]*contrib[24,9]+contrib[i,10]*contrib[24,10]+
    contrib[i,11]*contrib[24,11]+contrib[i,12]*contrib[24,12]+
    contrib[i,13]*contrib[24,13]+contrib[i,14]*contrib[24,14]+
    contrib[i,15]*contrib[24,15]+contrib[i,16]*contrib[24,16]
  }
contrib <- contrib[order(contrib$sum2),]
contrib$sum2 <- rowSums(contrib[,c(1:8)])

## 4. PCA results extraction
#Biplot with arrow for variables and individuals
fviz_pca_biplot(res.pca, label = "var", habillage=PCAMC$village_eng_preload,
                addEllipses=F, ellipse.level=0.95,
                ggtheme = theme_minimal())

## 5. AHC: https://www.youtube.com/watch?v=3tT29UtHqd0&t=34s
#RUn AHC
res.hcpc <- HCPC(res.pca)
#Add cluster numbers in the database:
Table1 <- res.hcpc$data.clust
summary(Table1$clust)
#Table export
write.csv2(Table1, file  = 'Datacluster.csv')
#Description des classes par les variables :
res.hcpc$desc.var
#Description par les individus :
res.hcpc$desc.ind
#Description par les axes :
res.hcpc$desc.axes

#PCA - Dien Bien
PCADB <- PCADB[,-c(4:7,12,13,15,18,26)]
res.pca <- PCA(PCADB[,c(3:17)], scale.unit = TRUE, ncp = 12, graph = TRUE)
