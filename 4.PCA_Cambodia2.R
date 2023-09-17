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
library(rstatix)

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
#OK Nothing to do - Column 705

#3.2 Access to non-timber forest products: village data
#We prepare a column with the total nb of days dedicated to Natural resources collection
  HouseholdCambodia_2C$Sum <- rowSums(HouseholdCambodia_2C[,c(726,729,732,735,738,741,744,747,750,753,
                                                              756,759,762,765,768)], na.rm=TRUE)
#Limits: does not really represent access to NTFP as maybe longer for some households
  #Column 2560

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
    dplyr::summarize(across(3:18, sum, na.rm = T))
  #Upland part
  dupc2 <- CuplandCambodia_2C[,c(2,3,4,9,11)]
  dupc2$d2_232 <- as.character(dupc2$d2_232)
  dupc2$d2_232 <- as.numeric(dupc2$d2_232)
  colnames(dupc2)[2] <- "o9"
  dupc2wide <- reshape(dupc2, direction = "wide", idvar = "pid", timevar = "d2_23e",v.names= c("d2_232"))
  uplandC <- dupc2wide %>%
    group_by(o9) %>%
    dplyr::summarize(across(3:19, sum, na.rm = T))
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, lowlandC, by = "o9")
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, uplandC, by = "o9")
  #3.3 Paddy area (ha)
  HouseholdCambodia_2C$Paddy <- rowSums(HouseholdCambodia_2C[,c(2561,2562)], na.rm = T)
  #3.4 Soybean area (ha)
  HouseholdCambodia_2C$Soybean <- rowSums(HouseholdCambodia_2C[,c(2567,2578,2582)], na.rm = T)
  #3.5 Cassava area (ha)
  HouseholdCambodia_2C$Cassava <- rowSums(HouseholdCambodia_2C[,c(2564,2566,2577,2579:2580)], na.rm = T)
  #3.6 Upland rice area (ha)
  HouseholdCambodia_2C$UpRice <- rowSums(HouseholdCambodia_2C[,c(2568,2583,2591)], na.rm = T)
  #3.7 Fruit area (ha): total area of fruits
  HouseholdCambodia_2C$Fruit <- rowSums(HouseholdCambodia_2C[,c(2584:2589,2593)], na.rm = T)
  #3.8 Vegetables area (ha): total area
  HouseholdCambodia_2C$Vegetables <- rowSums(HouseholdCambodia_2C[,c(2569:2576)], na.rm = T)
  #3.9 Cashew area (ha)
  HouseholdCambodia_2C$Cashew <- rowSums(HouseholdCambodia_2C[,c(2565,2566,2577:2579,2581,2591,2592)], na.rm = T)
  }
#2594:2600

  
#3.13 Nb of cattle & buffalo heads
  HouseholdCambodia_2C$Calf <- HouseholdCambodia_2C[,2050]/2
  HouseholdCambodia_2C$CalfBuff <- HouseholdCambodia_2C[,2048]/2
  HouseholdCambodia_2C$CattleBuff <- rowSums(HouseholdCambodia_2C[,c(2047,2049,2601,2602)], na.rm = T)
  #Column 2603

#3.16 Nb of pig heads
  HouseholdCambodia_2C$Piglet <- HouseholdCambodia_2C[,2052]/2
  HouseholdCambodia_2C$Pig <- rowSums(HouseholdCambodia_2C[,c(2051,2604)], na.rm = T)
  #Column 2605
  
#3.18 Nr of land uses: Lowland, Upland, Pasture, Fallow, Forest (?), Aquaculture land, Home garden
  for (i in 582:589){
    HouseholdCambodia_2C[,i] <- as.character(HouseholdCambodia_2C[,i])
    HouseholdCambodia_2C[,i] <- as.numeric(HouseholdCambodia_2C[,i])
  }
  HouseholdCambodia_2C$LU <- rowSums(HouseholdCambodia_2C[,c(582:589)], na.rm = T)
  #Column 2606

#3.22 Number of active household members
  HouMemberCambodia_2C$a6 <- as.numeric(HouMemberCambodia_2C$a6)
  Active <- HouMemberCambodia_2C %>% dplyr::filter(a6 == 1 | a6 == 2 | a6 == 3 |
                                                 a6 == 4 | a6 == 5 | a6 == 6 |
                                                  a6 == 7 | a6 == 9 | a6 == 11)
  Active <- Active %>% dplyr::count(hhid_re1)
  colnames(Active)[1] <- "o9"
  colnames(Active)[2] <- "Active"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, Active, by = "o9")

#Column 2607
  
#3.22 Number of household members working on the farm
  FarmW1 <- HouMemberCambodia_2C %>% dplyr::filter(a6 == 1)
  FarmW1 <- FarmW1 %>% dplyr::count(hhid_re1)
  colnames(FarmW1)[1] <- "o9"
  colnames(FarmW1)[2] <- "FarmW1"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, FarmW1, by = "o9")
  FarmW2 <- HouMemberCambodia_2C %>% dplyr::filter(a7 == "Agricultural work on their own farm (including livestock management)")
  FarmW2 <- FarmW2 %>% dplyr::count(hhid_re1)
  colnames(FarmW2)[1] <- "o9"
  colnames(FarmW2)[2] <- "FarmW2"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, FarmW2, by = "o9")
  HouseholdCambodia_2C$FarmW1 <- as.numeric(HouseholdCambodia_2C$FarmW1)
  HouseholdCambodia_2C$FarmW2 <- as.numeric(HouseholdCambodia_2C$FarmW2)
  HouseholdCambodia_2C$FarmW <- ifelse(!is.na(HouseholdCambodia_2C$FarmW1),HouseholdCambodia_2C$FarmW1,
                                      0) + ifelse(!is.na(HouseholdCambodia_2C$FarmW2),
                                                  (HouseholdCambodia_2C$FarmW2/2),0)
  

  #Column 2610

#3.24 Nr of hired labor
  #Column 2015

#3.25 Off-farm income: % over total income/ main source of income of the households
  x <- HouseholdCambodia_2C[,c(1,120,126)]
  y <- HouseholdCambodia_2C[,c(1,122,128)]
  z <- HouseholdCambodia_2C[,c(1,124,130)]
x <- x %>% dplyr::filter(b4_1 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_1 == "Non-farm wages/salary (salaried work in private or public company)" |
                           b4_1 == "Other")
y <- y %>% dplyr::filter(b4_2 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_2 == "Non-farm wages/salary (salaried work in private or public company)" |
                           b4_2 == "Other")
z <- z %>% dplyr::filter(b4_3 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_3 == "Non-farm wages/salary (salaried work in private or public company)" |
                           b4_3 == "Other")
All <- merge(x,y, by = "o9", all = TRUE)
All <- merge(All,z, by = "o9", all = TRUE)
All$SumNF <- ifelse(!is.na(All$b5_1),All$b5_1,0) + ifelse(!is.na(All$b5_2),All$b5_2,0) + ifelse(!is.na(All$b5_3),All$b5_3,0)
All <- All[,c(1,8)]
HouseholdCambodia_2C <- join(HouseholdCambodia_2C, All, by = "o9")
#WARNING: It is possible that some households earn non-farm income but not as one of the 
#first source of revenue, it is then not considered here

#2611                

#3.26 % of hh members that migrated
  Migr <- HouMemberCambodia_2C %>% dplyr::filter(a10 == "Yes")
  Migr <- Migr %>% dplyr::count(hhid_re1)
  colnames(Migr)[1] <- "o9"
  colnames(Migr)[2] <- "Migr"
  HouseholdCambodia_2C <- join(HouseholdCambodia_2C, Migr, by = "o9")
  HouseholdCambodia_2C$Migr <- HouseholdCambodia_2C$Migr / HouseholdCambodia_2C$Active
  HouseholdCambodia_2C[,2612] <- ifelse(is.na(HouseholdCambodia_2C[,2612]),
                                       0,HouseholdCambodia_2C[,2612])
  HouseholdCambodia_2C$Migr <- ifelse(HouseholdCambodia_2C$Migr >= 1,0.75,HouseholdCambodia_2C$Migr)
  #Column 2612
  
#3.27 Final Table creation
  VarPCA <- HouseholdCambodia_2C[,c(1,32,705,2015,2560,2594:2600,2603,2605:2607,2610:2612)]
  colnames(VarPCA) <- c("ID","Commune","1.Forest","15.HiredL","2.NTFP","3.Paddy","4.Soybean",
                        "5.Cassava","6. Upland rice","7.Fruit","8.Vegetables","9.Cashew",
                        "10.CattleBuff","11.Pig","12.LU","14.Active","13.FarmW","16.%incNF",
                        "17.%Migr")
  
  for (i in c(3:4,16,18)){
        VarPCA[,i] <- ifelse(is.na(VarPCA[,i]),'0',VarPCA[,i])
        VarPCA[,i] <- as.numeric(VarPCA[,i])
  }
  # Removing of households without land or animals
  #Crops / Land
  CropCheck <- HouseholdCambodia_2C[,c(1,582:590,600,2031,2047:2063)]
  #No household with neither crops nor animals
  
sum(VarPCA$'3.Paddy' == 0) / nrow(VarPCA)



#3.28 Descriptive statistics

#Summary statistics
summary(VarPCA[,3:19])
#Histogramm for each variable
for (i in 3:19){
  x <- ggplot(VarPCA, aes(x= VarPCA[,i])) + geom_histogram(binwidth = max(VarPCA[,i],na.rm = TRUE)/20)+
    ggtitle(colnames(VarPCA)[i])+
    xlab("number")
  print(x)
}
#Matrix of correlation
res <- cor(VarPCA[,3:19])
round(res, 2)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

  
## 4. PCA
#PCA - Rovieng
PCAR <- VarPCA[,-c(7,9:11,18)]
res.pca <- PCA(PCAR, scale.unit = T, ncp = 12, graph = TRUE,
               quali.sup = c(1:2))
# Scale.unit: une valeur logique. Si TRUE, les donn?es sont standardis?es/normalis?es avant l'analyse.
# Ncp: nombre de dimensions conserv?es dans les r?sultats finaux.
# Graph: une valeur logique. Si TRUE un graphique est affich?.
#Get contributions
contrib <- as.data.frame(res.pca$var$contrib)
#Get eigen values
eig.val <- as.data.frame(get_eigenvalue(res.pca))
#eig.val <- eig.val[c(1:23),]

## 4. PCA results extraction
#Biplot with arrow for variables and individuals
fviz_pca_biplot(res.pca, label = "var", habillage=VarPCA$Commune,
                addEllipses=F, ellipse.level=0.95,
                ggtheme = theme_minimal())

## 5. AHC: https://www.youtube.com/watch?v=3tT29UtHqd0&t=34s
#RUn AHC
res.hcpc <- HCPC(res.pca)
#Add cluster numbers in the database:
TableCambo <- res.hcpc$data.clust
summary(Table1$clust)
#Table export
write.csv2(Table1, file  = 'Datacluster.csv')
#Description des classes par les variables :
res.hcpc$desc.var
#Description par les individus :
res.hcpc$desc.ind
#Description par les axes :
res.hcpc$desc.axes


#Missing average values - PCAR
AVERAGEIF(Table1$clust, "2", Table1$'1.Forest')
AVERAGEIF(Table1$clust, "2", Table1$'2.NTFP')
AVERAGEIF(Table1$clust, "4", Table1$'2.NTFP')
AVERAGEIF(Table1$clust, "4", Table1$'3.Paddy')
AVERAGEIF(Table1$clust, "2", Table1$'5.Cassava')
AVERAGEIF(Table1$clust, "2", Table1$'9.Cashew')
AVERAGEIF(Table1$clust, "2", Table1$'11.Pig')
AVERAGEIF(Table1$clust, "4", Table1$'11.Pig')
AVERAGEIF(Table1$clust, "2", Table1$'12.LU')
AVERAGEIF(Table1$clust, "4", Table1$'14.Active')
AVERAGEIF(Table1$clust, "3", Table1$'15.HiredL')
AVERAGEIF(Table1$clust, "4", Table1$'17.Migr%')

#Histogramm for each variable
for (i in 3:15){
  x <- ggplot(TableCambo, aes(x= TableCambo[,i], fill = TableCambo$clust)) + geom_histogram(position = 'dodge',binwidth = max(TableCambo[,i],na.rm = TRUE)/10)+
    ggtitle(colnames(TableCambo[i]))+
    xlab("number")
  print(x)
}

## 7.6 Vilage coordinates gathering
Coordinates <- HouseholdCambodia_2C[,c(32,2523:2525)]
Coordinates$`___gps_latitude` <- as.character(Coordinates$`___gps_latitude`)
Coordinates$`___gps_longitude` <- as.character(Coordinates$`___gps_longitude`)
Coordinates$`___gps_altitude` <- as.character(Coordinates$`___gps_altitude`)
Coordinates$`___gps_latitude` <- as.numeric(Coordinates$`___gps_latitude`)
Coordinates$`___gps_longitude` <- as.numeric(Coordinates$`___gps_longitude`)
Coordinates$`___gps_altitude` <- as.numeric(Coordinates$`___gps_altitude`)
Lat <- Coordinates %>% group_by(village_eng_preload) %>%
  get_summary_stats(`___gps_latitude`, type = "mean")
Long <- Coordinates %>% group_by(village_eng_preload) %>%
  get_summary_stats(`___gps_longitude`, type = "mean")
Alt <- Coordinates %>% group_by(village_eng_preload) %>%
  get_summary_stats(`___gps_altitude`, type = "mean")
Coordinates <- cbind(Lat[,c(1,3,4)],Long[,4],Alt[,4])
colnames(Coordinates)[3:5] <- c("Latitude","Longitude","Altitude")
#Table export
write.table(Coordinates, file  = 'CoordinatesCambodia.csv', sep = ',', dec = '.', row.names = F)
