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
library(hrbrthemes)
library(rstatix)
library(stringr)

#WARNING: NA To be considered

### 2. Data import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")

### 3. Data preparation

## 3.1.	Number of days dedicated to Non Timber Forest Products collection each year (days)
#We prepare a column with the total nb of days dedicated to Natural resources collection
  HouseholdVietnam_2C$'1.NTFP' <- rowSums(HouseholdVietnam_2C[,c(780,783,786,789,792,795,798,
                                                          801,804,807,810,813,816,819,
                                                          822,825)], na.rm=TRUE)
#Limits: does not really represent access to NTFP as maybe longer for some households
  #Column 2731


#Script for all crops 
  #Lowland part
  #First we turn upland and lowland data to wide format (Duplicate alert = Normal,
  #we had no time to take care of it yet)
  dlowc2 <- ClowlandVietnam_2C[,c(1,2,3,9,11)]
  dlowc2$d2_132 <- as.character(dlowc2$d2_132)
  dlowc2$d2_132 <- as.numeric(dlowc2$d2_132)
  colnames(dlowc2)[2] <- "o9"
  dlowc2wide <- reshape(dlowc2, direction = "wide", idvar = "pid", timevar = "d2_13e",v.names= c("d2_132"))
  lowlandC <- dlowc2wide %>%
    group_by(o9) %>%
    dplyr::summarize(across(3:58, sum, na.rm = T))
  #Upland part
  dupc2 <- CuplandVietnam_2C[,c(1,2,3,8,10)]
  dupc2$d2_232 <- as.character(dupc2$d2_232)
  dupc2$d2_232 <- as.numeric(dupc2$d2_232)
  colnames(dupc2)[2] <- "o9"
  dupc2wide <- reshape(dupc2, direction = "wide", idvar = "pid", timevar = "d2_23e",v.names= c("d2_232"))
  uplandC <- dupc2wide %>%
    group_by(o9) %>%
    dplyr::summarize(across(3:40, sum, na.rm = T))
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, lowlandC, by = "o9")
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, uplandC, by = "o9")
  #3.2 Paddy area (ha)
  HouseholdVietnam_2C$'2.Paddy' <- rowSums(HouseholdVietnam_2C[,c(2732,2734,2791,2798)], na.rm = T)
  #3.3 Maize area (ha)
  HouseholdVietnam_2C$'3.Maize' <- rowSums(HouseholdVietnam_2C[,c(2742,2776,2790,2795)], na.rm = T)
  #3.4 Cassava area (ha)
  HouseholdVietnam_2C$'4.Cassava' <- rowSums(HouseholdVietnam_2C[,c(2770,2788)], na.rm = T)
  #3.5 Fruit area (ha): total area of fruits
  HouseholdVietnam_2C$'5.Fruit' <- rowSums(HouseholdVietnam_2C[,c(2738,2739,2743,2744,2747,2761,
                                                              2767,2785:2787,2789,2793,2796,2797,
                                                              2801,2804,2805,2807,2811,2815,2817:2819,2821)], na.rm = T)
  ## 3.6 Vegetables area (ha): total area - NOT USED ANYMORE
  HouseholdVietnam_2C$'6.Vegetables' <- rowSums(HouseholdVietnam_2C[,c(2733,2735,2737,2740,2741,2745,2746,2748:2760,2762:2766,
                                                                   2768,2769,2771:2775,2777,2779:2781,2783,2784,
                                                                   2800,2802,2810,2812,2813,2816,2822,2824)], na.rm = T)
  
  ## 3.7 Tea area (ha)
  HouseholdVietnam_2C$'7.Tea' <- rowSums(HouseholdVietnam_2C[,c(2736,2794)], na.rm = T)

  
#Columns 2826:2831
  
  ## 3.8 Nb of cattle & buffalo heads
  HouseholdVietnam_2C$Calf <- HouseholdVietnam_2C[,1889]/2
  HouseholdVietnam_2C$CalfBuff <- HouseholdVietnam_2C[,1887]/2
  HouseholdVietnam_2C$'CattleBuff' <- rowSums(HouseholdVietnam_2C[,c(1886,1888,2832,2833)], na.rm = T)
  #Column 2834

  ## 3.10 Nb of pig & goat heads
  HouseholdVietnam_2C$Piglet <- HouseholdVietnam_2C[,1891]/2
  HouseholdVietnam_2C$Kid <- HouseholdVietnam_2C[,1893]/2
  HouseholdVietnam_2C$'PigGoat' <- rowSums(HouseholdVietnam_2C[,c(1890,1892,2835,2836)], na.rm = T)
  #Column 2837
  
  ## 3.11 Nb of poultry
  HouseholdVietnam_2C$'11.Poultry' <- rowSums(HouseholdVietnam_2C[,c(1898:1901)], na.rm = T)
  #Column 2838
  
  ## 3.12 Nr of land uses: Lowland, Upland, Pasture, Fallow, Forest (?), Aquaculture land, Home garden
  for (i in 661:667){
    HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
    HouseholdVietnam_2C[,i] <- as.numeric(HouseholdVietnam_2C[,i])
  }
  HouseholdVietnam_2C$'12.LU' <- rowSums(HouseholdVietnam_2C[,c(661:667)], na.rm = T)
  #Column 2839

  ## 3.13 Distance of village from administrative centre: village data (proxy for remoteness and market access)
  Gps <- HouseholdVietnam_2C[,c(26,32,2342)]
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
                   "13.DistanceD"=c(8,7,8,12,16,14,14,25,21,26,39,41,51,16,15,19,18,13,12,18))
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, list, by = "village_eng_preload")

#Column 2840

#3.16 Number of active household members
HouMemberVietnam_2C$a6 <- as.numeric(HouMemberVietnam_2C$a6)
Active <- HouMemberVietnam_2C %>% dplyr::filter(a6 == 1 | a6 == 2 | a6 == 3 |
                                               a6 == 4 | a6 == 5 | a6 == 6 |
                                               a6 == 7 | a6 == 9 | a6 == 11)
Active <- Active %>% dplyr::count(hhid_re1)
colnames(Active)[1] <- "o9"
colnames(Active)[2] <- "Active"
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, Active, by = "o9")
x <- unique(HouseholdVietnam_2C$o9)
y <- unique(HouMemberVietnam_2C$hhid_re1)
z <- intersect(x,y)
a <- setdiff(x,z)
HouseholdVietnam_2C[,2841] <- ifelse(is.na(HouseholdVietnam_2C[,2841]) & !HouseholdVietnam_2C$o9 %in% a,
                                  0,HouseholdVietnam_2C[,2841])
#Column 2841

  ##3.14 Number of household members working on the farm
FarmW1 <- HouMemberVietnam_2C %>% dplyr::filter(a6 == 1)
FarmW1 <- FarmW1 %>% dplyr::count(hhid_re1)
colnames(FarmW1)[1] <- "o9"
colnames(FarmW1)[2] <- "FarmW1"
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, FarmW1, by = "o9")
FarmW2 <- HouMemberVietnam_2C %>% dplyr::filter(a7 == 1)
FarmW2 <- FarmW2 %>% dplyr::count(hhid_re1)
colnames(FarmW2)[1] <- "o9"
colnames(FarmW2)[2] <- "FarmW2"
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, FarmW2, by = "o9")
HouseholdVietnam_2C$FarmW1 <- as.numeric(HouseholdVietnam_2C$FarmW1)
HouseholdVietnam_2C$FarmW2 <- as.numeric(HouseholdVietnam_2C$FarmW2)
HouseholdVietnam_2C$FarmW <- ifelse(!is.na(HouseholdVietnam_2C$FarmW1),HouseholdVietnam_2C$FarmW1,
                                 0) + ifelse(!is.na(HouseholdVietnam_2C$FarmW2),
                                             (HouseholdVietnam_2C$FarmW2/2),0)
HouseholdVietnam_2C$FarmW <- ifelse(is.na(HouseholdVietnam_2C$FarmW1) & is.na(HouseholdVietnam_2C$FarmW2), NA, HouseholdVietnam_2C$FarmW)
HouseholdVietnam_2C[,2844] <- ifelse(is.na(HouseholdVietnam_2C[,2844]) & !HouseholdVietnam_2C$o9 %in% a,
                                  0,HouseholdVietnam_2C[,2844])
  
  #Column 2844

  ##3.15 Total number of household members
  #Column 59

  ##3.16 Off-farm income: % over total income/ main source of income of the households
  x <- HouseholdVietnam_2C[,c(1,144,150)]
  y <- HouseholdVietnam_2C[,c(1,146,152)]
  z <- HouseholdVietnam_2C[,c(1,148,154)]
  x <- x %>% dplyr::filter(b4_1 == 12 | b4_1 == 13 | b4_1 == 14)
  y <- y %>% dplyr::filter(b4_2 == 12 | b4_2 == 13 | b4_2 == 14)
  z <- z %>% dplyr::filter(b4_3 == 12 | b4_3 == 13 | b4_3 == 14)
  All <- merge(x,y, by = "o9", all = TRUE)
  All <- merge(All,z, by = "o9", all = TRUE)
  All$'16.%incNF' <- ifelse(!is.na(All$b5_1),All$b5_1,0) + ifelse(!is.na(All$b5_2),All$b5_2,0) + ifelse(!is.na(All$b5_3),All$b5_3,0)
  All <- All[,c(1,8)]
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, All, by = "o9")
  x <- ifelse(!is.na(HouseholdVietnam_2C$b5_1),HouseholdVietnam_2C$b5_1,0)+
  ifelse(!is.na(HouseholdVietnam_2C$b5_2),HouseholdVietnam_2C$b5_2,0)+
  ifelse(!is.na(HouseholdVietnam_2C$b5_3),HouseholdVietnam_2C$b5_3,0)
#WARNING: It is possible that some households earn non-farm income but not as one of the 
#first source of revenue, it is then not considered here
#2845                

  ##3.17 % of hh members that migrate
  Migr <- HouMemberVietnam_2C %>% dplyr::filter(a10 == 1)
  Migr <- Migr %>% dplyr::count(hhid_re1)
  colnames(Migr)[1] <- "o9"
  colnames(Migr)[2] <- "Migr"
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, Migr, by = "o9")
  HouseholdVietnam_2C$Migr <- as.numeric(HouseholdVietnam_2C$Migr)
  HouseholdVietnam_2C$Migr2 <- ifelse(is.na(HouseholdVietnam_2C$a14),0,HouseholdVietnam_2C$a14) +
    ifelse(is.na(HouseholdVietnam_2C$Migr),0,HouseholdVietnam_2C$Migr)
  HouseholdVietnam_2C$Migr2 <- HouseholdVietnam_2C$Migr2 / HouseholdVietnam_2C$Active
  HouseholdVietnam_2C[,2847] <- ifelse(is.na(HouseholdVietnam_2C[,2847]) & !HouseholdVietnam_2C$o9 %in% a,
                                    0,HouseholdVietnam_2C[,2847])
  HouseholdVietnam_2C$Migr2 <- ifelse(HouseholdVietnam_2C$Migr2 >= 1 | HouseholdVietnam_2C$Migr2 == 'inf',0.75,HouseholdVietnam_2C$Migr2)
  #Column 2847
  
  ##3.18 Nr of hired labor
  #Column 1855
  
  ##4. Final Table creation
  VarPCA <- HouseholdVietnam_2C[,c(1,23,32,1855,2731,2826:2831,2834,2837,2839,2840,2841,2844,2845,2847)]
  colnames(VarPCA) <- c("ID","Province","Village","1.HLabor","2.NTFP","3.Paddy","4.Maize","5.Cassava",
                        "6.Fruit","7.Vegetables","8.Tea","9.CattleBuff","10.PigGoat",
                        "11.LU","12.DistanceP","13.Active","14.FarmW","15.%incNF","16.%Migr")
  for (i in c(4,18)){
        VarPCA[,i] <- ifelse(is.na(VarPCA[,i]),'0',VarPCA[,i])
        VarPCA[,i] <- as.numeric(VarPCA[,i])
  }
  #We split the table depending on the province
  PCAMC <- VarPCA[VarPCA$Province == 'Son La province',]
  PCADB <- VarPCA[VarPCA$Province == 'Dien Bien province',]
  # Removing of households without land or animals
  #Crops / Land
  CropCheck <- HouseholdVietnam_2C[,c(1,661:668,676,1870,1886:1903)]
  #No household with neither crops nor animals

  #5. Descriptive statistics
  {
  #Son La province
  #Summary statistics
  summary(PCAMC[,c(4:18)])
  #Histogramm for each variable
  for (i in c(4:18)){
    x <- ggplot(PCAMC, aes(x= PCAMC[,i])) + geom_histogram(binwidth = max(PCAMC[,i],na.rm = TRUE)/20)+
      ggtitle(colnames(PCAMC)[i])+
      xlab("number")
    print(x)
  }
  #Matrix of correlation
    res <- cor(PCAMC[,c(4:18)], use="pairwise.complete.obs")
    round(res, 2)
    corrplot(res, type = "upper", 
             tl.col = "black", tl.srt = 45)

  #Dien Bien province
  #Summary statistics
  summary(PCADB[,c(4:18)])
  #Histogramm for each variable
  for (i in c(4:18)){
    x <- ggplot(PCADB, aes(x= PCADB[,i])) + geom_histogram(binwidth = max(PCADB[,i],na.rm = TRUE)/20)+
      ggtitle(colnames(PCADB)[i])+
      xlab("number")
    print(x)
  }
  #Matrix of correlation
  res <- cor(PCADB[,c(4:10,12:18)], use="pairwise.complete.obs")
  round(res, 2)
  corrplot(res, type = "upper", 
           tl.col = "black", tl.srt = 45)
  }
  
  ## 7. PCA
  ## 7.1 PCA 
  #PCA - Moc Chau
  PCAMC = PCAMC[PCAMC$ID != 610,]
  PCAMC = PCAMC[PCAMC$ID != 606,]
  res.pca <- PCA(PCAMC[,c(1:2,4:18)], scale.unit = T, ncp = 15, graph = TRUE,
                 quali.sup = c("ID","Province"))
  # Scale.unit: une valeur logique. Si TRUE, les donn?es sont standardis?es/normalis?es avant l'analyse.
  # Ncp: nombre de dimensions conserv?es dans les r?sultats finaux.
  # Graph: une valeur logique. Si TRUE un graphique est affich?.
  #Get contributions
  contrib <- as.data.frame(res.pca$var$contrib)
  #Get eigen values
  eig.val <- as.data.frame(get_eigenvalue(res.pca))

  ## 7.2 PCA results extraction - MOC Chau
  #Biplot with arrow for variables and individuals
  PCAMC$Village <- as.character(PCAMC$Village)
  PCAMC$Village <- as.factor(PCAMC$Village)
  Mypalette <- c("#6633FF","#CC6600","#CCFF00","#CC0000","#3399FF","#330066",
                "#66CC00","#336600","#33FF99","#FF6633")
  fviz_pca_biplot(res.pca, label = "var", habillage=PCAMC$Village,
                palette = Mypalette, addEllipses = F, ellipse.level=0.95,
                ggtheme = theme_minimal())

  ## 7.3 AHC: https://www.youtube.com/watch?v=3tT29UtHqd0&t=34s
  #RUn AHC
  res.hcpc <- HCPC(res.pca)
  #Add cluster numbers in the database:
  TableMC <- res.hcpc$data.clust
  #Table export
  write.csv2(Table1, file  = 'Datacluster.csv')
  #Description des classes par les variables :
  res.hcpc$desc.var
  #Description par les individus :
  res.hcpc$desc.ind
  #Description par les axes :
  res.hcpc$desc.axes
  
  ## 7.4 AHC additionnal material
  
  #Additionnal information
  #Product selling
  TableMC$ID <- str_replace(TableMC$ID, "ID_", "")
  colnames(TableMC)[1] <- "o9"
  Selling <- join(TableMC[,c(1,18)],HouseholdVietnam_2C[,c(1,80)], by = "o9")
  table(Selling[,c(2,3)])
  TableDB$ID <- str_replace(TableDB$ID, "ID_", "")
  colnames(TableDB)[1] <- "o9"
  Selling <- join(TableDB[,c(1,14)],HouseholdVietnam_2C[,c(1,80)], by = "o9")
  table(Selling[,c(2,3)])
  #Maize upland or lowland
  MC <- HouseholdVietnam_2C[HouseholdVietnam_2C$province_eng_preload == "Son La province",]
  sum(MC[,c(2742)], na.rm = T) / sum(MC[,c(2742,2776,2790)], na.rm = T)
  sum(MC[,c(2776)], na.rm = T) / sum(MC[,c(2742,2776,2790)], na.rm = T)
  sum(MC[,c(2790)], na.rm = T) / sum(MC[,c(2742,2776,2790)], na.rm = T)

  DB <- HouseholdVietnam_2C[HouseholdVietnam_2C$province_eng_preload == "Dien Bien province",]
  sum(DB[,c(2742)], na.rm = T) / sum(DB[,c(2742,2776,2790)], na.rm = T)
  sum(DB[,c(2776)], na.rm = T) / sum(DB[,c(2742,2776,2790)], na.rm = T)
  sum(DB[,c(2790)], na.rm = T) / sum(DB[,c(2742,2776,2790)], na.rm = T)
  Corn <- join(TableDB[,c(1,14)],HouseholdVietnam_2C[,c(1,2742,2776,2790)], by = "o9")
  Corn %>%
    group_by(clust) %>% 
    get_summary_stats(`d2_132.Maize (corn)`, type = "mean")
  Corn %>%
    group_by(clust) %>% 
    get_summary_stats(`d2_132.Maize biomass` , type = "mean")
  Corn %>%
    group_by(clust) %>% 
    get_summary_stats(`d2_232.Maize (corn)` , type = "mean")
  
  #Number of households in each cluster
  summary(Table1$clust)
  
  #Missing average values - PCAMC
  AVERAGEIF(TableMC$clust, "1", TableMC$'1.HLabor')
  AVERAGEIF(TableMC$clust, "4", TableMC$'1.HLabor')
  AVERAGEIF(TableMC$clust, "1", TableMC$'2.NTFP')
  AVERAGEIF(TableMC$clust, "4", TableMC$'2.NTFP')
  AVERAGEIF(TableMC$clust, "1", TableMC$'4.Maize')
  AVERAGEIF(TableMC$clust, "4", TableMC$'4.Maize')
  AVERAGEIF(TableMC$clust, "1", TableMC$'6.Fruit')
  AVERAGEIF(TableMC$clust, "4", TableMC$'6.Fruit')
  AVERAGEIF(TableMC$clust, "1", TableMC$'7.Vegetables')
  AVERAGEIF(TableMC$clust, "1", TableMC$'8.Tea')
  AVERAGEIF(TableMC$clust, "3", TableMC$'9.CattleBuff')
  AVERAGEIF(TableMC$clust, "4", TableMC$'10.PigGoat')
  AVERAGEIF(TableMC$clust, "1", TableMC$'12.DistanceP')
  AVERAGEIF(TableMC$clust, "2", TableMC$'13.Active')
  AVERAGEIF(TableMC$clust, "3", TableMC$'13.Active')
  AVERAGEIF(TableMC$clust, "3", TableMC$'14.FarmW')
  AVERAGEIF(TableMC$clust, "4", TableMC$'14.FarmW')
  AVERAGEIF(TableMC$clust, "4", TableMC$'15.%incNF')
  
  
  #Missing average values - PCADB
  AVERAGEIF(TableDB$clust, "3", TableDB$'3.Paddy')
  AVERAGEIF(TableDB$clust, "2", TableDB$'4.Maize')
  AVERAGEIF(TableDB$clust, "3", TableDB$'4.Maize')
  AVERAGEIF(TableDB$clust, "4", TableDB$'7.Vegetables')
  AVERAGEIF(TableDB$clust, "2", TableDB$'9.CattleBuff')
  AVERAGEIF(TableDB$clust, "3", TableDB$'9.CattleBuff')
  AVERAGEIF(TableDB$clust, "3", TableDB$'10.PigGoat')
  AVERAGEIF(TableDB$clust, "1", TableDB$'11.LU')
  AVERAGEIF(TableDB$clust, "2", TableDB$'11.LU')
  AVERAGEIF(TableDB$clust, "3", TableDB$'11.LU')
  AVERAGEIF(TableDB$clust, "3", TableDB$'13.Active')

  #Plotting variables depending on clusters - Moc Chau
  #Histogramm for each variable
  for (i in 3:18){
  x <- ggplot(TableDB, aes(x= TableDB[,i], fill = TableDB$clust)) + geom_histogram(position = 'dodge',binwidth = max(TableDB[,i],na.rm = TRUE)/10)+
    ggtitle(colnames(TableDB[i]))+
    xlab("number")
  print(x)
  }


  #PCA - Dien Bien
  res.pca <- PCA(PCADB[,c(1:2,6:8,10,12:18)], scale.unit = TRUE, ncp = 14, graph = TRUE,
                 quali.sup = c("ID","Province"))
  
  ## 7.5 PCA results extraction - Moc Chau
  #Biplot with arrow for variables and individuals
  PCADB$Village <- as.character(PCADB$Village)
  PCADB$Village <- as.factor(PCADB$Village)
  Mypalette <- c("#6633FF","#CCFF00","#3399FF","#66CC00","#336600","#330066","#33FF99",
                 "#003300","#00FFCC","#999900")
  fviz_pca_biplot(res.pca, label = "var", habillage=PCADB$Village,
                  palette = Mypalette, addEllipses = F, ellipse.level=0.95,
                  ggtheme = theme_minimal())

  
  ## 7.6 Vilage coordinates gathering
  Coordinates <- HouseholdVietnam_2C[,c(32,2343:2345)]
  Lat <- Coordinates %>% group_by(village_eng_preload) %>%
    get_summary_stats(`___gps_latitude`, type = "mean")
  Long <- Coordinates %>% group_by(village_eng_preload) %>%
    get_summary_stats(`___gps_longitude`, type = "mean")
  Alt <- Coordinates %>% group_by(village_eng_preload) %>%
    get_summary_stats(`___gps_altitude`, type = "mean")
  Coordinates <- cbind(Lat[,c(1,3,4)],Long[,4],Alt[,4])
  colnames(Coordinates)[3:5] <- c("Latitude","Longitude","Altitude")
  #Table export
  write.table(Coordinates, file  = 'CoordinatesVietnam.csv', sep = ',', dec = '.', row.names = F)
  