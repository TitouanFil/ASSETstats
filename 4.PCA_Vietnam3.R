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
{
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
    summarize(across(3:58, sum, na.rm = T))
  #Upland part
  dupc2 <- CuplandVietnam_2C[,c(1,2,3,8,10)]
  dupc2$d2_232 <- as.character(dupc2$d2_232)
  dupc2$d2_232 <- as.numeric(dupc2$d2_232)
  colnames(dupc2)[2] <- "o9"
  dupc2wide <- reshape(dupc2, direction = "wide", idvar = "pid", timevar = "d2_23e",v.names= c("d2_232"))
  uplandC <- dupc2wide %>%
    group_by(o9) %>%
    summarize(across(3:40, sum, na.rm = T))
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
                                                                   2768,2769,2771:2775,2777,2779:2781,2783,2784,2792,2799,
                                                                   2800,2802,2803,2806,2809,2810,2812,2813,2814,
                                                                   2816,2822,2824)], na.rm = T)
  
  ## 3.7 Tea area (ha)
  HouseholdVietnam_2C$'7.Tea' <- rowSums(HouseholdVietnam_2C[,c(2736,2794)], na.rm = T)
  }
  
#Columns 2826:2831
  
  ## 3.8 Nb of cattle heads
  HouseholdVietnam_2C$Calf <- HouseholdVietnam_2C[,1889]/2
  HouseholdVietnam_2C$'8.Cattle' <- rowSums(HouseholdVietnam_2C[,c(1888,2832)], na.rm = T)
  #Column 2833
  
  ## 3.9 Nb of buffalo heads 
  HouseholdVietnam_2C$CalfBuff <- HouseholdVietnam_2C[,1887]/2
  HouseholdVietnam_2C$'9.Buffalo' <- rowSums(HouseholdVietnam_2C[,c(1886,2834)], na.rm = T)
  #Column 2835

  ## 3.10 Nb of pig heads
  HouseholdVietnam_2C$Piglet <- HouseholdVietnam_2C[,1891]/2
  HouseholdVietnam_2C$'10.Pig' <- rowSums(HouseholdVietnam_2C[,c(1890,2836)], na.rm = T)
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

  ##3.14 Number of household members working on the farm
  FarmW <- HouMemberVietnam_2C %>% dplyr::filter(a6 == 1)
  FarmW <- FarmW %>% dplyr::count(hhid_re1)
  colnames(FarmW)[1] <- "o9"
  colnames(FarmW)[2] <- "14.FarmW"
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, FarmW, by = "o9")
  HouseholdVietnam_2C[,2841] <- ifelse(is.na(HouseholdVietnam_2C[,2841]),'0',HouseholdVietnam_2C[,2841])
  
  #Column 2841

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

#2842                

  ##3.17 % of hh members that migrate
  Migr <- HouMemberVietnam_2C %>% dplyr::filter(a10 == 1)
  Migr <- Migr %>% dplyr::count(hhid_re1)
  colnames(Migr)[1] <- "o9"
  colnames(Migr)[2] <- "17.%Migr"
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C, Migr, by = "o9")
  HouseholdVietnam_2C$'17.%Migr' <- as.numeric(HouseholdVietnam_2C$'17.%Migr')
  HouseholdVietnam_2C$'17.%Migr' <- HouseholdVietnam_2C$'17.%Migr' / HouseholdVietnam_2C$a0*100
  HouseholdVietnam_2C[,2843] <- ifelse(is.na(HouseholdVietnam_2C[,2843]),'0',HouseholdVietnam_2C[,2843])
  #Column 2843
  
  ##3.18 Nr of hired labor
  #Column 1855
  
  ##4. Final Table creation
  VarPCA <- HouseholdVietnam_2C[,c(23,32,2731,2826:2831,2833,2835,2837:2841,59,2842,2843,1855)]
  colnames(VarPCA)[c(15,17,20)] <- c("13.DistanceD","15.HouM","18.HiredW")
  for (i in 3:20){
        VarPCA[,i] <- ifelse(is.na(VarPCA[,i]),'0',VarPCA[,i])
        VarPCA[,i] <- as.numeric(VarPCA[,i])
  }
  #We split the table depending on the province
  PCAMC <- VarPCA[VarPCA$province_eng_preload == 'Son La province',]
  PCADB <- VarPCA[VarPCA$province_eng_preload == 'Dien Bien province',]

  #5. Descriptive statistics
  {
  #Son La province
  #Summary statistics
  summary(PCAMC[,c(4:10,12:15,17:20)])
  #Histogramm for each variable
  for (i in c(4:10,12:15,17:20)){
    x <- ggplot(PCAMC, aes(x= PCAMC[,i])) + geom_histogram(binwidth = max(PCAMC[,i],na.rm = TRUE)/20)+
      ggtitle(colnames(PCAMC)[i])+
      xlab("number")
    print(x)
  }
  #Matrix of correlation
  res <- cor(PCAMC[,c(4:10,12:15,17:20)])
  round(res, 2)
  corrplot(res, type = "upper", 
         tl.col = "black", tl.srt = 45)

  #Dien Bien province
  #Summary statistics
  summary(PCADB[,c(3:6,8,10:18,20)])
  #Histogramm for each variable
  for (i in c(3:6,8,10:18,20)){
    x <- ggplot(PCADB, aes(x= PCADB[,i])) + geom_histogram(binwidth = max(PCADB[,i],na.rm = TRUE)/20)+
      ggtitle(colnames(PCADB)[i])+
      xlab("number")
    print(x)
  }
  #Matrix of correlation
  res <- cor(PCADB[,c(3:6,8,10:18,20)])
  round(res, 2)
  corrplot(res, type = "upper", order = "hclust", 
          tl.col = "black", tl.srt = 45)
  }
  
  ## 7. PCA
  ## 7.1 PCA 
  #PCA - Moc Chau
  res.pca <- PCA(PCAMC[,c(4:10,12:15,17:20)], scale.unit = T, ncp = 12, graph = TRUE)
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

  ## 7.2 PCA results extraction - MOC Chau
  #Biplot with arrow for variables and individuals
  PCAMC$village_eng_preload <- as.character(PCAMC$village_eng_preload)
  PCAMC$village_eng_preload <- as.factor(PCAMC$village_eng_preload)
  fviz_pca_biplot(res.pca, label = "var", habillage=PCAMC$village_eng_preload,
                addEllipses = F, ellipse.level=0.95,
                ggtheme = theme_minimal())

  ## 7.3 AHC: https://www.youtube.com/watch?v=3tT29UtHqd0&t=34s
  #RUn AHC
  res.hcpc <- HCPC(res.pca)
  #Add cluster numbers in the database:
  Table1 <- res.hcpc$data.clust
  #Table export
  write.csv2(Table1, file  = 'Datacluster.csv')
  #Description des classes par les variables :
  res.hcpc$desc.var
  #Description par les individus :
  res.hcpc$desc.ind
  #Description par les axes :
  res.hcpc$desc.axes
  
  ## 7.4 AHC additionnal material
  
  #Number of households in each cluster
  summary(Table1$clust)
  
  #Missing average values - PCAMC
  AVERAGEIF(Table1$clust, "3", Table1$'3.Maize')
  AVERAGEIF(Table1$clust, "3", Table1$'5.Fruit')
  AVERAGEIF(Table1$clust, "2", Table1$'8.Cattle')
  AVERAGEIF(Table1$clust, "3", Table1$'10.Pig')
  AVERAGEIF(Table1$clust, "3", Table1$'11.Poultry')
  AVERAGEIF(Table1$clust, "2", Table1$'15.HouM')
  AVERAGEIF(Table1$clust, "1", Table1$'16.%incNF')
  AVERAGEIF(Table1$clust, "3", Table1$'16.%incNF')
  AVERAGEIF(Table1$clust, "2", Table1$'17.%Migr')
  AVERAGEIF(Table1$clust, "3", Table1$'18.HiredW')
  
  #Missing average values - PCADB
  AVERAGEIF(Table1$clust, "1", Table1$'1.NTFP')
  AVERAGEIF(Table1$clust, "2", Table1$'1.NTFP')
  AVERAGEIF(Table1$clust, "2", Table1$'3.Maize')
  AVERAGEIF(Table1$clust, "2", Table1$'8.Cattle')
  AVERAGEIF(Table1$clust, "3", Table1$'9.Buffalo')
  AVERAGEIF(Table1$clust, "3", Table1$'10.Pig')
  AVERAGEIF(Table1$clust, "3", Table1$'12.LU')
  AVERAGEIF(Table1$clust, "2", Table1$'18.HiredW')
  
  #Plotting variables depending on clusters - Moc Chau
  #Histogramm for each variable
  for (i in 1:15){
  x <- ggplot(Table1, aes(x= Table1[,i], fill = Table1$clust)) + geom_histogram(position = 'dodge',binwidth = max(Table1[,i],na.rm = TRUE)/10)+
    ggtitle(colnames(Table1[i]))+
    xlab("number")
  print(x)
  }


  #PCA - Dien Bien
  res.pca <- PCA(PCADB[,c(3:6,8,10:18,20)], scale.unit = TRUE, ncp = 12, graph = TRUE)
  
  ## 7.5 PCA results extraction - Moc Chau
  #Biplot with arrow for variables and individuals
  PCADB$village_eng_preload <- as.character(PCADB$village_eng_preload)
  PCADB$village_eng_preload <- as.factor(PCADB$village_eng_preload)
  fviz_pca_biplot(res.pca, label = "var", habillage=PCADB$village_eng_preload,
                  addEllipses = F, ellipse.level=0.95,
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
  