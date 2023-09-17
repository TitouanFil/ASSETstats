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
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
ClowlandLaos_2C <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2C <- readRDS("CuplandLaos_2.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")

### 3. Data preparation

#3.1 Access to non-timber forest products: village data
#We prepare a column with the total nb of days dedicated to Natural resources collection
  HouseholdLaos_2C$Sum <- rowSums(HouseholdLaos_2C[,c(737,740,743,746,749,752,755,758,761,
                                                      764,767,770,773,776,779,782)],
                                                    na.rm=TRUE)
#Limits: does not really represent access to NTFP as maybe longer for some households
  #Column 2320
  
#Script for all crops 
{
  #Lowland part
  #First we turn upland and lowland data to wide format (Duplicate alert = Normal,
  #we had no time to take care of it yet)
  dlowc2 <- ClowlandLaos_2C[,c(1,2,3,9,11)]
  dlowc2$d2_132 <- as.character(dlowc2$d2_132)
  dlowc2$d2_132 <- as.numeric(dlowc2$d2_132)
  colnames(dlowc2)[2] <- "o9"
  dlowc2wide <- reshape(dlowc2, direction = "wide", idvar = "pid", timevar = "d2_13e",v.names= c("d2_132"))
  lowlandC <- dlowc2wide %>%
    group_by(o9) %>%
    dplyr::summarize(across(3:31, sum, na.rm = T))
  #Upland part
  dupc2 <- CuplandLaos_2C[,c(1,2,3,8,10)]
  dupc2$d2_232 <- as.character(dupc2$d2_232)
  dupc2$d2_232 <- as.numeric(dupc2$d2_232)
  colnames(dupc2)[2] <- "o9"
  dupc2wide <- reshape(dupc2, direction = "wide", idvar = "pid", timevar = "d2_23e",v.names= c("d2_232"))
  uplandC <- dupc2wide %>%
    group_by(o9) %>%
    dplyr::summarize(across(3:34, sum, na.rm = T))
  HouseholdLaos_2C <- join(HouseholdLaos_2C, lowlandC, by = "o9")
  HouseholdLaos_2C <- join(HouseholdLaos_2C, uplandC, by = "o9")
  #3.2 Paddy area (ha)
  HouseholdLaos_2C$Paddy <- rowSums(HouseholdLaos_2C[,c(2321,2355)], na.rm = T)
  #3.3 Maize area (ha)
  HouseholdLaos_2C$Maize <- rowSums(HouseholdLaos_2C[,c(2323,2328,2351:2352)], na.rm = T)
  #3.4 Cassava area (ha)
  HouseholdLaos_2C$Cassava <- HouseholdLaos_2C[,2373]
  #3.5 Upland rice area (ha)
  HouseholdLaos_2C$UpRice <- rowSums(HouseholdLaos_2C[,c(2348,2350)], na.rm = T)
  #3.6 Fruit area (ha): total area of fruits
  HouseholdLaos_2C$Fruit <- rowSums(HouseholdLaos_2C[,c(2337,2347,2362,2372,2375,2379:2380)], na.rm = T)
  #3.7 Vegetables area (ha): total area
  HouseholdLaos_2C$Vegetables <- rowSums(HouseholdLaos_2C[,c(2324:2327,2329:2336,2338:2340,2342:2345,2349,
                                                             2356:2360,2363:2367,2369,2370,
                                                             2376:2378)], na.rm = T)
  HouseholdLaos_2C$Vegetables <- ifelse(HouseholdLaos_2C$Vegetables == 70000, 0, HouseholdLaos_2C$Vegetables)
  #3.8 Tea area (ha)
  HouseholdLaos_2C$Tea <- HouseholdLaos_2C[,2368]
  }
#2382:2388
  
  
#3.9 Nb of cattle & Buffalo heads
  HouseholdLaos_2C$Calf <- HouseholdLaos_2C[,1857]/2
  HouseholdLaos_2C$CalfBuff <- HouseholdLaos_2C[,1855]/2
  HouseholdLaos_2C$CattleBuff <- rowSums(HouseholdLaos_2C[,c(1854,1856,2389,2390)], na.rm = T)
  #Column 2390
#3.10 Nb of pig & goat heads
  HouseholdLaos_2C$Piglet <- HouseholdLaos_2C[,1859]/2
  HouseholdLaos_2C$Kid <- HouseholdLaos_2C[,1861]/2
  HouseholdLaos_2C$PigGoat <- rowSums(HouseholdLaos_2C[,c(1858,1860,2392,2393)], na.rm = T)
  #Column 2394
#3.11 Nb of poultry
  HouseholdLaos_2C$Poultry <- rowSums(HouseholdLaos_2C[,c(1866:1869)], na.rm = T)
  #Column 2395
  
#3.12 Nr of land uses: Lowland, Upland, Pasture, Fallow, Forest (?), Aquaculture land, Home garden
  for (i in 617:623){
    HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
    HouseholdLaos_2C[,i] <- as.numeric(HouseholdLaos_2C[,i])
  }
  HouseholdLaos_2C$LU <- rowSums(HouseholdLaos_2C[,c(617:623)], na.rm = T)
  #Column 2396

#3.13 Water pump system
  #Column 2265
  
#3.14 Irrigation system
  #Column 2266


#3.15 Distance of village from administrative centre: village data (proxy for remoteness and market access)
  Gps <- HouseholdLaos_2C[,c(25,28,2303)]
  #Distance to Xeingkhouang district center Phonsavan (19.457343, 103.198093) (Source: Google maps)
  #Ang: 19.5904412 102.9824738 : 38 km 
  #Gnotbiang: 19.6726784 103.296743 : 42km
  #Gnotphae: 19.737735 103.25073499999999 : 53km
  #Khangvieng: 19.5863278 103.3017861 : 32 km
  #Khay: 19.5715982 103.3253784 : 34 km
  #KuayMor: 19.6204595 103.5699411 : 55 km
  #Laethong: 19.7020928 103.2200906 : 47 km
  #Naphan: 19.7250973 103.4617085 : 55 km
  #Nong-On: 19.8111815 103.6794922 : 94 km
  #Phon: 19.647409999999997 103.28152999999999 : 34 km
  #Pua: 19.567223333333335 103.01150000000001 : 31 km
  #Samphanxai: 19.749753 103.421129 : 55 km
  #Song: 19.8108562 103.7040566 : 100 km
  #Tin-Nua: 19.379326666666667 103.15010833333332 : 15 km
  #Xay-nadu: 19.6608859 103.6074236 : 61 km
  #Xong: 19.5600327 102.9878347 : 34 km
list <- data.frame(village_eng_preload=c("Ang","Gnotbiang","Gnotphae","Khangvieng",
                                         "Khay","KuayMor","Laethong","Naphan","Nong-On",
                                         "Phon","Pua","Samphanxai","Song","Tin-Nua",
                                         "Xay-nadu ","Xong"),
                                          "distance (km)"=c(38,42,53,32,34,55,47,55,94,
                                                            34,31,55,100,15,61,34))
HouseholdLaos_2C <- join(HouseholdLaos_2C, list, by = "village_eng_preload")

#Column 2397


#3.16 Number of active household members
  HouMemberLaos_2C$a6 <- as.numeric(HouMemberLaos_2C$a6)
  Active <- HouMemberLaos_2C %>% dplyr::filter(a6 == 1 | a6 == 2 | a6 == 3 |
                                                 a6 == 4 | a6 == 5 | a6 == 6 |
                                                  a6 == 7 | a6 == 9 | a6 == 11)
  Active <- Active %>% dplyr::count(hhid_re1)
  colnames(Active)[1] <- "o9"
  colnames(Active)[2] <- "Active"
  HouseholdLaos_2C <- join(HouseholdLaos_2C, Active, by = "o9")
  x <- unique(HouseholdLaos_2C$o9)
  y <- unique(HouMemberLaos_2C$hhid_re1)
  z <- intersect(x,y)
  a <- setdiff(x,z)
  HouseholdLaos_2C[,2398] <- ifelse(is.na(HouseholdLaos_2C[,2398]) & !HouseholdLaos_2C$o9 %in% a,
                                       0,HouseholdLaos_2C[,2398])
#Column 2398

  
#3.17 Number of household members working on the farm
  FarmW1 <- HouMemberLaos_2C %>% dplyr::filter(a6 == 1)
  FarmW1 <- FarmW1 %>% dplyr::count(hhid_re1)
  colnames(FarmW1)[1] <- "o9"
  colnames(FarmW1)[2] <- "FarmW1"
  HouseholdLaos_2C <- join(HouseholdLaos_2C, FarmW1, by = "o9")
  FarmW2 <- HouMemberLaos_2C %>% dplyr::filter(a7 == "Agricultural work on their own farm (including livestock management)")
  FarmW2 <- FarmW2 %>% dplyr::count(hhid_re1)
  colnames(FarmW2)[1] <- "o9"
  colnames(FarmW2)[2] <- "FarmW2"
  HouseholdLaos_2C <- join(HouseholdLaos_2C, FarmW2, by = "o9")
  HouseholdLaos_2C$FarmW1 <- as.numeric(HouseholdLaos_2C$FarmW1)
  HouseholdLaos_2C$FarmW2 <- as.numeric(HouseholdLaos_2C$FarmW2)
  HouseholdLaos_2C$FarmW <- ifelse(!is.na(HouseholdLaos_2C$FarmW1),HouseholdLaos_2C$FarmW1,
                                   0) + ifelse(!is.na(HouseholdLaos_2C$FarmW2),
                                               (HouseholdLaos_2C$FarmW2/2),0)
  HouseholdLaos_2C$FarmW <- ifelse(is.na(HouseholdLaos_2C$FarmW1) & is.na(HouseholdLaos_2C$FarmW2), NA, HouseholdLaos_2C$FarmW)
  HouseholdLaos_2C[,2401] <- ifelse(is.na(HouseholdLaos_2C[,2401]) & !HouseholdLaos_2C$o9 %in% a,
                                    0,HouseholdLaos_2C[,2401])
  
  #Column 2401

#3.18 Total number of household members
  #Column 53

#3.19 Nr of hired labor
  #Column 1821

#3.20 Off-farm income: % over total income/ main source of income of the households
  x <- HouseholdLaos_2C[,c(1,107,113)]
  y <- HouseholdLaos_2C[,c(1,109,115)]
  z <- HouseholdLaos_2C[,c(1,111,117)]
x <- x %>% dplyr::filter(b4_1 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_1 == "Non-farm wages (salaried work in private or public company) " |
                           b4_1 == "Other  ${b3_2oth}")
y <- y %>% dplyr::filter(b4_2 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_2 == "Non-farm wages (salaried work in private or public company) " |
                           b4_2 == "Other  ${b3_2oth}")
z <- z %>% dplyr::filter(b4_3 == "Non-farm income (own business: shop, trader/collector etc.)" |
                           b4_3 == "Non-farm wages (salaried work in private or public company) " |
                           b4_3 == "Other  ${b3_2oth}")
All <- merge(x,y, by = "o9", all = TRUE)
All <- merge(All,z, by = "o9", all = TRUE)
All$SumNF <- ifelse(!is.na(All$b5_1),All$b5_1,0) + ifelse(!is.na(All$b5_2),All$b5_2,0) + ifelse(!is.na(All$b5_3),All$b5_3,0)
All <- All[,c(1,8)]
HouseholdLaos_2C <- join(HouseholdLaos_2C, All, by = "o9")
#WARNING: It is possible that some households earn non-farm income but not as one of the 
#first source of revenue, it is then not considered here

#2402                

#3.21 % of hh members that migrated seasonnaly or permanently
  Migr <- HouMemberLaos_2C %>% dplyr::filter(a10 == "Yes")
  Migr <- Migr %>% dplyr::count(hhid_re1)
  colnames(Migr)[1] <- "o9"
  colnames(Migr)[2] <- "Migr"
  HouseholdLaos_2C <- join(HouseholdLaos_2C, Migr, by = "o9")
  HouseholdLaos_2C$Migr <- as.numeric(HouseholdLaos_2C$Migr)
  HouseholdLaos_2C$Migr2 <- ifelse(is.na(HouseholdLaos_2C$a14),0,HouseholdLaos_2C$a14) +
                            ifelse(is.na(HouseholdLaos_2C$Migr),0,HouseholdLaos_2C$Migr)
  HouseholdLaos_2C$Migr2 <- HouseholdLaos_2C$Migr2 / HouseholdLaos_2C$Active
  HouseholdLaos_2C[,2403] <- ifelse(is.na(HouseholdLaos_2C[,2403]) & !HouseholdLaos_2C$o9 %in% a,
                                    0,HouseholdLaos_2C[,2403])
  HouseholdLaos_2C$Migr2 <- ifelse(HouseholdLaos_2C$Migr2 >= 1 | HouseholdLaos_2C$Migr2 == 'inf',0.75,HouseholdLaos_2C$Migr2)
  #Column 2404

  
#3.22 Final Table creation
  VarPCA <- HouseholdLaos_2C[,c(1,16,28,2320,2382:2383,2385:2388,2391,2394,2396,2397,2398,2401:2402,2404)]
  colnames(VarPCA) <- c("ID","District","Village","1.NTFP","2.Paddy","3.Maize","4.UplandRice",
                        "5.Fruit","6.Vegetables","7.Tea","8.CattleBuff","9.PigGoat",
                        "10.LU","11.DistanceP","12.Active","13.FarmW","14.%incNF","15.%Migr")
  
  for (i in c(10,17)){
        VarPCA[,i] <- ifelse(is.na(VarPCA[,i]),'0',VarPCA[,i])
        VarPCA[,i] <- as.numeric(VarPCA[,i])
  }
  # Removing of households without land or animals
  #Crops / Land
  CropCheck <- HouseholdLaos_2C[,c(1,617:624,632,1838,1854:1871)]
  CropCheck$Sum <- rowSums(CropCheck[,c(1:7)])
  #1 household with only homegarden but not removed as not a big issue
  

#4. Descriptive statistics
  {
    #Summary statistics
    summary(VarPCA[,c(4:18)])
    #Histogramm for each variable
    for (i in c(4:18)){
      x <- ggplot(VarPCA, aes(x= VarPCA[,i])) + geom_histogram(binwidth = max(VarPCA[,i],na.rm = TRUE)/20)+
        ggtitle(colnames(VarPCA)[i])+
        xlab("number")
      print(x)
    }
    #Matrix of correlation
    res <- cor(VarPCA[,c(4:18)], use="pairwise.complete.obs")
    round(res, 2)
    corrplot(res, type = "upper", 
             tl.col = "black", tl.srt = 45)
  
    #Check corn
    colnames(TableLaos)[1] <- "o9"
    Dummy <- join(HouseholdLaos_2C[,c(1,2323,2328,2351,2352)], TableLaos[,c(1,15)], by = "o9")
    Dummy$Lcorn <- rowSums(Dummy[,c(2,3)], na.rm = T)
    Dummy$Ucorn <- rowSums(Dummy[,c(4,5)], na.rm = T)
    Dummy %>%
      group_by(clust) %>% 
      get_summary_stats(Lcorn, type = "mean")
    Dummy %>%
      group_by(clust) %>% 
      get_summary_stats(Ucorn, type = "mean")
    sum(Dummy$Lcorn) / sum(Dummy$Ucorn)

  ## 5. PCA
  ## 5.1 PCA
  #We try to remove some households with uncommon characteristics
  VarPCA$ID <- as.character(VarPCA$ID)
  VarPCA <- VarPCA[VarPCA$ID != 3013,]
  VarPCA <- VarPCA[VarPCA$ID != 3229,]
  VarPCA <- VarPCA[VarPCA$ID != 3355,]
  VarPCA <- VarPCA[VarPCA$ID != 3417,]
  #5.1.1 PCA Global
  res.pca <- PCA(VarPCA[,c(1:2,4:7,10:17)], scale.unit = T, ncp = 10,
                 quali.sup= c(1,2), graph = TRUE)
  VarPCA$Village <- as.character(VarPCA$Village)
  VarPCA$Village <- as.factor(VarPCA$Village)
  Mypalette <- c("#6633FF","#99CCFF","#00CCFF","#003333","#CC99CC","#99CCCC","#336666",
                 "#CCFF00","#66CC00","#3399FF","#330066","#336600","#33FF99","#336699","#CC6600","#000033")
  fviz_pca_biplot(res.pca, label = "var", habillage=VarPCA$Village,
                  palette = Mypalette,addEllipses = F, ellipse.level=0.95,
                  ggtheme = theme_minimal())
   #Get contributions
  contrib <- as.data.frame(res.pca$var$contrib)
  #Get eigen values
  eig.val <- as.data.frame(get_eigenvalue(res.pca))
  
  ##AHC: https://www.youtube.com/watch?v=3tT29UtHqd0&t=34s
  #RUn AHC
  res.hcpc <- HCPC(res.pca)
  #Add cluster numbers in the database:
  TableLaos <- res.hcpc$data.clust
  summary(Table1$clust)
  #Table export
  write.csv2(Table1, file  = 'Datacluster.csv')
  #Description des classes par les variables :
  res.hcpc$desc.var
  #Description par les individus :
  res.hcpc$desc.ind
  #Description par les axes :
  res.hcpc$desc.axes
  

    ## 6. Vilage coordinates gathering
  Coordinates <- HouseholdLaos_2C[,c(28,2304:2306)]
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
  write.table(Coordinates, file  = 'CoordinatesLaos.csv', sep = ',', dec = '.', row.names = F)

  #Missing average values - PCADB
  AVERAGEIF(Table1$clust, "3", Table1$'1.NTFP')
  AVERAGEIF(Table1$clust, "1", Table1$'2.Paddy')
  AVERAGEIF(Table1$clust, "2", Table1$'9.PigGoat')
  AVERAGEIF(Table1$clust, "1", Table1$'12.Active')
  AVERAGEIF(Table1$clust, "2", Table1$'14.%incNF')

  table(Table1$Village,Table1$clust)
  
  #Plotting variables depending on clusters - Moc Chau
  #Histogramm for each variable
  for (i in 3:15){
    x <- ggplot(Table1, aes(x= Table1[,i], fill = Table1$clust)) + geom_histogram(position = 'dodge',binwidth = max(Table1[,i],na.rm = TRUE)/10)+
      ggtitle(colnames(Table1[i]))+
      xlab("number")
    print(x)
  }
