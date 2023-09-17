# # #1. Packages loading and preparation

##a. Packages loading
library(haven)
library(stringr)
library(labelled)

##b. Data loading
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HomegardenCambodia_2C <- readRDS("HomegardenCambodia_2.rds")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")
ClowlandLaos_2C <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2C <- readRDS("CuplandLaos_2.rds")
#We import all the dta files transmitted by Ky as *Vietnam* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")



# # #2. Data cleaning and exporting
##2.1. Cambodia
#Remove unwanted columns for each datasets
HouseholdCambodia_2C <- HouseholdCambodia_2C[,-c(6:7,43:47,58,60:65,92,474,597,608,789,2074,
                                                 2104,2125,2382,2389,2405,2484,2520,2527:2549,
                                                 2554,2558)]
HouMemberCambodia_2C <- HouMemberCambodia_2C[,-c(54:65,73,77)]
ClowlandCambodia_2C <- ClowlandCambodia_2C[,-c(8,24:26,34,38)]
CuplandCambodia_2C <- CuplandCambodia_2C[,-c(30,34)]
HomegardenCambodia_2C <- HomegardenCambodia_2C[,-c(30,34)]

##2.2. Laos
#Remove unwanted columns for each datasets
HouseholdLaos_2C <- HouseholdLaos_2C[,-c(6:7,38:41,52,641,803,1882,1910,1929,2164,2172,2188,
                                         2267,2308,2314,2316,2318)]
HouMemberLaos_2C <- HouMemberLaos_2C[,-c(47:58,66,68,70)]
ClowlandLaos_2C <- ClowlandLaos_2C[,-c(25,27,29)]
CuplandLaos_2C <- CuplandLaos_2C[,-c(24,26,28)]

##2.3. Vietnam
#Remove unwanted columns for each datasets
HouseholdVietnam_2C <- HouseholdVietnam_2C[,-c(6:7,43:47,58,73:76,86:97,99,105:112,114:118,
                                               135,169,573,589,600:601,633,654,656,675,684,
                                               846,858:860,1914,1942,1961,2018,2037,2079,
                                               2098,2140,2160,2205,2221,2300,2306,2330:2334,
                                               2342,2347:2709,2713,2714,2718)]
HouMemberVietnam_2C <- HouMemberVietnam_2C[,-c(59:83,90:91,95)]
ClowlandVietnam_2C <- ClowlandVietnam_2C[,c(7,24,25,29)]
CuplandLaos_2C <- CuplandLaos_2C[,-c(0,34)]

# # #3. Removal of personal information
##3.1. Cambodia
#Remove unwanted columns for each datasets
HouseholdCambodia_2C <- HouseholdCambodia_2C[,-c(33,48:49,2494:2499)]
HouMemberCambodia_2C <- HouMemberCambodia_2C[,-5]

##3.2. Laos
#Remove unwanted columns for each datasets
HouseholdLaos_2C <- HouseholdLaos_2C[,-c(29,43:44,2287:2291)]
HouMemberLaos_2C <- HouMemberLaos_2C[,-4]

##3.3. Vietnam
#Remove unwanted columns for each datasets
HouseholdVietnam_2C <- HouseholdVietnam_2C[,-c(48:49,2271:2274)]
HouMemberVietnam_2C <- HouMemberVietnam_2C[,-4]


# # #4. Database creation under different format
#Select the output folder
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSETOutputs/Anonymized database")

##4.1 Export of the database under rds format
#Cambodia
saveRDS(HouseholdCambodia_2C, "HouseholdCambodia_TFano.rds")
saveRDS(HouMemberCambodia_2C, "HouMemberCambodia_TFano.rds")
saveRDS(ClowlandCambodia_2C, "ClowlandCambodia_TFano.rds")
saveRDS(CuplandCambodia_2C, "CuplandCambodia_TFano.rds")
saveRDS(HomegardenCambodia_2C, "HomegardenCambodia_TFano.rds")
#Laos
saveRDS(HouseholdLaos_2C, "HouseholdLaos_TFano.rds")
saveRDS(HouMemberLaos_2C, "HouMemberLaos_TFano.rds")
saveRDS(ClowlandLaos_2C, "ClowlandLaos_TFano.rds")
saveRDS(CuplandLaos_2C, "CuplandLaos_TFano.rds")
#Vietnam
saveRDS(HouseholdVietnam_2C, "HouseholdVietnam_TFano.rds")
saveRDS(HouMemberVietnam_2C, "HouMemberVietnam_TFano.rds")
saveRDS(ClowlandVietnam_2C, "ClowlandVietnam_TFano.rds")
saveRDS(CuplandVietnam_2C, "CuplandVietnam_TFano.rds")

##4.2 Export of the database under dta format
#Cambodia
write_dta(HouseholdCambodia_2C, "HouseholdCambodia_TFano.dta")
write_dta(HouMemberCambodia_2C, "HouMemberCambodia_TFano.dta")
write_dta(ClowlandCambodia_2C, "ClowlandCambodia_TFano.dta")
write_dta(CuplandCambodia_2C, "CuplandCambodia_TFano.dta")
write_dta(HomegardenCambodia_2C, "HomegardenCambodia_TFano.dta")
#Laos
write_dta(HouseholdLaos_2C, "HouseholdLaos_TFano.dta")
write_dta(HouMemberLaos_2C, "HouMemberLaos_TFano.dta")
write_dta(ClowlandLaos_2C, "ClowlandLaos_TFano.dta")
write_dta(CuplandLaos_2C, "CuplandLaos_TFano.dta")
#Vietnam
colnames(HouseholdVietnam_2C) <- gsub("\\.","_",colnames(HouseholdVietnam_2C))
write_dta(HouseholdVietnam_2C, "HouseholdVietnam_TFano.dta")
write_dta(HouMemberVietnam_2C, "HouMemberVietnam_TFano.dta")
write_dta(ClowlandVietnam_2C, "ClowlandVietnam_TFano.dta")
write_dta(CuplandVietnam_2C, "CuplandVietnam_TFano.dta")

##4.3 Export of the database under csv format

#Cambodia (the labels are included into colnames)
colnames(HouseholdCambodia_2C) <- paste(colnames(HouseholdCambodia_2C),"_(",
                                        var_label(HouseholdCambodia_2C),")")
write.table(HouseholdCambodia_2C, "HouseholdCambodia_TFano.csv",sep=";",row.names=F)
colnames(HouMemberCambodia_2C) <- paste(colnames(HouMemberCambodia_2C),"_(",
                                        var_label(HouMemberCambodia_2C),")")
write.table(HouMemberCambodia_2C, "HouMemberCambodia_TFano.csv",sep=";",row.names=F)
colnames(ClowlandCambodia_2C) <- paste(colnames(ClowlandCambodia_2C),"_(",
                                       var_label(ClowlandCambodia_2C),")")
write.table(ClowlandCambodia_2C, "ClowlandCambodia_TFano.csv",sep=";",row.names=F)
colnames(CuplandCambodia_2C) <- paste(colnames(CuplandCambodia_2C),"_(",
                                      var_label(CuplandCambodia_2C),")")
write.table(CuplandCambodia_2C, "CuplandCambodia_TFano.csv",sep=";",row.names=F)
colnames(HomegardenCambodia_2C) <- paste(colnames(HomegardenCambodia_2C),"_(",
                                         var_label(HomegardenCambodia_2C),")")
write.table(HomegardenCambodia_2C, "HomegardenCambodia_TFano.csv",sep=";",row.names=F)
#Laos
colnames(HouseholdLaos_2C) <- paste(colnames(HouseholdLaos_2C),"_(",
                                    var_label(HouseholdLaos_2C),")")
write.table(HouseholdLaos_2C, "HouseholdLaos_TFano.csv",sep=";",row.names=F)
colnames(HouMemberLaos_2C) <- paste(colnames(HouMemberLaos_2C),"_(",
                                    var_label(HouMemberLaos_2C),")")
write.table(HouMemberLaos_2C, "HouMemberLaos_TFano.csv",sep=";",row.names=F)
colnames(ClowlandLaos_2C) <- paste(colnames(ClowlandLaos_2C),"_(",
                                   var_label(ClowlandLaos_2C),")")
write.table(ClowlandLaos_2C, "ClowlandLaos_TFano.csv",sep=";",row.names=F)
colnames(CuplandLaos_2C) <- paste(colnames(CuplandLaos_2C),"_(",
                                  var_label(CuplandLaos_2C),")")
write.table(CuplandLaos_2C, "CuplandLaos_TFano.csv",sep=";",row.names=F)
#Vietnam
colnames(HouseholdVietnam_2C) <- paste(colnames(HouseholdVietnam_2C),"_(",
                                       var_label(HouseholdVietnam_2C),")")
write.table(HouseholdVietnam_2C, "HouseholdVietnam_TFano.csv",sep=";",row.names=F)
colnames(HouMemberVietnam_2C) <- paste(colnames(HouMemberVietnam_2C),"_(",
                                       var_label(HouMemberVietnam_2C),")")
write.table(HouMemberVietnam_2C, "HouMemberVietnam_TFano.csv",sep=";",row.names=F)
colnames(ClowlandVietnam_2C) <- paste(colnames(ClowlandVietnam_2C),"_(",
                                      var_label(ClowlandVietnam_2C),")")
write.table(ClowlandVietnam_2C, "ClowlandVietnam_TFano.csv",sep=";",row.names=F)
colnames(CuplandVietnam_2C) <- paste(colnames(CuplandVietnam_2C),"_(",
                                     var_label(CuplandVietnam_2C),")")
write.table(CuplandVietnam_2C, "CuplandVietnam_TFano.csv",sep=";",row.names=F)
