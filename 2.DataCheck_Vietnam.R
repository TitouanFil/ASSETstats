### 1. Packages loading and data import
#Packages loading
library(dplyr)
library(ggplot2)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
ClowlandVietnam_2 <- readRDS("ClowlandVietnam_2.rds")
CuplandVietnam_2 <- readRDS("CuplandVietnam_2.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")

### 2. Summary and Plots display loop
for (i in ncol(HouseholdVietnam)){
  
  
}

#For FACT 
summary(HouseholdLaos_2C[,1])
ggplot(HouseholdLaos_2C, aes(x=HouseholdLaos_2C[,1])) +
  geom_bar(stat="identity")

#Checking correspondence between Upland crop and HouseholdVietnam databases

Test <- cbind(HouseholdVietnam[,18], HouseholdVietnam[,676:677])
TestUp <- CuplandVietnam[,c(1:3,9)]
TestUp$Crop3 <- paste(TestUp$searchcrop2, TestUp$d2_232)
count_if("TRUE",duplicated(TestUp$Crop3))
TestUp2 <- spread(TestUp, crop2_now, Crop3)
TestUp2 <- dcast(TestUp, hhid_re3 ~ crop2_now, value.var="Crop3")
TestUp2$Sum <- TestUp2[,2] + TestUp2[,3] + TestUp2[,4] + TestUp2[,5] + TestUp2[,6] + TestUp2[,7]
colnames(TestUp2)[1] <- 'o9'
Test <- merge(Test, TestUp2, all = TRUE, by = 'o9')
count_if("FALSE",Test$no_crop2 == Test$Sum)
Test$no_crop2 <- as.numeric(Test$no_crop2)
Test$diff <- (Test$no_crop2 - Test$Sum)


