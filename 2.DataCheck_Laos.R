### 1. Packages loading and data import
#Packages loading
library(dplyr)
library(ggplot2)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
ClowlandLaos_2 <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2 <- readRDS("CuplandLaos_2.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")

### 2. Summary and Plots display loop
for (i in ncol(HouseholdLaos)){
  
  
}

#For FACT 
summary(HouseholdLaos_2C[,1])
ggplot(HouseholdLaos_2C, aes(x=HouseholdLaos_2C[,1])) +
  geom_bar(stat="identity")




