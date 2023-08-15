### 1. Packages loading and data import
#Packages loading
library(dplyr)
library(ggplot2)
library(labelled)
library(sjlabelled)
library(dplyr)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
ClowlandLaos_2 <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2 <- readRDS("CuplandLaos_2.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")

#We select only quantitative variables
Num <- HouseholdLaos_2C %>% dplyr::select(where(is.numeric))

### 2. Summary and Plots display loop
for (i in 1:ncol(Num)){
    #T1 - Title of Variable
    summary(Num[,i])
    x <- ggplot(Num, aes(x= Num[,i])) + geom_histogram(binwidth = max(Num[,i],na.rm = TRUE)/20)+
    ggtitle(var_label(Num[,i]))+
    xlab("number")
    print(x)
}
i = 1

  








