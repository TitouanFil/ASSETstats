### 1. Packages and data preparation
##  1.1. Packages
library(plyr)
library(dplyr)
library(rstatix)

##  1.2. Data import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HomegardenCambodia_2C <- readRDS("HomegardenCambodia_2.rds")


##  1.3. Crop area Table preparation

#First we turn upland, lowland and homegarden crops data to wide format (Duplicate alert = Normal,
#we had no time to take care of it yet)
#Lowland
dlowc <- ClowlandCambodia_2C[,c(2:3,12)]
dlowcwide <- reshape(dlowc, direction = "wide", timevar = "crop1_now", idvar = "hhid_re2")
#Upland
dupc <- CuplandCambodia_2C[,c(2:3,11)]
colnames(dupc)[colnames(dupc) == "hhid_re3"] ="hhid_re2"
dupcwide <- reshape(dupc, direction = "wide", timevar = "crop2_now", idvar = "hhid_re2")
#Homegarden
dhom <- HomegardenCambodia_2C[,c(2:3,11)]
colnames(dhom)[colnames(dhom) == "hhid_re4"] ="hhid_re2"
dhomwide <- reshape(dhom, direction = "wide", timevar = "crop3_now", idvar = "hhid_re2")
#We create a table with all the id
HHid <- HouseholdCambodia_2C[,1:2]
colnames(HHid)[colnames(HHid) == "o9"] = "hhid_re2"
#We merge the tables
dcrops <- merge(HHid, dlowcwide, by.x = "hhid_re2", all = T)
dcrops <- merge(dcrops, dupcwide, by.x = "hhid_re2", all = T)
dcrops <- merge(dcrops, dhomwide, by.x = "hhid_re2", all = T)
#And we remove useless columns
dcrops <- dcrops[,-c(2,8,15,19:21)]
#We add a column with the total cultivated area for each household
dcrops$TotalArea <- rowSums(dcrops[,2:12], na.rm = T)
colnames(dcrops)[1] <- "o9"

##  1.4. Household scores preparation
Scores <- HouseholdCambodia_2C[,c(1:2)]


### 2. AE scores

##  2.1. Recycling

#   2.1.2. Maintaining/enhancing soil fertility practices (d17 & d18)
#Number of practices * % of the cultivated area concerned
#Score based on the sum of area concerned by each practice 
#(if a crop = 2 practices, it is counted 2 times) then divided by total cultivated area
#0 = Score of 0
#1 = Score <0.5 
#2 = Score >0.5 
d18subS <- Scores
d18 <- join(dcrops, HouseholdCambodia_2C[,c(1,1120:1133)], by = "o9")
for (i in 2:15){
  d18[,(i+15)] <- as.numeric(d18[,(i+15)])
  for (j in 1:nrow(d18)){
    d18[j,i+15] <- ifelse(d18[j,i+15] == 1, d18[j,i], 0)
  }
}
d18$AreaPract <- rowSums(d18[,17:30], na.rm = T)
d18$PractPer100 <- d18$AreaPract / d18$TotalArea
d18subS <- join(d18subS,d18[,c(1,32)], by = "o9")


##  2.2. Input reduction


##  2.7. Economic diversification


##  2.9. Social values and diets

## 2.13. Participation
#Score from 0 to 4

#Are you a member of one or more farmer group/cooperative/organization or village organization (e.g. crops/fruits/livestock/honey/ water/Forest etc.)? 
#Yes More than 1 = 2 
# Yes, one = 1 
# No = 0 

#Are you or anyone in your household active in any of the following? 
#Yes = 1 
#No = 0 


#Are you or anyone in your household involved in any advocacy work? 
#Yes = 1
#No = 0



