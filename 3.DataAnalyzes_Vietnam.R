### 1. Packages loading and data import
#Packages loading
library(haven)
library(expss)
library(dplyr)
library(labelled)
library(foreign)
library(sjlabelled)
library(stringr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(wesanderson)
library(paletteer)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Vietnam* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")

###2. Data analyzes - General

###3. Data analyzes for Agroecological principles

##3.1 - Recycling

# # #3.1.a. d17. Does your household use ecological/agroecological/integrated practices...
#First, we select the useful data
#We convert it to the proper format
HouseholdVietnam_2C$d17 <- as.factor(HouseholdVietnam_2C$d17)
#We create a counting table
d17count <- count(HouseholdVietnam_2C, d17)
#Changing the index by real answers:
d17count$d17 <- c("no","yes","unknown", "NA")
# Preparation of the plot
# Compute percentages
d17count$fraction <- d17count$n / sum(d17count$n)
# Compute the cumulative percentages (top of each rectangle)
d17count$ymax <- cumsum(d17count$fraction)
# Compute the bottom of each rectangle
d17count$ymin <- c(0, head(d17count$ymax, n=-1))
# Compute label position
d17count$labelPosition <- (d17count$ymax + d17count$ymin) / 2
# Compute a good label
d17count$label <- paste0(d17count$d17, "\n", round(d17count$fraction*100, digits = 1), "%")
# Make the plot
ggplot(d17count, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=d17)) +
  geom_rect(colour = "black") +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette="Paired") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  ggtitle(paste("d17. Does your household use ecological/
  agroecological/integrated practices to 
  maintain/enhance soil fertility in your fields?\n N=", sum(d17count$n)))

  

# # #3.1.b. d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?
#First, we select the useful data
d18dat <- HouseholdVietnam_2C[,c(1,23,1114:1125)]
#We modify long label names
var_label(d18dat$d183) <- 'Bokashi (fermented\norganic matter)'
var_label(d18dat$d184) <- 'Legume-based\ngreen manure'
var_label(d18dat$d185) <- 'Pulses in association\nand/or rotation\nwith main crop'
var_label(d18dat$d186) <- 'Cover crops in\nassociation and/\nor rotation\nwith main crop'
var_label(d18dat$d188) <- 'Crop residue\nmaintenance'
var_label(d18dat$d189) <- 'Recycling\ncrop waste'
var_label(d18dat$d1810) <- 'Ramial Wood Chip\n (RWC) or other\nwood chips'
var_label(d18dat$d1811) <- 'Organic agro-\nindustrial waste'
#And we change columns names for label names
colnames(d18dat) <- var_label(d18dat)
#Then we adapt the format to be able to plot it and create a table
d18melt <- melt(d18dat ,  id.vars = c('o9. household id', 'province name (english)'), variable.name = 'Practices')
#We can now create a counting table
d18count <- d18melt %>% count(Practices, value)
d18countNAR <- d18count %>%  filter(!is.na(value) & value != 0)
#We convert absolute values to % y divinding by the total number of households
d18countNAR$Percentage_of_households <- round(d18countNAR$n/nrow(d18dat)*100, digits=1)
#And now a distribution plot
ggplot(data=d18countNAR, aes(x=Practices, y=Percentage_of_households, fill = Practices))+
  geom_bar(colour="black",stat="identity")+
  theme(axis.text.x = element_text(face="bold", size=10,angle = 60), legend.position = "none",
  panel.grid.major=element_line(colour="grey"))+
  geom_text(aes(label = Percentage_of_households, vjust = -1)) + ylim(-7,35)+
  scale_fill_brewer(palette="YIGn", direction = -1)+
  ggtitle(paste("d18. Frequency of adoption of agro-ecological practices among households (Vietnam)\n N=", sum(nrow(d18dat))))

# # #3.1.c. d18_11. For which crop(s) do you use animal manure?
#First we turn upland and lowland data to wide format (Duplicate alert = Normal,
#we had no time to take care of it yet)
d1811lowc <- ClowlandVietnam_2C[,c(1:2,9)]
d1811lowcwide <- reshape(d1811lowc, direction = "wide", timevar = "crop1_now", idvar = "hhid_re2")
d1811lowcwide <- d1811lowcwide %>% relocate(d2_13e.2 , .before = d2_13e.3)
d1811upc <- CuplandVietnam_2C[,c(1:2,8)]
colnames(d1811upc)[colnames(d1811upc) == "hhid_re3"] ="hhid_re2"
d1811upcwide <- reshape(d1811upc, direction = "wide", timevar = "crop2_now", idvar = "hhid_re2")
#We create a table with all the id
HHid <- HouseholdVietnam_2C[,1:2]
colnames(HHid)[colnames(HHid) == "o9"] = "hhid_re2"
#We merge the tables
d1811crops <- merge(HHid, d1811lowcwide, by.x = "hhid_re2", all = T)
d1811crops <- merge(d1811crops, d1811upcwide, by.x = "hhid_re2", all = T)
#And we remove useless columns
d1811crops <- d1811crops[,-c(2,8)]
#Then, we'll prepare the data we want to analyze by replacing yes (&) values by crop name
d1811dat <- HouseholdVietnam_2C[,c(1,1128:1138)]
for (i in 2:12){
  d1811dat[,i] <- as.character(d1811dat[,i])
  for (j in 1:594){
  d1811dat[j,i] <- ifelse(d1811dat[j,i] == '1', d1811crops[j,i], d1811dat[j,i])
  }
}
# We melt data frame into long format
d1811melt <- melt(d1811dat, id.vars = "o9")
#We count the answers and filter NA and "no" values
d1811count <- d1811melt %>% count(value)
d1811countNAR <- d1811count %>%  filter(!is.na(value) & value != 0)
#We convert absolute values to % y divinding by the total number of households
d1811countNAR$Percentage_of_households <- round(d1811countNAR$n/d18countNAR[1,3]*100, digits=1)
#We order the table
d1811countNAR <- d1811countNAR[order(-d1811countNAR$Percentage_of_households), ]
d1811countNARa <- d1811countNAR[1:22,]
d1811countNARb <- d1811countNAR[23:44,]
#And now a distribution plot
ggplot(data=d1811countNARa, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
  geom_bar(colour="black",stat="identity") +
  theme(axis.text.x = element_text(face="bold", size=11, angle = 85), legend.position = "none", 
        panel.grid.major=element_line(colour="grey"), axis.title.x = element_blank())+
  geom_text(aes(label = Percentage_of_households, vjust = -1))+
  ylim(-30,60)+
  ggtitle(paste("d18_11. For which crop(s) do you use animal manure? (% of households using animal manure)\n N=",d18countNAR[1,3]))+
  scale_fill_viridis_d(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "E",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill")

#Re-order + mise en forme
