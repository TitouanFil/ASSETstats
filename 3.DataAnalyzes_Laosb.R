### 1. Preparation of analyses

##1.1 Packages loading and data import
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
library(RColorBrewer)
library(ggthemes)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
ClowlandLaos_2C <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2C <- readRDS("CuplandLaos_2.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")

#We relocate hosuehold id at the begining of the database
HouseholdLaos_2C <- HouseholdLaos_2C %>% relocate(o9 , .before = start_time)

##1.2 - Preparation of functions and data

# # #1.2.a. YES, NO, DNK, NA  PIE PLOT
PiePlotYN <- function(data,title,list){
  #Changing the index by real answers:
  data$d <- list
  # Preparation of the plot
  # Compute percentages
  data$fraction <- data$n / sum(data$n)
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  # Compute a good label
  data$label <- paste0(data$d, "\n", round(data$fraction*100, digits = 1), "%")
  #Prepare the color list
  col <- c("forestgreen","darkolivegreen2","lightblue","grey88")
  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=d)) +
    geom_rect(fill = col[1:nrow(data)],colour = "black") +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=4, fill = col[1:nrow(data)]) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    ggtitle(paste(title,"\n N=", sum(data$n)))
}

# # #1.2.b ADAPTATIVE PIE PLOT
PiePlot <- function(data,title,list){
  #Changing the index by real answers:
  data$d <- list
  # Preparation of the plot
  # Compute percentages
  data$fraction <- data$n / sum(data$n)
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  # Compute a good label
  data$label <- paste0(data$d, "\n", round(data$fraction*100, digits = 1), "%")
  # Make the plot
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=d)) +
    geom_rect(colour = "black") +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
    scale_fill_brewer(palette="Paired") +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
    ggtitle(paste(title,"\n N=", sum(data$n)))
}

# # #1.2.c. List PLOT
ListBPlot <- function(data,title,list,pond,xT,ylimn,dig){
  colnames(data)[1] <- 'Answer'
  data$Answer <- list
  #We convert absolute values to % y divinding by the total number of households
  data$Percentage_of_households <- round(data$n/as.numeric(pond)*100, digits=dig)
  #And now a distribution plot
  Plota <- ggplot(data=data, aes(x= reorder(Answer, -Percentage_of_households), y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity") +
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=11, angle = 85),
          legend.position = "none")+
    labs(x= xT)+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,max(data$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n N=",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plota)
}

# # #1.2.c. List PLOT without order
ListBPlotNO <- function(data,title,list,pond,xT,ylimn,dig){
  colnames(data)[1] <- 'Answer'
  data$Answer <- list
  #We convert absolute values to % y divinding by the total number of households
  data$Percentage_of_households <- round(data$n/as.numeric(pond)*100, digits=dig)
  #And now a distribution plot
  Plota <- ggplot(data=data, aes(x= reorder(Answer, Answer), y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity") +
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=11, angle = 85),
          legend.position = "none")+
    labs(x= xT)+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,max(data$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n N=",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plota)
}

# # #1.2.d. District Plot for One column results
ListBPlotDist <- function(data,title,pond,ylimn){
  #We convert absolute values to % y divinding by the total number of households
  dcount$nbHHtot <- ifelse(dcount$`o6. select appropriate district/ city` == 'KHAM',
                           sum(HouseholdLaos_2C$o6 == 'KHAM'), 
                           ifelse(dcount$`o6. select appropriate district/ city` == 'PEK',
                                  sum(HouseholdLaos_2C$o6 == 'PEK'),
                                  sum(HouseholdLaos_2C$o6 == 'Phoukoud')))
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=1)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= `o6. select appropriate district/ city`, y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, max(dcount$Percentage_of_households)+20)+
    scale_fill_brewer(palette="YIGn")+
    theme_stata()+
    ggtitle(paste(title,"\n N=", pond))
  print(plotb)
}

# # #1.2.e. PRACTICES PLOT
PracticePlot <- function(data,title,pond,pond2,xT,ylimn,ylimx,dig){
  #And we change columns names for label names
  colnames(data) <- var_label(data)
  #Then we adapt the format to be able to plot it and create a table
  dmelt <- melt(data ,  id.vars = c('o9. household id', "o6. select appropriate district/ city"), variable.name = 'Practices')
  
  # # # #b.a Plot with AE practicionners only as %
  #We can now create a counting table
  dcount <- dmelt %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNARa <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNARa$Percentage_of_households <- round(dcountNARa$n/as.numeric(pond)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- 18
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  #And now a distribution plot
  plota <- ggplot(data=dcountNARa, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = Practices))+
    geom_bar(colour="black",stat="identity")+
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60), legend.position = "none")+
    geom_text(aes(label = Percentage_of_households, vjust = -1)) + ylim(ylimn, ylimx)+
    scale_fill_manual(values= mycolors)+
    labs(x= xT)+
    ggtitle(paste(title,"\n N=", pond))
  print(plota)
  
  # # # #b.b Plot with districts and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`o6. select appropriate district/ city`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$`o6. select appropriate district/ city` == 'KHAM',
                           sum(HouseholdLaos_2C$o6 == 'KHAM'), 
                           ifelse(dcountNAR$`o6. select appropriate district/ city` == 'PEK',
                                  sum(HouseholdLaos_2C$o6 == 'PEK'),
                                  sum(HouseholdLaos_2C$o6 == 'Phoukoud')))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = `o6. select appropriate district/ city`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, ylimx)+
    scale_fill_brewer(palette="YIGn", direction = -1)+
    labs(x= xT)+
    ggtitle(paste(title,"\n N=", pond2))
  print(plotb)
}

# # #1.2.f. PRACTICES PLOT FOR MONTHS
PracticePlotmonth <- function(data,title,pond,pond2,xT,ylimn,ylimx,dig){
  #And we change columns names for label names
  colnames(data) <- var_label(data)
  #Then we adapt the format to be able to plot it and create a table
  dmelt <- melt(data ,  id.vars = c('o9. household id', "o6. select appropriate district/ city"), variable.name = 'Practices')
  
  # # # #b.a Plot with AE practicionners only as %
  #We can now create a counting table
  dcount <- dmelt %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNARa <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNARa$Percentage_of_households <- round(dcountNARa$n/as.numeric(pond)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- 18
  mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
  #And now a distribution plot
  plota <- ggplot(data=dcountNARa, aes(x=Practices, y=Percentage_of_households, fill = Practices))+
    geom_bar(colour="black",stat="identity")+
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60), legend.position = "none",
          panel.grid.major=element_line(colour="grey"))+
    geom_text(aes(label = Percentage_of_households, vjust = -1)) + ylim(ylimn, ylimx)+
    scale_fill_manual(values= mycolors)+
    labs(x= xT)+
    ggtitle(paste(title,"\n N=", pond))
  print(plota)
  
  # # # #b.b Plot with provinces and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`o6. select appropriate district/ city`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$`o6. select appropriate district/ city` == 'KHAM',
                              sum(HouseholdLaos_2C$o6 == 'KHAM'), 
                              ifelse(dcountNAR$`o6. select appropriate district/ city` == 'PEK',
                                     sum(HouseholdLaos_2C$o6 == 'PEK'),
                                     sum(HouseholdLaos_2C$o6 == 'Phoukoud')))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=Practices, y=Percentage_of_households, fill = `o6. select appropriate district/ city`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, ylimx)+
    scale_fill_brewer(palette="YIGn", direction = -1)+
    labs(x= xT)+
    ggtitle(paste(title,"\n N=", pond2))
  print(plotb)
}

# # #1.2.g. CROP PLOT
#For this function, we should use a database including column with household id 
#+ 11 columns with binary values for each crops
CropPlot <- function(data,pond, title,ylimn,ylimx,dig){
  #We replace binary values by crop names
  for (i in 2:12){
    data[,i] <- as.character(data[,i])
    for (j in 1:594){
      data[j,i] <- ifelse(data[j,i] == '1', dcrops[j,i], data[j,i])
    }
  }
  # We melt data frame into long format
  dmelt <- melt(data, id.vars = "o9")
  #We count the answers and filter NA and "no" values
  dcount <- dmelt %>% count(value)
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(pond)*100, digits=dig)
  #We order the table
  dcountNAR <- dcountNAR[order(-dcountNAR$Percentage_of_households), ]
  dcountNARa <- dcountNAR[1:as.numeric(nrow(dcountNAR)/2),]
  dcountNARb <- dcountNAR[as.numeric((nrow(dcountNAR)/2+1)):as.numeric(nrow(dcountNAR)),]
  #And now a distribution plot
  Plota <- ggplot(data=dcountNARa, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
    geom_bar(colour="black",stat="identity") +
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=11, angle = 85), legend.position = "none"
          , axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,ylimx)+
    ggtitle(paste(title,"\n N=",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plota)
  Plotb  <- ggplot(data=dcountNARb, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
    geom_bar(colour="black",stat="identity") +
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=11, angle = 85), legend.position = "none", 
          axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,ylimx)+
    ggtitle(paste(title,"\n N=",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plotb)
}

# # #1.2.h. ANIMAL PLOT
AnimalPlot <- function(data,title,pond){
  colnames(data)[1] <- "Animal"
  #We count the answers and filter NA and "no" values
  dcount <- data %>% count(Animal)
  dcountNAR <- dcount %>%  filter(!is.na(Animal) & Animal != 0 & Animal != '')
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(pond)*100, digits=1)
  #We order the table
  dcountNAR <- dcountNAR[order(-dcountNAR$Percentage_of_households), ]
  #And now a distribution plot
  Plota <- ggplot(data=dcountNAR, aes(x= reorder(Animal, -Percentage_of_households), y=Percentage_of_households, fill = Animal))+
    geom_bar(colour="black",stat="identity") +
    theme_stata()+
    theme(axis.text.x = element_text(face="bold", size=11, angle = 85), legend.position = "none", 
          axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(-10,max(dcountNAR$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n N=",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plota)
}

# # #1.3 Preparation of Crops reference table
#First we turn upland and lowland data to wide format (Duplicate alert = Normal,
#we had no time to take care of it yet)
dlowc <- ClowlandLaos_2C[,c(1:2,8)]
dlowcwide <- reshape(dlowc, direction = "wide", timevar = "crop1_now", idvar = "hhid_re2")
dlowcwide <- dlowcwide %>% relocate(d2_13e.2 , .before = d2_13e.3)
dupc <- CuplandLaos_2C[,c(1:2,7)]
colnames(dupc)[colnames(dupc) == "hhid_re3"] ="hhid_re2"
dupcwide <- reshape(dupc, direction = "wide", timevar = "crop2_now", idvar = "hhid_re2")
#We create a table with all the id
HHid <- HouseholdLaos_2C[,1:2]
colnames(HHid)[colnames(HHid) == "o9"] = "hhid_re2"
#We merge the tables
dcrops <- merge(HHid, dlowcwide, by.x = "hhid_re2", all = T)
dcrops <- merge(dcrops, dupcwide, by.x = "hhid_re2", all = T)
#And we remove useless columns
dcrops <- dcrops[,-c(2,8:10,17)]


###2. Data analyzes - General

##2.1 Household characteristics

# # #2.1.a. o10. Are both the man and the woman available for the interview?
#We create a counting table and a title
o10count <- count(HouseholdLaos_2C, o10)
o10count <- o10count[c("2","1"),]
title <- "o10. Are both the man and the woman available 
for the interview?"
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(o10count,title,list)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - General
#We create a counting table and a title
o11count <- count(HouseholdLaos_2C, o11)
o11count <- o11count[c("3","2","1"),]
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent)"
list <- c("Male","Female","Both man and woman")
pond <- sum(o11count$n)
#Now we create the table with the corresponding function
PiePlotYN(o11count,title,list)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - /District
#We select appropriate data
data <- HouseholdLaos_2C[,c(1,16,43)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>% 
  count(`o6. select appropriate district/ city`,`o11. who do you do the interview with?`)
colnames(dcount)[2] <- "Answer" 
title <- "o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - /District"
pond <- nrow(HouseholdLaos_2C)
ylimn <- 0
ListBPlotDist(dcount,title,pond,ylimn)

# # #2.1.c. Men and Women distribution as Household Head
HouMember

##2.2 Crops characteristics

# # #2.1 Growing crops - Yes or No

# # #2.2.a. Crop diversity - Lowland (Household based)
#We convert data to the proper format
dlowc$d2_13e <- as.factor(dlowc$d2_13e)
#We create a counting table and a title
Cropcount <- count(dlowc, d2_13e)
#We determine other function parameters
title <- "Crop diversity among households (household based)"
pond <- nrow(HouseholdLaos_2C)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_13e <- as.character(Cropcount$d2_13e)
#We change too large crops label names 
Cropcount[1,1] <- "Summer-autumn\n season rice"
Cropcount[16,1] <- "Other vegetable\n crop"
Cropcount[19,1] <- "Chinese\n flowering cabbage/\n choysum"
Cropcount[22,1] <- "Other fruit\n crop"
Cropcount[26,1] <- "Bell pepper\n, red"
xT <- "Crops"
ylimn <- -10
#For the 14 first crops
Cropcount1 <- Cropcount[c(1:10),]
list1 <- Cropcount$d2_13e[c(1:10)]
dig <- 0
ListBPlot(Cropcount1,title,list1,pond,xT,ylimn,dig)
#For the 14 second crops
ylimn <- -5
dig <- 1
Cropcount2 <- Cropcount[c(11:20),]
list2 <- Cropcount$d2_13e[c(11:20)]
ListBPlot(Cropcount2,title,list2,pond,xT,ylimn,dig)
#For the 14 third crops
Cropcount3 <- Cropcount[c(21:29),]
list3 <- Cropcount$d2_13e[c(21:29)]
ListBPlot(Cropcount3,title,list3,pond,xT,ylimn,dig)


# # #2.2.b. Crop diversity - Upland (Household based)
#We convert data to the proper format
dupc$d2_23e <- as.factor(dupc$d2_23e)
#We create a counting table and a title
Cropcount <- count(dupc, d2_23e)
#We determine other function parameters
title <- "Crop diversity among households (household based) - Upland"
pond <- nrow(HouseholdLaos_2C)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_23e <- as.character(Cropcount$d2_23e)
#We change too large crops label names 
Cropcount[5,1] <- "Summer-autumn\n season rice"
Cropcount[11,1] <- "Other \nvegetable crop"
Cropcount[13,1] <- "maize_\ntraditional"
Cropcount[8,1] <- "Ginger,\n rhizome"
Cropcount[17,1] <- "Hmong\n cucumber"
Cropcount[20,1] <- "Other \nannual crop"
Cropcount[24,1] <- "Chinese \nplantain, leaves"
Cropcount[21,1] <- "Other \nfruit crop"
xT <- "Crops"
ylimn <- -10
#For the 14 first crops
Cropcount1 <- Cropcount[c(1:11),]
list1 <- Cropcount$d2_23e[c(1:11)]
dig <- 0
ListBPlot(Cropcount1,title,list1,pond,xT,ylimn,dig)
#For the 14 second crops
ylimn <- -5
dig <- 1
Cropcount2 <- Cropcount[c(12:22),]
list2 <- Cropcount$d2_23e[c(12:22)]
ListBPlot(Cropcount2,title,list2,pond,xT,ylimn,dig)
#For the 14 third crops
Cropcount3 <- Cropcount[c(23:32),]
list3 <- Cropcount$d2_23e[c(23:32)]
ListBPlot(Cropcount3,title,list3,pond,xT,ylimn,dig)

# # #2.2.b. Crop diversity (Area based)


##2.3 Animal characteristics
# # #2.2.a. e2. If yes, which below animal does your household have?
#First, we select the useful data
e2dat <- HouseholdLaos_2C[,c(1,16,1839:1851)]
#We restore the column label real names
e2dat <- copy_labels(e2dat, HouseholdLaos_2C)
#We prepare arguments for the function
s <- summary(HouseholdLaos_2C$e1)
pond <- nrow(HouseholdLaos_2C)
pond2 <- s[2]
xT <- "Animals"
ylimn <- -10
ylimx <- 100
title <- "e2. Which below animal does your household have? (Among household having animals)"
dig = 0
PracticePlot(e2dat,title,pond,pond2,xT,ylimn,ylimx,dig)



###3. Data analyzes for Agroecological principles

##3.2 - Recycling

# # #3.2.a. d17. Does your household use ecological/agroecological/integrated practices...
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$d17 <- as.factor(HouseholdLaos_2C$d17)
#We create a counting table and a title
d17count <- count(HouseholdLaos_2C, d17)
#We order the answers
d17count <- d17count[c("4","3","2","1"),]
title <- "d17. Does your household use ecological/
agroecological/integrated practices maintain/
enhance soil fertility in your fields?"
list <- c("yes","no","do not know", "NA")
#Now we create the table with the corresponding function
PiePlotYN(d17count,title,list)

# # #3.2.b. d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?
#First, we select the useful data
d18dat <- HouseholdLaos_2C[,c(1,16,1068:1079)]
#We modify long label names
var_label(d18dat$d183) <- 'Bokashi (fermented\norganic matter)'
var_label(d18dat$d184) <- 'Legume-based\ngreen manure'
var_label(d18dat$d185) <- 'Pulses in association\nand/or rotation\nwith main crop'
var_label(d18dat$d186) <- 'Cover crops in\nassociation and/\nor rotation\nwith main crop'
var_label(d18dat$d188) <- 'Crop residue\nmaintenance'
var_label(d18dat$d189) <- 'Recycling\ncrop waste'
var_label(d18dat$d1810) <- 'Ramial Wood Chip\n (RWC) or other\nwood chips'
var_label(d18dat$d1811) <- 'Organic agro-\nindustrial waste'
dum <- summary(HouseholdLaos_2C$d181)
pond <- d17count[1,2]
pond2 <- sum(d17count$n)
title <- "d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?"
xT <- "Practices"
ylimn <- -25
ylimx <- 100
dig = 0
PracticePlot(d18dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.2.c. d18_11. For which crop(s) do you use animal manure?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1811dat <- HouseholdLaos_2C[,c(1,1082:1092)]
s <- summary(d18dat$d181)
val <- s[2]
title <- "d18_11. For which crop(s) do you use animal manure? (% of households using animal manure)"
ylimn <- -20
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d1811dat,val,title,ylimn,ylimx,dig)

# # #3.2.d d18_21. For which crop(s) do you use Compost (heap)?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1821dat <- HouseholdLaos_2C[,c(1,1096:1106)]
s <- summary(d18dat$d182)
val <- s[2]
title <- "18_21. For which crop(s) do you use compost (heap)?
(% of households using compost (heap)"
ylimn <- -20
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d1821dat,val,title,ylimn,ylimx,dig)

##3.3 - Input reduction

# # #3.3.a d20. Does your household use any ecological/ agroecological/ integrated practices to control weeds in your fields?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$d20 <- as.factor(HouseholdLaos_2C$d20)
#We create a counting table and a title
d20count <- count(HouseholdLaos_2C, d20)
d20count <- d20count[c("4","3","2","1"),]
title <- "d20. Does your household use any ecological/\n agroecological/ integrated practices to\n control weeds in your fields?"
list <- c("yes","no","do not know","NA")
#Now we create the table with the corresponding function
PiePlotYN(d20count,title,list)

# # #3.3.b d21. Which of the following ecological/agroecological/integrated practices
#does your household use to control weeds in your fields?
#First, we select the useful data
d21dat <- HouseholdLaos_2C[,c(1,16,1252:1266)]
#We modify long label names
var_label(d21dat$d211) <- "Crop rotation /\n intercropping"
var_label(d21dat$d214) <- "Sowing date /\n rate / depth"
var_label(d21dat$d215) <- "Crop spatial\n arrangement"
var_label(d21dat$d216) <- "Seed cleaning\n before sowing"
var_label(d21dat$d2114) <- "Post harvest\n weed seed destruction\n in field"
pond <- d20count[1,2]
pond2 <- sum(d20count$n)
title <- "d21. Which of the following ecological/agroecological/integrated practices
does your household use to control weeds in your fields?"
xT <- "Practices"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(d21dat,title,pond,pond2,xT,ylimn,ylimx,dig)

  
##3.4 - Soil health
# # #3.4.a. Soil conservation practices Yes or No
#First we create a counting table and a title
d140count <- count(HouseholdLaos_2C, d140)
var_label(HouseholdLaos_2C$d14)
#We order the answers
title <- "d14. Which of the following soil conservation practices do you use?"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(d140count,title,list)

# # #3.4.b. d14. Which of the following soil conservation practices do you use ?
#First, we select the useful data
d14dat <- HouseholdLaos_2C[,c(1,16,944:951)]
#We modify long label names
var_label(d14dat$d141) <-  "Sowing in \ncontour lines"
var_label(d14dat$d142) <- "Natural or planted\n grass strips"
var_label(d14dat$d143) <- "Trees conservation \nin agricultural plots"
var_label(d14dat$d144) <- "Agroforestry \n(trees + crops)"
var_label(d14dat$d145) <- "Crop residues\n maintained to\n cover the soil"
var_label(d14dat$d147) <- "Reduced to\n no-tillage"
s <- summary(as.factor(HouseholdLaos_2C$d140))
pond <- s[1]
pond2 <- sum(d20count$n)
title <- "d14. Which of the following soil conservation practices do you use ?"
xT <- "Practices"
ylimn <- -10
ylimx <- 50
dig = 0
PracticePlot(d14dat,title,pond,pond2,xT,ylimn,ylimx,dig)


##3.5 - Animal health
# # #3.5.a e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$e58 <- as.factor(HouseholdLaos_2C$e58)
#We create a counting table and a title
e58count <- count(HouseholdLaos_2C, e58)
e58count <- e58count[c("3","2","1"),]
title <- "e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e58count,title,list)


# # #3.5.a e58_1. If yes to e58, which months?
#First, we select the useful data
e58_1dat <- HouseholdLaos_2C[,c(1,16,2133:2144)]
#We prepare the parameters for the function
s <- summary(HouseholdLaos_2C$e58)
pond <- s[3]
pond2 <- sum(d20count$n)
title <- "e58_1. If yes to e58, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
PracticePlotmonth(e58_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)


# # #3.5.c e59. Is there any month in the year when there is a lack of feed for the animals?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$e59 <- as.factor(HouseholdLaos_2C$e59)
#We create a counting table and a title
e59count <- count(HouseholdLaos_2C, e59)
e59count <- e59count[c("3","2","1"),]
title <- "e59. Is there any month in the year when 
there is a lack of feed for the animals?"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e59count,title,list)

# # #3.5.a e59_1. If yes to e59, which months?
#First, we select the useful data
e59_1dat <- HouseholdLaos_2C[,c(1,16,2147:2158)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdLaos_2C$e59))
pond <- s[3]
pond2 <- sum(d20count$n)
title <- "e59_1. If yes to e59, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
PracticePlotmonth(e59_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

##3.6 - Biodiversity

# # #3.6.a d32. Do you conserve and use traditional/local seeds?
#First, we select the useful data
#We create a counting table and a title
d32count <- count(HouseholdLaos_2C, d32)
title <- "d32. Do you conserve and use traditional/local seeds?"
d32count <- d32count[c("3","2","1"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(d32count,title,list)

# # #3.6.b. d32_1. If Yes to d32, for which crops? 
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d32_1dat <- HouseholdLaos_2C[,c(1,1753:1763)]
s <- summary(HouseholdLaos_2C$d32)
val <- s[3]
title <- "d32_1. If Yes to d32, for which crops? "
ylimn <- -15
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d32_1dat,val,title,ylimn,ylimx,dig)

# # #3.6.c. e5_b. Do you have any local breeds of  e4_1 at the time of the survey?
#First, we select the useful data
#We create a counting table and a title
e5_bcount <- count(HouseholdLaos_2C, e5_b)
title <- "e5_b. Do you have any local breeds of most important animal
at the time of the survey?"
e5_bcount <- e5_bcount[c("3","2","1"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e5_bcount,title,list)

# # #3.6.c. e5_b_1. Animal diversity for local breeds (most important animal)
#First, we select the useful data
#We create a counting table and a title
e5_b_1count <- HouseholdLaos_2C[,c(1872,1879)]
title <- "e5_b_1. Animal diversity for local breeds (most important animal)"
s <- summary(HouseholdLaos_2C$e5_b)
pond <- s[3]
#Now we create the table with the corresponding function
AnimalPlot(e5_b_1count,title,pond)

##3.7 - Synergy

# # #3.7.a. d26. Does your household use agroecological/integrated practices to control pests and disease in your fields? 
#First, we select the useful data
#We create a counting table and a title
d26count <- count(HouseholdLaos_2C, d26)
title <- "d26. Does your household use agroecological/integrated practices
to control pests and disease in your fields? "
d26count <- d26count[c("4","3","2","1"),]
list <- c("yes","no","do not know","NA")
#Now we create the table with the corresponding function
PiePlotYN(d26count,title,list)

# # #3.7.b. d27. Which ecological/agroecological/integrated practices do you
#use to control pets and diseases in your fields?
#First, we select the useful data
d27dat <- HouseholdLaos_2C[,c(1,16,1484:1498)]
#We modify long label names
var_label(d27dat$d271) <- "Crop rotation /\n intercropping"
var_label(d27dat$d274) <- "Soil health \nmaintenance/\nimprovement"
var_label(d27dat$d275) <- "Sanitation practices\n (removal of damaged/\ninfected plants\n and fruits)"
var_label(d27dat$d277) <- "Water and\n nutrient \nmanagement"
var_label(d27dat$d278) <- "Cultivar choice\n (tolerant/resistant) /\n cultivar mixture"
var_label(d27dat$d279) <- "Biopesticide /\n organic pesticide"
var_label(d27dat$d2710) <- "BCAs"
var_label(d27dat$d2711) <- "Home-made efficient\n microorganism (EM)"
var_label(d27dat$d2712) <- "Commercial efficient\n microorganism (EM)"
s <- summary(HouseholdLaos_2C$d26)
pond <- as.numeric(s[4])
pond2 <- sum(d17count$n)
title <- "d27 - Which ecological/agroecological/integrated practices do you
use to control pets and diseases in your fields?"
xT <- "Practices"
ylimn <- -20
ylimx <- 30
dig = 1
#Function call
PracticePlot(d27dat,title,pond,pond2,xT,ylimn,ylimx,dig)


##3.8- Economic diversification

# # #3.8.a. (Need to create an indicator including all the product household sell,
#animal and crops)
#TO DO

# # #3.8.b. b9.Are there specific months of the year in which you face financial difficulties?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$b9 <- as.factor(HouseholdLaos_2C$b9)
#We create a counting table and a title
b9count <- count(HouseholdLaos_2C, b9)
title <- "b9. Are there specific months of the year in which
you face financial difficulties?"
b9count <- b9count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(b9count,title,list)

# # #3.8.c. b9_1. If yes to b9, which months?
#First, we select the useful data
b9_1dat <- HouseholdLaos_2C[,c(1,16,159:170)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdLaos_2C$b9))
pond <- s[2]
pond2 <- sum(d17count$n)
title <- "b9_1. If yes to b9, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 60
dig = 0
PracticePlotmonth(b9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

##3.9 - Co-creation of knowledge

# # #3.9.a. c14. Do you exchange your agricultural products,
#equipment or animals with other farmers? 
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$c14 <- as.factor(HouseholdLaos_2C$c14)
#We create a counting table and a title
c14count <- count(HouseholdLaos_2C, c14)
title <- "c14. Do you exchange your agricultural products,
equipment or animals with other farmers? "
c14count <- c14count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c14count,title,list)

# # #3.9.b. f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?
#We convert data to the proper format
HouseholdLaos_2C$f4 <- as.factor(HouseholdLaos_2C$f4)
#We create a counting table and a title
f4count <- count(HouseholdLaos_2C, f4)
title <- "f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?"
list <- c("4.Almost enough time","6.Do not know","3.Moderate amount of time","1.No time","5.Sufficient amount of time","2.Very little time")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(f4count,title,list,pond,xT,ylimn)

##3.10 - Social value and diet

# # #3.10.a. i1. In general, what is the proportion of the food (rice, vegetable, animal
#products, etc.) consumed by your family that comes from your own farm or homegarden?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$i1 <- as.factor(HouseholdLaos_2C$i1)
#We create a counting table and a title
i1count <- count(HouseholdLaos_2C, i1)
title <- "i1. Proportion of the food that comes from the household farm"
list <- c("2.25-50%","3.50-75%","1.Less than 25%","4.Over 75%")
pond <- sum(d17count$n)
#Now we create the table with the corresponding function
xT = "Answer"
ylimn <- -20
ListBPlotNO(i1count,title,list,pond,xT,ylimn)

# # #3.10.b g5. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$g5 <- as.factor(HouseholdLaos_2C$g5)
#We create a counting table and a title
g5count <- count(HouseholdLaos_2C, g5)
g5count <- g5count[c("2","1"),]
list <- c("yes","no")
title <- "g5. Do you think the working hours across family
members are equitably distributed?"
#Now we create the table with the corresponding function
PiePlotYN(g5count,title,list)

##3.11 - Fairness

# # #3.11.a. b22. Did you sell any certified crop/vegetables/fruit/livestock
#related product in the past year/12 months?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$b22 <- as.factor(HouseholdLaos_2C$b22)
#We create a counting table and a title
b22count <- count(HouseholdLaos_2C, b22)
b22count <- b22count[c("2","1"),]
list <- c("yes","no")
title <- "b22. Did you sell any certified crop/vegetables/fruit/livestock
related product in the past year/12 months?"
#Now we create the table with the corresponding function
PiePlotYN(b22count,title,list)

# # #3.11.b. b22_1. If yes, which one(s)? 
#First, we select the useful data
b22_1dat <- HouseholdLaos_2C[,c(1,22,208:222)]
#We modify long label names
var_label(b22_1dat$b22_113) <- "Dried meat (pork,\n beef, etc.)"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdLaos_2C$b22))
pond <- s[2]
pond2 <- nrow(HouseholdLaos_2C)
title <- "b22_1. If yes, which one(s)?"
xT <- "Certified crops"
ylimn <- -20
ylimx <- 110
dig = 0
#Function Call
var_label(b22_1dat[2]) <- "o6. select appropriate district/ city"
PracticePlot(b22_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.11.b. c8. Did you sign a contract whereby the buyer commits to buy 
#from you following some at specific conditions (price, volume, quality, time...)?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$c8 <- as.factor(HouseholdLaos_2C$c8)
#We create a counting table and a title
c8count <- count(HouseholdLaos_2C, c8)
title <- "c8. Did you sign a contract whereby the buyer commits to buy 
from you following some at specific conditions (price, volume, quality, time...)?"
c8count <- c8count[c("3","2","1"),]
list <- c("yes","no","NA")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(c8count,title,list,pond,xT,ylimn)

##3.12 - Connectivity
# # #3.12.a. c13. Do you collaborate with other people for any task ?
#We create a counting table and a title
c130count <- count(HouseholdLaos_2C, c130)
list <- c("yes","no")
title <- "c13. Do you collaborate with other people for any task ?"
#Now we create the table with the corresponding function
PiePlotYN(c130count,title,list)

# # #3.12.b. c13. For which tasks ?
#First, we select the useful data
c13dat <- HouseholdLaos_2C[,c(1,16,605:610,612)]
#We modify long label names
var_label(c13dat$c131) <- "Share labor (mutual\n help, working together\n on each other farm)"
var_label(c13dat$c132) <- "Manage water/\nirrigation systems"
var_label(c13dat$c134) <- "Buy agricultural\n inputs"
var_label(c13dat$c135) <- "Selling products\n to the markets for\n other farmers"
var_label(c13dat$c136) <- "Experiment new\n farming practices"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdLaos_2C$c130))
pond <- s[1]
pond2 <- sum(d17count$n)
title <- "c13. For which tasks do you collaborate with other people ?"
xT <- "Months"
ylimn <- -20
ylimx <- 100
dig = 0
#Function Call
PracticePlot(c13dat,title,pond,pond2,xT,ylimn,ylimx,dig)


# # #3.12.b c15. Are you involved in some form of advocacy work (aiming 
#to influence decision-making within political institutions)?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$c15 <- as.factor(HouseholdLaos_2C$c15)
#We create a counting table and a title
c15count <- count(HouseholdLaos_2C, c15)
title <- "c15. Are you involved in some form of advocacy work (aiming 
to influence decision-making within political institutions)?"
c15count <- c15count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c15count,title,list)

##3.13 - Land and natural resource governance

# # #3.13.a. l1. In the past year/12 months, did your household benefit from government
#subsidies to support investment in production or commercialization activities?
#We create a counting table and a title
l1count <- count(HouseholdLaos_2C, l1)
title <- "l1. In the past year/12 months, did your household
benefit from government subsidies to support investment
in production or commercialization activities?"
l1count <- l1count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(l1count,title,list)


# # #3.13.b. l2. How much of your total household income in 2022 did these 
#government subsidies represent?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$l2 <- as.factor(HouseholdLaos_2C$l2)
#We create a counting table and a title
l2count <- count(HouseholdLaos_2C, l2)
l2count <- l2count %>%  filter(!is.na(l2))
title <- "l2. How much of your total household income in 2022 did these 
government subsidies represent?"
list <- c("6.NA","2.25-50%","3.50-75%","5.Do not know","1.Less than 25%","4.over 75%")
pond <- sum(l2count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(l2count,title,list,pond,xT,ylimn)

# # #3.13.c. d9-10-11. Land ownership
#First, we select the useful data
d9_1dat <- HouseholdLaos_2C[,c(1,16,804,806,808)]
#We modify long label names
d9_1dat$d9_1 <- str_replace(d9_1dat$d9_1, "Yes", "1")
d9_1dat$d9_1 <- str_replace(d9_1dat$d9_1, "No", "0")
d9_1dat$d10_1 <- str_replace(d9_1dat$d10_1, "Yes", "1")
d9_1dat$d10_1 <- str_replace(d9_1dat$d10_1, "No", "0")
d9_1dat$d11_1 <- str_replace(d9_1dat$d11_1, "Yes", "1")
d9_1dat$d11_1 <- str_replace(d9_1dat$d11_1, "No", "0")
var_label(d9_1dat$d9_1) <- "Having plots\n rented-in?"
var_label(d9_1dat$d10_1) <- "Having plots\n rented-out?"
var_label(d9_1dat$d11_1) <- "Having plots\n owned?"
#We prepare the parameters for the function
pond <- sum(d17count$n)
pond2 <- sum(d17count$n)
title <- "d9-10-11. Land ownership"
xT <- ""
ylimn <- -20
ylimx <- 100
dig = 1
#Function call
PracticePlot(d9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)


##3.14 - Participation

# # #3.14.a. c2. Are you a member of one or more farmer organization (e.g. crops/fruits/
#livestock/honey/ water/Forest etc)?
#First, we select the useful data
#We convert it to the proper format
HouseholdLaos_2C$c2 <- as.factor(HouseholdLaos_2C$c2)
#We create a counting table and a title
c2count <- count(HouseholdLaos_2C, c2)
title <- "Are you a member of one or more farmer organization 
(e.g. crops/fruits/livestock/honey/ water/Forest etc)?"
list <- c("1.No","3.Yes, more than one","2.Yes, one")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(c2count,title,list,pond,xT,ylimn)

# # #3.14.b. c1. Are you or anyone in your household active in one or several
#of the following Y/N ?
#We create a counting table and a title
c1_0count <- count(HouseholdLaos_2C, c1_0)
title <- "c1. Are you or anyone in your household active in one or several
of an Union?"
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c1_0count,title,list)

# # #3.14.b. c1. Are you or anyone in your household active in one or several
#of the following ?
#First, we select the useful data
c1dat <- HouseholdLaos_2C[,c(1,16,544:550)]
#We modify long label names
var_label(c1dat$c1_1) <- "c1_1. Women\n union"
var_label(c1dat$c1_2) <- "c1_2. Youth\n union"
var_label(c1dat$c1_3) <- "c1_3. Veteran\n union"
var_label(c1dat$c1_4) <- "c1_4. Farmer\n union"
var_label(c1dat$c1_5) <- "c1_5. Elderly\n union"
var_label(c1dat$c1_6) <- "c1_6. Political\n party"
var_label(c1dat$c1_88) <- "Do not know"
#We prepare the parameters for the function
pond <- sum(d17count$n)
pond2 <- sum(d17count$n)
title <- "c1. Are you or anyone in your household active in one or several
of the following?"
xT <- "Months"
ylimn <- -20
ylimx <- 85
dig = 0
#Function Call
PracticePlot(c1dat,title,pond,pond2,xT,ylimn,ylimx,dig)



  