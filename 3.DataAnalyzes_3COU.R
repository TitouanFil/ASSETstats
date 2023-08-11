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
#We import all the dta files transmitted by Ky as *Vietnam* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos_2C <- readRDS("HouseholdLaos_2C.rds")
ClowlandLaos_2C <- readRDS("ClowlandLaos_2.rds")
CuplandLaos_2C <- readRDS("CuplandLaos_2.rds")
HouMemberLaos_2C <- readRDS("HouMemberLaos_2C.rds")

## Get gps data for each database and export it under csv format
#gpscamb <- HouseholdCambodia_2C[,c(1,20,23,26,29,32,2523:2525)]
#gpslao <- HouseholdLaos_2C[,c(17,19,22,25,28,2304:2306)]
#gpsviet <- HouseholdVietnam_2C[,c(1,20,23,26,29,32,2343:2345)]

#Export
#write.csv2(gpscamb, file = "gpscamb.csv")
#write.csv2(gpslao, file = "gpslao.csv")
#write.csv2(gpsviet, file = "gpsviet.csv")


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

# # #1.2.c. List PLOT with Country information (as x)
ListBplotCoun <- function(data,title,pond,ylimn,dig){
  dcount <- data
  colnames(dcount)[2] <- "Answer" 
  #We convert absolute values to % y divinding by the total number of households
  dcount$nbHHtot <- ifelse(dcount$country_eng_preload  == 'Vietnam',
                           sum(CLV$country_eng_preload == 'Vietnam'),
                           ifelse(dcount$country_eng_preload  == 'Cambodia',
                           sum(CLV$country_eng_preload == 'Cambodia'),
                           sum(CLV$country_eng_preload == 'Lao')))
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= `country_eng_preload`, y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity",position = 'stack')+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = dcount$Percentage_of_households/10,
              position = "stack")+
    scale_fill_brewer(palette="YIGn")+
    theme_stata()+
    ggtitle(paste(title))
  print(plotb)
  
summary(HouseholdVietnam_2C$village_eng_preload)
}

# # #1.2.d. List PLOT with Country information (as fill)
ListBplotCoun2 <- function(data,title,pond,ylimn,dig){
  dcount <- data
  #We convert absolute values to % y divinding by the total number of households
  dcount$nbHHtot <- ifelse(dcount$'Country name (english)' == 'Rous Ran',
                           sum(HouseholdVietnam_2C$Country_eng_preload == 'Rous Ran'),
                           ifelse(dcount$'Country name (english)' == 'Rohas',
                                  sum(HouseholdVietnam_2C$Country_eng_preload == 'Rohas'),
                                  ifelse(dcount$'Country name (english)' == 'Rik Reay',
                                         sum(HouseholdVietnam_2C$Country_eng_preload == 'Rik Reay'),
                                         sum(HouseholdVietnam_2C$Country_eng_preload == 'Reaksmei'))))
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= Answer, y=Percentage_of_households, fill = `Country name (english)`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, max(dcount$Percentage_of_households)+20)+
    scale_fill_brewer(palette="YIGn")+
    theme_stata()+
    ggtitle(paste(title,"\n N=", pond))
  print(plotb)
}

# # #1.2.d. Country Plot for One column results (for HouMember data)
ListBplotCounHouM <- function(data,title,pond,ylimn,dig){
dcount <- data
dcount$nbHHtot <- ifelse(dcount$'Country' == 'Rous Ran',
                         sum(HouseholdVietnam_2C$Country_eng_preload == 'Rous Ran'),
                         ifelse(dcount$'Country' == 'Rohas',
                                sum(HouseholdVietnam_2C$Country_eng_preload == 'Rohas'),
                                ifelse(dcount$'Country' == 'Rik Reay',
                                       sum(HouseholdVietnam_2C$Country_eng_preload == 'Rik Reay'),
                                       sum(HouseholdVietnam_2C$Country_eng_preload == 'Reaksmei'))))
dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
#And now a distribution plot
plotb <- ggplot(data=dcount, aes(x= Answer, y=Percentage_of_households, fill = Country))+
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
  dmelt <- melt(data ,  id.vars = c('o9. household id', 'country name (english)'), variable.name = 'Practices')
  
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
  
  # # # #b.b Plot with countrys and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`country name (english)`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  dcountNAR <- dcountNAR %>% filter(`country name (english)` != '')
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$'country name (english)'  == 'Vietnam',
                           sum(CLV$country_eng_preload == 'Vietnam'),
                           ifelse(dcountNAR$'country name (english)'  == 'Cambodia',
                                  sum(CLV$country_eng_preload == 'Cambodia'),
                                  sum(CLV$country_eng_preload == 'Lao')))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = `country name (english)`))+
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
Monthplot <- function(data,title,pond,pond2,xT,ylimn,ylimx,dig){
  #And we change columns names for label names
  colnames(data) <- var_label(data)
  #Then we adapt the format to be able to plot it and create a table
  dmelt <- melt(data ,  id.vars = c('o9. household id', 'country name (english)'), variable.name = 'Practices')
  
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
  
  # # # #b.b Plot with countrys and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`country name (english)`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$'country name (english)'  == 'Vietnam',
                              sum(CLV$country_eng_preload == 'Vietnam'),
                              ifelse(dcountNAR$'country name (english)'  == 'Cambodia',
                                     sum(CLV$country_eng_preload == 'Cambodia'),
                                     sum(CLV$country_eng_preload == 'Lao')))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=Practices, y=Percentage_of_households, fill = `country name (english)`))+
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

# # #1.2.h. CROP PLOT small tables
#For this function, we should use a database including column with household id 
#+ 11 columns with binary values for each crops
CropPlot2 <- function(data,pond, title,ylimn,ylimx,dig){
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
  #And now a distribution plot
  Plota <- ggplot(data=dcountNAR, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
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
}

# # #1.2.h. ANIMAL PLOT
AnimalPlot <- function(data,title,pond){
  data[,2] <- as.character(data[,2])
  colnames(data)[2] <- "Animal"
  for (j in 1:594){
    data[j,2] <- ifelse(data[j,2] == 1, data[j,1], data[j,2])
  }
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


###2. Data analyzes - General

##2.1 Household characteristics
{
# # #2.1.a. o10. Are both the man and the woman available for the interview?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(20,48)]
Camb$o10 <- as.character(Camb$o10)
Lao <- HouseholdLaos_2C[,c(19,42)]
Lao$o10 <- as.character(Lao$o10)
Lao$o10[Lao$o10 == "Yes (both man and woman)"] <- "2"
Lao$o10[Lao$o10 == "No (only 1 person)"] <- "1"
Viet <- HouseholdVietnam_2C[,c(20,48)]
Viet$o10 <- as.character(Viet$o10)
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet) 
#We create a counting table and a title
dcount <- CLV %>% count(country_eng_preload, o10)
#We remove NA
dcount <- dcount %>% filter(country_eng_preload != '')
title <- "o10. Are both the man and the woman available 
for the interview?"
dcount$o10 <- c("yes","no","no","yes","yes","no")
pond <- 20
xT <- "Answer"
ylim <- 0
dig <- 0
#Now we create the table with the corresponding function
ListBplotCoun(dcount,title,pond,ylimn,dig)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - General
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(20,48,49)]
Camb$o11 <- as.character(Camb$o11)
Lao <- HouseholdLaos_2C[,c(19,42,43)]
Lao$o11 <- as.character(Lao$o11)
Lao$o10 <- as.character(Lao$o10)
Lao$o10[Lao$o10 == "Yes (both man and woman)"] <- "1"
Lao$o10[Lao$o10 == "No (only 1 person)"] <- "2"
Lao$o11[Lao$o11 == "Male"] <- "1"
Lao$o11[Lao$o11 == "Female"] <- "2"
Lao$o11[Lao$o11 == "Both man and woman"] <- "3"
Viet <- HouseholdVietnam_2C[,c(20,48,49)]
Viet$o11 <- as.character(Viet$o11)
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
CLV$o11b <- ifelse(CLV$o10 == "1", '3', CLV$o11)
#We create a counting table and a title
dcount <- CLV %>% count(country_eng_preload, o11b)
#We remove NA
dcount <- dcount %>% filter(country_eng_preload != '')
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent)"
dcount$o11b <- c("Male","Female","Both man and woman","Male","Female","Both man and woman","Male","Female","Both man and woman")
pond <- 20
xT <- "Answer"
ylim <- 0
dig <- 0
#Now we create the table with the corresponding function
ListBplotCoun(dcount,title,pond,ylimn,dig)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - /Country
#We select appropriate data
data <- HouseholdVietnam_2C[,c(1,29,49)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`Country name (english)`,`o11. who do you do the interview with?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("Male","Female")
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent) - /Country"
pond <- nrow(HouseholdVietnam_2C)
ylimn <- 0
ListBplotCoun(dcount,title,pond,ylimn)
}

##2.2 Land & Crops characteristics
{
# # #2.2.a. d1. What kinds of land did your household have in the past 12 months?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(1,20,582:589)]
for (i in 3:10){
  Camb[,i] <- as.character(Camb[,i])
}
Camb$d1_4 <- ifelse(Camb$d1_4 !=1 & Camb$d1_8 == 1, '1',Camb$d1_4)
Camb <- Camb[,c(1:9)]
Lao <- HouseholdLaos_2C[,c(17,19,617:623)]
for (i in 3:9){
  Lao[,i] <- as.character(Lao[,i])
}
Viet <- HouseholdVietnam_2C[,c(1,20,661:667)]
for (i in 3:9){
  Viet[,i] <- as.character(Viet[,i])
}
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We restore the column label real names
CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#We fulfil the information required in the function
pond <- nrow(CLV)
pond2 <- pond
xT <- "Kind of land owned"
ylimn <- -20
ylimx <- 100
title <- "d1. What kinds of land did your household have in the past 12 months?"
dig = 0
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)


##2.3 Animal characteristics
# # #2.2.a. e2. If yes, which below animal does your household have?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(1,20,2032:2044)]
for (i in 3:15){
  Camb[,i] <- as.character(Camb[,i])
}
Camb <- Camb[,c(1:9)]
Lao <- HouseholdLaos_2C[,c(17,19,1839:1851)]
for (i in 3:15){
  Lao[,i] <- as.character(Lao[,i])
}
Viet <- HouseholdVietnam_2C[,c(1,20,1871:1883)]
for (i in 3:15){
  Viet[,i] <- as.character(Viet[,i])
}
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We restore the column label real names
CLV <- copy_labels(CLV, HouseholdVietnam_2C)
pond <- nrow(CLV)
pond2 <- nrow(CLV)
xT <- "Animals"
ylimn <- -10
ylimx <- 100
title <- "e2. Which below animal does your household have?"
dig = 0
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)
}


###3. Data analyzes for Agroecological principles

##3.1 - Recycling
{

# # #3.1.a. d17. Does your household use ecological/agroecological/integrated practices...
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(20,1104)]
Camb$d17 <- as.character(Camb$d17)
Lao <- HouseholdLaos_2C[,c(19,1066)]
Lao$d17 <- as.character(Lao$d17)
Lao$d17[Lao$d17 == "Yes"] <- "1"
Lao$d17[Lao$d17 == "No"] <- "0"
Lao$d17[Lao$d17 == "Do not know"] <- "88"
Viet <- HouseholdVietnam_2C[,c(20,1112)]
Viet$d17 <- as.character(Viet$d17)
Viet$d17[Viet$d17 == "2"] <- "1"
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet) 
#We create a counting table and a title
d17count <- CLV %>% count(country_eng_preload, d17)
d17count <- d17count %>% filter(country_eng_preload != '')
d17count$d17 <- ifelse(d17count$d17 == '', NA, d17count$d17)
d17count <- d17count[order(d17count$country_eng_preload,d17count$d17),]
#We order the answers
d17count$d17 <- c("no","yes","do not know","NA")
pond <- 20
xT <- "Answer"
ylimn <- 0
dig <- 0
title <- "d17. Does your household use ecological/
agroecological/integrated practices maintain/
enhance soil fertility in your fields?"
list <- c("no","yes","do not know", "NA")
#Now we create the table with the corresponding function
ListBplotCoun(d17count,title,pond,ylimn,dig)

# # #3.1.b. d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(1,20,1106:1117)]
for (i in 3:14){
  Camb[,i] <- as.character(Camb[,i])
}
Lao <- HouseholdLaos_2C[,c(17,19,1068:1079)]
for (i in 3:14){
  Lao[,i] <- as.character(Lao[,i])
}
Viet <- HouseholdVietnam_2C[,c(1,20,1114:1125)]
for (i in 3:14){
  Viet[,i] <- as.character(Viet[,i])
}
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We restore the column label real names
CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#We fulfil the information required in the function
#First, we select the useful data
#We modify long label names
var_label(CLV$d183) <- 'Bokashi (fermented\norganic matter)'
var_label(CLV$d184) <- 'Legume-based\ngreen manure'
var_label(CLV$d185) <- 'Pulses in association\nand/or rotation\nwith main crop'
var_label(CLV$d186) <- 'Cover crops in\nassociation and/\nor rotation\nwith main crop'
var_label(CLV$d188) <- 'Crop residue\nmaintenance'
var_label(CLV$d189) <- 'Recycling\ncrop waste'
var_label(CLV$d1810) <- 'Ramial Wood Chip\n (RWC) or other\nwood chips'
var_label(CLV$d1811) <- 'Organic agro-\nindustrial waste'
dum <- summary(CLV$d181)
pond <- d17count[1,2]
pond2 <- sum(d17count$n)
title <- "d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?"
xT <- "Practices"
ylimn <- -15
ylimx <- 100
dig = 0
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)

}

##3.2 - Input reduction
{
  
# # #3.2.a. d20. Does your household use any ecological/ agroecological/ integrated practices to control weeds in your fields?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,1324)]
  Camb$d20 <- as.character(Camb$d20)
  Lao <- HouseholdLaos_2C[,c(19,1250)]
  Lao$d20 <- as.character(Lao$d20)
  Lao$d20[Lao$d20 == "Yes"] <- "1"
  Lao$d20[Lao$d20 == "No"] <- "0"
  Lao$d20[Lao$d20 == "Do not know"] <- "88"
  Viet <- HouseholdVietnam_2C[,c(20,1296)]
  Viet$d20 <- as.character(Viet$d20)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  d20count <- CLV %>% count(country_eng_preload, d20)
  d20count <- d20count %>% filter(country_eng_preload != '')
  d20count$d20 <- ifelse(d20count$d20 == '', NA, d20count$d20)
  d20count <- d20count[order(d20count$country_eng_preload,d20count$d20),]
  #We order the answers
  d20count$d20 <- c("no","yes","do not know","NA","no","yes","do not know","NA","no","yes","do not know")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "d20. Does your household use any ecological/ agroecological/
  integrated practices to control weeds in your fields?"
  list <- c("no","yes","do not know", "NA")
  #Now we create the table with the corresponding function
  ListBplotCoun(d20count,title,pond,ylimn,dig)

# # #3.2.b. d21. Which of the following ecological/agroecological/integrated practices
#does your household use to control weeds in your fields?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,1326:1340)]
  for (i in 3:14){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,1252:1266)]
  for (i in 3:14){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,1298:1312)]
  for (i in 3:14){
    Viet[,i] <- as.character(Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#We modify long label names
var_label(CLV$d211) <- "Crop rotation /\n intercropping"
var_label(CLV$d214) <- "Sowing date /\n rate / depth"
var_label(CLV$d215) <- "Crop spatial\n arrangement"
var_label(CLV$d216) <- "Seed cleaning\n before sowing"
var_label(CLV$d2114) <- "Post harvest\n weed seed destruction\n in field"
pond <- d20count[1,2]
pond2 <- sum(d20count$n)
title <- "d21. Which of the following ecological/agroecological/integrated practices
does your household use to control weeds in your fields?"
xT <- "Practices"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)


}
  
##3.3 - Soil health
{

# # #3.3.a. Soil conservation practices Yes or No
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,957)]
  Camb$d140 <- as.character(Camb$d140)
  Lao <- HouseholdLaos_2C[,c(19,943)]
  Lao$d140 <- as.character(Lao$d140)
  Viet <- HouseholdVietnam_2C[,c(20,989)]
  Viet$d140 <- as.character(Viet$d140)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  d140count <- CLV %>% count(country_eng_preload, d140)
  d140count <- d140count %>% filter(country_eng_preload != '')
  #We order the answers
  d140count$d140 <- c("yes","no","NA")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  #We order the answers
  title <- "d140. Does your household use Soil conservation practices"
  list <- c("yes","no","NA")
  #Now we create the table with the corresponding function
  ListBplotCoun(d140count,title,pond,ylimn,dig)

# # #3.3.b. d14. Which of the following soil conservation practices do you use ?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,958:965)]
  for (i in 3:10){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,944:951)]
  for (i in 3:10){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,990:997)]
  for (i in 3:10){
    Viet[,i] <- as.character(Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#We modify long label names
var_label(CLV$d141) <-  "Sowing in \ncontour lines"
var_label(CLV$d142) <- "Natural or planted\n grass strips"
var_label(CLV$d143) <- "Trees conservation \nin agricultural plots"
var_label(CLV$d144) <- "Agroforestry \n(trees + crops)"
var_label(CLV$d145) <- "Crop residues\n maintained to\n cover the soil"
var_label(CLV$d147) <- "Reduced to\n no-tillage"
pond <- 20
pond2 <- nrow(CLV)
title <- "d14. Which of the following soil conservation practices do you use ?"
xT <- "Practices"
ylimn <- -10
ylimx <- 50
dig = 0
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.4 - Animal health
{

# # #3.4.a. e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,2349)]
  Camb$e58 <- as.character(Camb$e58)
  Lao <- HouseholdLaos_2C[,c(19,2131)]
  Lao$e58 <- as.character(Lao$e58)
  Viet <- HouseholdVietnam_2C[,c(20,2165)]
  Viet$e58 <- as.character(Viet$e58)
  Lao$e58[Lao$e58 == "Yes"] <- "1"
  Lao$e58[Lao$e58 == "No"] <- "0"
  Lao$e58[Lao$e58 == "Do not know"] <- "88"
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  e58count <- CLV %>% count(country_eng_preload, e58)
  e58count <- e58count %>% filter(country_eng_preload != '')
  e58count$e58 <- ifelse(e58count$e58 == '', NA, e58count$e58)
  e58count <- e58count[order(e58count$country_eng_preload,e58count$e58),]
  #We order the answers
  e58count$e58 <- c("no","yes","NA")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?"
  list <- c("no","yes", "NA")
  #Now we create the table with the corresponding function
  ListBplotCoun(e58count,title,pond,ylimn,dig)



# # #3.4.b. e58_1. If yes to e58, which months?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,2351:2362)]
  for (i in 3:14){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,2133:2144)]
  for (i in 3:14){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,2167:2178)]
  for (i in 3:14){
    Viet[,i] <- as.character(Viet[,i])
    Viet[,i] <- ifelse(Viet[,i] == "1", "0", Viet[,i])
    Viet[,i] <- ifelse(Viet[,i] == "2", "1", Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#We prepare the parameters for the function
pond <- nrow(CLV)
pond2 <- nrow(CLV)
title <- "e58_1. If yes to e58, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
Monthplot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)

}

##3.5 - Biodiversity
{

# # #3.5.a d32. Do you conserve and use traditional/local seeds?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,1942)]
  Camb$d32<- as.character(Camb$d32)
  Lao <- HouseholdLaos_2C[,c(19,1751)]
  Lao$d32 <- as.character(Lao$d32)
  Viet <- HouseholdVietnam_2C[,c(20,1784)]
  Viet$d32 <- as.character(Viet$d32)
  Lao$d32[Lao$d32 == "Yes"] <- "1"
  Lao$d32[Lao$d32 == "No"] <- "0"
  Lao$d32[Lao$d32 == "Do not know"] <- "88"
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  d32count <- CLV %>% count(country_eng_preload, d32)
  d32count <- d32count %>% filter(country_eng_preload != '')
  d32count$d32 <- ifelse(d32count$d32 == '', NA, d32count$d32)
  d32count <- d32count[order(d32count$country_eng_preload,d32count$d32),]
  #We order the answers
  d32count$d32 <- c("no","yes","NA")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "d32. Do you conserve and use traditional/local seeds?"
  list <- c("no","yes", "NA")
  #Now we create the table with the corresponding function
  ListBplotCoun(d32count,title,pond,ylimn,dig)

}

##3.6 - Synergy
{

# # #3.6.a. d26. Does your household use agroecological/integrated practices to control pests and disease in your fields? 
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,1601)]
  Camb$d26<- as.character(Camb$d26)
  Lao <- HouseholdLaos_2C[,c(19,1482)]
  Lao$d26 <- as.character(Lao$d26)
  Lao$d26[Lao$d26 == "Yes"] <- "1"
  Lao$d26[Lao$d26 == "No"] <- "0"
  Lao$d26[Lao$d26 == "Do not know"] <- "88"
  Viet <- HouseholdVietnam_2C[,c(20,1515)]
  Viet$d26 <- as.character(Viet$d26)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  d26count <- CLV %>% count(country_eng_preload, d26)
  d26count <- d26count %>% filter(country_eng_preload != '')
  d26count$d26 <- ifelse(d26count$d26 == '', NA, d26count$d26)
  d26count <- d26count[order(d26count$country_eng_preload,d26count$d26),]
  #We order the answers
  d26count$d26 <- c("no","yes","Do not know", "NA")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "d26. Does your household use agroecological/integrated practices
to control pests and disease in your fields? "
  list <- c("no","yes","Do not know", "NA")
  #Now we create the table with the corresponding function
  ListBplotCoun(d26count,title,pond,ylimn,dig)

# # #3.6.b. d27. Which ecological/agroecological/integrated practices do you
#use to control pets and diseases in your fields?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,1603:1616,1618)]
  for (i in 3:17){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,1484:1498)]
  for (i in 3:17){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,1517:1531)]
  for (i in 3:17){
    Viet[,i] <- as.character(Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
  CLV <- CLV[,1:11]
#We modify long label names
var_label(CLV$d271) <- "Crop rotation /\n intercropping"
var_label(CLV$d274) <- "Soil health \nmaintenance/\nimprovement"
var_label(CLV$d275) <- "Sanitation practices\n (removal of damaged/\ninfected plants\n and fruits)"
var_label(CLV$d277) <- "Water and\n nutrient \nmanagement"
var_label(CLV$d278) <- "Cultivar choice\n (tolerant/resistant) /\n cultivar mixture"
var_label(CLV$d279) <- "Biopesticide /\n organic pesticide"
var_label(CLV$d2710) <- "BCAs"
var_label(CLV$d2711) <- "Home-made efficient\n microorganism (EM)"
var_label(CLV$d2712) <- "Counercial efficient\n microorganism (EM)"
pond <- nrow(CLV)
pond2 <- nrow(CLV)
title <- "d27 - Which ecological/agroecological/integrated practices do you
use to control pets and diseases in your fields?"
xT <- "Practices"
ylimn <- -20
ylimx <- 30
dig = 0
#Function call
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.7 - Economic diversification
{

# # #3.7.d. b9.Are there specific months of the year in which you face financial difficulties?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,171)]
  Camb$b9<- as.character(Camb$b9)
  Lao <- HouseholdLaos_2C[,c(19,157)]
  Lao$b9 <- as.character(Lao$b9)
  Lao$b9[Lao$b9 == "Yes"] <- "1"
  Lao$b9[Lao$b9 == "No"] <- "0"
  Lao$b9[Lao$b9 == "Do not know"] <- "88"
  Viet <- HouseholdVietnam_2C[,c(20,194)]
  Viet$b9 <- as.character(Viet$b9)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  b9count <- CLV %>% count(country_eng_preload, b9)
  b9count <- b9count %>% filter(country_eng_preload != '')
  b9count <- b9count[order(b9count$country_eng_preload,b9count$b9),]
  #We order the answers
  b9count$b9 <- c("no","yes")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "b9.Are there specific months of the year in
  which you face financial difficulties?"
  list <- c("no","yes")
  #Now we create the table with the corresponding function
  ListBplotCoun(b9count,title,pond,ylimn,dig)

# # #3.7.e. b9_1. If yes to b9, which months?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,173:184)]
  for (i in 3:14){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,159:170)]
  for (i in 3:14){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,196:207)]
  for (i in 3:14){
    Viet[,i] <- as.character(Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- CLV[CLV$country_eng_preload != '',]
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
  #We prepare the parameters for the function
  pond <- nrow(CLV)
  pond2 <- nrow(CLV)
title <- "b9_1. If yes to b9, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 60
dig = 0
Monthplot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)
}
data <- CLV

##3.8 - Co-creation of knowledge
{

# # #3.8.a. c14. Do you exchange your agricultural products,
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,578)]
  Camb$c14<- as.character(Camb$c14)
  Lao <- HouseholdLaos_2C[,c(19,614)]
  Lao$c14 <- as.character(Lao$c14)
  Lao$c14[Lao$c14 == "Yes"] <- "1"
  Lao$c14[Lao$c14 == "No"] <- "0"
  Lao$c14[Lao$c14 == "Do not know"] <- "88"
  Viet <- HouseholdVietnam_2C[,c(20,658)]
  Viet$c14 <- as.character(Viet$c14)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  c14count <- CLV %>% count(country_eng_preload, c14)
  c14count <- c14count %>% filter(country_eng_preload != '')
  c14count <- c14count[order(c14count$country_eng_preload,c14count$c14),]
title <- "c14. Do you exchange your agricultural products,
equipment or animals with other farmers? "
c14count$c14 <- c("no","yes")
pond <- 20
xT <- "Answer"
ylimn <- 0
dig <- 0
list <- c("no","yes")
#Now we create the table with the corresponding function
ListBplotCoun(c14count,title,pond,ylimn,dig)

# # #3.8.b. f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(20,2381)]
Camb$f4<- as.character(Camb$f4)
Lao <- HouseholdLaos_2C[,c(19,2163)]
Lao$f4 <- as.character(Lao$f4)
Lao$f4[Lao$f4 == "No time"] <- "1"
Lao$f4[Lao$f4 == "Very little time"] <- "2"
Lao$f4[Lao$f4 == "Moderate amount of time"] <- "3"
Lao$f4[Lao$f4 == "Almost enough time"] <- "4"
Lao$f4[Lao$f4 == "Sufficient amount of time"] <- "5"
Lao$f4[Lao$f4 == "Do not know"] <- "88"
Viet <- HouseholdVietnam_2C[,c(20,2197)]
Viet$f4 <- as.character(Viet$f4)
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We create a counting table and a title
f4count <- CLV %>% count(country_eng_preload, f4)
f4count <- f4count %>% filter(country_eng_preload != '')
title <- "f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?"
f4count$f4 <- c("1.No time","2.Very little time","3.Moderate amount of time",
                "4.Almost enough time","5.Sufficient amount of time","1.No time","2.Very little time","3.Moderate amount of time",
                "4.Almost enough time","5.Sufficient amount of time","6.Do not know","1.No time","2.Very little time","3.Moderate amount of time",
                "4.Almost enough time","5.Sufficient amount of time","6.Do not know")
pond <- 20
xT <- "Answer"
ylimn <- 0
dig <- 0
list <- c("1.No time","2.Very little time","3.Moderate amount of time",
          "4.Almost enough time","5.Sufficient amount of time","1.No time","2.Very little time","3.Moderate amount of time",
          "4.Almost enough time","5.Sufficient amount of time","6.Do not know","1.No time","2.Very little time","3.Moderate amount of time",
          "4.Almost enough time","5.Sufficient amount of time","6.Do not know")
#Now we create the table with the corresponding function
ListBplotCoun(f4count,title,pond,ylimn,dig)
}

##3.9 - Social value and diet
{

# # #3.9.a. i1. In general, what is the proportion of the food (rice, vegetable, animal
#products, etc.) consumed by your family that comes from your own farm or homegarden?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,2406)]
  Camb$i1<- as.character(Camb$i1)
  Lao <- HouseholdLaos_2C[,c(19,2189)]
  Lao$i1 <- as.character(Lao$i1)
  Lao$i1[Lao$i1 == "Less than 25%"] <- "1"
  Lao$i1[Lao$i1 == "25-50%"] <- "2"
  Lao$i1[Lao$i1 == "50-75%"] <- "3"
  Lao$i1[Lao$i1 == "Over 75%"] <- "4"
  Viet <- HouseholdVietnam_2C[,c(20,2222)]
  Viet$i1 <- as.character(Viet$i1)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  i1count <- CLV %>% count(country_eng_preload, i1)
  i1count <- i1count %>% filter(country_eng_preload != '')
  #We order the answers
  i1count$i1 <- c("1.Less than 25%","2.25-50%","3.50-75%","4.Over 75%","0.NA","1.Less than 25%","2.25-50%","3.50-75%","4.Over 75%","1.Less than 25%","2.25-50%","3.50-75%","4.Over 75%")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  title <- "i1. Proportion of the food consumed by your family 
  that comes from your own farm"
  list <- c("Less than 25%","25-50%","50-75%","Over 75%")
  #Now we create the table with the corresponding function
  ListBplotCoun(i1count,title,pond,ylimn,dig)

# # #3.9.b g5. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,2387)]
  Camb$g5<- as.character(Camb$g5)
  Lao <- HouseholdLaos_2C[,c(19,2169)]
  Lao$g5 <- as.character(Lao$g5)
  Lao$g5[Lao$g5 == "Yes"] <- "1"
  Lao$g5[Lao$g5 == "No"] <- "0"
  Viet <- HouseholdVietnam_2C[,c(20,2203)]
  Viet$g5 <- as.character(Viet$g5)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  g5count <- CLV %>% count(country_eng_preload, g5)
  g5count <- g5count %>% filter(country_eng_preload != '')
  title <- "g5. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed?"
  g5count$g5 <- c("no","yes")
  pond <- 20
  xT <- "Answer"
  ylimn <- 0
  dig <- 0
  list <- c("no","yes")
  #Now we create the table with the corresponding function
  ListBplotCoun(g5count,title,pond,ylimn,dig)

}

##3.10 - Fairness
{

# # #3.10.a. b22. Did you sell any certified crop/vegetables/fruit/livestock
#related product in the past year/12 months?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,225)]
  Camb$b22<- as.character(Camb$b22)
  Lao <- HouseholdLaos_2C[,c(19,206)]
  Lao$b22 <- as.character(Lao$b22)
  Lao$b22[Lao$b22 == "Yes"] <- "1"
  Lao$b22[Lao$b22 == "No"] <- "0"
  Viet <- HouseholdVietnam_2C[,c(20,243)]
  Viet$b22 <- as.character(Viet$b22)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  b22count <- CLV %>% count(country_eng_preload, b22)
  b22count <- b22count %>% filter(country_eng_preload != '')
  b22count <- b22count[order(b22count$country_eng_preload,b22count$b22),]
  b22count[2,3] <- 128
  b22count[1,3] <- 483
b22count$b22 <- c("no","yes")
list <- c("no","yes")
title <- "b22. Did you sell any certified crop/vegetables/fruit/livestock
related product in the past year/12 months?"
#Now we create the table with the corresponding function
ListBplotCoun(b22count,title,pond,ylimn,dig)

# # #3.10.b. b22_1. If yes, which one(s)? 
#First, we select the useful data
b22_1dat <- HouseholdVietnam_2C[,c(1,29,245:259)]
#We modify long label names
var_label(b22_1dat$b22_113) <- "Dried meat (pork,\n beef, etc.)"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdVietnam_2C$b22))
pond <- s[2]
pond2 <- nrow(HouseholdVietnam_2C)
title <- "b22_1. If yes, which one(s)?"
xT <- "Certified crops"
ylimn <- -20
ylimx <- 110
dig = 0
#Function Call
PracticePlot(b22_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)


}

##3.11 - Connectivity
{

# # #3.11.a. c13. Do you collaborate with other people for any task ?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(1,20,569:574,576)]
  for (i in 3:9){
    Camb[,i] <- as.character(Camb[,i])
  }
  Lao <- HouseholdLaos_2C[,c(17,19,605:610,612)]
  for (i in 3:9){
    Lao[,i] <- as.character(Lao[,i])
  }
  Viet <- HouseholdVietnam_2C[,c(1,20,647:652,655)]
  for (i in 3:9){
    Viet[,i] <- as.character(Viet[,i])
  }
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We restore the column label real names
  CLV <- copy_labels(CLV, HouseholdVietnam_2C)
  #We fulfil the information required in the function
#We modify long label names
var_label(CLV$c131) <- "Share labor (mutual\n help, working together\n on each other farm)"
var_label(CLV$c132) <- "Manage water/\nirrigation systems"
var_label(CLV$c134) <- "Buy agricultural\n inputs"
var_label(CLV$c135) <- "Selling products\n to the markets for\n other farmers"
var_label(CLV$c136) <- "Experiment new\n farming practices"
var_label(CLV$c130) <-"No collaboration\n with other people\n on these issues"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdVietnam_2C$b9))
pond <- nrow(CLV)
pond2 <- nrow(CLV)
title <- "c13. Do you collaborate with other people for any task ?"
xT <- "Answer"
ylimn <- -20
ylimx <- 100
dig = 0
#Function Call
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.11.b c15. Are you involved in some form of advocacy work (aiming 
#to influence decision-making within political institutions)?
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(20,579)]
Camb$c15<- as.character(Camb$c15)
Lao <- HouseholdLaos_2C[,c(19,615)]
Lao$c15 <- as.character(Lao$c15)
Lao$c15[Lao$c15 == "Yes"] <- "1"
Lao$c15[Lao$c15 == "No"] <- "0"
Viet <- HouseholdVietnam_2C[,c(20,659)]
Viet$c15 <- as.character(Viet$c15)
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We create a counting table and a title
c15count <- CLV %>% count(country_eng_preload, c15)
c15count <- c15count %>% filter(country_eng_preload != '')
c15count$c15 <- c("no","yes")
list <- c("no","yes")
title <- "c15. Are you involved in some form of advocacy work (aiming 
to influence decision-making within political institutions)"
#Now we create the table with the corresponding function
ListBplotCoun(c15count,title,pond,ylimn,dig)

# # #3.11.c f3.Do you have enough time for your family and your social relationships?
#We create a counting table and a title
f3count <- count(HouseholdVietnam_2C, f3)
title <- "f3.Do you have enough time for your family and your social relationships?"
list <- c("1. No time","2. Very little time","3. Moderate amount of time",
          "4. Almost enough time","5. Sufficient amount of time")
pond <- nrow(HouseholdVietnam_2C)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(f3count,title,list,pond,xT,ylimn)

# # #3.11.d b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
#First we select useful data
data <- HouseholdVietnam_2C[,c(1,29,230)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`Country name (english)`,`b10. if you sell crops/vegetables/fruits or processed products, do you know what`)
colnames(dcount)[2] <- "Answer" 
title <- "b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
- /Country"
dcount$Answer <- c("1.The local market",
                   "2.The provincial/national market","3.Export","4.Other",
                   "6.Do not know","7.NA","5.No","1.The local market",
                   "2.The provincial/national market","3.Export","4.Other",
                   "6.Do not know","7.NA")
pond <- nrow(HouseholdVietnam_2C)
ylimn <- 0
ListBplotCoun(dcount,title,pond,ylimn)

# # #3.11.d b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
#First we select useful data
data <- HouseholdVietnam_2C[,c(1,29,240)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`Country name (english)`,`b16. if you sell livestock or livestock derived products, do you know what is th`)
colnames(dcount)[2] <- "Answer" 
title <- "b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
- /Country"
dcount$Answer <- c("1.The local market",
                   "2.The provincial/national market","4.Other",
                   "6.Do not know","7.NA","5.No","1.The local market",
                   "2.The provincial/national market","4.Other",
                   "6.Do not know","7.NA")
pond <- nrow(HouseholdVietnam_2C)
ylimn <- 0
ListBplotCoun(dcount,title,pond,ylimn)
}

##3.12 - Land and natural resource governance
{

# # #3.12.a. l1. In the past year/12 months, did your household benefit from government
#subsidies to support investment in production or Counercialization activities?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,2485)]
  Camb$l1<- as.character(Camb$l1)
  Lao <- HouseholdLaos_2C[,c(19,2268)]
  Lao$l1 <- as.character(Lao$l1)
  Lao$l1[Lao$l1 == "Yes"] <- "1"
  Lao$l1[Lao$l1 == "No"] <- "0"
  Viet <- HouseholdVietnam_2C[,c(20,2301)]
  Viet$l1 <- as.character(Viet$l1)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  l1count <- CLV %>% count(country_eng_preload, l1)
  l1count <- l1count %>% filter(country_eng_preload != '')
  l1count$l1 <- c("no","yes")
  list <- c("no","yes")
  title <- "l1. In the past year/12 months, did your household benefit from government
subsidies to support investment in production or Commercialization activities?"
  #Now we create the table with the corresponding function
  ListBplotCoun(l1count,title,pond,ylimn,dig)

# # #3.12.b. l2. How much of your total household income in 2022 did these 
#government subsidies represent?
#First, we select the useful data
#We convert it to the proper format
HouseholdVietnam_2C$l2 <- as.factor(HouseholdVietnam_2C$l2)
#We create a counting table and a title
l2count <- count(HouseholdVietnam_2C, l2)
l2count <- l2count %>%  filter(!is.na(l2))
title <- "l2. How much of your total household income in 2022 did these 
government subsidies represent?"
list <- c("1.Less than 25%","2.25-50%","3.50-75%","4.Do not know")
pond <- sum(l2count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(l2count,title,list,pond,xT,ylimn)

# # #3.12.c. d9-10-11. Land ownership
#We prepare the data of each country
Camb <- HouseholdCambodia_2C[,c(1,20,790,793,796)]
for (i in 3:5){
  Camb[,i] <- as.character(Camb[,i])
}
Lao <- HouseholdLaos_2C[,c(17,19,804,806,808)]
for (i in 3:5){
  Lao[,i] <- as.character(Lao[,i])
  Lao[,i] <- ifelse(Lao[,i] == "Yes", "1", Lao[,i])
  Lao[,i] <- ifelse(Lao[,i] == "No", "0", Lao[,i])
}
Viet <- HouseholdVietnam_2C[,c(1,20,847,849,851)]
for (i in 3:5){
  Viet[,i] <- as.character(Viet[,i])
}
#We merge a table regrouping data for each countries
CLV <- rbind(Camb,Lao,Viet)
#We restore the column label real names
CLV <- copy_labels(CLV, HouseholdVietnam_2C)
#First, we select the useful data
#We modify long label names
var_label(CLV$d9_1) <- "Having plots\n rented-in?"
var_label(CLV$d10_1) <- "Having plots\n rented-out?"
var_label(CLV$d11_1) <- "Having plots\n owned?"
#We prepare the parameters for the function
pond <- nrow(CLV)
pond2 <- nrow(CLV)
title <- "d9-10-11. Land ownership"
xT <- ""
ylimn <- -20
ylimx <- 120
dig = 1
#Function call
PracticePlot(CLV,title,pond,pond2,xT,ylimn,ylimx,dig)


}

##3.13 - Participation
{

# # #3.13.a. c2. Are you a member of one or more farmer organization (e.g. crops/fruits/
#livestock/honey/ water/Forest etc)?
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,498)]
  Camb$c2<- as.character(Camb$c2)
  Lao <- HouseholdLaos_2C[,c(19,552)]
  Lao$c2 <- as.character(Lao$c2)
  Lao$c2[Lao$c2 == "Yes, more than one"] <- "2"
  Lao$c2[Lao$c2 == "Yes, one"] <- "1"
  Lao$c2[Lao$c2 == "No"] <- "0"
  Viet <- HouseholdVietnam_2C[,c(20,591)]
  Viet$c2 <- as.character(Viet$c2)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  c2count <- CLV %>% count(country_eng_preload, c2)
  c2count <- c2count %>% filter(country_eng_preload != '')
  c2count$c2 <- c("No","Yes, one","Yes, more than one")
  list <- c("No","Yes, one","Yes, more than one")
  title <- "c2. Are you a member of one or more farmer organization (e.g. crops/fruits/
livestock/honey/ water/Forest etc)?"
  #Now we create the table with the corresponding function
  ListBplotCoun(c2count,title,pond,ylimn,dig)

# # #3.13.b. c1. Are you or anyone in your household active in one or several
#Unions? YN
  #We prepare the data of each country
  Camb <- HouseholdCambodia_2C[,c(20,496)]
  Camb$c1_0<- as.character(Camb$c1_0)
  Lao <- HouseholdLaos_2C[,c(19,543)]
  Lao$c1_0 <- as.character(Lao$c1_0)
  Viet <- HouseholdVietnam_2C[,c(20,588)]
  Viet$c1_0 <- as.character(Viet$c1_0)
  #We merge a table regrouping data for each countries
  CLV <- rbind(Camb,Lao,Viet)
  #We create a counting table and a title
  c1_0count <- CLV %>% count(country_eng_preload, c1_0)
  c1_0count <- c1_0count %>% filter(country_eng_preload != '')
  c1_0count$c1_0 <- c("Yes","No")
  list <- c("Yes","No")
  title <- "c1_0 Are you or anyone in your household active in one or several
#Unions?"
  #Now we create the table with the corresponding function
  ListBplotCoun(c1_0count,title,pond,ylimn,dig)

# # #3.13.c. c1. Are you or anyone in your household active in one or several
#of the following?
#First, we select the useful data
c1dat <- HouseholdVietnam_2C[,c(1,29,581:589)]
#We modify long label names
var_label(c1dat$c1_1) <- "c1_1. Women\n union"
var_label(c1dat$c1_2) <- "c1_2. Youth\n union"
var_label(c1dat$c1_3) <- "c1_3. Veteran\n union"
var_label(c1dat$c1_4) <- "c1_4. Farmer\n union"
var_label(c1dat$c1_5) <- "c1_5. Elderly\n union"
var_label(c1dat$c1_6) <- "c1_6. Political\n party"
var_label(c1dat$c188) <- "Do not know"
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
}


  
  