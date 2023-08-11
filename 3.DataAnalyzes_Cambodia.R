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
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia_2C <- readRDS("HouseholdCambodia_2C.rds")
ClowlandCambodia_2C <- readRDS("ClowlandCambodia_2C.rds")
CuplandCambodia_2C <- readRDS("CuplandCambodia_2.rds")
HouMemberCambodia_2C <- readRDS("HouMemberCambodia_2C.rds")

summary(HouseholdCambodia_2C$commune_eng)
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

# # #1.2.c. List PLOT with commune information (as x)
ListBplotComm <- function(data,title,pond,ylimn,dig){
  dcount <- data
  #We convert absolute values to % y divinding by the total number of households
  dcount$nbHHtot <- ifelse(dcount$'commune name (english)' == 'Rous Ran',
                           sum(HouseholdCambodia_2C$commune_eng_preload == 'Rous Ran'),
                           ifelse(dcount$'commune name (english)' == 'Rohas',
                           sum(HouseholdCambodia_2C$commune_eng_preload == 'Rohas'),
                           ifelse(dcount$'commune name (english)' == 'Rik Reay',
                                  sum(HouseholdCambodia_2C$commune_eng_preload == 'Rik Reay'),
                                  sum(HouseholdCambodia_2C$commune_eng_preload == 'Reaksmei'))))
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= `commune name (english)`, y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, max(dcount$Percentage_of_households)+20)+
    scale_fill_brewer(palette="YIGn")+
    theme_stata()+
    ggtitle(paste(title,"\n N=", pond))
  print(plotb)
}

# # #1.2.d. List PLOT with commune information (as fill)
ListBplotComm2 <- function(data,title,pond,ylimn,dig){
  dcount <- data
  #We convert absolute values to % y divinding by the total number of households
  dcount$nbHHtot <- ifelse(dcount$'commune name (english)' == 'Rous Ran',
                           sum(HouseholdCambodia_2C$commune_eng_preload == 'Rous Ran'),
                           ifelse(dcount$'commune name (english)' == 'Rohas',
                                  sum(HouseholdCambodia_2C$commune_eng_preload == 'Rohas'),
                                  ifelse(dcount$'commune name (english)' == 'Rik Reay',
                                         sum(HouseholdCambodia_2C$commune_eng_preload == 'Rik Reay'),
                                         sum(HouseholdCambodia_2C$commune_eng_preload == 'Reaksmei'))))
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= Answer, y=Percentage_of_households, fill = `commune name (english)`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme(axis.text.x = element_text(face="bold", size=10,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, max(dcount$Percentage_of_households)+20)+
    scale_fill_brewer(palette="YIGn")+
    theme_stata()+
    ggtitle(paste(title,"\n N=", pond))
  print(plotb)
}

# # #1.2.d. commune Plot for One column results (for HouMember data)
ListBplotCommHouM <- function(data,title,pond,ylimn,dig){
dcount <- data
dcount$nbHHtot <- ifelse(dcount$'commune' == 'Rous Ran',
                         sum(HouseholdCambodia_2C$commune_eng_preload == 'Rous Ran'),
                         ifelse(dcount$'commune' == 'Rohas',
                                sum(HouseholdCambodia_2C$commune_eng_preload == 'Rohas'),
                                ifelse(dcount$'commune' == 'Rik Reay',
                                       sum(HouseholdCambodia_2C$commune_eng_preload == 'Rik Reay'),
                                       sum(HouseholdCambodia_2C$commune_eng_preload == 'Reaksmei'))))
dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=0)
#And now a distribution plot
plotb <- ggplot(data=dcount, aes(x= Answer, y=Percentage_of_households, fill = commune))+
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
  dmelt <- melt(data ,  id.vars = c('o9. household id', 'commune name (english)'), variable.name = 'Practices')
  
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
  
  # # # #b.b Plot with communes and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`commune name (english)`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$'commune name (english)' == 'Rous Ran',
                           sum(HouseholdCambodia_2C$commune_eng_preload == 'Rous Ran'),
                           ifelse(dcountNAR$'commune name (english)' == 'Rohas',
                           sum(HouseholdCambodia_2C$commune_eng_preload == 'Rohas'),
                           ifelse(dcountNAR$'commune name (english)' == 'Rik Reay',
                                  sum(HouseholdCambodia_2C$commune_eng_preload == 'Rik Reay'),
                                  sum(HouseholdCambodia_2C$commune_eng_preload == 'Reaksmei'))))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = `commune name (english)`))+
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
  dmelt <- melt(data ,  id.vars = c('o9. household id', 'commune name (english)'), variable.name = 'Practices')
  
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
  
  # # # #b.b Plot with communes and total %
  #We create a counting table
  dcount <- dmelt %>% group_by(`commune name (english)`) %>%
    count(Practices, value) %>%  complete(Practices, value, fill = list(n = 0)) 
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$nbHHtot <- ifelse(dcountNAR$'commune name (english)' == 'Dien Bien commune',
                              sum(HouseholdCambodia_2C$commune_eng_preload == 'Dien Bien commune'),
                              sum(HouseholdCambodia_2C$commune_eng_preload == 'Son La commune'))
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=Practices, y=Percentage_of_households, fill = `commune name (english)`))+
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

# # #1.3 Preparation of Crops reference table
{
#First we turn upland and lowland data to wide format (Duplicate alert = Normal,
#we had no time to take care of it yet)
dlowc <- ClowlandCambodia_2C[,c(2:3,10)]
dlowcwide <- reshape(dlowc, direction = "wide", timevar = "crop1_now", idvar = "hhid_re2")
dlowcwide <- dlowcwide %>% relocate(d2_13e.6 , .after = d2_13e.5)
dlowcwide <- dlowcwide %>% relocate(d2_13e.4 , .before = d2_13e.5)
dlowcwide <- dlowcwide %>% relocate(d2_13e.1 , .before = d2_13e.2)
dupc <- CuplandCambodia_2C[,c(2:3,8)]
colnames(dupc)[colnames(dupc) == "hhid_re3"] ="hhid_re2"
dupcwide <- reshape(dupc, direction = "wide", timevar = "crop2_now", idvar = "hhid_re2")
#We create a table with all the id
HHid <- HouseholdCambodia_2C[,1:2]
colnames(HHid)[colnames(HHid) == "o9"] = "hhid_re2"
#We merge the tables
dcrops <- merge(HHid, dlowcwide, by.x = "hhid_re2", all = T)
dcrops <- merge(dcrops, dupcwide, by.x = "hhid_re2", all = T)
#And we remove useless columns
dcrops <- dcrops[,-c(2,8)]
}

# # #Preparation of household head table
{
Commune <- cbind(as.character(HouseholdCambodia_2C$o9),as.character(HouseholdCambodia_2C$commune_eng_preload))
HouMemberProv <- merge(Commune, HouMemberCambodia_2C, by.x = "V1", by.y = "hhid_re1")
HouHead <- HouMemberProv[HouMemberProv$a2 == '1',]
HouHead <- copy_labels(HouHead, HouMemberCambodia_2C)
colnames(HouMemberProv)[1:2] <- c("o9","commune")
colnames(HouHead)[1:2] <- c("o9","commune")
}

###2. Data analyzes - General

##2.1 Household characteristics
{
# # #2.1.a. o10. Are both the man and the woman available for the interview?
#We create a counting table and a title
o10count <- count(HouseholdCambodia_2C, o10)
title <- "o10. Are both the man and the woman available 
for the interview?"
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(o10count,title,list)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - General
#We create a counting table and a title
o11count <- count(HouseholdCambodia_2C, o11)
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent)"
list <- c("Male","Female")
pond <- sum(o11count$n)
#Now we create the table with the corresponding function
PiePlot(o11count,title,list)

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - /Commune
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,49)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`o11. who do you do the interview with?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("Male","Female")
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent) - /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #2.1.c. a3. What is the sex of a1 (Household head)?
#We create a counting table and a title
a3count <- count(HouHead, a3)
title <- "a3. What is the sex of a1 (Household head)?"
list <- c("Male","Female")
pond <- sum(a3count$n)
#Now we create the table with the corresponding function
PiePlot(a3count,title,list)

# # #2.1.d. Household head age distribution
#First we create a categorical variable with age range instead of quantitative value
HouHead$AgeCat <- cut(HouHead$a4_1,
                       breaks=c(20,30, 35, 40, 45, 50, 55, 60, 65, 70,100),
                       labels=c('<30', '30-34', '35-39', '40-44','45-49','50-54',
                                '55-59','60-64','65-69','>69'))
#Then we create a counting table and a title
dcount <- HouHead %>%
  count(`commune`,`AgeCat`)
colnames(dcount)[2] <- "Answer" 
title <- "a4_1. Age of a1 (Household head) - commune based"
list <- c('<30', '30-34', '35-39', '40-44','45-49','50-54',
          '55-59','60-64','65-69','>69')
pond <- sum(dcount$n)
xT <- "Age (years)"
ylimn <- 0
dig <- 0
ListBplotCommHouM(dcount,title,pond,ylimn,dig)

# # #2.1.e. Household head occupation distribution
dcount <- HouHead %>%
  count(`commune`,`a6`)
colnames(dcount)[2] <- "Answer" 
dcount <- dcount[order(dcount[,1], dcount[,2]),]
dcount$Answer <- c('Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
                  'Other non-agricultural work (please specify)','Salaried employment with government',
                  'Salaried employment with company','Salaried employment with NGOs','Unable to work',
                  'Other','Agricultural work\n (own farm)','Other farm\n salaried employment\n (seasonnal)','Daily wage sale of labor','Business (owner)', 
                  'Other non-agricultural work (please specify)','Salaried employment with government',
                  'Salaried employment with company','Salaried employment with NGOs','Housewife/ caregiver',
                  'Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
                  'Salaried employment with government', 'Salaried employment with company','Salaried employment with NGOs',
                  'Other','Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
                  'Salaried employment with government','Salaried employment with NGOs','Unable to work')
list <- c('Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
          'Other non-agricultural work (please specify)','Salaried employment with government',
          'Salaried employment with company','Salaried employment with NGOs','Unable to work',
          'Other','Agricultural work\n (own farm)','Other farm\n salaried employment\n (seasonnal)','Daily wage sale of labor','Business (owner)', 
          'Other non-agricultural work (please specify)','Salaried employment with government',
          'Salaried employment with company','Salaried employment with NGOs','Housewife/ caregiver',
          'Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
          'Salaried employment with government', 'Salaried employment with company','Salaried employment with NGOs',
          'Other','Agricultural work\n (own farm)','Daily wage sale of labor','Business (owner)', 
          'Salaried employment with government','Salaried employment with NGOs','Unable to work')
pond <- sum(dcount$n)
xT <- "Occupation"
ylimn <- -20
dig <- 0
title <- "a6. What is a1's main occupation for the
past 12 months? (Household head)"
ListBplotCommHouM(dcount,title,pond,ylimn,dig)

# # #2.1.f. Distribution of Household members number
#First we count the number of household id repetition (=num of household members)
p_nocount <- HouMemberProv %>%
  count(`commune`,`o9`)
c <- count(HouMemberProv, o9)
colnames(p_nocount)[3] <- "dum"
#Then we count the frequency of number of household member per commune
nhmcount <- p_nocount %>%
  count(`commune`,`dum`)
colnames(nhmcount)[2] <- "Answer"
nhmcount$Answer <- as.factor(nhmcount$Answer)
pond <- sum(nhmcount$n)
xT <- "Num of household members"
ylimn <- 0
dig <- 0
title <- "How many members in the household ?"
ListBplotCommHouM(nhmcount,title,pond,ylimn,dig)
}

##2.2 Land & Crops characteristics
{
# # #2.2.a. d1. What kinds of land did your household have in the past 12 months?
#First, we select the useful data
d1dat <- HouseholdCambodia_2C[,c(1,29,582:589)]
#We restore the column label real names
d1dat <- copy_labels(d1dat, HouseholdCambodia_2C)
#We fulfil the information required in the function
pond <- nrow(HouseholdCambodia_2C)
pond2 <- pond
xT <- "Kind of land owned"
ylimn <- -10
ylimx <- 100
title <- "d1. What kinds of land did your household have in the past 12 months?"
dig = 0
PracticePlot(d1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #2.2.b. Growing crops - Yes or No
HouseholdCambodia_2C$CropYN <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), '0', '1')
#We create a counting table and a title
CropYNcount <- count(HouseholdCambodia_2C, CropYN)
CropYNcount <- CropYNcount[c("2","1"),]
title <- "Are you growing crops ?"
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(CropYNcount,title,list)

# # #2.2.c. Crop diversity - Lowland (Household based)
#We convert data to the proper format
dlowc$d2_13e <- as.factor(dlowc$d2_13e)
#We create a counting table and a title
Cropcount <- count(dlowc, d2_13e)
#We determine other function parameters
title <- "Crop diversity among households (household based)"
pond <- nrow(HouseholdCambodia_2C)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_13e <- as.character(Cropcount$d2_13e)
xT <- "Crops"
ylimn <- -10
#For the 14 first crops
list <- Cropcount$d2_13e
dig <- 0
ListBPlot(Cropcount,title,list,pond,xT,ylimn,dig)

# # #2.3.d. Crop diversity - Upland (Household based)
#We convert data to the proper format
dupc$d2_23e <- as.factor(dupc$d2_23e)
#We create a counting table and a title
Cropcount <- count(dupc, d2_23e)
#We determine other function parameters
title <- "Crop diversity among households (household based) - Upland"
pond <- nrow(HouseholdCambodia_2C)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_23e <- as.character(Cropcount$d2_23e)
#We change too large crops label names 
xT <- "Crops"
ylimn <- -10
#For the 14 first crops
list <- Cropcount$d2_23e
dig <- 0
ListBPlot(Cropcount,title,list,pond,xT,ylimn,dig)

# # #2.2.e. Crop diversity - Lowland (Area based)
dlowc2 <- ClowlandCambodia_2C[,c(1:2,9,12)]
dlowc2$d2_132 <- as.character(dlowc2$d2_132)
dlowc2$d2_132 <- as.numeric(dlowc2$d2_132)
#We sum the area per crops
result  <- aggregate(d2_132 ~ d2_13e, dlowc2, sum)
result$AverageTot <- result$d2_132/sum(!is.na(HouseholdCambodia_2C$no_crop1))
result$AverageTot <- as.numeric(format(result$AverageTot,scientific = FALSE, digits = 1))
#Now we prepare the data for the function
#We determine other function parameters
title <- "Average crop area - Lowland (10 most popular)"
pond <- sum(!is.na(HouseholdCambodia_2C$no_crop1))
result <- result[order(-result$AverageTot),]
#As there are many different crops, we'll display several plots
result$d2_13e <- as.character(result$d2_13e)
colnames(result)[3] <- 'n'
xT <- "Crops"
ylimn <- -300
#We'll plot only the 10 first crops
list <- result$d2_13e
dig <- 0
Plota <- ggplot(data=result, aes(x= reorder(d2_13e, -n), y= n, fill = d2_13e))+
  geom_bar(colour="black",stat="identity") +
  theme_stata()+
  theme(axis.text.x = element_text(face="bold", size=11, angle = 85),
        legend.position = "none")+
  labs(x= xT)+
  labs(y= "area (m2)")+
  geom_text(aes(label = n, vjust = -1))+
  ylim(ylimn,max(result$n)+20)+
  ggtitle(paste(title,"\n N=",pond))+
  scale_fill_viridis_d(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "E",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill")
Plota

# # #2.2.f. Crop diversity - Upland (Area based)
dupc2 <- CuplandCambodia_2C[,c(1:2,8,10)]
dupc2$d2_232 <- as.character(dupc2$d2_232)
dupc2$d2_232 <- as.numeric(dupc2$d2_232)
#We sum the area per crops
result  <- aggregate(d2_232 ~ d2_23e, dupc2, sum)
result$AverageTot <- result$d2_232/sum(!is.na(HouseholdCambodia_2C$no_crop2))
result$AverageTot <- as.numeric(format(result$AverageTot,scientific = FALSE, digits = 1))
#Now we prepare the data for the function
#We determine other function parameters
title <- "Average crop area - Upland (10 most popular)"
pond <- sum(!is.na(HouseholdCambodia_2C$no_crop1))
result <- result[order(-result$AverageTot),]
#As there are many different crops, we'll display several plots
result$d2_23e <- as.character(result$d2_23e)
colnames(result)[3] <- 'n'
#We change too large crops label names 
xT <- "Crops"
ylimn <- -300
#We'll plot only the 10 first crops
list <- result$d2_23e
dig <- 0
Plota <- ggplot(data=result1, aes(x= reorder(d2_23e, -n), y= n, fill = d2_23e))+
  geom_bar(colour="black",stat="identity") +
  theme_stata()+
  theme(axis.text.x = element_text(face="bold", size=11, angle = 85),
        legend.position = "none")+
  labs(x= xT)+
  labs(y= "area (m2)")+
  geom_text(aes(label = n, vjust = -1))+
  ylim(ylimn,max(result1$n)+20)+
  ggtitle(paste(title,"\n N=",pond))+
  scale_fill_viridis_d(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "E",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill")
Plota


##2.3 Animal characteristics
# # #2.2.a. e2. If yes, which below animal does your household have?
#First, we select the useful data
e2dat <- HouseholdCambodia_2C[,c(1,29,2032:2044)]
for (i in 3:15){
  e2dat[,i] <- as.factor(e2dat[,i])
}
#We restore the column label real names
e2dat <- copy_labels(e2dat, HouseholdCambodia_2C)
#We modify long label names
s <- summary(HouseholdCambodia_2C$e1)
pond <- nrow(HouseholdCambodia_2C)
pond2 <- s[2]
xT <- "Animals"
ylimn <- -10
ylimx <- 85
title <- "e2. Which below animal does your household have?"
dig = 0
PracticePlot(e2dat,title,pond,pond2,xT,ylimn,ylimx,dig)
}


###3. Data analyzes for Agroecological principles

##3.1 - Recycling
{

# # #3.1.a. d17. Does your household use ecological/agroecological/integrated practices...
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$d17 <- as.factor(HouseholdCambodia_2C$d17)
#We create a counting table and a title
d17count <- count(HouseholdCambodia_2C, d17)
#We order the answers
d17count <- d17count[c("2","1","3","4"),]
title <- "d17. Does your household use ecological/
agroecological/integrated practices maintain/
enhance soil fertility in your fields?"
list <- c("yes","no","do not know", "NA")
#Now we create the table with the corresponding function
PiePlotYN(d17count,title,list)

# # #3.1.b. d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?
#First, we select the useful data
d18dat <- HouseholdCambodia_2C[,c(1,29,1106:1117)]
#We modify long label names
var_label(d18dat$d183) <- 'Bokashi (fermented\norganic matter)'
var_label(d18dat$d184) <- 'Legume-based\ngreen manure'
var_label(d18dat$d185) <- 'Pulses in association\nand/or rotation\nwith main crop'
var_label(d18dat$d186) <- 'Cover crops in\nassociation and/\nor rotation\nwith main crop'
var_label(d18dat$d188) <- 'Crop residue\nmaintenance'
var_label(d18dat$d189) <- 'Recycling\ncrop waste'
var_label(d18dat$d1810) <- 'Ramial Wood Chip\n (RWC) or other\nwood chips'
var_label(d18dat$d1811) <- 'Organic agro-\nindustrial waste'
dum <- summary(HouseholdCambodia_2C$d181)
pond <- d17count[1,2]
pond2 <- sum(d17count$n)
title <- "d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?"
xT <- "Practices"
ylimn <- -15
ylimx <- 100
dig = 0
PracticePlot(d18dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.1.c. d18_11. For which crop(s) do you use animal manure?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1811dat <- HouseholdCambodia_2C[,c(1,1120:1130)]
s <- summary(d18dat$d181)
val <- s[2]
title <- "d18_11. For which crop(s) do you use animal manure?
(% of households using animal manure)"
ylimn <- -40
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d1811dat,val,title,ylimn,ylimx,dig)

# # #3.1.d. d18_21. For which crop(s) do you use Compost (heap)?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1821dat <- HouseholdCambodia_2C[,c(1,1142:1152)]
s <- summary(d18dat$d182)
val <- s[2]
title <- "18_21. For which crop(s) do you use compost (heap)?
(% of households using compost)"
ylimn <- -20
ylimx <- 60
dig = 0
#We apply the plotting function
CropPlot2(d1821dat,val,title,ylimn,ylimx,dig)

# # #3.1.e. d18_31. For which crop(s) do you use Bokashi (fermented organic matter)?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1831dat <- HouseholdCambodia_2C[,c(1,1156:1166)]
s <- summary(d18dat$d183)
val <- s[2]
title <- "d18_31. For which crop(s) do you use Bokashi (fermented organic matter)?
(% of households using Bokashi)"
ylimn <- -20
ylimx <- 60
dig = 0
#We apply the plotting function
CropPlot2(d1831dat,val,title,ylimn,ylimx,dig)

# # #3.1.f. d18_41. For which crop(s) do you use legume-based green manure?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1841dat <- HouseholdCambodia_2C[,c(1,1170:1180)]
s <- summary(d18dat$d184)
val <- s[2]
title <- "d18_41. For which crop(s) do you use legume-based green manure?
(% of households using legume-based green manure)"
ylimn <- -20
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot2(d1841dat,val,title,ylimn,ylimx,dig)

# # #3.1.g. d18_51. For which crop(s) do you use pulses in association and/or rotation with main crop ?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1851dat <- HouseholdCambodia_2C[,c(1,1184:1194)]
s <- summary(d18dat$d185)
val <- s[2]
title <- "d18_51. For which crop(s) do you use pulses in association and/or rotation with main crop ?
(% of households using pulses)"
ylimn <- -20
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot2(d1851dat,val,title,ylimn,ylimx,dig)

# # #3.1.h. d18_61. For which crop(s) do you use cover crops in association and/or rotation with main crop ?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1861dat <- HouseholdCambodia_2C[,c(1,1198:1208)]
s <- summary(d18dat$d186)
val <- s[2]
title <- "d18_61. For which crop(s) do you use cover crops in association and/or rotation with main crop ?
(% of households using cover crops)"
ylimn <- -20
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot2(d1861dat,val,title,ylimn,ylimx,dig)

# # #3.1.i. d18_71. For which crop(s) do you use Biochar ?
#No household use biochar

# # #3.1.j. d18_81. For which crop(s) do you use crop residue maintenance?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1881dat <- HouseholdCambodia_2C[,c(1,1226:1236)]
s <- summary(d18dat$d188)
val <- s[2]
title <- "d18_81. For which crop(s) do you use crop residue maintenance?
(% of households using crop residue maintenance)"
ylimn <- -20
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot2(d1881dat,val,title,ylimn,ylimx,dig)

# # #3.1.k. d18_91. For which crop(s) do you use recycling crop waste?
#No household use recycling crop waste

# # #3.1.l. d18_101. d18_101. For which crop(s) do you use ramial Wood Chip (RWC) or other wood chips?
#No household use RWC

# # #3.1.m. d18_111. d18_101. d18_11. For which crop(s) do you use Organic agro-industrial waste ? 
#No household use waste

# # #3.1.n. d18_99. For which crop(s) do you use Other ?
#No interest in these answers as no translation for now

}

##3.2 - Input reduction
{
  
# # #3.2.a. d20. Does your household use any ecological/ agroecological/ integrated practices to control weeds in your fields?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$d20 <- as.factor(HouseholdCambodia_2C$d20)
#We create a counting table and a title
d20count <- count(HouseholdCambodia_2C, d20)
d20count <- d20count[c("2","1","3","4"),]
title <- "d20. Does your household use any ecological/\n agroecological/ integrated practices to\n control weeds in your fields?"
list <- c("yes","no","do not know","NA")
#Now we create the table with the corresponding function
PiePlotYN(d20count,title,list)

# # #3.2.b. d21. Which of the following ecological/agroecological/integrated practices
#does your household use to control weeds in your fields?
#First, we select the useful data
d21dat <- HouseholdCambodia_2C[,c(1,29,1326:1340)]
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

# # #3.2.c. e13. Does your household fertilize the forage or improved pasture?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2144)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e13. does your household fertilize the forage or improved pasture?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("No","Yes","NA","No","Yes","NA","No","NA","NA")
title <- "e13. Does your household fertilize the forage or improved pasture?
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.2.d. e13_1. if yes, is it with Synthetic fertilizer, Organic manure or Both above ?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2145)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e13_1. if yes, is it with`)
colnames(dcount)[2] <- "Answer"
dcount <- dcount %>% filter(!is.na(dcount$Answer))
dcount$Answer <- c("2.Organic manure","1.Synthetic fertilizer")
title <- "e13. Does your household fertilize the forage or improved pasture?
- /commune"
pond <- sum(nrow(HouseholdCambodia_2C))
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.2.e. e29. Does your household use antibiotics on your cattle/buffalo?
#First, we select the useful data
e29dat <- HouseholdCambodia_2C[,c(1,29,2206:2209)]
#We modify long label names
var_label(e29dat$e291) <- "For treatment\n diseases only"
var_label(e29dat$e292) <- "For prevention of\n diseases only"
var_label(e29dat$e293) <- "For growth\n promotion"
var_label(e29dat$e290) <- "I don’t use\n antibiotics at all"
pond <- sum(!is.na(HouseholdCambodia_2C$e16))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e29. Does your household use antibiotics on your cattle/buffalo?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e29dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.2.f. e43. Does your household use antibiotics on your pigs?
#First, we select the useful data
e43dat <- HouseholdCambodia_2C[,c(1,29,2273:2276)]
#We modify long label names
var_label(e43dat$e431) <- "For treatment\n diseases only"
var_label(e43dat$e432) <- "For prevention of\n diseases only"
var_label(e43dat$e433) <- "For growth\n promotion"
var_label(e43dat$e430) <- "I don’t use\n antibiotics at all"
pond <- sum(!is.na(HouseholdCambodia_2C$e31))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e43. Does your household use antibiotics on your pigs?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e43dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.2.g. e57. Does your household use antibiotics on your poultry?
#First, we select the useful data
e57dat <- HouseholdCambodia_2C[,c(1,29,2341:2344)]
#We modify long label names
var_label(e57dat$e571) <- "For treatment\n diseases only"
var_label(e57dat$e572) <- "For prevention of\n diseases only"
var_label(e57dat$e573) <- "For growth\n promotion"
var_label(e57dat$e570) <- "I don’t use\n antibiotics at all"
pond <- sum(!is.na(HouseholdCambodia_2C$e44))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e57. Does your household use antibiotics on your poultry?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e57dat,title,pond,pond2,xT,ylimn,ylimx,dig)

}
  
##3.3 - Soil health
{

# # #3.3.a. Soil conservation practices Yes or No
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$d140 <- as.factor(HouseholdCambodia_2C$d140)
#We create a counting table and a title
d140count <- count(HouseholdCambodia_2C, d140)
#We order the answers
title <- "d140. Does your household use Soil conservation practices"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(d140count,title,list)

# # #3.3.b. d14. Which of the following soil conservation practices do you use ?
#First, we select the useful data
d14dat <- HouseholdCambodia_2C[,c(1,29,990:997)]
for (i in 3:10){
  d14dat[,i] <- as.factor(d14dat[,i])
}
#We recode the variables into binary
d14dat <- d14dat %>%
  mutate(across(d141:d1499, ~ recode(.x,"2" = "1", "1" = "0")))
#We restore the column label real names
d14dat <- copy_labels(d14dat, HouseholdCambodia_2C)
#We modify long label names
var_label(d14dat$d141) <-  "Sowing in \ncontour lines"
var_label(d14dat$d142) <- "Natural or planted\n grass strips"
var_label(d14dat$d143) <- "Trees conservation \nin agricultural plots"
var_label(d14dat$d144) <- "Agroforestry \n(trees + crops)"
var_label(d14dat$d145) <- "Crop residues\n maintained to\n cover the soil"
var_label(d14dat$d147) <- "Reduced to\n no-tillage"
s <- summary(as.factor(HouseholdCambodia_2C$d140))
pond <- s[1]
pond2 <- sum(d20count$n)
title <- "d14. Which of the following soil conservation practices do you use ?"
xT <- "Practices"
ylimn <- -10
ylimx <- 30
dig = 0
PracticePlot(d14dat,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.4 - Animal health
{

# # #3.4.a. e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$e58 <- as.factor(HouseholdCambodia_2C$e58)
#We create a counting table and a title
e58count <- count(HouseholdCambodia_2C, e58)
e58count <- e58count[c("2","1","3"),]
title <- "e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e58count,title,list)

# # #3.4.b. e58_1. If yes to e58, which months?
#First, we select the useful data
e58_1dat <- HouseholdCambodia_2C[,c(1,29,2167:2178)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$e58))
pond <- s[2]
pond2 <- sum(d20count$n)
title <- "e58_1. If yes to e58, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 15
dig = 0
Monthplot(e58_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.4.c e59. Is there any month in the year when there is a lack of feed for the animals?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$e59 <- as.factor(HouseholdCambodia_2C$e59)
#We create a counting table and a title
e59count <- count(HouseholdCambodia_2C, e59)
e59count <- e59count[c("2","1","3"),]
title <- "e59. Is there any month in the year when 
there is a lack of feed for the animals?"
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e59count,title,list)

# # #3.4.d. e59_1. If yes to e59, which months?
#First, we select the useful data
e59_1dat <- HouseholdCambodia_2C[,c(1,29,2181:2192)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$e59))
pond <- s[2]
pond2 <- sum(d20count$n)
title <- "e59_1. If yes to e59, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 40
dig = 0
Monthplot(e59_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.4.f. e60. Do the animals have access to the water?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2193)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e60. do the animals have access to the water?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("No","Yes","NA","No","Yes","NA")
title <- "e60. Do the animals have access to the water?
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.4.g. e26. Does your household have problems with Cattle/Buffalo parasites?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2021)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e26.does your household have problems with cattle/buffalo parasites?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("No","Yes","NA","No","Yes","NA")
title <- "e26. Does your household have problems with Cattle/Buffalo parasites?
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.4.h. e27. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e27dat <- HouseholdCambodia_2C[,c(1,29,2024:2026)]
pond <- sum(!is.na(HouseholdCambodia_2C$e16))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e27. How does your household deal with Cattle/Buffalo parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e27dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.4.i. e40. Does your household have problems with Pigs parasites?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2082)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e40. does your household have problems with pigs parasites?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("No","Yes","NA","No","Yes","NA")
title <- "e40. Does your household have problems with pigs parasites?
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.4.j. e41. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e41dat <- HouseholdCambodia_2C[,c(1,29,2085:2087)]
pond <- sum(!is.na(HouseholdCambodia_2C$e31))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e41. How does your household deal with pigs parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e41dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.4.k. e54. Does your household have problems with Poultry parasites?
#We select appropriate data
data <- HouseholdCambodia_2C[,c(1,29,2143)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`e54. does your household have problems with poultry parasites?`)
colnames(dcount)[2] <- "Answer" 
dcount$Answer <- c("No","Yes","NA","No","Yes","NA")
title <- "e54. Does your household have problems with poultry parasites?
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.4.l. e41. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e41dat <- HouseholdCambodia_2C[,c(1,29,2146:2148)]
pond <- sum(!is.na(HouseholdCambodia_2C$e44))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "e41. How does your household deal with pigs parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
#Function call
PracticePlot(e41dat,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.5 - Biodiversity
{

# # #3.5.a d32. Do you conserve and use traditional/local seeds?
#First, we select the useful data
#We create a counting table and a title
d32count <- count(HouseholdCambodia_2C, d32)
title <- "d32. Do you conserve and use traditional/local seeds?"
d32count <- d32count[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(d32count,title,list)

# # #3.5.b. d32_1. If Yes to d32, for which crops? 
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d32_1dat <- HouseholdCambodia_2C[,c(1,1944:1957)]
s <- summary(HouseholdCambodia_2C$d32)
val <- s[2]
title <- "d32_1. If Yes to d32, for which crops? "
ylimn <- -15
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d32_1dat,val,title,ylimn,ylimx,dig)

# # #3.5.c. e5_b. Do you have any local breeds of  e4_1 at the time of the survey?
#First, we select the useful data
#We create a counting table and a title
e5_bcount <- count(HouseholdCambodia_2C, e5_b)
title <- "e5_b. Do you have any local breeds of most important animal
at the time of the survey?"
e5_bcount <- e5_bcount[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e5_bcount,title,list)

# # #3.5.d. e5_b_1. Animal diversity for local breeds (most important animal)
#First, we select the useful data
#We create a counting table and a title
e5_b_1count <- HouseholdCambodia_2C[,c(1905,1911)]
title <- "e5_b_1. Animal diversity for local breeds (most important animal)"
s <- summary(HouseholdCambodia_2C$e5_b)
pond <- s[2]
#Now we create the table with the corresponding function
AnimalPlot(e5_b_1count,title,pond)

# # #3.5.e. Number of crops grown during the last 12 months
HouseholdCambodia_2C$TotCrop <- ifelse(!is.na(HouseholdCambodia_2C$no_crop1),HouseholdCambodia_2C$no_crop1,0) +
                                ifelse(!is.na(HouseholdCambodia_2C$no_crop2),HouseholdCambodia_2C$no_crop2,0)
HouseholdCambodia_2C$TotCrop <- as.factor(HouseholdCambodia_2C$TotCrop)
data <- HouseholdCambodia_2C[,c(1,29,2731)]
#And we change columns names for label names
colnames(data) <- var_label(data)
colnames(data)[3] <- 'ncrop'
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`ncrop`)
colnames(dcount)[2] <- "Answer" 
title <- "Number of crops grown during the last 12 months
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

}

##3.6 - Synergy
{

# # #3.6.a. d26. Does your household use agroecological/integrated practices to control pests and disease in your fields? 
#First, we select the useful data
#We create a counting table and a title
d26count <- count(HouseholdCambodia_2C, d26)
title <- "d26. Does your household use agroecological/integrated practices
to control pests and disease in your fields? "
d26count <- d26count[c("2","1","3"),]
list <- c("yes","no","do not know")
#Now we create the table with the corresponding function
PiePlotYN(d26count,title,list)

# # #3.6.b. d27. Which ecological/agroecological/integrated practices do you
#use to control pets and diseases in your fields?
#First, we select the useful data
d27dat <- HouseholdCambodia_2C[,c(1,29,1517:1531)]
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
s <- summary(HouseholdCambodia_2C$d26)
pond <- as.numeric(s[2])
pond2 <- sum(d17count$n)
title <- "d27 - Which ecological/agroecological/integrated practices do you
use to control pets and diseases in your fields?"
xT <- "Practices"
ylimn <- -20
ylimx <- 30
dig = 1
#Function call
PracticePlot(d27dat,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.7 - Economic diversification
{

# # #3.7.a. b1. Does your household sell agricultural products 
#(crops, vegetables, fruits or processed products) and/or livestock? 
#First we select useful data
data <- HouseholdCambodia_2C[,c(1,29,80)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
    count(`commune name (english)`,`b1. does your household sell agricultural products (crops, vegetables, fruits or`)
colnames(dcount)[2] <- "Answer" 
title <- "b1. Does your household sell agricultural products 
(crops, vegetables, fruits or processed products) and/or livestock?
- /commune"
dcount$Answer <- c("4.No selling\n agri-products","1.Selling own\n crops/fruits/\n vegetable",
                     "2.Selling own\n livestock products","3.Selling both own\n crops/fruits/vegetables\n and livestock products",
                     "5.Do not know","6.NA")
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm2(dcount,title,pond,ylimn)

# # #3.7.b. b2. Does your household sell derived/processed products and which?
#First we select useful data
data <- HouseholdCambodia_2C[,c(1,29,81)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`b2. does your household sell derived/processed products and which?`)
colnames(dcount)[2] <- "Answer" 
title <- "b2. Does your household sell derived/processed products and which?
- /commune"
dcount$Answer <- c("4.No processed/derived agri-products","1.Selling processed products from crops","3.Selling both above processed/derived agri-products")
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm2(dcount,title,pond,ylimn)
  
# # #3.7.c. (Need to create an indicator including all the product household sell,
#animal and crops)
#1st for lowland
commune <- cbind(as.character(HouseholdCambodia_2C$o9),as.character(HouseholdCambodia_2C$commune_eng_preload))
LowlandProv <- merge(commune, ClowlandCambodia_2C, by.x = "V1", by.y = "hhid_re2")
colnames(LowlandProv)[1:2] <- c("o9","commune")
LowlandSold <- LowlandProv %>% filter(LowlandProv$d2_136 == 0)
ncrop_soldl <- LowlandSold %>%
  count(`commune`,`o9`)
#2nd for upland
UplandProv <- merge(commune, CuplandCambodia_2C, by.x = "V1", by.y = "hhid_re3")
colnames(UplandProv)[1:2] <- c("o9","commune")
UplandSold <- UplandProv %>% filter(UplandProv$d2_236 == 0)
ncrop_soldu <- UplandSold %>%
  count(`commune`,`o9`)
#3rd for the 3 main animals
AniSold <- HouseholdCambodia_2C[,c(1,29,1918,1946,1965)]
for (i in 3:5){
  AniSold[,i] <- ifelse(AniSold[,i] > 0, '1', '0')
  AniSold[,i] <- as.numeric(AniSold[,i])
}
AniSold$Nani <- AniSold$e5_4 + AniSold$e6_4 + AniSold$e7_4
#Now we merge these 3 tables together
Dummy <- merge(ncrop_soldl, ncrop_soldu, by = "o9", all = T)
SoldProd <- merge(AniSold,Dummy, by = "o9", all = T)
SoldProd$SoldProdTot <- as.numeric(ifelse(!is.na(SoldProd$Nani), SoldProd$Nani,'0'))+ 
  as.numeric(ifelse(!is.na(SoldProd$n.x), SoldProd$n.x,'0'))+
  as.numeric(ifelse(!is.na(SoldProd$n.y), SoldProd$n.y,'0'))
SoldProd <- SoldProd[,c(1:2,11)]
colnames(SoldProd)[2] <- "commune name (english)"
dcount <- SoldProd %>%
  count(`commune name (english)`,`SoldProdTot`)
colnames(dcount)[2] <- "Answer"
dcount$Answer <- as.factor(dcount$Answer)
title <- "Number of crop or animal products sold by the household
(Including only the 3 first animal products)
- /commune"
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm2(dcount,title,pond,ylimn)

# # #3.7.d. b9.Are there specific months of the year in which you face financial difficulties?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$b9 <- as.factor(HouseholdCambodia_2C$b9)
#We create a counting table and a title
b9count <- count(HouseholdCambodia_2C, b9)
title <- "b9. Are there specific months of the year in which
you face financial difficulties?"
b9count <- b9count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(b9count,title,list)

# # #3.7.e. b9_1. If yes to b9, which months?
#First, we select the useful data
b9_1dat <- HouseholdCambodia_2C[,c(1,29,182:193)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$b9))
pond <- s[2]
pond2 <- sum(d17count$n)
title <- "b9_1. If yes to b9, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 60
dig = 0
Monthplot(b9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)
}

##3.8 - Co-creation of knowledge
{

# # #3.8.a. c14. Do you exchange your agricultural products,
#equipment or animals with other farmers? 
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$c14 <- as.factor(HouseholdCambodia_2C$c14)
#We create a counting table and a title
c14count <- count(HouseholdCambodia_2C, c14)
title <- "c14. Do you exchange your agricultural products,
equipment or animals with other farmers? "
c14count <- c14count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c14count,title,list)

# # #3.8.b. f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?
#We convert data to the proper format
HouseholdCambodia_2C$f4 <- as.factor(HouseholdCambodia_2C$f4)
#We create a counting table and a title
f4count <- count(HouseholdCambodia_2C, f4)
title <- "f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?"
list <- c("1.No time","2.Very little time","3.Moderate amount of time","4.Almost enough time",
  "5.Sufficient amount of time", "6.Do not know")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(f4count,title,list,pond,xT,ylimn)
}

##3.9 - Social value and diet
{

# # #3.9.a. i1. In general, what is the proportion of the food (rice, vegetable, animal
#products, etc.) consumed by your family that comes from your own farm or homegarden?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$i1 <- as.factor(HouseholdCambodia_2C$i1)
#We create a counting table and a title
i1count <- count(HouseholdCambodia_2C, i1)
title <- "i1. Proportion of the food that comes from the household farm"
list <- c("1.Less than 25%","2.25-50%","3.50-75%","4.Over 75%")
pond <- sum(d17count$n)
#Now we create the table with the corresponding function
xT = "Answer"
ylimn <- -20
ListBPlotNO(i1count,title,list,pond,xT,ylimn)

# # #3.9.b g5. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$g5 <- as.factor(HouseholdCambodia_2C$g5)
#We create a counting table and a title
g5count <- count(HouseholdCambodia_2C, g5)
g5count <- g5count[c("2","1"),]
list <- c("yes","no")
title <- "g5. Do you think the working hours across family
members are equitably distributed?"
#Now we create the table with the corresponding function
PiePlotYN(g5count,title,list)

# # #3.9.c. i2. Were there months, in the past 12 months, in which you did 
#not have enough food to meet your family’s needs?
#We create a counting table and a title
i2count <- count(HouseholdCambodia_2C, i2)
title <- "i2. Are there specific months of the year in which
you face financial difficulties?"
i2count <- i2count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(i2count,title,list)

# # #3.9.d. i3. If yes, which were the months in the past 12 months during 
#which you did not have enough food to meet your family’s needs? 
#First, we select the useful data
i3dat <- HouseholdCambodia_2C[,c(1,29,2225:2236)]
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$i2))
pond <- s[2]
pond2 <- nrow(HouseholdCambodia_2C)
title <- "i3. If yes, which were the months in the past 12 months during 
#which you did not have enough food to meet your family’s needs?"
xT <- "Months"
ylimn <- 0
ylimx <- 60
dig = 0
Monthplot(i3dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.9.e. g1. Who makes the decision on what (e.g. crops, animals) and how to 
#produce (e.g., with or without herbicide, tillage or no tillage, compost or chemical fertiliser)?
#We create a counting table and a title
g1count <- count(HouseholdCambodia_2C, g1)
title <- "g1.  Who makes the decision on what (e.g. crops, animals) 
and how to produce ?"
list <- c("Myself alone","Me in consultation with spouse/other family members",
          "My spouse/other family members")
pond <- sum(g1count$n)
#Now we create the table with the corresponding function
PiePlot(g1count,title,list)

# # #3.9.f. g2. Who makes the decision on purchasing, selling, or transferring major 
#household assets (land, cattle, equipment)?
#We create a counting table and a title
g2count <- count(HouseholdCambodia_2C, g2)
title <- "g2.  Who makes the decision on purchasing, selling,
or transferring major household assets ?"
list <- c("Myself alone","Me in consultation with spouse/other family members",
          "My spouse/other family members", "Do not know")
pond <- sum(g2count$n)
#Now we create the table with the corresponding function
PiePlot(g2count,title,list)

# # #3.9.g. g3. Who makes the decision on borrowing or lending money?
#We create a counting table and a title
g3count <- count(HouseholdCambodia_2C, g3)
title <- "g3. Who makes the decision on borrowing or lending money?"
list <- c("Myself alone","Me in consultation with spouse/other family members",
          "My spouse/other family members", "Do not know")
pond <- sum(g3count$n)
#Now we create the table with the corresponding function
PiePlot(g3count,title,list)

# # #3.9.h. g4. Who makes the decision about how the household income is used?
#We create a counting table and a title
g4count <- count(HouseholdCambodia_2C, g4)
title <- "g4. Who makes the decision about how the household income is used?"
list <- c("Myself alone","Me in consultation with spouse/other family members",
          "My spouse/other family members", "Do not know")
pond <- sum(g4count$n)
#Now we create the table with the corresponding function
PiePlot(g4count,title,list)

}

##3.10 - Fairness
{

# # #3.10.a. b22. Did you sell any certified crop/vegetables/fruit/livestock
#related product in the past year/12 months?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$b22 <- as.factor(HouseholdCambodia_2C$b22)
#We create a counting table and a title
b22count <- count(HouseholdCambodia_2C, b22)
b22count <- b22count[c("2","1"),]
list <- c("yes","no")
title <- "b22. Did you sell any certified crop/vegetables/fruit/livestock
related product in the past year/12 months?"
#Now we create the table with the corresponding function
PiePlotYN(b22count,title,list)

# # #3.10.b. b22_1. If yes, which one(s)? 
#First, we select the useful data
b22_1dat <- HouseholdCambodia_2C[,c(1,29,228:236)]
#We modify long label names
var_label(b22_1dat$b22_0b13) <- "Dried meat (pork,\n beef, etc.)"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$b22))
pond <- 128
pond2 <- nrow(HouseholdCambodia_2C)
title <- "b22_1. If yes, which one(s)?"
xT <- "Certified crops"
ylimn <- -20
ylimx <- 120
dig = 0
#Function Call
PracticePlot(b22_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)
summary(b22_1dat$b22_0b11)

# # #3.10.c. c8. Did you sign a contract whereby the buyer commits to buy 
#from you following some at specific conditions (price, volume, quality, time...)?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$c8 <- as.factor(HouseholdCambodia_2C$c8)
#We create a counting table and a title
c8count <- count(HouseholdCambodia_2C, c8)
title <- "c8. Did you sign a contract whereby the buyer commits to buy 
from you following some at specific conditions (price, volume, quality, time...)?"
list <- c("no","NA")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(c8count,title,list,pond,xT,ylimn)

# # #3.10.d. b14_1. Does “buyer” provide any of the following
#First, we select the useful data
b14_1dat <- HouseholdCambodia_2C[,c(1,29,717:725)]
#We prepare the parameters for the function
pond <- sum(!is.na(HouseholdCambodia_2C$b12_1))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "b14_1. Does “buyer” provide any of the following ?"
xT <- "Answer"
ylimn <- -20
ylimx <- 100
dig = 0
#Function Call
PracticePlot(b14_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.10.e. b14_2. Does “buyer” provide any of the following
#First, we select the useful data
b14_2dat <- HouseholdCambodia_2C[,c(1,29,742:750)]
#We prepare the parameters for the function
pond <- sum(!is.na(HouseholdCambodia_2C$b12_2))
pond2 <- nrow(HouseholdCambodia_2C)
title <- "b14_2. Does “buyer” provide any of the following ?"
xT <- "Answer"
ylimn <- -20
ylimx <- 100
dig = 0
#Function Call
PracticePlot(b14_2dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.10.f. l7. In general, does your household have difficulties paying
#back your loans?
#We create a counting table and a title
l7count <- count(HouseholdCambodia_2C, l7)
title <- "l7. In general, does your household have difficulties paying
back your loans ?"
l7count <- l7count[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(l7count,title,list)

# # #3.10.g. h1. Would you like your children to be farmers too?
#We create a counting table and a title
h1count <- count(HouseholdCambodia_2C, h1)
title <- "h1. Did you sign a contract whereby the buyer commits to buy 
from you following some at specific conditions (price, volume, quality, time...)?"
list <- c("1. Yes, strongly","2. Yes, maybe","3. They should emigrate\n if they had the chance",
          "4. No, agriculture is\n not a good job","5. Do not know")
pond <- nrow(HouseholdCambodia_2C)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(h1count,title,list,pond,xT,ylimn)
}

##3.11 - Connectivity
{

# # #3.11.a. b22_1. Do you collaborate with other people for any task ?
#First, we select the useful data
b22_1dat <- HouseholdCambodia_2C[,c(1,29,647:653,655)]
#We modify long label names
var_label(b22_1dat$b22_11) <- "Share labor (mutual\n help, working together\n on each other farm)"
var_label(b22_1dat$b22_12) <- "Manage water/\nirrigation systems"
var_label(b22_1dat$b22_14) <- "Buy agricultural\n inputs"
var_label(b22_1dat$b22_15) <- "Selling products\n to the markets for\n other farmers"
var_label(b22_1dat$b22_16) <- "Experiment new\n farming practices"
var_label(b22_1dat$b22_10) <-"No collaboration\n with other people\n on these issues"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$b9))
pond <- s[2]
pond2 <- sum(d17count$n)
title <- "b22_1. Do you collaborate with other people for any task ?"
xT <- "Months"
ylimn <- -20
ylimx <- 100
dig = 0
#Function Call
PracticePlot(b22_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.11.b c15. Are you involved in some form of advocacy work (aiming 
#to influence decision-making within political institutions)?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$c15 <- as.factor(HouseholdCambodia_2C$c15)
#We create a counting table and a title
c15count <- count(HouseholdCambodia_2C, c15)
title <- "c15. Are you involved in some form of advocacy work (aiming 
to influence decision-making within political institutions)?"
c15count <- c15count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c15count,title,list)

# # #3.11.c f3.Do you have enough time for your family and your social relationships?
#We create a counting table and a title
f3count <- count(HouseholdCambodia_2C, f3)
title <- "f3.Do you have enough time for your family and your social relationships?"
list <- c("1. No time","2. Very little time","3. Moderate amount of time",
          "4. Almost enough time","5. Sufficient amount of time")
pond <- nrow(HouseholdCambodia_2C)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(f3count,title,list,pond,xT,ylimn)

# # #3.11.d b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
#First we select useful data
data <- HouseholdCambodia_2C[,c(1,29,230)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`b10. if you sell crops/vegetables/fruits or processed products, do you know what`)
colnames(dcount)[2] <- "Answer" 
title <- "b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
- /commune"
dcount$Answer <- c("1.The local market",
                   "2.The provincial/national market","3.Export","4.Other",
                   "6.Do not know","7.NA","5.No","1.The local market",
                   "2.The provincial/national market","3.Export","4.Other",
                   "6.Do not know","7.NA")
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)

# # #3.11.d b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
#First we select useful data
data <- HouseholdCambodia_2C[,c(1,29,240)]
#And we change columns names for label names
colnames(data) <- var_label(data)
#We can now create a counting table
dcount <- data %>%
  count(`commune name (english)`,`b16. if you sell livestock or livestock derived products, do you know what is th`)
colnames(dcount)[2] <- "Answer" 
title <- "b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
- /commune"
dcount$Answer <- c("1.The local market",
                   "2.The provincial/national market","4.Other",
                   "6.Do not know","7.NA","5.No","1.The local market",
                   "2.The provincial/national market","4.Other",
                   "6.Do not know","7.NA")
pond <- nrow(HouseholdCambodia_2C)
ylimn <- 0
ListBplotComm(dcount,title,pond,ylimn)
}

##3.12 - Land and natural resource governance
{

# # #3.12.a. l1. In the past year/12 months, did your household benefit from government
#subsidies to support investment in production or commercialization activities?
#We create a counting table and a title
l1count <- count(HouseholdCambodia_2C, l1)
title <- "l1. In the past year/12 months, did your household
benefit from government subsidies to support investment
in production or commercialization activities?"
l1count <- l1count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(l1count,title,list)

# # #3.12.b. l2. How much of your total household income in 2022 did these 
#government subsidies represent?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$l2 <- as.factor(HouseholdCambodia_2C$l2)
#We create a counting table and a title
l2count <- count(HouseholdCambodia_2C, l2)
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
#First, we select the useful data
d9_1dat <- HouseholdCambodia_2C[,c(1,29,847,849,851)]
#We modify long label names
var_label(d9_1dat$d9_1) <- "Having plots\n rented-in?"
var_label(d9_1dat$d10_1) <- "Having plots\n rented-out?"
var_label(d9_1dat$d11_1) <- "Having plots\n owned?"
#We prepare the parameters for the function
s <- summary(as.factor(HouseholdCambodia_2C$b9))
pond <- sum(d17count$n)
pond2 <- sum(d17count$n)
title <- "d9-10-11. Land ownership"
xT <- ""
ylimn <- -20
ylimx <- 100
dig = 1
#Function call
PracticePlot(d9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig)

# # #3.12.c. e12. Does your household have private land to feed the animals?
HouseholdCambodia_2C$OwnPast <- ifelse(HouseholdCambodia_2C$e12_1 == '1' |
                                        HouseholdCambodia_2C$e12_3 == '1' |
                                        HouseholdCambodia_2C$e12_5 == '1', '1', '0')
#We create a counting table and a title
e12count <- count(HouseholdCambodia_2C, OwnPast)
title <- "e12. Does your household have private land to feed the animals?"
e12count <- e12count[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e12count,title,list)
}

##3.13 - Participation
{

# # #3.13.a. c2. Are you a member of one or more farmer organization (e.g. crops/fruits/
#livestock/honey/ water/Forest etc)?
#First, we select the useful data
#We convert it to the proper format
HouseholdCambodia_2C$c2 <- as.factor(HouseholdCambodia_2C$c2)
#We create a counting table and a title
c2count <- count(HouseholdCambodia_2C, c2)
title <- "Are you a member of one or more farmer organization 
(e.g. crops/fruits/livestock/honey/ water/Forest etc)?"
list <- c("1.No","2.Yes, one","3.Yes, more than one")
pond <- sum(d17count$n)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(c2count,title,list,pond,xT,ylimn)

# # #3.13.b. c1. Are you or anyone in your household active in one or several
#Unions? YN
#We create a counting table and a title
c1_0count <- count(HouseholdCambodia_2C, c1_0)
title <- "c1. Are you or anyone in your household active in one or several
Unions? YN"
c1_0count <- c1_0count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c1_0count,title,list)

# # #3.13.c. c1. Are you or anyone in your household active in one or several
#of the following?
#First, we select the useful data
c1dat <- HouseholdCambodia_2C[,c(1,29,581:589)]
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


  