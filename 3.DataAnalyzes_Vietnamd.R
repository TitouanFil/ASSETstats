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
library(plyr)
library(reshape2)
library(ggplot2)
library(wesanderson)
library(paletteer)
library(RColorBrewer)
library(ggthemes)
library(survey)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Vietnam* database
HouseholdVietnam_2C <- readRDS("HouseholdVietnam_2C.rds")
ClowlandVietnam_2C <- readRDS("ClowlandVietnam_2C.rds")
CuplandVietnam_2C <- readRDS("CuplandVietnam_2.rds")
HouMemberVietnam_2C <- readRDS("HouMemberVietnam_2C.rds")

#
colnames(HouseholdVietnam_2C)[26] <- "district_eng"
  
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
  plot <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=d)) +
    geom_rect(fill = col[1:nrow(data)],colour = "black") +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=4, fill = col[1:nrow(data)]) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 13), legend.position = "none") +
    ggtitle(paste(title,"\n Sample =", round(sum(data$n)/3.62, digits = 0), "/  Population = ",sum(data$n)))
  print(plot)
  }

# # #1.2.b. ADAPTATIVE PIE PLOT
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
  #Prepare the color list
  col <- c("#0066CC","lightblue","grey88")
  # Make the plot
  plot <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=d)) +
    geom_rect(fill = col[1:nrow(data)],colour = "black") +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=4, fill = col[1:nrow(data)]) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 13), legend.position = "none") +
    ggtitle(paste(title,"\n Sample =", round(sum(data$n)/3.62, digits = 0), "/  Population = ",sum(data$n)))
  print(plot)
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
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=9, angle = 85),
          legend.position = "none")+
    labs(x= xT)+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,max(data$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
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

# # #1.2.d. List PLOT without order
ListBPlotNO <- function(data,title,list,pond,xT,ylimn,dig){
  colnames(data)[1] <- 'Answer'
  data$Answer <- list
  #We convert absolute values to % y divinding by the total number of households
  data$Percentage_of_households <- round(data$n/as.numeric(pond)*100, digits=dig)
  #And now a distribution plot
  Plota <- ggplot(data=data, aes(x= reorder(Answer, Answer), y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity") +
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=9, angle = 85),
          legend.position = "none")+
    labs(x= xT)+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,max(data$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
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

# # #1.2.e. List PLOT with province information (as x)
ListBPlotProv <- function(data,title,pond,ylimn,dig,pal){
  dcount <- data
  #We convert absolute values to % y divinding by the total number of households
  provtot <- aggregate(Weight ~ district_eng, data=HouseholdVietnam_2C, sum)
  dcount <- join(dcount, provtot, by = "district_eng")
  colnames(dcount)[4] <- "nbHHtot"
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- nrow(dcount)/2
  mycolors <- colorRampPalette(brewer.pal(9,pal))(nb.cols)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= `district_eng`, y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, max(dcount$Percentage_of_households)+20)+
    scale_fill_manual(values = mycolors)+
    theme_stata(base_size = 8)+
    ggtitle(paste(title,"\n Dien Bien dist.: S =",round(pond*0.51/3.62,digits = 0), "/ P =", round(pond*0.51,digits = 0) ,
                  "\n Moc Chau dist.: S = ",round(pond*0.49/3.62,digits = 0), "/ P =", round(pond*0.49,digits = 0)))
  print(plotb)
}

# # #1.2.f. List PLOT with province information (as x)
ListBPlotProvYN <- function(data,title,pond,ylimn,dig,pal){
  dcount <- data
  #We convert absolute values to % y divinding by the total number of households
  provtot <- aggregate(Weight ~ district_eng, data=HouseholdVietnam_2C, sum)
  dcount <- join(dcount, provtot, by = "district_eng")
  colnames(dcount)[4] <- "nbHHtot"
  dcount$Percentage_of_households <- round(dcount$n/as.numeric(dcount$nbHHtot)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- nrow(dcount)
  mycolors <- colorRampPalette(brewer.pal(9,pal))(nb.cols)
  #And now a distribution plot
  plotb <- ggplot(data=dcount, aes(x= `district_eng`, y=Percentage_of_households, fill = Answer))+
    geom_bar(colour="black",position="stack", stat="identity")+
    geom_text(aes(label = Percentage_of_households), position = position_stack(vjust = .5))+
    scale_fill_manual(values = mycolors)+
    theme_stata(base_size = 8)+
    ggtitle(paste(title,"\n Dien Bien dist.: S =",round(pond*0.51/3.62,digits = 0), "/ P =", round(pond*0.51,digits = 0) ,
                  "\n Moc Chau dist.: S = ",round(pond*0.49/3.62,digits = 0), "/ P =", round(pond*0.49,digits = 0)))
  print(plotb)
}

# # #1.2.g. PRACTICES PLOT
PracticePlot <- function(data,title,pond,pond2,xT,ylimn,ylimx,dig,pal){
  #Then we adapt the format to be able to plot it and create a table
  dmelt <- melt(data ,  id.vars = c('o9', 'district_eng','village_eng_preload','fpc','Weight'), variable.name = 'Practices')
  dstratb <- svydesign(
    id = ~1, strata = ~village_eng_preload,
    weights = ~Weight, data = dmelt)
  
  # # # #b.a Plot with AE practicionners only as %
  #We can now create a counting table
  dcount <- as.data.frame(svytable(~Practices+value, design=dstratb, exclude=NULL))
  colnames(dcount)[2] <- "Answer"
  colnames(dcount)[3] <- "n"
  dcountNAR <- dcount %>%  filter(!is.na(Answer) & Answer != '' & Answer != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(pond)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- nrow(dcountNAR)
  mycolors <- colorRampPalette(brewer.pal(8, pal))(nb.cols)
  #And now a distribution plot
  plota <- ggplot(data=dcountNAR, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = Practices))+
    geom_bar(colour="black",stat="identity")+
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8,angle = 60), legend.position = "none")+
    geom_text(aes(label = Percentage_of_households, vjust = -1)) + ylim(ylimn, ylimx)+
    scale_fill_manual(values= mycolors)+
    labs(x= xT)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))
  print(plota)
  
  # # # #b.b Plot with provinces and total %
  #We create a counting table
  dcount <- as.data.frame(svytable(~district_eng+Practices+value, design=dstratb, exclude=NULL))
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != '' & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  provtot <- aggregate(Weight ~ district_eng, data=HouseholdVietnam_2C, sum)
  dcountNAR <- join(dcountNAR, provtot, by = "district_eng")
  colnames(dcountNAR)[4] <- "n"
  colnames(dcountNAR)[5] <- "nbHHtot"
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=reorder(Practices, -Percentage_of_households), y=Percentage_of_households, fill = `district_eng`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, ylimx)+
    scale_fill_brewer(palette= pal, direction = -1)+
    labs(x= xT)+
    ggtitle(paste(title,"\n Dien Bien dist.: S =",round(pond2*0.51/3.62,digits = 0), "/ P =", round(pond2*0.51,digits = 0) ,
                  "\n Moc Chau dist.: S = ",round(pond2*0.49/3.62,digits = 0), "/ P =", round(pond2*0.49,digits = 0)))
  print(plotb)
}

# # #1.2.h. PRACTICES PLOT FOR MONTHS
Monthplot <- function(data,title,pond,pond2,xT,ylimn,ylimx,dig,pal){
  #Then we adapt the format to be able to plot it and create a table
  dmelt <- melt(data ,  id.vars = c('o9', 'district_eng','village_eng_preload','fpc','Weight'), variable.name = 'Practices')
  dstratb <- svydesign(
    id = ~1, strata = ~village_eng_preload,
    weights = ~Weight, data = dmelt)
  
  # # # #b.a Plot with AE practicionners only as %
  #We can now create a counting table
  dcount <- as.data.frame(svytable(~Practices+value, design=dstratb, exclude=NULL))
  colnames(dcount)[2] <- "Answer"
  colnames(dcount)[3] <- "n"
  dcountNARa <- dcount %>%  filter(!is.na(Answer) & Answer != 0)
  #We convert absolute values to % y divinding by the total number of households
  dcountNARa$Percentage_of_households <- round(dcountNARa$n/as.numeric(pond)*100, digits=dig)
  # Define the number of colors you want
  nb.cols <- nrow(dcountNARa)
  mycolors <- colorRampPalette(brewer.pal(8, pal))(nb.cols)
  #And now a distribution plot
  plota <- ggplot(data=dcountNARa, aes(x=Practices, y=Percentage_of_households, fill = Practices))+
    geom_bar(colour="black",stat="identity")+
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8,angle = 60), legend.position = "none",
          panel.grid.major=element_line(colour="grey"))+
    geom_text(aes(label = Percentage_of_households, vjust = -1)) + ylim(ylimn, ylimx)+
    scale_fill_manual(values= mycolors)+
    labs(x= xT)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))
  print(plota)
  
  # # # #b.b Plot with provinces and total %
  #We create a counting table
  dcount <- as.data.frame(svytable(~district_eng+Practices+value, design=dstratb, exclude=NULL))
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  #We convert absolute values to % y divinding by the total number of households
  provtot <- aggregate(Weight ~ district_eng, data=HouseholdVietnam_2C, sum)
  dcountNAR <- join(dcountNAR, provtot, by = "district_eng")
  colnames(dcountNAR)[4] <- "n"
  colnames(dcountNAR)[5] <- "nbHHtot"
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(dcountNAR$nbHHtot)*100, digits=dig)
  #And now a distribution plot
  plotb <- ggplot(data=dcountNAR, aes(x=Practices, y=Percentage_of_households, fill = `district_eng`))+
    geom_bar(colour="black",stat="identity",position = 'dodge')+
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8,angle = 60))+
    geom_text(aes(label = Percentage_of_households), vjust = -1, position = position_dodge(0.9))+
    ylim(ylimn, ylimx)+
    scale_fill_brewer(palette= pal, direction = -1)+
    labs(x= xT)+
    ggtitle(paste(title,"\n Dien Bien dist.: S =",round(pond2*0.51/3.62,digits = 0), "/ P =", round(pond2*0.51,digits = 0) ,
                  "\n Moc Chau dist.: S = ",round(pond2*0.49/3.62,digits = 0), "/ P =", round(pond2*0.49,digits = 0)))
  print(plotb)
}

# # #1.2.i. CROP PLOT (big or small tables)
#For this function, we should use a database including column with household id 
#+ 11 columns with binary values for each crops
CropPlot <- function(data,pond, title,ylimn,ylimx,dig){
  #We replace binary values by crop names
  for (i in 3:13){
    data[,i] <- as.character(data[,i])
    for (j in 1:595){
      data[j,i] <- ifelse(data[j,i] == '1', dcrops[j,i], data[j,i])
    }
  }
  # We melt data frame into long format
  dmelt <- melt(data, id.vars = c("o9","village_eng_preload","fpc","Weight"))
  dmelt <- dmelt %>%  filter(!is.na(o9))
  # SVy design implementation
  dstrate <- svydesign(
    id = ~1, strata = ~village_eng_preload,
    weights = ~Weight, data = dmelt)
  #We count the answers and filter NA and "no" values
  dcount <- as.data.frame(svytable(~value, design=dstrate, exclude=NULL))
  dcountNAR <- dcount %>%  filter(!is.na(value) & value != 0)
  colnames(dcountNAR)[2] <- "n"
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(pond)*100, digits=dig)
  #We order the table
  dcountNAR <- dcountNAR[order(-dcountNAR$Percentage_of_households), ]
  if (nrow(dcountNAR) > 15){
    dcountNARa <- dcountNAR[1:as.numeric(nrow(dcountNAR)/2),]
    dcountNARb <- dcountNAR[as.numeric((nrow(dcountNAR)/2+1)):as.numeric(nrow(dcountNAR)),]
    #And now a distribution plot
    Plota <- ggplot(data=dcountNARa, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
    geom_bar(colour="black",stat="identity") +
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8, angle = 85), legend.position = "none"
          , axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,ylimx)+
      ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
    print(Plota)
    #2nd plot
    Plotb  <- ggplot(data=dcountNARb, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
    geom_bar(colour="black",stat="identity") +
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8, angle = 85), legend.position = "none", 
          axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(ylimn,ylimx)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
    scale_fill_viridis_d(alpha = 1,
                         begin = 0,
                         end = 1,
                         direction = 1,
                         option = "E",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "fill")
  print(Plotb)} else {
    #And now a distribution plot
      Plota <- ggplot(data=dcountNAR, aes(x= reorder(value, -Percentage_of_households), y=Percentage_of_households, fill = value))+
      geom_bar(colour="black",stat="identity") +
      theme_stata(base_size = 8)+
      theme(axis.text.x = element_text(size=8, angle = 85), legend.position = "none"
            , axis.title.x = element_blank())+
      geom_text(aes(label = Percentage_of_households, vjust = -1))+
      ylim(ylimn,ylimx)+
      ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
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
}

# # #1.2.j. ANIMAL PLOT
AnimalPlot <- function(data,title,pond){
  data[,3] <- as.character(data[,3])
  colnames(data)[2] <- "Animal"
  for (j in 1:594){
    data[j,3] <- ifelse(data[j,3] == 1, data[j,2], data[j,3])
  }
  data$Animal <- ifelse(is.na(data$Animal), '', data$Animal)
  #We count the answers and filter NA and "no" values
  dstratAn <- svydesign(
    id = ~1, strata = ~village_eng_preload,
    weights = ~Weight, data = data)
  
  # # # #b.a Plot with AE practicionners only as %
  #We can now create a counting table
  dcount <- as.data.frame(svytable(~Animal, design=dstratAn, exclude=NULL))
  colnames(dcount)[2] <- "n"
  dcountNAR <- dcount %>%  filter(Animal != 0 & Animal != '')
  #We convert absolute values to % y divinding by the total number of households
  dcountNAR$Percentage_of_households <- round(dcountNAR$n/as.numeric(pond)*100, digits=1)
  #We order the table
  dcountNAR <- dcountNAR[order(-dcountNAR$Percentage_of_households), ]
  #And now a distribution plot
  Plota <- ggplot(data=dcountNAR, aes(x= reorder(Animal, -Percentage_of_households), y=Percentage_of_households, fill = Animal))+
    geom_bar(colour="black",stat="identity") +
    theme_stata(base_size = 8)+
    theme(axis.text.x = element_text(size=8, angle = 85), legend.position = "none", 
          axis.title.x = element_blank())+
    geom_text(aes(label = Percentage_of_households, vjust = -1))+
    ylim(-10,max(dcountNAR$Percentage_of_households)+20)+
    ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
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
dlowc <- ClowlandVietnam_2C[,c(1:2,9)]
dlowcwide <- reshape(dlowc, direction = "wide", timevar = "crop1_now", idvar = "hhid_re2")
dlowcwide <- dlowcwide %>% relocate(d2_13e.2 , .before = d2_13e.3)
dupc <- CuplandVietnam_2C[,c(1:2,8)]
colnames(dupc)[colnames(dupc) == "hhid_re3"] ="hhid_re2"
dupcwide <- reshape(dupc, direction = "wide", timevar = "crop2_now", idvar = "hhid_re2")
#We create a table with all the id
HHid <- HouseholdVietnam_2C[,1:2]
colnames(HHid)[colnames(HHid) == "o9"] = "hhid_re2"
#We merge the tables
dcrops <- merge(HHid, dlowcwide, by.x= "hhid_re2", all = T)
dcrops <- merge(dcrops, dupcwide, by.x = "hhid_re2", all = T)
#And we remove useless columns
dcrops <- dcrops[,-c(2,8)]
x <- HouseholdVietnam_2C[,c(1,2)]
colnames(x)[1] <- "hhid_re2"
dcrops <- join(x,dcrops,by = "hhid_re2", match = "first")
# #Additionnal stuffs
#We convert data to the proper format
dlowc$d2_13e <- as.factor(dlowc$d2_13e)
dupc$d2_23e <- as.factor(dupc$d2_23e)
#We create a column including the information if the household is growing crops or no
HouseholdVietnam_2C$CropYN <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), '0', '1')
}

# # #Preparation of household head table and related stuff
{
Province <- cbind(as.character(HouseholdVietnam_2C$o9),as.character(HouseholdVietnam_2C$district_eng))
HouMemberProv <- merge(Province, HouMemberVietnam_2C, by.x = "V1", by.y = "hhid_re1")
HouHead <- HouMemberProv[HouMemberProv$a2 == '1',]
HouHead <- copy_labels(HouHead, HouMemberVietnam_2C)
colnames(HouMemberProv)[1:2] <- c("o9","Province")
colnames(HouHead)[1:2] <- c("o9","Province")
#We add HH information to the main table
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, HouHead, by = "o9", match = "first")
HouseholdVietnam_2C$Fill <- c(1)
HouseholdVietnam_2C <- HouseholdVietnam_2C %>% relocate(Fill, .after = d18_11111_a)

#Also, we create a categorical variable with age range instead of quantitative value
HouseholdVietnam_2C$AgeCat <- cut(HouseholdVietnam_2C$a4_1,
                      breaks=c(20,30, 35, 40, 45, 50, 55, 60, 65, 70,100),
                      labels=c('<30', '30-34', '35-39', '40-44','45-49','50-54',
                               '55-59','60-64','65-69','>69'))
}

# # #Preparation of other columns
{
#For some columns, we just add NA as factor level to include it to the tables then

  HouseholdVietnam_2C$a3 <- factor(HouseholdVietnam_2C$a3, exclude=NULL)
HouseholdVietnam_2C$AgeCat <- factor(HouseholdVietnam_2C$AgeCat, exclude=NULL)
HouseholdVietnam_2C$a6 <- factor(HouseholdVietnam_2C$a6, exclude=NULL)
HouseholdVietnam_2C$d17 <- factor(HouseholdVietnam_2C$d17, exclude=NULL)
HouseholdVietnam_2C$d20 <- factor(HouseholdVietnam_2C$d20, exclude=NULL)
HouseholdVietnam_2C$e13 <- factor(HouseholdVietnam_2C$e13, exclude=NULL)
HouseholdVietnam_2C$e13_1 <- factor(HouseholdVietnam_2C$e13_1, exclude=NULL)
HouseholdVietnam_2C$d140 <- factor(HouseholdVietnam_2C$d140, exclude=NULL)
HouseholdVietnam_2C$e58 <- factor(HouseholdVietnam_2C$e58, exclude=NULL)
HouseholdVietnam_2C$e59 <- factor(HouseholdVietnam_2C$e59, exclude=NULL)
HouseholdVietnam_2C$e60 <- factor(HouseholdVietnam_2C$e60, exclude=NULL)
HouseholdVietnam_2C$e26 <- factor(HouseholdVietnam_2C$e26, exclude=NULL)
HouseholdVietnam_2C$e40 <- factor(HouseholdVietnam_2C$e40, exclude=NULL)
HouseholdVietnam_2C$e54 <- factor(HouseholdVietnam_2C$e54, exclude=NULL)
HouseholdVietnam_2C$d32 <- factor(HouseholdVietnam_2C$d32, exclude=NULL)
HouseholdVietnam_2C$e5_b <- factor(HouseholdVietnam_2C$e5_b, exclude=NULL)
HouseholdVietnam_2C$TotCrop <- ifelse(!is.na(HouseholdVietnam_2C$no_crop1),HouseholdVietnam_2C$no_crop1,0) +
  ifelse(!is.na(HouseholdVietnam_2C$no_crop2),HouseholdVietnam_2C$no_crop2,0)
HouseholdVietnam_2C$TotCrop <- as.factor(HouseholdVietnam_2C$TotCrop)
HouseholdVietnam_2C$TotCrop <- factor(HouseholdVietnam_2C$TotCrop, exclude=NULL)
HouseholdVietnam_2C$d26 <- factor(HouseholdVietnam_2C$d26, exclude=NULL)
HouseholdVietnam_2C$b1 <- factor(HouseholdVietnam_2C$b1, exclude=NULL)
HouseholdVietnam_2C$b2 <- factor(HouseholdVietnam_2C$b2, exclude=NULL)
HouseholdVietnam_2C$b9 <- factor(HouseholdVietnam_2C$b9, exclude=NULL)
HouseholdVietnam_2C$c8 <- factor(HouseholdVietnam_2C$c8, exclude=NULL)
HouseholdVietnam_2C$l7 <- factor(HouseholdVietnam_2C$l7, exclude=NULL)
HouseholdVietnam_2C$b10 <- factor(HouseholdVietnam_2C$b10, exclude=NULL)
HouseholdVietnam_2C$b16 <- factor(HouseholdVietnam_2C$b16, exclude=NULL)
HouseholdVietnam_2C$l2 <- factor(HouseholdVietnam_2C$l2, exclude=NULL)

#(Need to create an indicator including all the product household sell,
  #animal and crops)
  #1st for lowland
  Province <- cbind(as.character(HouseholdVietnam_2C$o9),as.character(HouseholdVietnam_2C$district_eng))
  LowlandProv <- merge(Province, ClowlandVietnam_2C, by.x = "V1", by.y = "hhid_re2")
  colnames(LowlandProv)[1:2] <- c("o9","Province")
  LowlandSold <- LowlandProv %>% filter(LowlandProv$d2_136 == 0)
  ncrop_soldl <- LowlandSold %>%
    dplyr::count(`Province`,`o9`)
  #2nd for upland
  UplandProv <- merge(Province, CuplandVietnam_2C, by.x = "V1", by.y = "hhid_re3")
  colnames(UplandProv)[1:2] <- c("o9","Province")
  UplandSold <- UplandProv %>% filter(UplandProv$d2_236 == 0)
  ncrop_soldu <- UplandSold %>%
    dplyr::count(`Province`,`o9`)
  #3rd for the 3 main animals
  AniSold <- HouseholdVietnam_2C[,c(1,26,1918,1946,1965)]
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
  SoldProd <- SoldProd[,c(1,11)]
  HouseholdVietnam_2C <- join(HouseholdVietnam_2C,SoldProd, by = "o9")
  
  #Create another column
  HouseholdVietnam_2C$OwnPast <- ifelse(HouseholdVietnam_2C$e12_1 == '1' |
                                          HouseholdVietnam_2C$e12_3 == '1' |
                                          HouseholdVietnam_2C$e12_5 == '1', '1', '0')
  HouseholdVietnam_2C$OwnPast <- factor(HouseholdVietnam_2C$OwnPast, exclude=NULL)
}

##1.4 Preparation of Sampling weight through svy design function
{
#First we add a column in which we add the sampling weight for each household
x <- as.data.frame(summary(HouseholdVietnam_2C$village_eng_preload))
x$fpc <- c(66,56,89,121,95,75,51,139,121,94,180,252,176,
           123,94,109,83,46,95,87)
x$Weight <- x$fpc/x$`summary(HouseholdVietnam_2C$village_eng_preload)`
x$village_eng_preload <- rownames(x)
colnames(x)[1] <- "SurH"
HouseholdVietnam_2C <- join(HouseholdVietnam_2C, x, by = "village_eng_preload")
dstrat <- svydesign(
  id = ~1, strata = ~village_eng_preload,
  weights = ~Weight, data = HouseholdVietnam_2C,
  fpc = ~fpc)
#We add the proper labels to the main database
label <- readRDS("HouseholdVietnam_2C.rds")
HouseholdVietnam_2C <- copy_labels(HouseholdVietnam_2C,label)
}


###2. Data analyzes - General

##2.1 Household characteristics
{
# # #2.1.a. o10. Are both the man and the woman available for the interview?
#We create a counting table and a title
o10count <- as.data.frame(svytable(~o10, design=dstrat))
colnames(o10count)[2] <- "n"
title <- "o10. Are both the man and the woman available 
for the interview?"
list <- c("yes","no")
#Now we create the plot with the corresponding function
PiePlotYN(o10count,title,list)
#Check

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - General
#We create a counting table and a title
o11count <- as.data.frame(svytable(~o11, design=dstrat))
colnames(o11count)[2] <- "n"
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent)"
list <- c("Male","Female")
pond <- sum(o11count$n)
#Now we create the plot with the corresponding function
PiePlot(o11count,title,list)
#Check

# # #2.1.b. o11. Who do you do the interview with? (Men and Women
#distribution as respondent) - /Province
#We create a counting table and add the information required for the function
dcount <- as.data.frame(svytable(~district_eng+o11, design=dstrat))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount$Answer <- c("Male","Male","Female","Female")
title <- "o11. Who do you do the interview with? (Men and Women
distribution as respondent) - /Province"
pond <- sum(dcount$n)
ylimn <- 0
dig = 1
pal = "Blues"
#Now we create the plot with the corresponding function
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#Check

# # #2.1.c. a3. What is the sex of a1 (Household head)?
#We create a counting table and a title
a3count <- as.data.frame(svytable(~a3, design=dstrat, exclude=NULL))
colnames(a3count)[2] <- "n"
title <- "a3. What is the sex of a1 (Household head)?"
list <- c("Male","Female","NA")
pond <- sum(a3count$n)
#Now we create the table with the corresponding function
PiePlot(a3count,title,list)
#COMMENT: HH maybe passed away, I wait for confirmation (also for following plots)
#Check

# # #2.1.d. Household head age distribution
#First we create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+AgeCat, design=dstrat))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
title <- "a4_1. Age of a1 (Household head) - Province based"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
pond <- sum(dcount$n)
xT <- "Age (years)"
ylimn <- 0
dig = 0
pal = "Greens"
#Now we create the plot with the corresponding function
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check

# # #2.1.e. Household head occupation distribution
dcount <- as.data.frame(svytable(~district_eng+a6, design=dstrat))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
dcount$Answer <- c('a. Agricultural work\n (own farm)','b.Other farm\n salaried employment\n (permanent)',
                   'c.Other farm\n salaried employment\n (temporary)','d.Government','e.Business (owner)', 
                   'f.Salaried employment\n (non-agricultural work)','g.Housewife/\n caregiver',
                   'h.Unable\n to work','i.Other',NA)
pond <- sum(dcount$n)
xT <- "Occupation"
ylimn <- -20
dig <- 0
title <- "a6. What is a1's main occupation for the
past 12 months? (Household head)"
pal = "Greens"
#Now we create the plot with the corresponding function
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check

# # #2.1.f. Distribution of Household members number
#First we create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+a0, design=dstrat))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
pond <- sum(dcount$n)
xT <- "Num of household members"
ylimn <- 0
dig <- 0
title <- "How many members in the household ?"
pal = "Greens"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check
}

##2.2 Land & Crops characteristics
{
# # #2.2.a. d1. What kinds of land did your household have in the past 12 months?
#First, we select the useful data
d1dat <- HouseholdVietnam_2C[,c(1,26,32,661:667,2832,2833)]
#We restore the column proper names
colnames(d1dat)[4:10] <- var_label(d1dat)[4:10]
#We fulfil the information required in the function
pond <- sum(HouseholdVietnam_2C$Weight)
pond2 <- sum(HouseholdVietnam_2C$Weight)
xT <- "Kind of land owned"
ylimn <- -20
ylimx <- 110
title <- "d1. What kinds of land did your household have in the past 12 months?"
dig = 0
pal = "Dark2"
PracticePlot(d1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #2.2.b. Growing crops - Yes or No
#We create a counting table and a title
CropYNcount <- as.data.frame(svytable(~CropYN, design=dstrat))
CropYNcount <- CropYNcount[c("2","1"),]
colnames(CropYNcount)[2] <- "n"
title <- "Are you growing crops ?"
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(CropYNcount,title,list)
#Check

# # #2.2.c. Crop diversity - Lowland (Household based)
#First we need to apply a new survey design
Dumm <- HouseholdVietnam_2C[,c(1,32,2832,2833)]
colnames(dlowc)[2] <- "o9"
dlowc2 <- join(dlowc, Dumm, by = "o9")
dstrat <- svydesign(
  id = ~1, strata = ~village_eng_preload,
  weights = ~Weight, data = dlowc2)
#We create a counting table and a title
Cropcount <- as.data.frame(svytable(~d2_13e, design=dstrat))
colnames(Cropcount)[2] <- "n"
#We determine other function parameters
title <- "Crop diversity among households (% of households) - Lowland"
pond <- sum(HouseholdVietnam_2C$Weight)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_13e <- as.character(Cropcount$d2_13e)
#We change too large crops label names 
Cropcount[1,1] <- "Summer-autumn\n season rice"
Cropcount[2,1] <- "Winter-Spring\n season rice"
Cropcount[5,1] <- "Other vegetable\n crop"
Cropcount[6,1] <- "Sweet potatoes\n, tuber"
Cropcount[17,1] <- "Cauliflowers,\n Broccoli"
Cropcount[18,1] <- "Chinese kale/\n Gailan"
Cropcount[29,1] <- "Chinese\n flowering cabbage/\n choysum"
Cropcount[30,1] <- "Chrysanthemum,\n leaves"
Cropcount[32,1] <- "Lettuce,\n romaine,\n leaves"
Cropcount[37,1] <- "Other orchards\n crop"
Cropcount[39,1] <- "Zucchini,\n common green,\n fruit"
Cropcount[40,1] <- "Basil, sweet,\n leaves"
Cropcount[43,1] <- "Green mustard/\n Choysum"
Cropcount[47,1] <- "Long bean,\ Chinese"
Cropcount[48,1] <- "Moutainous\n rice"
Cropcount[49,1] <- "Other annual\n crop"
Cropcount[53,1] <- "Pomelos and\n grapefruits"
Cropcount[55,1] <- "Sawtooth herb /\n Culantro"
xT <- "Crops"
ylimn <- -15
#For the 14 first crops
Cropcount1 <- Cropcount[c(1:14),]
list1 <- Cropcount$d2_13e[c(1:14)]
dig <- 0
ListBPlot(Cropcount1,title,list1,pond,xT,ylimn,dig)
#For the 14 second crops
ylimn <- -10
dig <- 1
Cropcount2 <- Cropcount[c(15:28),]
list2 <- Cropcount$d2_13e[c(15:28)]
ListBPlot(Cropcount2,title,list2,pond,xT,ylimn,dig)
#For the 14 third crops
Cropcount3 <- Cropcount[c(29:42),]
list3 <- Cropcount$d2_13e[c(29:42)]
ListBPlot(Cropcount3,title,list3,pond,xT,ylimn,dig)
#For the 14 fourth crops
Cropcount4 <- Cropcount[c(43:56),]
list4 <- Cropcount$d2_13e[c(43:56)]
ListBPlot(Cropcount4,title,list4,pond,xT,ylimn,dig)
#Check

# # #2.3.d. Crop diversity - Upland (Household based)
#First we need to apply a new survey design
colnames(dupc)[2] <- "o9"
dupc2 <- join(dupc, Dumm, by = "o9")
dupc2 <- dupc2[!is.na(dupc2$Weight),]
dstrat <- svydesign(
  id = ~1, strata = ~village_eng_preload,
  weights = ~Weight, data = dupc2)
#We create a counting table and a title
Cropcount <- as.data.frame(svytable(~d2_23e, design=dstrat))
colnames(Cropcount)[2] <- "n"
#We determine other function parameters
title <- "Crop diversity among households (% of households) - Upland"
pond <- sum(HouseholdVietnam_2C$Weight)
#As there are many different crops, we'll display several plots
Cropcount <- Cropcount[order(-Cropcount$n),]
Cropcount$d2_23e <- as.character(Cropcount$d2_23e)
#We change too large crops label names 
Cropcount[8,1] <- "Summer-autumn\n season rice"
Cropcount[11,1] <- "Other vegetable\n crop"
Cropcount[12,1] <-"Winter-Spring\n season rice"
Cropcount[20,1] <- "Other orchards\n crop"
Cropcount[23,1] <- "Pomelos and\n grapefruits"
Cropcount[27,1] <-"Other perenial\n crop"
Cropcount[28,1] <- "Eggplant,\n Thai"
Cropcount[38,1] <- "Sweet potatoes,\n leaves"
xT <- "Crops"
ylimn <- -10
#For the 14 first crops
Cropcount1 <- Cropcount[c(1:14),]
list1 <- Cropcount$d2_23e[c(1:14)]
dig <- 0
ListBPlot(Cropcount1,title,list1,pond,xT,ylimn,dig)
#For the 14 second crops
ylimn <- -5
dig <- 1
Cropcount2 <- Cropcount[c(15:28),]
list2 <- Cropcount$d2_23e[c(15:28)]
ListBPlot(Cropcount2,title,list2,pond,xT,ylimn,dig)
#For the 14 third crops
Cropcount3 <- Cropcount[c(29:38),]
list3 <- Cropcount$d2_23e[c(29:38)]
ListBPlot(Cropcount3,title,list3,pond,xT,ylimn,dig)
#Check

# # #2.2.e. Crop diversity - Lowland (Area based)
dlowc2 <- ClowlandVietnam_2C[,c(1:2,9,11)]
dlowc2$d2_132 <- as.character(dlowc2$d2_132)
dlowc2$d2_132 <- as.numeric(dlowc2$d2_132)
colnames(dlowc2)[2] <- "o9"
dlowc2 <- join(dlowc2, Dumm, by = "o9")
dlowc2$NewAre <- dlowc2$d2_132*dlowc2$Weight
#We sum the area per crops
result  <- aggregate(NewAre ~ d2_13e, dlowc2, sum)
result$AverageTot <- result$NewAre/sum(ifelse(!is.na(HouseholdVietnam_2C$no_crop1), 1, 0)*HouseholdVietnam_2C$Weight)
result$AverageTot <- as.numeric(format(result$AverageTot,scientific = FALSE, digits = 1))
result$AverageTot <- round(result$AverageTot, digits = 0)
#Now we prepare the data for the function
#We determine other function parameters
title <- "Average crop area - Lowland (10 most popular)"
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$no_crop1), 1, 0)*HouseholdVietnam_2C$Weight), digits = 0)
result <- result[order(-result$AverageTot),]
#As there are many different crops, we'll display several plots
result$d2_13e <- as.character(result$d2_13e)
colnames(result)[3] <- 'n'
#We change too large crops label names 
result[1,1] <- "Summer-autumn\n season rice"
result[2,1] <- "Winter-Spring\n season rice"
result[5,1] <- "Other vegetable\n crop"
result[10,1] <- "Sweet potatoes\n, tuber"
result[19,1] <- "Zucchini,\n common green,\n fruit"
result[24,1] <- "Other orchards\n crop"
result[26,1] <- "Long bean,\ Chinese"
result[28,1] <- "Pomelos and\n grapefruits"
result[30,1] <- "Cauliflowers,\n Broccoli"
result[31,1] <- "Other annual\n crop"
result[37,1] <- "Chinese\n flowering cabbage/\n choysum"
result[38,1] <- "Basil, sweet,\n leaves"
result[41,1] <- "Moutainous\n rice"
result[43,1] <- "Chinese kale/\n Gailan"
result[45,1] <- "Chrysanthemum,\n leaves"
result[54,1] <- "Sawtooth herb /\n Culantro"
result[56,1] <- "Lettuce,\n romaine,\n leaves"
result[57,1] <- "Green mustard/\n Choysum"
xT <- "Crops"
ylimn <- -400
#We'll plot only the 10 first crops
result1 <- result[c(1:10),]
list1 <- result$d2_13e[c(1:10)]
dig <- 0
Plota <- ggplot(data=result1, aes(x= reorder(d2_13e, -n), y= n, fill = d2_13e))+
  geom_bar(colour="black",stat="identity") +
  theme_stata(base_size = 8)+
  theme(axis.text.x = element_text(size=8, angle = 85),
        legend.position = "none")+
  labs(x= xT)+
  labs(y= "area (m2)")+
  geom_text(aes(label = n, vjust = -1))+
  ylim(ylimn,max(result1$n)+200)+
  ggtitle(paste(title,"\n Sample =", round(pond/3.62, digits = 0), "/  Population = ",pond))+
  scale_fill_viridis_d(alpha = 1,
                       begin = 0,
                       end = 1,
                       direction = 1,
                       option = "E",
                       na.value = "grey50",
                       guide = "colourbar",
                       aesthetics = "fill")
Plota
#Check

# # #2.2.f. Crop diversity - Upland (Area based)
dupc2 <- CuplandVietnam_2C[,c(1:2,8,10)]
dupc2$d2_232 <- as.character(dupc2$d2_232)
dupc2$d2_232 <- as.numeric(dupc2$d2_232)
colnames(dupc2)[2] <- "o9"
dupc2 <- join(dupc2, Dumm, by = "o9")
dupc2$NewAre <- dupc2$d2_232*dupc2$Weight
#We sum the area per crops
result  <- aggregate(NewAre ~ d2_23e, dupc2, sum)
result$AverageTot <- result$NewAre/sum(ifelse(!is.na(HouseholdVietnam_2C$no_crop2), 1, 0)*HouseholdVietnam_2C$Weight)
result$AverageTot <- round(result$AverageTot, digits = 0)
#Now we prepare the data for the function
#We determine other function parameters
title <- "Average crop area - Upland (10 most popular)"
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$no_crop2), 1, 0)*HouseholdVietnam_2C$Weight), digits = 0)
result <- result[order(-result$AverageTot),]
#As there are many different crops, we'll display several plots
result$d2_23e <- as.character(result$d2_23e)
colnames(result)[3] <- 'n'
#We change too large crops label names 
result[9,1] <- "Summer-autumn\n season rice"
result[13,1] <- "Other vegetable\n crop"
result[17,1] <-"Winter-Spring\n season rice"
result[19,1] <-"Other perenial\n crop"
result[20,1] <- "Other orchards\n crop"
result[27,1] <- "Pomelos and\n grapefruits"
result[33,1] <- "Eggplant,\n Thai"
result[38,1] <- "Sweet potatoes,\n leaves"
xT <- "Crops"
ylimn <- -600
#We'll plot only the 10 first crops
result1 <- result[c(1:10),]
list1 <- result$d2_23e[c(1:10)]
dig <- 0
Plota <- ggplot(data=result1, aes(x= reorder(d2_23e, -n), y= n, fill = d2_23e))+
  geom_bar(colour="black",stat="identity") +
  theme_stata(base_size = 8)+
  theme(axis.text.x = element_text(size=9, angle = 85),
        legend.position = "none")+
  labs(x= xT)+
  labs(y= "area (m2)")+
  geom_text(aes(label = n, vjust = -1))+
  ylim(ylimn,max(result1$n)+500)+
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
#Check
}

##2.3 Animal characteristics
{
# # #2.2.a. e2. If yes, which below animal does your household have?
#First, we select the useful data
e2dat <- HouseholdVietnam_2C[,c(1,26,32,1871:1883,2832,2833)]
for (i in 4:16){
  e2dat[,i] <- as.factor(e2dat[,i])
  #We restore the column label real names
  colnames(e2dat)[i] <- var_label(e2dat)[i]
}
pond <- round(sum(ifelse(HouseholdVietnam_2C$e1 == '1',1,0)*HouseholdVietnam_2C$Weight), digits = 0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
xT <- "Animals"
ylimn <- -10
ylimx <- 85
title <- "e2. Which below animal does your household have?"
dig = 0
pal = "Dark2"
PracticePlot(e2dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check
}


###3. Data analyzes for Agroecological principles

##3.1 - Recycling
{
# # #3.1.a. d17. Does your household use ecological/agroecological/integrated practices...
#We create a counting table and a title
d17count <- as.data.frame(svytable(~d17, design=dstrat, exclude=NULL))
d17count$d17 <- as.character(d17count$d17)
colnames(d17count)[2] <- "n"
d17count[4,1] <- '/'
#We order the answers
d17count <- d17count[c("2","1","3","4"),]
title <- "d17. Does your household use ecological/
agroecological/integrated practices maintain/
enhance soil fertility in your fields?"
list <- c("yes","no","do not know", "/")
#Now we create the table with the corresponding function
PiePlotYN(d17count,title,list)
#Check

# # #3.1.b. d18 - Which of the following ecological/agroecological/integrated practices
#does your household use to maintain/enhance soil fertility in your fields?
#First, we select the useful data
d18dat <- HouseholdVietnam_2C[,c(1,26,32,1114:1125,2832,2833)]
#We modify long label names
var_label(d18dat$d183) <- 'Bokashi (fermented\norganic matter)'
var_label(d18dat$d184) <- 'Legume-based\ngreen manure'
var_label(d18dat$d185) <- 'Pulses in association and/or\n rotation with main crop'
var_label(d18dat$d186) <- 'Cover crops in association and/\nor rotation with main crop'
var_label(d18dat$d188) <- 'Crop residue\nmaintenance'
var_label(d18dat$d189) <- 'Recycling\ncrop waste'
var_label(d18dat$d1810) <- 'Ramial Wood Chip (RWC) or\n otherwood chips'
var_label(d18dat$d1811) <- 'Organic agro-\nindustrial waste'
colnames(d18dat)[4:15] <- var_label(d18dat)[4:15]
pond <- round(sum(ifelse(HouseholdVietnam_2C$d17 == '2', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "d18 - Which of the following ecological/agroecological/integrated practices
does your household use to maintain/enhance soil fertility in your fields?"
xT <- "Practices"
ylimn <- -35
ylimx <- 100
dig = 0
pal = "Dark2"
PracticePlot(d18dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.1.c. d18_11. For which crop(s) do you use animal manure?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1811dat <- HouseholdVietnam_2C[,c(1,32,1128:1138,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d181 != 0 & !is.na(HouseholdVietnam_2C$d181), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_11. For which crop(s) do you use animal manure?
(% of households using animal manure)"
ylimn <- -45
ylimx <- 60
dig = 0
#We apply the plotting function
CropPlot(d1811dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.d. d18_21. For which crop(s) do you use Compost (heap)?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1821dat <- HouseholdVietnam_2C[,c(1,32,1142:1152,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d182 != 0 & !is.na(HouseholdVietnam_2C$d182), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "18_21. For which crop(s) do you use compost (heap)?
(% of households using compost)"
ylimn <- -45
ylimx <- 60
dig = 0
#We apply the plotting function
CropPlot(d1821dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.e. d18_31. For which crop(s) do you use Bokashi (fermented organic matter)?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1831dat <- HouseholdVietnam_2C[,c(1,32,1156:1166,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d183 != 0 & !is.na(HouseholdVietnam_2C$d183), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_31. For which crop(s) do you use Bokashi (fermented organic matter)?
(% of households using Bokashi)"
ylimn <- -45
ylimx <- 100
dig = 0
#We apply the plotting function
CropPlot(d1831dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.f. d18_41. For which crop(s) do you use legume-based green manure?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1841dat <- HouseholdVietnam_2C[,c(1,32,1170:1180,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d184 != 0 & !is.na(HouseholdVietnam_2C$d184), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_41. For which crop(s) do you use legume-based green manure?
(% of households using legume-based green manure)"
ylimn <- -45
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot(d1841dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.g. d18_51. For which crop(s) do you use pulses in association and/or rotation with main crop ?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1851dat <- HouseholdVietnam_2C[,c(1,32,1184:1194,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d185 != 0 & !is.na(HouseholdVietnam_2C$d185), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_51. For which crop(s) do you use pulses in association and/or rotation with main crop ?
(% of households using pulses)"
ylimn <- -45
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot(d1851dat,val,title,ylimn,ylimx,dig)

# # #3.1.h. d18_61. For which crop(s) do you use cover crops in association and/or rotation with main crop ?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1861dat <- HouseholdVietnam_2C[,c(1,32,1198:1208,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d186 != 0 & !is.na(HouseholdVietnam_2C$d186), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_61. For which crop(s) do you use cover crops 
in association and/or rotation with main crop ?
(% of households using cover crops)"
ylimn <- -20
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot(d1861dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.i. d18_71. For which crop(s) do you use Biochar ?
#No household use biochar

# # #3.1.j. d18_81. For which crop(s) do you use crop residue maintenance?
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d1881dat <- HouseholdVietnam_2C[,c(1,32,1226:1236,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d188 != 0 & !is.na(HouseholdVietnam_2C$d188), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d18_81. For which crop(s) do you use crop residue maintenance?
(% of households using crop residue maintenance)"
ylimn <- -45
ylimx <- 80
dig = 0
#We apply the plotting function
CropPlot(d1881dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.1.k. d18_91. For which crop(s) do you use recycling crop waste?
#No household use recycling crop waste

# # #3.1.l. d18_101. For which crop(s) do you use ramial Wood Chip (RWC) or other wood chips?
#No household use RWC

# # #3.1.m. d18_111. For which crop(s) do you use Organic agro-industrial waste ? 
#No household use waste

# # #3.1.n. d18_99. For which crop(s) do you use Other ?
#No interest in these answers as no translation for now

}

##3.2 - Input reduction
{
# # #3.2.a. d20. Does your household use any ecological/ agroecological/ integrated practices to control weeds in your fields?
#First, we select the useful data
#We create a counting table and a title
d20count <- as.data.frame(svytable(~d20, design=dstrat, exclude=NULL))
d20count <- d20count[c("2","1","3","4"),]
colnames(d20count)[2] <- "n"
d20count$d20 <- as.character(d20count$d20)
d20count[4,1] <- '/'
title <- "d20. Does your household use any ecological/\n agroecological/ integrated practices to\n control weeds in your fields?"
list <- c("yes","no","do not know","NA")
#Now we create the table with the corresponding function
PiePlotYN(d20count,title,list)
#Check

# # #3.2.b. d21. Which of the following ecological/agroecological/integrated practices
#does your household use to control weeds in your fields?
#First, we select the useful data
d21dat <- HouseholdVietnam_2C[,c(1,26,32,1298:1312,2832,2833)]
#We modify long label names
var_label(d21dat$d211) <- "Crop rotation /\n intercropping"
var_label(d21dat$d214) <- "Sowing date /\n rate / depth"
var_label(d21dat$d215) <- "Crop spatial\n arrangement"
var_label(d21dat$d216) <- "Seed cleaning\n before sowing"
var_label(d21dat$d2114) <- "Post harvest weed seed\n destruction in field"
colnames(d21dat)[4:18] <- var_label(d21dat)[4:18]
pond <- round(sum(ifelse(HouseholdVietnam_2C$d20 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "d21. Which of the following ecological/agroecological/integrated practices
does your household use to control weeds in your fields?"
xT <- "Practices"
ylimn <- -20
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(d21dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.2.c. e13. Does your household fertilize the forage or improved pasture?
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+e13, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
dcount$Answer <- c("No","Yes","/")
title <- "e13. Does your household fertilize the forage or improved pasture?
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.2.d. e13_1. if yes, is it with Synthetic fertilizer, Organic manure or Both above ?
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+e13_1, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
dcount <- dcount %>% filter(!is.na(dcount$Answer))
dcount$Answer <- c("1.Synthetic fertilizer","2.Organic manure","3.Both")
title <- "e13_1. if yes, is it with Synthetic fertilizer, Organic manure or Both above ?
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Set3"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.2.e. e29. Does your household use antibiotics on your cattle/buffalo?
#First, we select the useful data
e29dat <- HouseholdVietnam_2C[,c(1,26,32,2033:2036,2832,2833)]
#We modify long label names
var_label(e29dat$e291) <- "For treatment\n diseases only"
var_label(e29dat$e292) <- "For prevention of\n diseases only"
var_label(e29dat$e293) <- "For growth\n promotion"
var_label(e29dat$e290) <- "I don’t use\n antibiotics at all"
colnames(e29dat)[4:7] <- var_label(e29dat)[4:7]
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$e28), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e29. Does your household use antibiotics on your cattle/buffalo?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e29dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#COMMENT: Many households mentionned several answers
#Check

# # #3.2.f. e43. Does your household use antibiotics on your pigs?
#First, we select the useful data
e43dat <- HouseholdVietnam_2C[,c(1,26,32,2094:2097,2832,2833)]
#We modify long label names
var_label(e43dat$e431) <- "For treatment\n diseases only"
var_label(e43dat$e432) <- "For prevention of\n diseases only"
var_label(e43dat$e433) <- "For growth\n promotion"
var_label(e43dat$e430) <- "I don’t use\n antibiotics at all"
colnames(e43dat)[4:7] <- var_label(e43dat)[4:7]
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$e43) & HouseholdVietnam_2C$e43 != '', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e43. Does your household use antibiotics on your pigs?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e43dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#COMMENT: Many households mentionned several answers
#Check

# # #3.2.g. e57. Does your household use antibiotics on your poultry?
#First, we select the useful data
e57dat <- HouseholdVietnam_2C[,c(1,26,32,2156:2159,2832,2833)]
#We modify long label names
var_label(e57dat$e571) <- "For treatment\n diseases only"
var_label(e57dat$e572) <- "For prevention of\n diseases only"
var_label(e57dat$e573) <- "For growth\n promotion"
var_label(e57dat$e570) <- "I don’t use\n antibiotics at all"
colnames(e57dat)[4:7] <- var_label(e57dat)[4:7]
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$e56), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e57. Does your household use antibiotics on your poultry?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e57dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#COMMENT: Many households mentionned several answers
#Check
}
  
##3.3 - Soil health
{
# # #3.3.a. Soil conservation practices Yes or No
#We create a counting table and a title
d140count <- as.data.frame(svytable(~d140, design=dstrat, exclude=NULL))
colnames(d140count)[2] <- "n"
#We order the answers
title <- "d140. Does your household use Soil conservation practices"
d140count$d140 <- as.character(d140count$d140)
d140count[3,1] <- '/'
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(d140count,title,list)
#Check

# # #3.3.b. d14. Which of the following soil conservation practices do you use ?
#First, we select the useful data
d14dat <- HouseholdVietnam_2C[,c(1,26,32,990:997,2832,2833)]
#We modify long label names
var_label(d14dat$d141) <-  "Sowing in \ncontour lines"
var_label(d14dat$d142) <- "Natural or planted\n grass strips"
var_label(d14dat$d143) <- "Trees conservation \nin agricultural plots"
var_label(d14dat$d144) <- "Agroforestry \n(trees + crops)"
var_label(d14dat$d145) <- "Crop residues\n maintained to\n cover the soil"
var_label(d14dat$d147) <- "Reduced to\n no-tillage"
colnames(d14dat)[4:11] <- var_label(d14dat)[4:11]
pond <- round(sum(ifelse(HouseholdVietnam_2C$d140 == '0', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "d14. Which of the following soil conservation practices do you use ?"
xT <- "Practices"
ylimn <- -10
ylimx <- 50
dig = 0
pal ="Dark2"
PracticePlot(d14dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check
}

##3.4 - Animal health
{
# # #3.4.a. e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?
#We create a counting table and a title
e58count <- as.data.frame(svytable(~e58, design=dstrat, exclude=NULL))
colnames(e58count)[2] <- "n"
e58count <- e58count[c("2","1","3"),]
title <- "e58. Can you see the Ribs/ Bones of the ruminants, in the past 1 year?"
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(e58count,title,list)
#Check

# # #3.4.b. e58_1. If yes to e58, which months?
#First, we select the useful data
e58_1dat <- HouseholdVietnam_2C[,c(1,26,32,2167:2178,2832,2833)]
#We prepare the parameters for the function
colnames(e58_1dat)[4:15] <- var_label(e58_1dat)[4:15]
pond <- round(sum(ifelse(HouseholdVietnam_2C$e58 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e58_1. If yes to e58, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
pal = "Dark2"
Monthplot(e58_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.4.c e59. Is there any month in the year when there is a lack of feed for the animals?
#We create a counting table and a title
e59count <- as.data.frame(svytable(~e59, design=dstrat, exclude=NULL))
colnames(e59count)[2] <- "n"
e59count <- e59count[c("2","1","3"),]
title <- "e59. Is there any month in the year when 
there is a lack of feed for the animals?"
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(e59count,title,list)
#Check

# # #3.4.d. e59_1. If yes to e59, which months?
#First, we select the useful data
e59_1dat <- HouseholdVietnam_2C[,c(1,26,32,2181:2192,2832,2833)]
#We prepare the parameters for the function
colnames(e59_1dat)[4:15] <- var_label(e59_1dat)[4:15]
pond <- round(sum(ifelse(HouseholdVietnam_2C$e59 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e59_1. If yes to e59, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
pal = "Dark2"
Monthplot(e59_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.4.f. e60. Do the animals have access to the water?
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+e60, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
dcount$Answer <- c("No","Yes","/")
title <- "e60. Do the animals have access to the water?
- /Province"
pond <-sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.4.g. e26. Does your household have problems with Cattle/Buffalo parasites?
#We create a counting table and a title
dcount1 <- as.data.frame(svytable(~district_eng+e26, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),] 
dcount$Answer <- c("No","Yes","/")
title <- "e26. Does your household have problems with Cattle/Buffalo parasites?
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.4.h. e27. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e27dat <- HouseholdVietnam_2C[,c(1,26,32,2024:2026,2832,2833)]
colnames(e27dat)[4:6] <- var_label(e27dat)[4:6]
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$e26_1), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e27. How does your household deal with Cattle/Buffalo parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e27dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.4.i. e40. Does your household have problems with Pigs parasites?
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+e40, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),] 
dcount$Answer <- c("NA","No","Yes","/")
title <- "e40. Does your household have problems with pigs parasites?
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#COMMENT: MISSING ANSWERS FROM PEOPLE OWNING PIGS
#Check

# # #3.4.j. e41. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e41dat <- HouseholdVietnam_2C[,c(1,26,32,2085:2087,2832,2833)]
colnames(e41dat)[4:6] <- var_label(e41dat)[4:6]
pond <- round(sum(ifelse(HouseholdVietnam_2C$e40 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e41. How does your household deal with pigs parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e41dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.4.k. e54. Does your household have problems with Poultry parasites?
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+e54, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),] 
dcount$Answer <- c("No","Yes","/")
title <- "e54. Does your household have problems with poultry parasites?
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#COMMENT: MISSING ANSWERS FROM PEOPLE OWNING POULTRY
#Check

# # #3.4.l. e55. How does your household deal with it? Nothing/ Traditional treatment/
#Chemicals (define)
#First, we select the useful data
e55dat <- HouseholdVietnam_2C[,c(1,26,32,2146:2148,2832,2833)]
colnames(e55dat)[4:6] <- var_label(e55dat)[4:6]
pond <- round(sum(ifelse(HouseholdVietnam_2C$e54 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "e55. How does your household deal with pigs parasites?"
xT <- "Answer"
ylimn <- -15
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(e55dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check
}

##3.5 - Biodiversity
{
# # #3.5.a d32. Do you conserve and use traditional/local seeds?
#We create a counting table and a title
d32count <- as.data.frame(svytable(~d32, design=dstrat, exclude=NULL))
colnames(d32count)[2] <- "n"
title <- "d32. Do you conserve and use traditional/local seeds?"
d32count <- d32count[c("2","1","3"),]
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(d32count,title,list)
#Check

# # #3.5.b. d32_1. If Yes to d32, for which crops? 
#We prepare the data we want to analyze by replacing yes (&) values by crop name
d32_1dat <- HouseholdVietnam_2C[,c(1,32,1786:1796,2832,2833)]
val <- round(sum(ifelse(HouseholdVietnam_2C$d32 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
title <- "d32_1. If Yes to d32, for which crops? "
ylimn <- -15
ylimx <- 60
dig = 0
#We apply the plotting function
CropPlot(d32_1dat,val,title,ylimn,ylimx,dig)
#Check

# # #3.5.c. e5_b. Do you have any local breeds of  e4_1 at the time of the survey?
#We create a counting table and a title
e5_bcount <- as.data.frame(svytable(~e5_b, design=dstrat, exclude=NULL))
colnames(e5_bcount)[2] <- "n"
title <- "e5_b. Do you have any local breeds of most important animal
at the time of the survey?"
e5_bcount <- e5_bcount[c("2","1","3"),]
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(e5_bcount,title,list)
#Check

# # #3.5.d. e5_b_1. Animal diversity for local breeds (most important animal)
#First, we select the useful data
#We create a counting table and a title
e5_b_1count <- HouseholdVietnam_2C[,c(32,1905,1911,2832,2833)]
title <- "e5_b_1. Animal diversity for local breeds (most important animal)"
pond <- round(sum(ifelse(HouseholdVietnam_2C$e5_b == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
#Now we create the table with the corresponding function
AnimalPlot(e5_b_1count,title,pond)
#Check

# # #3.5.e. Number of crops grown during the last 12 months
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+TotCrop, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
title <- "Number of crops grown during the last 12 months
- /Province"
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check
}

##3.6 - Synergy
{

# # #3.6.a. d26. Does your household use agroecological/integrated practices to control pests and disease in your fields? 
#We create a counting table and a title
d26count <- as.data.frame(svytable(~d26, design=dstrat, exclude=NULL))
colnames(d26count)[2] <- "n"
title <- "d26. Does your household use agroecological/integrated practices
to control pests and disease in your fields? "
d26count <- d26count[c("2","1","3","4"),]
list <- c("yes","no","do not know","/")
#Now we create the table with the corresponding function
PiePlotYN(d26count,title,list)
#Check

# # #3.6.b. d27. Which ecological/agroecological/integrated practices do you
#use to control pets and diseases in your fields?
#First, we select the useful data
d27dat <- HouseholdVietnam_2C[,c(1,26,32,1517:1531,2832,2833)]
#We modify long label names
var_label(d27dat$d271) <- "Crop rotation /\n intercropping"
var_label(d27dat$d274) <- "Soil health \nmaintenance/improvement"
var_label(d27dat$d275) <- "Sanitation practices"
var_label(d27dat$d277) <- "Water and nutrient\n management"
var_label(d27dat$d278) <- "Cultivar choice (tolerant/\nresistant)/cultivar mixture"
var_label(d27dat$d279) <- "Biopesticide /\n organic pesticide"
var_label(d27dat$d2710) <- "BCAs"
var_label(d27dat$d2711) <- "Home-made efficient\n microorganism (EM)"
var_label(d27dat$d2712) <- "Commercial efficient\n microorganism (EM)"
colnames(d27dat)[4:18] <- var_label(d27dat)[4:18]
pond <- round(sum(ifelse(HouseholdVietnam_2C$d26 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "d27 - Which ecological/agroecological/integrated practices do you
use to control pets and diseases in your fields?"
xT <- "Practices"
ylimn <- -30
ylimx <- 100
dig = 0
pal = "Dark2"
#Function call
PracticePlot(d27dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check
}

##3.7 - Economic diversification
{
# # #3.7.a. b1. Does your household sell agricultural products 
#(crops, vegetables, fruits or processed products) and/or livestock? 
#We create a counting table and a title
dcount <- as.data.frame(svytable(~district_eng+b1, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
title <- "b1. Does your household sell agricultural products 
(crops, vegetables, fruits or processed products) and/or livestock?
- /Province"
dcount$Answer <- c("0.No selling\n agri-products","1.Selling own\n crops/fruits/\n vegetable",
                     "2.Selling own\n livestock products","3.Selling both own\n crops/fruits/vegetables\n and livestock products")
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProvYN(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.7.b. b2. Does your household sell derived/processed products and which?
#First we select useful data
dcount <- as.data.frame(svytable(~district_eng+b2, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
title <- "b2. Does your household sell derived/processed products and which?
- /Province"
dcount$Answer <- c("0.No processed/derived agri-products","1.Selling processed products from crops","3.Selling both above processed/derived agri-products")
pond <- nrow(HouseholdVietnam_2C)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check
  
# # #3.7.c. Diversity of activities
#First we select useful data
dcount <- as.data.frame(svytable(~district_eng+SoldProdTot, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
dcount$Answer <- as.factor(dcount$Answer)
title <- "Number of crop or animal products sold by the household
(Including only the 3 first animal products)
- /Province"
pond <- nrow(HouseholdVietnam_2C)
ylimn <- 0
dig = 0
pal = "Greens"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.7.d. b9.Are there specific months of the year in which you face financial difficulties?
#We create a counting table and a title
b9count <- as.data.frame(svytable(~b9, design=dstrat, exclude=NULL))
b9count$b9 <- as.character(b9count$b9)
colnames(b9count)[2] <- "n"
title <- "b9. Are there specific months of the year in which
you face financial difficulties?"
b9count <- b9count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(b9count,title,list)
#Check

# # #3.7.e. b9_1. If yes to b9, which months?
#First, we select the useful data
b9_1dat <- HouseholdVietnam_2C[,c(1,26,32,182:193,2832,2833)]
#We prepare the parameters for the function
colnames(b9_1dat)[4:15] <- var_label(b9_1dat)[4:15]
pond <- round(sum(ifelse(HouseholdVietnam_2C$b9 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "b9_1. If yes to b9, which months?"
xT <- "Months"
ylimn <- 0
ylimx <- 100
dig = 0
pal = "Dark2"
Monthplot(b9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check
}

##3.8 - Co-creation of knowledge
{

# # #3.8.a. c14. Do you exchange your agricultural products,
#equipment or animals with other farmers? 
#We create a counting table and a title
c14count <- as.data.frame(svytable(~c14, design=dstrat, exclude=NULL))
c14count$c14 <- as.character(c14count$c14)
colnames(c14count)[2] <- "n"
title <- "c14. Do you exchange your agricultural products,
equipment or animals with other farmers? "
c14count <- c14count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c14count,title,list)
#Check

# # #3.8.b. f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?
#First we select useful data
f4count <- as.data.frame(svytable(~f4, design=dstrat, exclude=NULL))
colnames(f4count)[1] <- "Answer"
colnames(f4count)[2] <- "n"
title <- "f4. Do you have sufficient time to acquire new knowledge
#and improve your skills?"
list <- c("1.No time","2.Very little time","3.Moderate amount of time","4.Almost enough time",
  "5.Sufficient amount of time", "6.Do not know")
pond <- sum(HouseholdVietnam_2C$Weight)
xT = "Answer"
ylimn <- -30
#Now we create the table with the corresponding function
ListBPlotNO(f4count,title,list,pond,xT,ylimn)
#Check
}

##3.9 - Social value and diet
{
# # #3.9.a. i1. In general, what is the proportion of the food (rice, vegetable, animal
#products, etc.) consumed by your family that comes from your own farm or homegarden?
#First, we select the useful data
i1count <- as.data.frame(svytable(~i1, design=dstrat, exclude=NULL))
colnames(i1count)[1] <- "Answer"
colnames(i1count)[2] <- "n"
title <- "i1. Proportion of the food that comes from the household farm"
list <- c("1.Less than 25%","2.25-50%","3.50-75%","4.Over 75%")
pond <- sum(HouseholdVietnam_2C$Weight)
#Now we create the table with the corresponding function
xT = "Answer"
ylimn <- -10
ListBPlotNO(i1count,title,list,pond,xT,ylimn)
#Check

# # #3.9.b g5. Do you think the working hours (including household chores and taking 
#care of family members) across family members are equitably distributed?
#We create a counting table and a title
g5count <- as.data.frame(svytable(~g5, design=dstrat, exclude=NULL))
g5count$g5 <- as.character(g5count$g5)
colnames(g5count)[2] <- "n"
g5count <- g5count[c("2","1"),]
list <- c("yes","no")
title <- "g5. Do you think the working hours across family
members are equitably distributed?"
#Now we create the table with the corresponding function
PiePlotYN(g5count,title,list)
#Check

# # #3.9.c. i2. Were there months, in the past 12 months, in which you did 
#not have enough food to meet your family’s needs?
#We create a counting table and a title
i2count <- as.data.frame(svytable(~i2, design=dstrat, exclude=NULL))
i2count$i2 <- as.character(i2count$i2)
colnames(i2count)[2] <- "n"
title <- "i2. Were there months, in the past 12 months, in which you did 
not have enough food to meet your family’s needs?"
i2count <- i2count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(i2count,title,list)
#Check

# # #3.9.d. i3. If yes, which were the months in the past 12 months during 
#which you did not have enough food to meet your family’s needs? 
#First, we select the useful data
i3dat <- HouseholdVietnam_2C[,c(1,26,32,2225:2236,2832,2833)]
#We prepare the parameters for the function
colnames(b9_1dat)[4:15] <- var_label(b9_1dat)[4:15]
pond <- round(sum(ifelse(HouseholdVietnam_2C$i2 == '1', 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "i3. If yes, which were the months in the past 12 months during 
#which you did not have enough food to meet your family’s needs?"
xT <- "Months"
ylimn <- 0
ylimx <- 60
dig = 0
pal = "Dark2"
Monthplot(i3dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.9.e. g1. Who makes the decision on what (e.g. crops, animals) and how to 
#produce (e.g., with or without herbicide, tillage or no tillage, compost or chemical fertiliser)?
#We create a counting table and a title
g1count <- as.data.frame(svytable(~g1, design=dstrat, exclude=NULL))
g1count$g1 <- as.character(g1count$g1)
colnames(g1count)[2] <- "n"
title <- "g1.  Who makes the decision on what (e.g. crops, animals) 
and how to produce ?"
list <- c("Myself alone","Me in consultation with\n spouse/other family members",
          "My spouse/other\n family members")
pond <- sum(HouseholdVietnam_2C$Weight)
#Now we create the table with the corresponding function
PiePlot(g1count,title,list)
#Check

# # #3.9.f. g2. Who makes the decision on purchasing, selling, or transferring major 
#household assets (land, cattle, equipment)?
#We create a counting table and a title
g2count <- as.data.frame(svytable(~g2, design=dstrat, exclude=NULL))
g2count$g2 <- as.character(g2count$g2)
colnames(g2count)[2] <- "n"
title <- "g2.  Who makes the decision on purchasing, selling,
or transferring major household assets ?"
list <- c("Myself alone","Me in consultation with\n spouse/other family members",
          "My spouse/other\n family members", "DNK")
pond <- sum(HouseholdVietnam_2C$Weight)
#Now we create the table with the corresponding function
PiePlot(g2count,title,list)
#Check

# # #3.9.g. g3. Who makes the decision on borrowing or lending money?
#We create a counting table and a title
g3count <- as.data.frame(svytable(~g3, design=dstrat, exclude=NULL))
g3count$g3 <- as.character(g3count$g3)
colnames(g3count)[2] <- "n"
title <- "g3. Who makes the decision on borrowing or lending money?"
list <- c("Myself alone","Me in consultation with\n spouse/other family members",
          "My spouse/other\n family members", "DNK")
pond <- sum(HouseholdVietnam_2C$Weight)
#Now we create the table with the corresponding function
PiePlot(g3count,title,list)
#Check

# # #3.9.h. g4. Who makes the decision about how the household income is used?
#We create a counting table and a title
g4count <- as.data.frame(svytable(~g4, design=dstrat, exclude=NULL))
g4count$g4 <- as.character(g4count$g4)
colnames(g4count)[2] <- "n"
title <- "g4. Who makes the decision about how the household income is used?"
list <- c("Myself alone","Me in consultation with\n spouse/other family members",
          "My spouse/other\n family members", "DNK")
pond <- sum(HouseholdVietnam_2C$Weight)
#Now we create the table with the corresponding function
PiePlot(g4count,title,list)
#Check

}

##3.10 - Fairness
{
# # #3.10.a. b22. Did you sell any certified crop/vegetables/fruit/livestock
#related product in the past year/12 months?
#We create a counting table and a title
b22count <- as.data.frame(svytable(~b22, design=dstrat, exclude=NULL))
b22count$b22 <- as.character(b22count$b22)
colnames(b22count)[2] <- "n"
b22count <- b22count[c("2","1"),]
list <- c("yes","no")
title <- "b22. Did you sell any certified crop/vegetables/fruit/livestock
related product in the past year/12 months?"
#Now we create the table with the corresponding function
PiePlotYN(b22count,title,list)
#Check

# # #3.10.b. b22_1. If yes, which one(s)? 
#First, we select the useful data
b22_1dat <- HouseholdVietnam_2C[,c(1,26,32,245:259,2832,2833)]
#We modify long label names
var_label(b22_1dat$b22_113) <- "Dried meat (pork,\n beef, etc.)"
#We prepare the parameters for the function
colnames(b22_1dat)[4:18] <- var_label(b22_1dat)[4:18]
#WARNING
pond <- 10.4
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "b22_1. If yes, which one(s)?"
xT <- "Certified crops"
ylimn <- -20
ylimx <- 110
dig = 0
pal = "Dark2"
#Function Call
PracticePlot(b22_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.10.c. c8. Did you sign a contract whereby the buyer commits to buy 
#from you following some at specific conditions (price, volume, quality, time...)?
#First, we select the useful data
c8count <- as.data.frame(svytable(~c8, design=dstrat, exclude=NULL))
colnames(c8count)[1] <- "Answer"
colnames(c8count)[2] <- "n"
title <- "c8. Did you sign a contract whereby the buyer commits to buy 
from you following some at specific conditions (price, volume, quality, time...)?"
list <- c("no","/")
pond <- sum(HouseholdVietnam_2C$Weight)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(c8count,title,list,pond,xT,ylimn)
#Check

# # #3.10.d. b14_1. Does “buyer” provide any of the following
#First, we select the useful data
b14_1dat <- HouseholdVietnam_2C[,c(1,26,32,717:725,2832,2833)]
#We prepare the parameters for the function
colnames(b14_1dat)[4:12] <- var_label(b14_1dat)[4:12]
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$b12_1), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "b14_1. Does “buyer” provide any of the following ?"
xT <- "Answer"
ylimn <- -20
ylimx <- 100
dig = 0
pal = "Dark2"
#Function Call
PracticePlot(b14_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.10.e. b14_2. Does “buyer” provide any of the following
#First, we select the useful data
b14_2dat <- HouseholdVietnam_2C[,c(1,26,32,742:750,2832,2833)]
#We prepare the parameters for the function
pond <- round(sum(ifelse(!is.na(HouseholdVietnam_2C$b12_2), 1,0)*HouseholdVietnam_2C$Weight), digits=0)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "b14_2. Does “buyer” provide any of the following ?"
xT <- "Answer"
ylimn <- -20
ylimx <- 100
dig = 0
pal = "Dark2"
#Function Call
PracticePlot(b14_2dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.10.f. l7. In general, does your household have difficulties paying
#back your loans?
#We create a counting table and a title
l7count <- as.data.frame(svytable(~l7, design=dstrat, exclude=NULL))
l7count$l7 <- as.character(l7count$l7)
colnames(l7count)[2] <- "n"
title <- "l7. In general, does your household have difficulties paying
back your loans ?"
l7count <- l7count[c("2","1","3"),]
list <- c("yes","no","/")
#Now we create the table with the corresponding function
PiePlotYN(l7count,title,list)
#Check

# # #3.10.g. h1. Would you like your children to be farmers too?
#First, we select the useful data
h1count <- as.data.frame(svytable(~h1, design=dstrat, exclude=NULL))
colnames(h1count)[1] <- "Answer"
colnames(h1count)[2] <- "n"
title <- "h1. Would you like your children to be farmers too?"
list <- c("1. Yes, strongly","2. Yes, maybe","3. They should emigrate\n if they had the chance",
          "4. No, agriculture is\n not a good job","5. Do not know")
pond <- sum(HouseholdVietnam_2C$Weight)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(h1count,title,list,pond,xT,ylimn)
#Check
}

##3.11 - Connectivity
{
# # #3.11.a. c13. Do you collaborate with other people for any task ?
#First, we select the useful data
c13dat <- HouseholdVietnam_2C[,c(1,26,32,647:653,655,2832,2833)]
#We modify long label names
var_label(c13dat$c131) <- "Share labor (mutual\n help, working together\n on each other farm)"
var_label(c13dat$c132) <- "Manage water/\nirrigation systems"
var_label(c13dat$c134) <- "Buy agricultural\n inputs"
var_label(c13dat$c135) <- "Selling products\n to the markets for\n other farmers"
var_label(c13dat$c136) <- "Experiment new\n farming practices"
var_label(c13dat$c130) <-"No collaboration\n with other people\n on these issues"
#We prepare the parameters for the function
colnames(c13dat)[4:11] <- var_label(c13dat)[4:11]
pond <- sum(HouseholdVietnam_2C$Weight)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "c13. Do you collaborate with other people for any task ?"
xT <- "Months"
ylimn <- -20
ylimx <- 100
dig = 0
pal = "Dark2"
#Function Call
PracticePlot(c13dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.11.b c15. Are you involved in some form of advocacy work (aiming 
#to influence decision-making within political institutions)?
#We create a counting table and a title
c15count <- as.data.frame(svytable(~c15, design=dstrat, exclude=NULL))
c15count$c15 <- as.character(c15count$c15)
colnames(c15count)[2] <- "n"
title <- "c15. Are you involved in some form of advocacy work (aiming 
to influence decision-making within political institutions)?"
c15count <- c15count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(c15count,title,list)
#Check

# # #3.11.c f3.Do you have enough time for your family and your social relationships?
#First, we select the useful data
f3count <- as.data.frame(svytable(~f3, design=dstrat, exclude=NULL))
colnames(f3count)[1] <- "Answer"
colnames(f3count)[2] <- "n"
title <- "f3.Do you have enough time for your family and your social relationships?"
list <- c("1. No time","2. Very little time","3. Moderate amount of time",
          "4. Almost enough time","5. Sufficient amount of time")
pond <- sum(HouseholdVietnam_2C$Weight)
xT = "Answer"
ylimn <- -30
#Now we create the table with the corresponding function
ListBPlotNO(f3count,title,list,pond,xT,ylimn)
#Check

# # #3.11.d b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
#First we select useful data
dcount <- as.data.frame(svytable(~district_eng+b10, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[c(3:14),]
dcount <- dcount[order(dcount$district_eng,dcount$Answer),]
title <- "b10. If you sell crops/vegetables/fruits or processed products,
#do you know what is their main final destination?
- /Province"
dcount$Answer <- c("a.The local market \n(lower or equal to district level market)",
                    "b.The provincial or national market","c.Export","d.Other",
                    "e.Do not know","f./")
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Dark2"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check

# # #3.11.d b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
#First we select useful data
dcount <- as.data.frame(svytable(~district_eng+b16, design=dstrat, exclude=NULL))
colnames(dcount)[2] <- "Answer"
colnames(dcount)[3] <- "n"
dcount <- dcount[c(3:12),]
title <- "b16. If you sell livestock or livestock derived products, 
#do you know what is their main final destination?
- /Province"
dcount$Answer <- c("a.The local market (lower or equal to district level market)",
                   "b.The provincial or national market","d.Other",
                   "e.Do not know","f./")
pond <- sum(HouseholdVietnam_2C$Weight)
ylimn <- 0
dig = 0
pal = "Dark2"
ListBPlotProv(dcount,title,pond,ylimn,dig,pal)
#Check
}

##3.12 - Land and natural resource governance
{

# # #3.12.a. l1. In the past year/12 months, did your household benefit from government
#subsidies to support investment in production or commercialization activities?
#We create a counting table and a title
l1count <- as.data.frame(svytable(~l1, design=dstrat, exclude=NULL))
l1count$l1 <- as.character(l1count$l1)
colnames(l1count)[2] <- "n"
title <- "l1. In the past year/12 months, did your household
benefit from government subsidies to support investment
in production or commercialization activities?"
l1count <- l1count[c("2","1"),]
list <- c("yes","no")
#Now we create the table with the corresponding function
PiePlotYN(l1count,title,list)
#Check

# # #3.12.b. l2. How much of your total household income in 2022 did these 
#government subsidies represent?
#We create a counting table and a title
l2count <- as.data.frame(svytable(~l2, design=dstrat, exclude=NULL))
colnames(l2count)[1] <- "Answer"
colnames(l2count)[2] <- "n"
l2count <- l2count %>%  filter(!is.na(Answer))
title <- "l2. How much of your total household income in 2022 did these 
government subsidies represent?"
list <- c("1.Less than 25%","2.25-50%","3.50-75%","4.Do not know")
pond <- round(sum(l2count$n),digits = 0)
xT = "Answer"
ylimn <- -20
#Now we create the table with the corresponding function
ListBPlotNO(l2count,title,list,pond,xT,ylimn)
#Check

# # #3.12.c. d9-10-11. Land ownership
#First, we select the useful data
d9_1dat <- HouseholdVietnam_2C[,c(1,26,32,847,849,851,2832,2833)]
#We modify long label names
var_label(d9_1dat$d9_1) <- "Having plots\n rented-in?"
var_label(d9_1dat$d10_1) <- "Having plots\n rented-out?"
var_label(d9_1dat$d11_1) <- "Having plots\n owned?"
#We prepare the parameters for the function
colnames(c13dat)[4:6] <- var_label(c13dat)[4:6]
pond <- sum(HouseholdVietnam_2C$Weight)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "d9-10-11. Land ownership"
xT <- ""
ylimn <- -20
ylimx <- 110
dig = 1
pal = "Dark2"
#Function call
PracticePlot(d9_1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
#Check

# # #3.12.c. e12. Does your household have private land to feed the animals?
#We create a counting table and a title
e12count <- as.data.frame(svytable(~OwnPast, design=dstrat, exclude=NULL))
e12count$OwnPast <- as.character(e12count$OwnPast)
colnames(e12count)[2] <- "n"
title <- "e12. Does your household have private land to feed the animals?"
e12count <- e12count[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(e12count,title,list)
#Check
}

##3.13 - Participation
{
# # #3.13.a. c2. Are you a member of one or more farmer organization (e.g. crops/fruits/
#livestock/honey/ water/Forest etc)?
c2count <- as.data.frame(svytable(~c2, design=dstrat, exclude=NULL))
colnames(c2count)[1] <- "Answer"
colnames(c2count)[2] <- "n"
title <- "Are you a member of one or more farmer organization 
(e.g. crops/fruits/livestock/honey/ water/Forest etc)?"
list <- c("1.No","2.Yes, one","3.Yes, more than one")
pond <- sum(HouseholdVietnam_2C$Weight)
xT = "Answer"
ylimn <- -25
#Now we create the table with the corresponding function
ListBPlotNO(c2count,title,list,pond,xT,ylimn)

# # #3.13.b. c1. Are you or anyone in your household active in one or several
#Unions? YN
c1_0count <- as.data.frame(svytable(~OwnPast, design=dstrat, exclude=NULL))
c1_0count$OwnPast <- as.character(c1_0count$OwnPast)
colnames(c1_0count)[2] <- "n"
title <- "c1. Are you or anyone in your household active in one or several
Unions? YN"
c1_0count <- c1_0count[c("2","1","3"),]
list <- c("yes","no","NA")
#Now we create the table with the corresponding function
PiePlotYN(c1_0count,title,list)

# # #3.13.c. c1. Are you or anyone in your household active in one or several
#of the following?
#First, we select the useful data
c1dat <- HouseholdVietnam_2C[,c(1,26,32,581:588,2832,2833)]
#We modify long label names
var_label(c1dat$c1_1) <- "c1_1. Women\n union"
var_label(c1dat$c1_2) <- "c1_2. Youth\n union"
var_label(c1dat$c1_3) <- "c1_3. Veteran\n union"
var_label(c1dat$c1_4) <- "c1_4. Farmer\n union"
var_label(c1dat$c1_5) <- "c1_5. Elderly\n union"
var_label(c1dat$c1_6) <- "c1_6. Political\n party"
#We prepare the parameters for the function
colnames(c1dat)[4:11] <- var_label(c1dat)[4:11]
pond <- sum(HouseholdVietnam_2C$Weight)
pond2 <- sum(HouseholdVietnam_2C$Weight)
title <- "c1. Are you or anyone in your household active in one or several
of the following?"
xT <- "Months"
ylimn <- -20
ylimx <- 85
dig = 0
pal = "Dark2"
#Function Call
PracticePlot(c1dat,title,pond,pond2,xT,ylimn,ylimx,dig,pal)
}


  