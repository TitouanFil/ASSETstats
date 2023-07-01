### 1. Packages loading and data import
#Packages loading
library(haven)
library(data.table)
library(expss)
library(dplyr)
library(labelled)
library(foreign)
library(sjlabelled)
library(stringr)
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Laos* database
HouseholdLaos <- read_dta("asset_household_survey_in_Laos.dta")
ClowlandLaos <- read_dta("d2_1_lowland_Laos.dta")
CuplandLaos <- read_dta("d2_2_upland_Laos.dta")
HouMemberLaos <- read_dta("household_roster_laos.dta")

HouseholdLaos2 <- read.csv("HouseholdLaos2.csv")
ClowlandLaos2 <- read.csv("ClowlandLaos2.csv")
CuplandLaos2 <- read.csv("CuplandLaos2.csv")
HouMemberLaos2 <- read.csv("HouMemberLaos2.csv")

#We check for columns correspondences and re-adjust when it is necesary
#First for Household Laos database
HouseholdLaos <- HouseholdLaos[,-c(2320:2325)]
HouseholdLaos2 <- HouseholdLaos2[,-632]
HouseholdLaos2 <- HouseholdLaos2 %>% relocate(c9_oth..Specify.other.benefits.of...strong...font.color..blue....c3a_text...font....strong. , .after = c9..What.are.the.benefits.of.belonging.to.this..strong...font.color..blue....c3a_text...font....strong...Other)
#For Household members database
HouMemberLaos2 <- HouMemberLaos2[,-1]
#For Lowland crops database
ClowlandLaos2 <- ClowlandLaos2[,-1]
#For upland crops database
CuplandLaos2 <- CuplandLaos2[,-1]

#List creation to check if columns are similar or not
x <- rbind(var_label(HouseholdLaos),colnames(HouseholdLaos2)[1:2320])
y <- rbind(var_label(HouMemberLaos),colnames(HouMemberLaos2)[1:70])
z <- rbind(var_label(ClowlandLaos),colnames(ClowlandLaos2)[1:28])
a <- rbind(var_label(CuplandLaos),colnames(CuplandLaos2)[1:27])

#We re-attribute names of databases
#First for Household Laos database
colnames(HouseholdLaos2) <- colnames(HouseholdLaos)
HouseholdLaos2 <- copy_labels(HouseholdLaos2, HouseholdLaos)
HouseholdLaos <- HouseholdLaos2
#For Household members database
colnames(HouMemberLaos2) <- colnames(HouMemberLaos)
HouMemberLaos2 <- copy_labels(HouMemberLaos2, HouMemberLaos)
HouMemberLaos <- HouMemberLaos2
#For Lowland crops database
colnames(ClowlandLaos2) <- colnames(ClowlandLaos)
ClowlandLaos2 <- copy_labels(ClowlandLaos2, ClowlandLaos)
ClowlandLaos <- ClowlandLaos2
#For upland crops database
colnames(CuplandLaos2) <- colnames(CuplandLaos)
CuplandLaos2 <- copy_labels(CuplandLaos2, CuplandLaos)
CuplandLaos <- CuplandLaos2


### 2. Columns and sub-columns name changing
##2.1. Work on the first database: "HouseholdLaos" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

# # #o.
#C1 = FACT
#C2 = FACT
#C3 = FACT
#C4 = ? (Probably Kobo ID, maybe necessary to replace it later)
#C5 = REMOVE (Text from the questionnaire)
#C6 = REMOVE (Text from the questionnaire)
#C7 = FACT, Change the label: "Consent: 1 = yes, 2 = no"
var_label(HouseholdLaos$consent) <- "Consent: 1 = yes, 2 = no"
#C8 = FACT 
#C9 = FACT, (Enumerators names = number, maybe creating a correspondence table)
#C10 = OK, (Empty, decide if necessary to remove it or not)
#C11 = FACT, (Supervisors names = number, maybe creating a correspondence table)
#C12 = OK, (Empty, decide if necessary to remove it or not)
#d13 = FACT, (Each country = number, Laos = 3)
#C14 = FACT, (Each province = number starting by the country number, only one province in Laos)
#C15 = FACT, (Each district = number starting by the country and province number)
#C16 = FACT, (Each village = number starting by the country, province and district number)
#C17 = FACT, check duplicates, (starting by country id) = 23 duplicates... 
#Normally it is one of the issue for which Ky should find a solution
count_if("TRUE",duplicated(HouseholdLaos[,17]))
#C18 = FACT, (same than previous)
#C19 = FACT
#C20 = FACT, (Same than previous)
#C21 = FACT, (Same than "13")
#C22 = FACT
#C23 = FACT
#C24 = FACT, (Same than "14")
#C25 = FACT
#C26 = FACT
#C27 = FACT, (Same than "15")
#C28 = FACT
#C29 = FACT
#C30 = FACT, (Same than "16")
#C31 = OK, change label "header's name (lao)"
var_label(HouseholdLaos$header_name_preload) <- "header's name (lao)"
#C32 = NUMERIC, Age instead of solar year, change label names (see if necessary to change also column name)
var_label(HouseholdLaos$year_birth_preload) <- "header's age (years)"
#C33 = CHAR
#C34 = FACT, (Should replace "20")
#C35 = FACT, (Same than "23")
#C36 = FACT, (Same than "26")
#C37 = FACT, (Same than "29")
#C38 = REMOVE (text from the questionnaire)
#C39 = REMOVE (text from the questionnaire)
#C40 = REMOVE (text from the questionnaire)
# # # Good order from this point, re-order previous columns
#C41 = REMOVE (Not really o8)
#C42 = FACT, (Answer = number of HH members included in the interview ??)
#C43 = FACT, (Enumerators names = number, maybe creating a correspondence table)
#C44 = FACT
#C45 = FACT, (1 = yes, 0 = no ??)
#C46 = FACT, (1 =	Less than 1 year, 2 =	1-5 year, 3 =	5-10 years, 4 = More than 10 years)
#C47 = FACT, (1 =	Other commune within district, 2 = Another district within province, 3 = Another province)
#C48 = FACT (the answer is still in Lao, Ky will maybe solve it)
#C49 = CHAR
#C50 = CHAR
#C51 = FACT, (We don't have the correspondence for this question in the questionaire...)
#C52 = REMOVE (Text from the questionnaire)

# # #a.
#C53 = OK
#C54 = CHAR, NI
#C55 = CHAR, NI
#C56 = CHAR, NI
#C57 = CHAR, NI
#C58 = CHAR, NI
#C59 = CHAR, NI
#C60 = CHAR, NI
#C61 = NUMERIC
#C62 = FACT, (table of correspondence), (see answers below)
#Death or health problem of one household member 
#Harvest failure
#Animal loss
#Unsold harvest or animals
#Other
#Do not know
#C63 = FACT, replace label: "Death or health problem of one household member"
var_label(HouseholdLaos$a151) <- "Death or health problem of one household member"
#C64 = FACT, replace label: "Harvest failure"
var_label(HouseholdLaos$a152) <- "Harvest failure"
#C65 = FACT, replace label: "Animal loss"
var_label(HouseholdLaos$a153) <- "Animal loss"
#C66 = FACT, replace label: "Unsold harvest or animals"
var_label(HouseholdLaos$a154) <- "Unsold harvest or animals"
#C67 = FACT, replace label: "Other" 
var_label(HouseholdLaos$a1599) <- "Other"
#C68 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$a1588) <- "Do not know"
#C69 = OK, (the answer is still in Lao, Ky will maybe solve it)

# # #b.
#C70 = FACT, (table of correspondence), (see answers below)
#No selling agri-products
#Selling own crops/fruits/ vegetable
#Selling own livestock products
#Selling both own crops/fruits/vegetables and livestock products
#C71 = FACT, (table of correspondence), (see answers below)
#No processed/derived agri-products
#Selling processed products from crops
#Selling processed products from livestocks
#Selling both above processed/derived agri-products
#C72 = FACT, (table of correspondence), (see answers below)
#Dried/ fermented vegetables (carrot, mustard…)
#Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)
#Dried/ fermented bambooshoot
#Selling other products
#C73 = FACT, replace label: "Dried/ fermented vegetables (carrot, mustard…)"
var_label(HouseholdLaos$b2_11) <- "Dried/ fermented vegetables (carrot, mustard…)"
#C74 = FACT, replace label: "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
var_label(HouseholdLaos$b2_12) <- "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
#C75 = FACT, replace label: "Dried/ fermented bambooshoot"
var_label(HouseholdLaos$b2_13) <- "Dried/ fermented bambooshoot"
#C76 = FACT, replace label: "Selling other products"
var_label(HouseholdLaos$b2_199) <- "Selling other products"
#C77 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C78 = FACT, (table of correspondence), (see answers below)
#Dried/fermented meat (pork, beef, etc.)
#Honey
#Cheese
#Selling other products
#C79 = FACT, replace label: "Dried/fermented meat (pork, beef, etc.)"
var_label(HouseholdLaos$b2_21) <- "Dried/fermented meat (pork, beef, etc.)"
#C80 = FACT, replace label: "Honey"
var_label(HouseholdLaos$b2_22) <- "Honey"
#C81 = FACT, replace label: "Cheese"
var_label(HouseholdLaos$b2_23) <- "Cheese"
#C82 = FACT, replace label: "Selling other products"
var_label(HouseholdLaos$b2_299) <- "Selling other products"
#C83 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C84 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C85 = FACT, replace label: "Selling own vegetables"
var_label(HouseholdLaos$b31) <- "Selling own vegetables"
#C86 = FACT, replace label: "Selling own fruits"
var_label(HouseholdLaos$b32) <- "Selling own fruits"
#C87 = FACT, replace label: "Selling rice from the farm"
var_label(HouseholdLaos$b33) <- "Selling rice from the farm"
#C88 = FACT, replace label: "Selling coffee from the farm"
var_label(HouseholdLaos$b34) <- "Selling coffee from the farm"
#C89 = FACT, replace label: "Selling maize from the farm"
var_label(HouseholdLaos$b35) <- "Selling maize from the farm"
#C90 = FACT, replace label: "Selling other crops from the farm"
var_label(HouseholdLaos$b36) <- "Selling other crops from the farm"
#C91 = FACT, replace label: "Selling cattle and buffalo"
var_label(HouseholdLaos$b37) <- "Selling cattle and buffalo"
#C92 = FACT, replace label: "Selling pigs"
var_label(HouseholdLaos$b38) <- "Selling pigs"
#C93 = FACT, replace label: "Selling poultry"
var_label(HouseholdLaos$b39) <- "Selling poultry"
#C94 = FACT, replace label: "b3/10 Selling other farm products"
var_label(HouseholdLaos$b310) <- "Selling other farm products"
#C95 = FACT, replace label: "Sell derived/processed products"
var_label(HouseholdLaos$b311) <- "Sell derived/processed products"
#C96 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
var_label(HouseholdLaos$b312) <- "Non-farm wages (salaried work in private or public company)"
#C97 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdLaos$b313) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C98 = FACT, replace label: "Selling labor"
var_label(HouseholdLaos$b314) <- "Selling labor"
#C99 = FACT, replace label: "Remittances"
var_label(HouseholdLaos$b315) <- "Remittances"
#C100 = FACT, replace label: "Pension"
var_label(HouseholdLaos$b316) <- "Pension"
#C101 = FACT, replace label: "Rented land"
var_label(HouseholdLaos$b317) <- "Rented land"
#C102 = FACT, replace label: "Financial support / gift"
var_label(HouseholdLaos$b318) <- "Financial support / gift"
#C103 = FACT, replace label: "Other"
var_label(HouseholdLaos$b399) <- "Other"
#C104 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$b388) <- "Do not know"
#C105 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C106 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C107 = FACT, (Table of correspondence)
#C108 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C109 = FACT, (Table of correspondence)
#C110 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C111 = FACT, (Table of correspondence)
#C112 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C113 = OK, change label name for "What is the proportion of total household income from b4_1"
var_label(HouseholdLaos$b5_1) <- "What is the proportion of total household income from b4_1"
#C114 = NUMERIC, Useless column
#C115 = OK, change label name for "What is the proportion of total household income from b4_2"
var_label(HouseholdLaos$b5_2) <- "What is the proportion of total household income from b4_2"
#C116 = NUMERIC,column
#C117 = OK, change label name for "What is the proportion of total household income from b4_3"
var_label(HouseholdLaos$b5_3) <- "What is the proportion of total household income from b4_3"
#C118 = NUMERIC, Useless column
#C119 = FACT, (bin), (0 = no, 1 = yes)
#C120 = OK, (Useless answer-MultipleCombined, use the following columns)
#C121 = FACT, replace label: "Selling own vegetables"
var_label(HouseholdLaos$b71) <- "Selling own vegetables"
#C122 = FACT, replace label: "Selling own fruits"
var_label(HouseholdLaos$b72) <- "Selling own fruits"
#C123 = FACT, replace label: "Selling rice from the farm"
var_label(HouseholdLaos$b73) <- "Selling rice from the farm"
#C124 = FACT, replace label: "Selling coffee from the farm"
var_label(HouseholdLaos$b74) <- "Selling coffee from the farm"
#C125 = FACT, replace label: "Selling maize from the farm"
var_label(HouseholdLaos$b75) <- "Selling maize from the farm"
#C126 = FACT, replace label: "Selling other crops from the farm"
var_label(HouseholdLaos$b76) <- "Selling other crops from the farm"
#C127 = FACT, replace label: "Selling cattle and buffalo"
var_label(HouseholdLaos$b77) <- "Selling cattle and buffalo"
#C128 = FACT, replace label: "Selling pigs"
var_label(HouseholdLaos$b78) <- "Selling pigs"
#C129 = FACT, replace label: "Selling poultry"
var_label(HouseholdLaos$b79) <- "Selling poultry"
#C130 = FACT, replace label: "Selling other farm products"
var_label(HouseholdLaos$b710) <- "Selling other farm products"
#C131 = FACT, replace label: "Sell derived/processed products"
var_label(HouseholdLaos$b711) <- "Sell derived/processed products"
#C132 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
var_label(HouseholdLaos$b712) <- "Non-farm wages (salaried work in private or public company)"
#C133 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdLaos$b713) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C134 = FACT, replace label: "Selling labor"
var_label(HouseholdLaos$b714) <- "Selling labor"
#C135 = FACT, replace label: "Remittances"
var_label(HouseholdLaos$b715) <- "Remittances"
#C136 = FACT, replace label: "Pension"
var_label(HouseholdLaos$b716) <- "Pension"
#C137 = FACT, replace label: "Rented land"
var_label(HouseholdLaos$b717) <- "Rented land"
#C138 = FACT, replace label: "Financial support / gift"
var_label(HouseholdLaos$b718) <- "Financial support / gift"
#C139 = FACT, replace label: "Other"
var_label(HouseholdLaos$b799) <- "Other"
#C140 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$b788) <- "Do not know"
#C141 = CHAR, (the answer is still in Lao, Ky will maybe solve it)
#C142 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C143 = FACT, (1 = yes, 0 = no)
#C144 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C145 = FACT, replace label: "Jan"
var_label(HouseholdLaos$b8_11) <- "Jan"
#C146 = FACT, replace label: "Feb"
var_label(HouseholdLaos$b8_12) <- "Feb"
#C147 = FACT, replace label: "Mar"
var_label(HouseholdLaos$b8_13) <- "Mar"
#C148 = FACT, replace label: "Apr"
var_label(HouseholdLaos$b8_14) <- "Apr"
#C149 = FACT, replace label: "May"
var_label(HouseholdLaos$b8_15) <- "May"
#C150 = FACT, replace label: "Jun"
var_label(HouseholdLaos$b8_16) <- "Jun"
#C151 = FACT, replace label: "Jul"
var_label(HouseholdLaos$b8_17) <- "Jul"
#C152 = FACT, replace label: "Aug"
var_label(HouseholdLaos$b8_18) <- "Aug"
#C153 = FACT, replace label: "Sep"
var_label(HouseholdLaos$b8_19) <- "Sep"
#C154 = FACT, replace label: "Oct"
var_label(HouseholdLaos$b8_110) <- "Oct"
#C155 = FACT, replace label: "Nov"
var_label(HouseholdLaos$b8_111) <- "Nov"
#C156 = FACT, replace label: "Dec"
var_label(HouseholdLaos$b8_112) <- "Dec"
#C157 = FACT, (1 = yes, 0 = no)
#C158 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C159 = FACT, replace label: "Jan"
var_label(HouseholdLaos$b9_11) <- "Jan"
#C160 = FACT, replace label: "Feb"
var_label(HouseholdLaos$b9_12) <- "Feb"
#C161 = FACT, replace label: "Mar"
var_label(HouseholdLaos$b9_13) <- "Mar"
#C162 = FACT, replace label: "Apr"
var_label(HouseholdLaos$b9_14) <- "Apr"
#C163 = FACT, replace label: "May"
var_label(HouseholdLaos$b9_15) <- "May"
#C164 = FACT, replace label: "Jun"
var_label(HouseholdLaos$b9_16) <- "Jun"
#C165 = FACT, replace label: "Jul"
var_label(HouseholdLaos$b9_17) <- "Jul"
#C166 = FACT, replace label: "Aug"
var_label(HouseholdLaos$b9_18) <- "Aug"
#C167 = FACT, replace label: "Sep"
var_label(HouseholdLaos$b9_19) <- "Sep"
#C168 = FACT, replace label: "Oct"
var_label(HouseholdLaos$b9_110) <- "Oct"
#C169 = FACT, replace label: "Nov"
var_label(HouseholdLaos$b9_111) <- "Nov"
#C170 = FACT, replace label: "Dec"
var_label(HouseholdLaos$b9_112) <- "Dec"
#C171 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C172 = FACT, replace label: "Non-working members went out to look for work"
var_label(HouseholdLaos$b9_21) <- "Non-working members went out to look for work"
#C173 = FACT, replace label: "b9_2/2 Working members increased work hours"
var_label(HouseholdLaos$b9_22) <- "Working members increased work hours"
#C174 = FACT, replace label: "One or more members changes residence"
var_label(HouseholdLaos$b9_23) <- "One or more members changes residence"
#C175 = FACT, replace label: "Spent savings"
var_label(HouseholdLaos$b9_24) <- "Spent savings"
#C176 = FACT, replace label: "Went into debt"
var_label(HouseholdLaos$b9_25) <- "Went into debt"
#C177 = FACT, replace label: "Sold property or assets"
var_label(HouseholdLaos$b9_26) <- "Sold property or assets"
#C178 = FACT, replace label: "Withdrew children from school"
var_label(HouseholdLaos$b9_27) <- "Withdrew children from school"
#C179 = FACT, replace label: "Decreased food expenses"
var_label(HouseholdLaos$b9_28) <- "Decreased food expenses"
#C180 = FACT, replace label: "Changed agricultural practices"
var_label(HouseholdLaos$b9_29) <- "Changed agricultural practices"
#C181 = FACT, replace label: "Did nothing"
var_label(HouseholdLaos$b9_20) <- "Did nothing"
#C182 = FACT, replace label: "Other"
var_label(HouseholdLaos$b9_299) <- "Other"
#C183 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C184 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C185 = FACT, replace label: "Community-based tourism or agroecological tourism"
var_label(HouseholdLaos$b9_31) <- "Community-based tourism or agroecological tourism"
#C186 = FACT, replace label: "Hosting events (e.g. for projects from NGOs, research)"
var_label(HouseholdLaos$b9_32) <- "Hosting events (e.g. for projects from NGOs, research)"
#C187 = FACT, replace label: "Education and training of others (e.g. training of other farmers, school visits)"
var_label(HouseholdLaos$b9_33) <- "Education and training of others (e.g. training of other farmers, school visits)"
#C188 = FACT, replace label: "Food processing"
var_label(HouseholdLaos$b9_34) <- "Food processing"
#C189 = FACT, replace label: "Restaurant"
var_label(HouseholdLaos$b9_35) <- "Restaurant"
#C190 = FACT, replace label: "Selling products from other farms"
var_label(HouseholdLaos$b9_36) <- "Selling products from other farms"
#C191 = FACT, replace label: "None of the above"
var_label(HouseholdLaos$b9_30) <- "None of the above"
#C192 = FACT, replace label: "I do not know"
var_label(HouseholdLaos$b9_388) <- "I do not know"
#C193 = FACT, (Table of correspondence), (Table of correspondence), (see answers below)
#I do not sell (crops/vegetables/fruits or livestock) = NA
#The local market ( lower or equal to district level market)
#The provincial or national market
#Export
#Other
#Do not know
#C194 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C195 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C196 = FACT, replace label: "China"
var_label(HouseholdLaos$b10_11) <- "China"
#C197 = FACT, replace label: "South East Asia"
var_label(HouseholdLaos$b10_12) <- "South East Asia"
#C198 = FACT, replace label: "East Asia"
var_label(HouseholdLaos$b10_13) <- "East Asia"
#C199 = FACT, replace label: "Europe"
var_label(HouseholdLaos$b10_14) <- "Europe"
#C200 = FACT, replace label: "US"
var_label(HouseholdLaos$b10_15) <- "US"
#C201 = FACT, replace label: "Other left countries"
var_label(HouseholdLaos$b10_16) <- "Other left countries"
#C202 = FACT, (Table of correspondence)
#C203 = FACT, (Table of correspondence)
#C204 = FACT, (Table of correspondence)
#C205 = FACT, (Table of correspondence)
#C206 = FACT, (0 = no, 1 = yes)
#C207 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C208 = FACT, replace label: "Vegetables"
var_label(HouseholdLaos$b22_11) <- "Vegetables"
#C209 = FACT, replace label: "Plum"
var_label(HouseholdLaos$b22_12) <- "Plum"
#C210 = FACT, replace label: "Logan"
var_label(HouseholdLaos$b22_13) <- "Logan"
#C211 = FACT, replace label: "Mango"
var_label(HouseholdLaos$b22_14) <- "Mango"
#C212 = FACT, replace label: "Avocado"
var_label(HouseholdLaos$b22_15) <- "Avocado"
#C213 = FACT, replace label: "Dragon fruits"
var_label(HouseholdLaos$b22_16) <- "Dragon fruits"
#C214 = FACT, replace label: "Banana"
var_label(HouseholdLaos$b22_17) <- "Banana"
#C215 = FACT, replace label: "Strawberry"
var_label(HouseholdLaos$b22_18) <- "Strawberry"
#C216 = FACT, replace label: "Honey"
var_label(HouseholdLaos$b22_19) <- "Honey"
#C217 = FACT, replace label: "Tea"
var_label(HouseholdLaos$b22_110) <- "Tea"
#C218 = FACT, replace label: "Rice"
var_label(HouseholdLaos$b22_111) <- "Rice"
#C219 = FACT, replace label: "Noodle"
var_label(HouseholdLaos$b22_112) <- "Noodle"
#C220 = FACT, replace label: "Dried meat (pork, beef, etc.)"
var_label(HouseholdLaos$b22_113) <- "Dried meat (pork, beef, etc.)"
#C221 = FACT, replace label: "Coffee"
var_label(HouseholdLaos$b22_114) <- "Coffee"
#C222 = FACT, replace label: "Other"
var_label(HouseholdLaos$b22_199) <- "Other"
#C223 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C224 = FACT, (Table of correspondence)
#C225 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C226 = FACT, (only NA)
#C227 = OK, (Lao?)
#C228 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C229 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_1a1) <- "Village collector"
#C230 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_1a2) <- "Collector outside the village"
#C231 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_1a3) <- "Trader in the district"
#C232 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_1a4) <- "Trader from the province"
#C233 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_1a5) <- "Trader from another province"
#C234 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_1a6) <- "Cooperative of which you are a member"
#C235 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_1a7) <- "Cooperative of which you are not a member"
#C236 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_1a8) <- "Consumers on the farm"
#C237 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_1a9) <- "Local markets where you sell your products directly to final consumers"
#C238 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_1a10) <- "Consumers through online sales"
#C239 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_1a11) <- "Foreign trader (e.g. from China)"
#C240 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_1a12) <- "Processors"
#C241 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_1a99) <- "Other"
#C242 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_1a88) <- "Do not know/ no the second outlet"
#C243 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C244 = FACT, (Table of correspondence)
#C245 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C246 = FACT, (only NA?)
#C247 = OK, (Lao?)
#C248 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C249 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_2a1) <- "Village collector"
#C250 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_2a2) <- "Collector outside the village"
#C251 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_2a3) <- "Trader in the district"
#C252 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_2a4) <- "Trader from the province"
#C253 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_2a5) <- "Trader from another province"
#C254 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_2a6) <- "Cooperative of which you are a member"
#C255 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_2a7) <- "Cooperative of which you are not a member"
#C256 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_2a8) <- "Consumers on the farm"
#C257 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_2a9) <- "Local markets where you sell your products directly to final consumers"
#C258 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_2a10) <- "Consumers through online sales"
#C259 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_2a11) <- "Foreign trader (e.g. from China)"
#C260 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_2a12) <- "Processors"
#C261 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_2a99) <- "Other"
#C262 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_2a88) <- "Do not know/ no the second outlet"
#C263 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C264 = FACT, (Table of correspondence)
#C265 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C266 = FACT, (only NA?)
#C267 = OK, (Lao?)
#C268 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C269 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_3a1) <- "Village collector"
#C270 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_3a2) <- "Collector outside the village"
#C271 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_3a3) <- "Trader in the district"
#C272 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_3a4) <- "Trader from the province"
#C273 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_3a5) <- "Trader from another province"
#C274 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_3a6) <- "Cooperative of which you are a member"
#C275 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_3a7) <- "Cooperative of which you are not a member"
#C276 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_3a8) <- "Consumers on the farm"
#C277 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_3a9) <- "Local markets where you sell your products directly to final consumers"
#C278 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_3a10) <- "Consumers through online sales"
#C279 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_3a11) <- "Foreign trader (e.g. from China)"
#C280 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_3a12) <- "Processors"
#C281 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_3a99) <- "Other"
#C282 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_3a88) <- "Do not know/ no the second outlet"
#C283 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C284 = FACT, (Table of correspondence)
#C285 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C286 = FACT, (only NA?)
#C287 = OK, (Lao?)
#C288 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C289 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_4a1) <- "Village collector"
#C290 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_4a2) <- "Collector outside the village"
#C291 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_4a3) <- "Trader in the district"
#C292 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_4a4) <- "Trader from the province"
#C293 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_4a5) <- "Trader from another province"
#C294 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_4a6) <- "Cooperative of which you are a member"
#C295 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_4a7) <- "Cooperative of which you are not a member"
#C296 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_4a8) <- "Consumers on the farm"
#C297 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_4a9) <- "Local markets where you sell your products directly to final consumers"
#C298 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_4a10) <- "Consumers through online sales"
#C299 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_4a11) <- "Foreign trader (e.g. from China)"
#C300 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_4a12) <- "Processors"
#C301 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_4a99) <- "Other"
#C302 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_4a88) <- "Do not know/ no the second outlet"
#C303 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C304 = FACT, (Table of correspondence)
#C305 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C306 = FACT, (only NA?)
#C307 = OK, (Lao?)
#C308 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C309 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_5a1) <- "Village collector"
#C310 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_5a2) <- "Collector outside the village"
#C311 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_5a3) <- "Trader in the district"
#C312 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_5a4) <- "Trader from the province"
#C313 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_5a5) <- "Trader from another province"
#C314 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_5a6) <- "Cooperative of which you are a member"
#C315 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_5a7) <- "Cooperative of which you are not a member"
#C316 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_5a8) <- "Consumers on the farm"
#C317 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_5a9) <- "Local markets where you sell your products directly to final consumers"
#C318 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_5a10) <- "Consumers through online sales"
#C319 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_5a11) <- "Foreign trader (e.g. from China)"
#C320 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_5a12) <- "Processors"
#C321 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_5a99) <- "Other"
#C322 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_5a88) <- "Do not know/ no the second outlet"
#C323 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C324 = FACT, (Table of correspondence)
#C325 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C326 = FACT, (only NA?)
#C327 = OK, (Lao?)
#C328 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C329 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_6a1) <- "Village collector"
#C330 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_6a2) <- "Collector outside the village"
#C331 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_6a3) <- "Trader in the district"
#C332 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_6a4) <- "Trader from the province"
#C333 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_6a5) <- "Trader from another province"
#C334 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_6a6) <- "Cooperative of which you are a member"
#C335 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_6a7) <- "Cooperative of which you are not a member"
#C336 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_6a8) <- "Consumers on the farm"
#C337 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_6a9) <- "Local markets where you sell your products directly to final consumers"
#C338 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_6a10) <- "Consumers through online sales"
#C339 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_6a11) <- "Foreign trader (e.g. from China)"
#C340 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_6a12) <- "Processors"
#C341 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_6a99) <- "Other"
#C342 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_6a88) <- "Do not know/ no the second outlet"
#C343 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C344 = FACT, (Table of correspondence)
#C345 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C346 = FACT, (only NA?)
#C347 = OK, (Lao?)
#C348 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C349 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_7a1) <- "Village collector"
#C350 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_7a2) <- "Collector outside the village"
#C351 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_7a3) <- "Trader in the district"
#C352 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_7a4) <- "Trader from the province"
#C353 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_7a5) <- "Trader from another province"
#C354 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_7a6) <- "Cooperative of which you are a member"
#C355 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_7a7) <- "Cooperative of which you are not a member"
#C356 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_7a8) <- "Consumers on the farm"
#C357 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_7a9) <- "Local markets where you sell your products directly to final consumers"
#C358 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_7a10) <- "Consumers through online sales"
#C359 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_7a11) <- "Foreign trader (e.g. from China)"
#C360 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_7a12) <- "Processors"
#C361 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_7a99) <- "Other"
#C362 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_7a88) <- "Do not know/ no the second outlet"
#C363 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C364 = FACT, (Table of correspondence)
#C365 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C366 = FACT, (only NA?)
#C367 = OK, (Lao?)
#C368 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C369 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_8a1) <- "Village collector"
#C370 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_8a2) <- "Collector outside the village"
#C371 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_8a3) <- "Trader in the district"
#C372 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_8a4) <- "Trader from the province"
#C373 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_8a5) <- "Trader from another province"
#C374 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_8a6) <- "Cooperative of which you are a member"
#C375 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_8a7) <- "Cooperative of which you are not a member"
#C376 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_8a8) <- "Consumers on the farm"
#C377 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_8a9) <- "Local markets where you sell your products directly to final consumers"
#C378 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_8a10) <- "Consumers through online sales"
#C379 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_8a11) <- "Foreign trader (e.g. from China)"
#C380 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_8a12) <- "Processors"
#C381 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_8a99) <- "Other"
#C382 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_8a88) <- "Do not know/ no the second outlet"
#C383 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C384 = FACT, (Table of correspondence)
#C385 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C386 = FACT, (only NA?)
#C387 = OK, (Lao?)
#C388 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C389 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_9a1) <- "Village collector"
#C390 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_9a2) <- "Collector outside the village"
#C391 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_9a3) <- "Trader in the district"
#C392 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_9a4) <- "Trader from the province"
#C393 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_9a5) <- "Trader from another province"
#C394 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_9a6) <- "Cooperative of which you are a member"
#C395 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_9a7) <- "Cooperative of which you are not a member"
#C396 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_9a8) <- "Consumers on the farm"
#C397 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_9a9) <- "Local markets where you sell your products directly to final consumers"
#C398 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_9a10) <- "Consumers through online sales"
#C399 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_9a11) <- "Foreign trader (e.g. from China)"
#C400 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_9a12) <- "Processors"
#C401 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_9a99) <- "Other"
#C402 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_9a88) <- "Do not know/ no the second outlet"
#C403 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C404 = FACT, (Table of correspondence)
#C405 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C406 = FACT, (only NA?)
#C407 = OK, (Lao?)
#C408 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C409 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_10a1) <- "Village collector"
#C410 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_10a2) <- "Collector outside the village"
#C411 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_10a3) <- "Trader in the district"
#C412 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_10a4) <- "Trader from the province"
#C413 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_10a5) <- "Trader from another province"
#C414 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_10a6) <- "Cooperative of which you are a member"
#C415 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_10a7) <- "Cooperative of which you are not a member"
#C416 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_10a8) <- "Consumers on the farm"
#C417 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_10a9) <- "Local markets where you sell your products directly to final consumers"
#C418 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_10a10) <- "Consumers through online sales"
#C419 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_10a11) <- "Foreign trader (e.g. from China)"
#C420 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_10a12) <- "Processors"
#C421 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_10a99) <- "Other"
#C422 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_10a88) <- "Do not know/ no the second outlet"
#C423 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C424 = FACT, (Table of correspondence)
#C425 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C426 = FACT, (only NA?)
#C427 = OK, (Lao?)
#C428 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C429 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_11a1) <- "Village collector"
#C430 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_11a2) <- "Collector outside the village"
#C431 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_11a3) <- "Trader in the district"
#C432 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_11a4) <- "Trader from the province"
#C433 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_11a5) <- "Trader from another province"
#C434 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_11a6) <- "Cooperative of which you are a member"
#C435 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_11a7) <- "Cooperative of which you are not a member"
#C436 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_11a8) <- "Consumers on the farm"
#C437 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_11a9) <- "Local markets where you sell your products directly to final consumers"
#C438 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_11a10) <- "Consumers through online sales"
#C439 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_11a11) <- "Foreign trader (e.g. from China)"
#C440 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_11a12) <- "Processors"
#C441 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_11a99) <- "Other"
#C442 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_11a88) <- "Do not know/ no the second outlet"
#C443 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C444 = FACT, (Table of correspondence)
#C445 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C446 = FACT, (only NA?)
#C447 = OK, (Lao?)
#C448 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C449 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_12a1) <- "Village collector"
#C450 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_12a2) <- "Collector outside the village"
#C451 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_12a3) <- "Trader in the district"
#C452 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_12a4) <- "Trader from the province"
#C453 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_12a5) <- "Trader from another province"
#C454 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_12a6) <- "Cooperative of which you are a member"
#C455 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_12a7) <- "Cooperative of which you are not a member"
#C456 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_12a8) <- "Consumers on the farm"
#C457 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_12a9) <- "Local markets where you sell your products directly to final consumers"
#C458 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_12a10) <- "Consumers through online sales"
#C459 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_12a11) <- "Foreign trader (e.g. from China)"
#C460 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_12a12) <- "Processors"
#C461 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_12a99) <- "Other"
#C462 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_12a88) <- "Do not know/ no the second outlet"
#C463 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C464 = FACT, (Table of correspondence)
#C465 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C466 = FACT, (only NA?)
#C467 = OK, (Lao?)
#C468 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C469 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_13a1) <- "Village collector"
#C470 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_13a2) <- "Collector outside the village"
#C471 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_13a3) <- "Trader in the district"
#C472 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_13a4) <- "Trader from the province"
#C473 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_13a5) <- "Trader from another province"
#C474 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_13a6) <- "Cooperative of which you are a member"
#C475 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_13a7) <- "Cooperative of which you are not a member"
#C476 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_13a8) <- "Consumers on the farm"
#C477 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_13a9) <- "Local markets where you sell your products directly to final consumers"
#C478 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_13a10) <- "Consumers through online sales"
#C479 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_13a11) <- "Foreign trader (e.g. from China)"
#C480 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_13a12) <- "Processors"
#C481 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_13a99) <- "Other"
#C482 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_13a88) <- "Do not know/ no the second outlet"
#C483 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C484 = FACT, (Table of correspondence)
#C485 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C486 = FACT, (only NA?)
#C487 = OK, (Lao?)
#C488 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C489 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_14a1) <- "Village collector"
#C490 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_14a2) <- "Collector outside the village"
#C491 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_14a3) <- "Trader in the district"
#C492 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_14a4) <- "Trader from the province"
#C493 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_14a5) <- "Trader from another province"
#C494 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_14a6) <- "Cooperative of which you are a member"
#C495 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_14a7) <- "Cooperative of which you are not a member"
#C496 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_14a8) <- "Consumers on the farm"
#C497 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_14a9) <- "Local markets where you sell your products directly to final consumers"
#C498 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_14a10) <- "Consumers through online sales"
#C499 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_14a11) <- "Foreign trader (e.g. from China)"
#C500 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_14a12) <- "Processors"
#C501 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_14a99) <- "Other"
#C502 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_14a88) <- "Do not know/ no the second outlet"
#C503 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C504 = FACT, (Table of correspondence)
#C505 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C506 = FACT, (only NA?)
#C507 = OK, (Lao?)
#C508 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "b24_99a. for certified b22_1oth, which were the buyers/outlets in 2022"
var_label(HouseholdLaos$b24_99a) <- "b24_99a. for certified b22_1oth, which were the buyers/outlets in 2022"
#C509 = FACT, replace label: "Village collector"
var_label(HouseholdLaos$b24_99a1) <- "Village collector"
#C510 = FACT, replace label: "Collector outside the village"
var_label(HouseholdLaos$b24_99a2) <- "Collector outside the village"
#C511 = FACT, replace label: "Trader in the district"
var_label(HouseholdLaos$b24_99a3) <- "Trader in the district"
#C512 = FACT, replace label: "Trader from the province"
var_label(HouseholdLaos$b24_99a4) <- "Trader from the province"
#C513 = FACT, replace label: "Trader from another province"
var_label(HouseholdLaos$b24_99a5) <- "Trader from another province"
#C514 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdLaos$b24_99a6) <- "Cooperative of which you are a member"
#C515 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdLaos$b24_99a7) <- "Cooperative of which you are not a member"
#C516 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdLaos$b24_99a8) <- "Consumers on the farm"
#C517 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdLaos$b24_99a9) <- "Local markets where you sell your products directly to final consumers"
#C518 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdLaos$b24_99a10) <- "Consumers through online sales"
#C519 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdLaos$b24_99a11) <- "Foreign trader (e.g. from China)"
#C520 = FACT, replace label: "Processors"
var_label(HouseholdLaos$b24_99a12) <- "Processors"
#C521 = FACT, replace label: "Other"
var_label(HouseholdLaos$b24_99a99) <- "Other"
#C522 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdLaos$b24_99a88) <- "Do not know/ no the second outlet"
#C523 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C524 = OK, (lao? empty?)
#C525 = FACT, (bin)
#C526 = FACT, (bin)
#C527 = FACT, (bin)
#C528 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C529 = FACT, replace label: "Logo"
var_label(HouseholdLaos$b27_11) <- "Logo"
#C530 = FACT, replace label: "Card visit"
var_label(HouseholdLaos$b27_12) <- "Card visit"
#C531 = FACT, replace label: "Leaflet"
var_label(HouseholdLaos$b27_13) <- "Leaflet"
#C532 = FACT, replace label: "Signboard in the field"
var_label(HouseholdLaos$b27_14) <- "Signboard in the field"
#C533 = FACT, replace label: "Signboard at the cooperative/Company"
var_label(HouseholdLaos$b27_15) <- "Signboard at the cooperative/Company"
#C534 = FACT, replace label: "Social media"
var_label(HouseholdLaos$b27_16) <- "Social media"
#C535 = FACT, replace label: "Internet or other media (TV, Radio, etc.)"
var_label(HouseholdLaos$b27_17) <- "Internet or other media (TV, Radio, etc.)"
#C536 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$b27_188) <- "Do not know"
#C537 = OK, (empty)
#C538 = FACT, (bin)
#C539 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C540 = FACT, (bin)
#C541 = FACT, (bin)

# # #c.
#C542 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C543 = FACT, replace label: "None"
var_label(HouseholdLaos$c1_0) <- "None"
#C544 = FACT, replace label: "Women Union"
var_label(HouseholdLaos$c1_1) <- "Women Union"
#C545 = FACT, replace label: "Youth union"
var_label(HouseholdLaos$c1_2) <- "Youth union"
#C546 = FACT, replace label: "Veteran union"
var_label(HouseholdLaos$c1_3) <- "Veteran union"
#C547 = FACT, replace label: "Farmer union"
var_label(HouseholdLaos$c1_4) <- "Farmer union"
#C548 = FACT, replace label: "Elderly Union"
var_label(HouseholdLaos$c1_5) <- "Elderly Union"
#C549 = FACT, replace label: "Political party"
var_label(HouseholdLaos$c1_6) <- "Political party"
#C550 = FACT, replace label: "Other"
var_label(HouseholdLaos$c1_99) <- "Other"
#C551 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C552 = FACT, (Table of correspondence), (see answers below)
#No
#Yes, one
#Yes, more than one
#C553 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C554 = FACT, replace label: "Farmer organization on crops"
var_label(HouseholdLaos$c31) <- "Farmer organization on crops"
#C555 = FACT, replace label: "Farmer organization on fruits"
var_label(HouseholdLaos$c32) <- "Farmer organization on fruits"
#C556 = FACT, replace label: "Farmer organization on livestock"
var_label(HouseholdLaos$c33) <- "Farmer organization on livestock"
#C557 = FACT, replace label: "Farmer organization on honey"
var_label(HouseholdLaos$c34) <- "Farmer organization on honey"
#C558 = FACT, replace label: "Farmer organization on water"
var_label(HouseholdLaos$c35) <- "Farmer organization on water"
#C559 = FACT, replace label: "Farmer organization on forest"
var_label(HouseholdLaos$c36) <- "Farmer organization on forest"
#C560 = FACT, replace label: "Other farmer organization"
var_label(HouseholdLaos$c399) <- "Other farmer organization"
#C561 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C562 = FACT, (Table of correspondence), (see answers below)
#Farmer organization on crops
#Farmer organization on fruits
#Farmer organization on livestock
#Farmer organization on honey
#Farmer organization on water
#Farmer organization on forest
#Other
#C563 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "Specify"
var_label(HouseholdLaos$c3a_text) <- "Specify"
#C564 = FACT, (Table of correspondence),(Table of correspondence), (see answers below)
#Farmer cooperative old style (under 2003 law) 
#Farmer cooperative new style (under 2012 law)
#Farmer group
#Other
#Do not know
#President 
#Treasurer 
#Internal control person
#Trainer 
#Collector 
#Ordinary member
#Other
#Do not know
#replace label: "c4. what is the legal type of this c3a"
var_label(HouseholdLaos$c4) <- "c4. what is the legal type of this c3a"
#C565 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C566 = FACT, (Table of correspondence), (see answers below)
#President 
#Treasurer 
#Internal control person
#Trainer 
#Collector 
#Ordinary member
#Other
#Do not know
#replace label: "c5. what is your role / responsibility in the c3a"
var_label(HouseholdLaos$c5) <- "c5. what is your role / responsibility in the c3a"
#C567 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "c5_oth. specify your other role in the c3a"
var_label(HouseholdLaos$c5_oth) <- "c5_oth. specify your other role in the c3a"
#C568 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "c9. what are the benefits of belonging to this c3a"
var_label(HouseholdLaos$c9) <- "c9. what are the benefits of belonging to this c3a"
#C569 = FACT, replace label: "To borrow money "
var_label(HouseholdLaos$c91) <- "To borrow money"
#C570 = FACT, replace label: "To get advice"
var_label(HouseholdLaos$c92) <- "To get advice"
#C571 = FACT, replace label: "To save money"
var_label(HouseholdLaos$c93) <- "To save money"
#C572 = FACT, replace label: "To buy inputs"
var_label(HouseholdLaos$c94) <- "To buy inputs"
#C573 = FACT, replace label: "To use water"
var_label(HouseholdLaos$c95) <- "To use water"
#C574 = FACT, replace label: "To get training"
var_label(HouseholdLaos$c96) <- "To get training"
#C575 = FACT, replace label: "Access to markets"
var_label(HouseholdLaos$c97) <- "Access to markets"
#C576 = FACT, replace label: "Secure demand from existing markets"
var_label(HouseholdLaos$c98) <- "Secure demand from existing markets"
#C577 = FACT, replace label: "Sell certified products at a better price "
var_label(HouseholdLaos$c99) <- "Sell certified products at a better price "
#C578 = FACT, replace label: "Other"
var_label(HouseholdLaos$c999) <- "Other"
#C579 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "Specify other"
var_label(HouseholdLaos$c9_oth) <- "Specify other"
#C580 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$c988) <- "Do not know"
#C581 = FACT, (bin), replace label: "Did c3a give you loan/credit in the past 3 years"
var_label(HouseholdLaos$c6) <- "Did c3a give you loan/credit in the past 3 years"
#C582 = FACT, (bin), replace label: "Did c3a give you technical advice or training in the past 3 years?"
var_label(HouseholdLaos$c7) <- "Did c3a give you technical advice or training in the past 3 years?"
#C583 = FACT, (bin), replace label: "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
var_label(HouseholdLaos$c8) <- "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
#C584 = FACT, (bin), replace label: "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
var_label(HouseholdLaos$c10) <- "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
#C585 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C586 = FACT, replace label: "GAP"
var_label(HouseholdLaos$c111) <- "GAP"
#C587 = FACT, replace label: "Pest-management"
var_label(HouseholdLaos$c112) <- "Pest-management"
#C588 = FACT, replace label: "Record-keeping"
var_label(HouseholdLaos$c113) <- "Record-keeping"
#C589 = FACT, replace label: "Composting"
var_label(HouseholdLaos$c114) <- "Composting"
#C590 = FACT, replace label: "Markets & prices"
var_label(HouseholdLaos$c115) <- "Markets & prices"
#C591 = FACT, replace label: "Certification"
var_label(HouseholdLaos$c116) <- "Certification"
#C592 = FACT, replace label: "Safe use of chemicals"
var_label(HouseholdLaos$c117) <- "Safe use of chemicals"
#C593 = FACT, replace label: "Use of fertilizers"
var_label(HouseholdLaos$c118) <- "Use of fertilizers"
#C594 = FACT, replace label: "IPM"
var_label(HouseholdLaos$c119) <- "IPM"
#C595 = FACT, replace label: "Weeding"
var_label(HouseholdLaos$c1110) <- "Weeding"
#C596 = FACT, replace label: "Pheromones"
var_label(HouseholdLaos$c1111) <- "Pheromones"
#C597 = FACT, replace label: "Forage production"
var_label(HouseholdLaos$c1112) <- "Forage production"
#C598 = FACT, replace label: "Forage treatment / silage"
var_label(HouseholdLaos$c1113) <- "Forage treatment / silage"
#C599 = FACT, replace label: "Farmer organization"
var_label(HouseholdLaos$c1114) <- "Farmer organization"
#C600 = FACT, replace label: "Organic"
var_label(HouseholdLaos$c1115) <- "Organic"
#C601 = FACT, replace label: "PGS"
var_label(HouseholdLaos$c1116) <- "PGS"
#C602 = FACT, replace label: "Other"
var_label(HouseholdLaos$c1199) <- "Other"
#C603 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C604 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "Now I would like to ask you whether you collaborate with other people to do any of the following?"
var_label(HouseholdLaos$c13) <- "Now I would like to ask you whether you collaborate with other people to do any of the following?"
#C605 = FACT, replace label: "Share labor (mutual help, working together on each other farm)"
var_label(HouseholdLaos$c131) <- "Share labor (mutual help, working together on each other farm)"
#C606 = FACT, replace label: "Manage water/irrigation systems"
var_label(HouseholdLaos$c132) <- "Manage water/irrigation systems"
#C607 = FACT, replace label: "Raise livestock"
var_label(HouseholdLaos$c133) <- "Raise livestock"
#C608 = FACT, replace label: "Buy agricultural inputs"
var_label(HouseholdLaos$c134) <- "Buy agricultural inputs"
#C609 = FACT, replace label: "Selling products to the markets for other farmers "
var_label(HouseholdLaos$c135) <- "Selling products to the markets for other farmers "
#C610 = FACT, replace label: "Experiment new farming practices"
var_label(HouseholdLaos$c136) <- "Experiment new farming practices"
#C611 = FACT, replace label: "No collaboration with other people on these issues"
var_label(HouseholdLaos$c130) <- "No collaboration with other people on these issues"
#C612 = FACT, replace label: "Other"
var_label(HouseholdLaos$c1399) <- "Other"
#C613 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C614 = FACT, (bin), replace label: "Do you exchange your agricultural products, equipment or animals with other farmers?"
var_label(HouseholdLaos$c14) <- "Do you exchange your agricultural products, equipment or animals with other farmers?"
#C615 = FACT, (bin), replace label: "Are you involved in some form of advocacy work (aiming to influence decision-making within political institutions)?"
var_label(HouseholdLaos$c15) <- "Are you involved in some form of advocacy work (aiming to influence decision-making within political institutions)?"

# # #d.
#C616 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C617 = FACT, replace label: "Lowland"
var_label(HouseholdLaos$d1_1) <- "Lowland"
#C618 = FACT, replace label: "Upland"
var_label(HouseholdLaos$d1_2) <- "Upland"
#C619 = FACT, replace label: "Pasture"
var_label(HouseholdLaos$d1_3) <- "Pasture"
#C620 = FACT, replace label: "Fallow"
var_label(HouseholdLaos$d1_4) <- "Fallow"
#C621 = FACT, replace label: "Forest"
var_label(HouseholdLaos$d1_5) <- "Forest"
#C622 = FACT, replace label: "Aquaculture land"
var_label(HouseholdLaos$d1_6) <- "Aquaculture land"
#C623 = FACT, replace label: "Homegarden"
var_label(HouseholdLaos$d1_7) <- "Homegarden"
#C624 = OK, replace label: "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdLaos$d2a_1) <- "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
#C625 = REMOVE, (Similar to the previous one)
#C626 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C627 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C628 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C629 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C630 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C631 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C632 = OK, replace label: "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdLaos$d2b_1) <- "d2a_1. UPLAND - how many crop did you that your household planted in the past 12 month"
#C633 = REMOVE, (Similar to the previous one)
#C634 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C635 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C636 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C637 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C638 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C639 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C640 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C641 = REMOVE, text from the questionnaire
#C642 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#From 1 to 5 = Lowland crops in order, from 6 to 1x = Upland crops in order
#C643 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d2_sum1) <- "Lowland crop n1"
#C644 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d2_sum2) <- "Lowland crop n2"
#C645 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d2_sum3) <- "Lowland crop n3"
#C646 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d2_sum4) <- "Lowland crop n4"
#C647 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d2_sum5) <- "Lowland crop n5"
#C648 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d2_sum6) <- "Upland crop n1"
#C649 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d2_sum7) <- "Upland crop n2"
#C650 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d2_sum8) <- "Upland crop n3"
#C651 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d2_sum9) <- "Upland crop n4"
#C652 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d2_sum10) <- "Upland crop n5"
#C653 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d2_sum11) <- "Upland crop n6"
#C654 = FACT, (Table of correspondence)
#Village collector 
#Collector outside the village 
#Trader in the district 
#Trader from the province
#Trader from another province
#Cooperative of which you are a member 
#Cooperative of which you are not a member 
#Consumers on the farm 
#Local markets where you sell your products directly to final consumers 
#Consumers through online sales 
#Foreign trader (e.g. from China) 
#Processors 
#Other
#Do not know/ no the second outlet
#C655 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C656 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C657 = FACT, (Table of correspondence)
#C658 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C659 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C660 = FACT, empty, check if necessary to Remove or not
#C661 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdLaos$b13_011) <- "Lowland crop n1"
#C662 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdLaos$b13_012) <- "Lowland crop n2"
#C663 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdLaos$b13_013) <- "Lowland crop n3"
#C664 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdLaos$b13_014) <- "Lowland crop n4"
#C665 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdLaos$b13_015) <- "Lowland crop n5"
#C666 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdLaos$b13_016) <- "Upland crop n1"
#C667 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdLaos$b13_017) <- "Upland crop n2"
#C668 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdLaos$b13_018) <- "Upland crop n3"
#C669 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdLaos$b13_019) <- "Upland crop n4"
#C670 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdLaos$b13_0110) <- "Upland crop n5"
#C671 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdLaos$b13_0111) <- "Upland crop n6"
#C672 = FACT, (Table of correspondence)
#C673 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdLaos$b14_1) <- "does b12_1 provide any of the following"
#C674 = FACT, empty, replace label: "Nothing"
var_label(HouseholdLaos$b14_10) <- "Nothing"
#We move this column at the right place
HouseholdLaos <- HouseholdLaos %>% relocate(b14_10 , .after = b14_1)
#C675 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdLaos$b14_11) <- "Inputs (sold)"
#C676 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdLaos$b14_12) <- "Inputs on credit"
#C677 = FACT, empty, replace label: "Gas Credit"
var_label(HouseholdLaos$b14_13) <- "Gas Credit"
#C678 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdLaos$b14_14) <- "Technical advice/training"
#C679 = FACT, empty, replace label: "Market information"
var_label(HouseholdLaos$b14_15) <- "Market information"
#C680 = FACT, empty, UNKNOWN, replace label: "Regular sales"
var_label(HouseholdLaos$b14_16) <- "Regular sales"
#C681 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$b14_199) <- "Other"
#C682 = FACT, empty, replace label: "Do not know"
var_label(HouseholdLaos$b14_188) <- "Do not know"
#C683 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "b14_1oth. specify other provision from b12_1"
var_label(HouseholdLaos$b14_1oth) <- "b14_1oth. specify other provision from b12_1"
#C684 = FACT, (Table of correspondence), , (see answers below)
#Formal contract
#Informal contract 
#No contract/ no prior arrangements
#Spot relations 
#Do not know
#replace label: "b15_1. do you have a contract with b12_1"
var_label(HouseholdLaos$b15_1) <- "b15_1. do you have a contract with b12_1"
#C685 = FACT, empty, check if necessary to Remove or not
#C686 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdLaos$b13_021) <- "Lowland crop n1"
#C687 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdLaos$b13_022) <- "Lowland crop n2"
#C688 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdLaos$b13_023) <- "Lowland crop n3"
#C689 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdLaos$b13_024) <- "Lowland crop n4"
#C690 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdLaos$b13_025) <- "Lowland crop n5"
#C691 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdLaos$b13_026) <- "Upland crop n1"
#C692 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdLaos$b13_027) <- "Upland crop n2"
#C693 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdLaos$b13_028) <- "Upland crop n3"
#C694 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdLaos$b13_029) <- "Upland crop n4"
#C695 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdLaos$b13_0210) <- "Upland crop n5"
#C696 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdLaos$b13_0211) <- "Upland crop n6"
#C697 = FACT, (Table of correspondence, see below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C698 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdLaos$b14_2) <- "does b12_2 provide any of the following"
#C699 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdLaos$b14_20) <- "Inputs (sold)"
#C700 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdLaos$b14_21) <- "Inputs on credit"
#C701 = FACT, empty, replace label: "Cash credit"
var_label(HouseholdLaos$b14_22) <- "Cash credit"
#C702 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdLaos$b14_23) <- "Technical advice/training"
#C703 = FACT, empty, replace label: "Market information "
var_label(HouseholdLaos$b14_24) <- "Market information "
#C704 = FACT, empty, replace label: "Regular sales"
var_label(HouseholdLaos$b14_25) <- "Regular sales"
#C705 = FACT, empty, UNKNOWN, replace label: "??"
var_label(HouseholdLaos$b14_26) <- "??"
#C706 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$b14_299) <- "Other"
#C707 = FACT, empty, replace label: "Do not know"
var_label(HouseholdLaos$b14_288) <- "Do not know"
#C708 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "b14_2oth. specify other provision from b12_1"
var_label(HouseholdLaos$b14_2oth) <- "b14_2oth. specify other provision from b12_2"
#C709 = FACT, (Table of correspondence), replace label: "b15_2. do you have a contract with b12_1"
var_label(HouseholdLaos$b15_2) <- "b15_2. do you have a contract with b12_2"
#C710 = OK
#C711 = OK
#C712 = OK
#C713 = OK
#C714 = OK
#C715 = OK
#C716 = OK
#C717 = OK
#C718 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C719 = FACT, empty, replace label: "None of forest product"
var_label(HouseholdLaos$d70) <- "None of forest product"
#C720 = FACT, empty, replace label: "Hunting"
var_label(HouseholdLaos$d71) <- "Hunting"
#C721 = FACT, empty, replace label: "Fishing"
var_label(HouseholdLaos$d72) <- "Fishing"
#C722 = FACT, empty, replace label: "Fuelwood"
var_label(HouseholdLaos$d73) <- "Fuelwood"
#C723 = FACT, empty, replace label: "Mushrooms"
var_label(HouseholdLaos$d74) <- "Mushrooms"
#C724 = FACT, empty, replace label: "Bamboo shoots"
var_label(HouseholdLaos$d75) <- "Bamboo shoots"
#C725 = FACT, empty, UNKNOWN, replace label: "Bamboo poles"
var_label(HouseholdLaos$d76) <- "Bamboo poles"
#C726 = FACT, empty, replace label: "Broom grass"
var_label(HouseholdLaos$d77) <- "Broom grass"
#C727 = FACT, empty, replace label: "Honey"
var_label(HouseholdLaos$d78) <- "Honey"
#C728 = FACT, empty, replace label: "Rattan"
var_label(HouseholdLaos$d79) <- "Rattan"
#C729 = FACT, empty, replace label: "Cardamom"
var_label(HouseholdLaos$d710) <- "Cardamom"
#C730 = FACT, empty, replace label: "Galangal"
var_label(HouseholdLaos$d711) <- "Galangal"
#C731 = FACT, empty, replace label: "Dammar gum"
var_label(HouseholdLaos$d712) <- "Dammar gum"
#C732 = FACT, empty, replace label: "Wild pepper"
var_label(HouseholdLaos$d713) <- "Wild pepper"
#C733 = FACT, empty, UNKNOWN, replace label: "Medicinal plants"
var_label(HouseholdLaos$d714) <- "Medicinal plants"
#C734 = FACT, empty, replace label: "Paper mulberry bark"
var_label(HouseholdLaos$d715) <- "Paper mulberry bark"
#C735 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$d799) <- "Other"
#C736 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C737 = OK
#C738 = FACT, (bin)
#C739 = OK
#C740 = OK
#C741 = FACT, (bin)
#C742 = OK
#C743 = OK
#C744 = FACT, (bin)
#C745 = OK
#C746 = OK
#C747 = FACT, (bin)
#C748 = OK
#C749 = OK
#C750 = FACT, (bin)
#C751 = OK
#C752 = OK
#C753 = FACT, (bin)
#C754 = OK
#C755 = OK
#C756 = FACT, (bin)
#C757 = OK
#C758 = OK
#C759 = FACT, (bin)
#C760 = OK
#C761 = OK
#C762 = FACT, (bin)
#C763 = OK
#C764 = OK
#C765 = FACT, (bin)
#C766 = OK
#C767 = OK
#C768 = FACT, (bin)
#C769 = OK
#C770 = OK
#C771 = FACT, (bin)
#C772 = OK
#C773 = OK
#C774 = FACT, (bin)
#C775 = OK
#C776 = OK
#C777 = FACT, (bin)
#C778 = OK
#C779 = OK
#C780 = FACT, (bin)
#C781 = OK
#C782 = OK, replace label: "d7_991. how many days did your household spend to collect d7_oth over 12 months"
var_label(HouseholdLaos$d7_991) <- "d7_991. how many days did your household spend to collect d7_oth over 12 months"
#C783 = FACT, (bin), replace label: "d7_992. did your household sell collect d7_oth product in the last 12 month"
var_label(HouseholdLaos$d7_992) <- "d7_992. did your household sell collect d7_oth product in the last 12 month"
#C784 = OK, replace label: "d7_993. what was your household income from collecting d7_oth in the last 12 months"
var_label(HouseholdLaos$d7_993) <- "d7_993. what was your household income from collecting d7_oth in the last 12 months"
#C785 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C786 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C787 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C788 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C789 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C790 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C791 = FACT, (table of correspondence), see choices below, replace label: "d81_1a.what was the main reason for planting d81_text"
var_label(HouseholdLaos$d81_1a) <- "d81_1a.what was the main reason for planting d81_text"
#Market price and demand
#Household consumption preferences; 
#Well adapted to local conditions (soil, climate, …)
#Do not know
#Other
#C792 = OK, replace label: "d81_1a_oth. specify other reason for planting d81_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d81_1a_oth) <- "d81_1a_oth. specify other reason for planting d81_text"
#C793 = FACT, (table of correspondence), see choices below, replace label: "d81_1b. what was the major constraints for d81_text"
var_label(HouseholdLaos$d81_1b) <- "d81_1b. what was the major constraints for d81_text"
#Water management
#Soil fertility
#Other agronomic constraints
#Insects
#Diseases
#Market price
#Product quality
#Do not know
#Other
#C794 = OK, replace label: "d81_1b_oth. specify other constrain for d81_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d81_1b_oth) <- "d81_1b_oth. specify other constrain for d81_text"
#C795 = FACT, replace label: "d82_1a.what was the main reason for planting d82_text"
var_label(HouseholdLaos$d82_1a) <- "d82_1a.what was the main reason for planting d82_text"
#C796 = OK, replace label: "d82_1a_oth. specify other reason for planting d82_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d82_1a_oth) <- "d82_1a_oth. specify other reason for planting d82_text"
#C797 = FACT, see choices below, replace label: "d82_1b. what was the major constraints for d82_text"
var_label(HouseholdLaos$d82_1b) <- "d82_1b. what was the major constraints for d82_text"
#C798 = OK, replace label: "d82_1b_oth. specify other constrain for d82_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d82_1b_oth) <- "d82_1b_oth. specify other constrain for d82_text"
#C799 = FACT, replace label: "d83_1a.what was the main reason for planting d83_text"
var_label(HouseholdLaos$d83_1a) <- "d83_1a.what was the main reason for planting d83_text"
#C800 = OK, replace label: "d83_1a_oth. specify other reason for planting d83_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d83_1a_oth) <- "d83_1a_oth. specify other reason for planting d83_text"
#C801 = FACT, see choices below, replace label: "d83_1b. what was the major constraints for d83_text"
var_label(HouseholdLaos$d83_1b) <- "d83_1b. what was the major constraints for d83_text"
#C802 = OK, replace label: "d83_1b_oth. specify other constrain for d83_text", (the answer is still in Lao, Ky will maybe solve it)
var_label(HouseholdLaos$d83_1b_oth) <- "d83_1b_oth. specify other constrain for d83_text"
#C803 = REMOVE (Text from the questionnaire)
#C804 = FACT, (bin)
#C805 = OK, High values (>1000)
#C806 = FACT, (bin)
#C807 = OK, High values (>1000)
#C808 = FACT, (bin)
#C809 = OK
#C810 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C811 = FACT, replace label: "Title deed"
var_label(HouseholdLaos$d11_41) <- "Title deed"
#C812 = FACT, replace label: "Certificate of customary tenure"
var_label(HouseholdLaos$d11_42) <- "Certificate of customary tenure"
#C813 = FACT, replace label: "Certificate of occupancy"
var_label(HouseholdLaos$d11_43) <- "Certificate of occupancy"
#C814 = FACT, replace label: "Registered will or registered certificate of hereditary acquisition"
var_label(HouseholdLaos$d11_44) <- "Registered will or registered certificate of hereditary acquisition"
#C815 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$d11_488) <- "Do not know"
#C816 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$d11_499) <- "Other"
#C817 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C818 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C819 = FACT, replace label: "Rainwater collection/conservation"
var_label(HouseholdLaos$d121) <- "Rainwater collection/conservation"
#C820 = FACT, replace label: "Greywater recycling"
var_label(HouseholdLaos$d122) <- "Greywater recycling"
#C821 = FACT, replace label: "Ponds (for water conservation)"
var_label(HouseholdLaos$d123) <- "Ponds (for water conservation)"
#C822 = FACT, replace label: "Terraces building"
var_label(HouseholdLaos$d124) <- "Terraces building"
#C823 = FACT, replace label: "Swales digging"
var_label(HouseholdLaos$d125) <- "Swales digging"
#C824 = FACT, empty, replace label: "Land levelling"
var_label(HouseholdLaos$d126) <- "Land levelling"
#C825 = FACT, empty, replace label: "Mulching"
var_label(HouseholdLaos$d127) <- "Mulching"
#C826 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$d1299) <- "Other"
#C827 = FACT, empty, replace label: "Do not know"
var_label(HouseholdLaos$d1288) <- "Do not know"
#C828 = FACT, empty, replace label: "No water conservation practice"
var_label(HouseholdLaos$d120) <- "No water conservation practice"
#C829 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C830 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C831 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d131_11) <- "Lowland crop n1"
#C832 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d131_12) <- "Lowland crop n2"
#C833 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d131_13) <- "Lowland crop n3"
#C834 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d131_14) <- "Lowland crop n4"
#C835 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d131_15) <- "Lowland crop n5"
#C836 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d131_16) <- "Upland crop n1"
#C837 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d131_17) <- "Upland crop n2"
#C838 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d131_18) <- "Upland crop n3"
#C839 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d131_19) <- "Upland crop n4"
#C840 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d131_110) <- "Upland crop n5"
#C841 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d131_111) <- "Upland crop n6"
#C842 = FACT, (table of correspondence), see choices below
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C843 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C844 = CHAR, (empty), (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C845 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d132_11) <- "Lowland crop n1"
#C846 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d132_12) <- "Lowland crop n2"
#C847 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d132_13) <- "Lowland crop n3"
#C848 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d132_14) <- "Lowland crop n4"
#C849 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d132_15) <- "Lowland crop n5"
#C850 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d132_16) <- "Upland crop n1"
#C851 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d132_17) <- "Upland crop n2"
#C852 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d132_18) <- "Upland crop n3"
#C853 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d132_19) <- "Upland crop n4"
#C854 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d132_110) <- "Upland crop n5"
#C855 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d132_111) <- "Upland crop n6"
#C856 = FACT, (empty), (table of correspondence)
#Other
#C857 = OK, (empty)
#C858 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C859 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d133_11) <- "Lowland crop n1"
#C860 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d133_12) <- "Lowland crop n2"
#C861 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d133_13) <- "Lowland crop n3"
#C862 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d133_14) <- "Lowland crop n4"
#C863 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d133_15) <- "Lowland crop n5"
#C864 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d133_16) <- "Upland crop n1"
#C865 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d133_17) <- "Upland crop n2"
#C866 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d133_18) <- "Upland crop n3"
#C867 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d133_19) <- "Upland crop n4"
#C868 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d133_110) <- "Upland crop n5"
#C869 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d133_111) <- "Upland crop n6"
#C870 = FACT, (table of correspondence)
#C871 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C872 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C873 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d134_11) <- "Lowland crop n1"
#C874 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d134_12) <- "Lowland crop n2"
#C875 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d134_13) <- "Lowland crop n3"
#C876 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d134_14) <- "Lowland crop n4"
#C877 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d134_15) <- "Lowland crop n5"
#C878 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d134_16) <- "Upland crop n1"
#C879 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d134_17) <- "Upland crop n2"
#C880 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d134_18) <- "Upland crop n3"
#C881 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d134_19) <- "Upland crop n4"
#C882 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d134_110) <- "Upland crop n5"
#C883 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d134_111) <- "Upland crop n6"
#C884 = FACT, (table of correspondence)
#C885 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C886 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C887 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d135_11) <- "Lowland crop n1"
#C888 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d135_12) <- "Lowland crop n2"
#C889 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d135_13) <- "Lowland crop n3"
#C890 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d135_14) <- "Lowland crop n4"
#C891 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d135_15) <- "Lowland crop n5"
#C892 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d135_16) <- "Upland crop n1"
#C893 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d135_17) <- "Upland crop n2"
#C894 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d135_18) <- "Upland crop n3"
#C895 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d135_19) <- "Upland crop n4"
#C896 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d135_110) <- "Upland crop n5"
#C897 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d135_111) <- "Upland crop n6"
#C898 = FACT, (table of correspondence)
#C899 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C900 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C901 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d136_11) <- "Lowland crop n1"
#C902 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d136_12) <- "Lowland crop n2"
#C903 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d136_13) <- "Lowland crop n3"
#C904 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d136_14) <- "Lowland crop n4"
#C905 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d136_15) <- "Lowland crop n5"
#C906 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d136_16) <- "Upland crop n1"
#C907 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d136_17) <- "Upland crop n2"
#C908 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d136_18) <- "Upland crop n3"
#C909 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d136_19) <- "Upland crop n4"
#C910 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d136_110) <- "Upland crop n5"
#C911 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d136_111) <- "Upland crop n6"
#C912 = FACT, (table of correspondence)
#C913 = OK, (empty)
#C914 = CHAR, (empty), (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C915 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d137_11) <- "Lowland crop n1"
#C916 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d137_12) <- "Lowland crop n2"
#C917 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d137_13) <- "Lowland crop n3"
#C918 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d137_14) <- "Lowland crop n4"
#C919 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d137_15) <- "Lowland crop n5"
#C920 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d137_16) <- "Upland crop n1"
#C921 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d137_17) <- "Upland crop n2"
#C922 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d137_18) <- "Upland crop n3"
#C923 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d137_19) <- "Upland crop n4"
#C924 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d137_110) <- "Upland crop n5"
#C925 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d137_111) <- "Upland crop n6"
#C926 = FACT, (empty), (table of correspondence)
#C927 = OK, (empty)
#C928 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C929 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d138_11) <- "Lowland crop n1"
#C930 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d138_12) <- "Lowland crop n2"
#C931 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d138_13) <- "Lowland crop n3"
#C932 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d138_14) <- "Lowland crop n4"
#C933 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d138_15) <- "Lowland crop n5"
#C934 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d138_16) <- "Upland crop n1"
#C935 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d138_17) <- "Upland crop n2"
#C936 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d138_18) <- "Upland crop n3"
#C937 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d138_19) <- "Upland crop n4"
#C938 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d138_110) <- "Upland crop n5"
#C939 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d138_111) <- "Upland crop n6"
#C940 = FACT, (table of correspondence)
#C941 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C942 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C943 = FACT, replace label: "No soil conservation practice"
var_label(HouseholdLaos$d140) <- "No soil conservation practice"
#C944 = FACT, replace label: "Sowing in contour lines"
var_label(HouseholdLaos$d141) <- "Sowing in contour lines"
#C945 = FACT, replace label: "Natural or planted grass strips"
var_label(HouseholdLaos$d142) <- "Natural or planted grass strips"
#C946 = FACT, replace label: "Trees conservation in agricultural plots"
var_label(HouseholdLaos$d143) <- "Trees conservation in agricultural plots"
#C947 = FACT, replace label: "Agroforestry (trees + crops)"
var_label(HouseholdLaos$d144) <- "Agroforestry (trees + crops)"
#C948 = FACT, empty, replace label: "Crop residues maintained to cover the soil"
var_label(HouseholdLaos$d145) <- "Crop residues maintained to cover the soil"
#C949 = FACT, empty, replace label: "Use of cover crops"
var_label(HouseholdLaos$d146) <- "Use of cover crops"
#C950 = FACT, empty, replace label: "Reduced to no-tillage"
var_label(HouseholdLaos$d147) <- "Reduced to no-tillage"
#C951 = FACT, empty, replace label: "Other"
var_label(HouseholdLaos$d1499) <- "Other"
#C952 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C953 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C954 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d151_11) <- "Lowland crop n1"
#C955 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d151_12) <- "Lowland crop n2"
#C956 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d151_13) <- "Lowland crop n3"
#C957 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d151_14) <- "Lowland crop n4"
#C958 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d151_15) <- "Lowland crop n5"
#C959 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d151_16) <- "Upland crop n1"
#C960 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d151_17) <- "Upland crop n2"
#C961 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d151_18) <- "Upland crop n3"
#C962 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d151_19) <- "Upland crop n4"
#C963 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d151_110) <- "Upland crop n5"
#C964 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d151_111) <- "Upland crop n6"
#C965 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C966 = OK, (empty)
#C967 = CHAR, (empty), (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C968 = FACT, (empty),replace label: "Lowland crop n1"
var_label(HouseholdLaos$d152_11) <- "Lowland crop n1"
#C969 = FACT, (empty),replace label: "Lowland crop n2"
var_label(HouseholdLaos$d152_12) <- "Lowland crop n2"
#C970 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d152_13) <- "Lowland crop n3"
#C971 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d152_14) <- "Lowland crop n4"
#C972 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d152_15) <- "Lowland crop n5"
#C973 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d152_16) <- "Upland crop n1"
#C974 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d152_17) <- "Upland crop n2"
#C975 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d152_18) <- "Upland crop n3"
#C976 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d152_19) <- "Upland crop n4"
#C977 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d152_110) <- "Upland crop n5"
#C978 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d152_111) <- "Upland crop n6"
#C979 = FACT, (empty), (table of correspondence)
#C980 = OK, (empty)
#C981 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C982 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d153_11) <- "Lowland crop n1"
#C983 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d153_12) <- "Lowland crop n2"
#C984 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d153_13) <- "Lowland crop n3"
#C985 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d153_14) <- "Lowland crop n4"
#C986 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d153_15) <- "Lowland crop n5"
#C987 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d153_16) <- "Upland crop n1"
#C988 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d153_17) <- "Upland crop n2"
#C989 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d153_18) <- "Upland crop n3"
#C990 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d153_19) <- "Upland crop n4"
#C991 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d153_110) <- "Upland crop n5"
#C992 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d153_111) <- "Upland crop n6"
#C993 = FACT, (table of correspondence)
#C994 = OK, (empty)
#C995 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C996 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d154_11) <- "Lowland crop n1"
#C997 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d154_12) <- "Lowland crop n2"
#C998 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d154_13) <- "Lowland crop n3"
#C999 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d154_14) <- "Lowland crop n4"
#C1000 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d154_15) <- "Lowland crop n5"
#C1001 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d154_16) <- "Upland crop n1"
#C1002 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d154_17) <- "Upland crop n2"
#C1003 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d154_18) <- "Upland crop n3"
#C1004 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d154_19) <- "Upland crop n4"
#C1005 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d154_110) <- "Upland crop n5"
#C1006 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d154_111) <- "Upland crop n6"
#C1007 = FACT, (table of correspondence)
#C1008 = OK, (empty)
#C1009 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1010 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d155_11) <- "Lowland crop n1"
#C1011 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d155_12) <- "Lowland crop n2"
#C1012 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d155_13) <- "Lowland crop n3"
#C1013 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d155_14) <- "Lowland crop n4"
#C1014 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d155_15) <- "Lowland crop n5"
#C1015 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d155_16) <- "Upland crop n1"
#C1016 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d155_17) <- "Upland crop n2"
#C1017 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d155_18) <- "Upland crop n3"
#C1018 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d155_19) <- "Upland crop n4"
#C1019 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d155_110) <- "Upland crop n5"
#C1020 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d155_111) <- "Upland crop n6"
#C1021 = FACT, (table of correspondence)
#C1022 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1023 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1024 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d156_11) <- "Lowland crop n1"
#C1025 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d156_12) <- "Lowland crop n2"
#C1026 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d156_13) <- "Lowland crop n3"
#C1027 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d156_14) <- "Lowland crop n4"
#C1028 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d156_15) <- "Lowland crop n5"
#C1029 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d156_16) <- "Upland crop n1"
#C1030 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d156_17) <- "Upland crop n2"
#C1031 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d156_18) <- "Upland crop n3"
#C1032 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d156_19) <- "Upland crop n4"
#C1033 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d156_110) <- "Upland crop n5"
#C1034 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d156_111) <- "Upland crop n6"
#C1035 = FACT, (table of correspondence)
#C1036 = OK, (empty)
#C1037 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1038 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d157_11) <- "Lowland crop n1"
#C1039 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d157_12) <- "Lowland crop n2"
#C1040 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d157_13) <- "Lowland crop n3"
#C1041 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d157_14) <- "Lowland crop n4"
#C1042 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d157_15) <- "Lowland crop n5"
#C1043 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d157_16) <- "Upland crop n1"
#C1044 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d157_17) <- "Upland crop n2"
#C1045 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d157_18) <- "Upland crop n3"
#C1046 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d157_19) <- "Upland crop n4"
#C1047 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d157_110) <- "Upland crop n5"
#C1048 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d157_111) <- "Upland crop n6"
#C1049 = FACT, (table of correspondence)
#C1050 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1051 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#replace label: "d158_1.  for which crop(s) do you use d14_oth"
var_label(HouseholdLaos$d158_1) <- "d158_1.  for which crop(s) do you use d14_oth"
#C1052 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d158_11) <- "Lowland crop n1"
#C1053 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d158_12) <- "Lowland crop n2"
#C1054 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d158_13) <- "Lowland crop n3"
#C1055 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d158_14) <- "Lowland crop n4"
#C1056 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d158_15) <- "Lowland crop n5"
#C1057 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d158_16) <- "Upland crop n1"
#C1058 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d158_17) <- "Upland crop n2"
#C1059 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d158_18) <- "Upland crop n3"
#C1060 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d158_19) <- "Upland crop n4"
#C1061 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d158_110) <- "Upland crop n5"
#C1062 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d158_111) <- "Upland crop n6"
#C1063 = FACT, (table of correspondence), replace label: "d158_2. what was your main motivation to implement d14_oth"
var_label(HouseholdLaos$d158_2) <- "d158_2. what was your main motivation to implement d14_oth"
#C1064 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1065 = FACT, (table of correspondence), (see choices below),
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1066 = FACT, (table of correspondence), (0 = "no", 1 = "yes", 88 = "Do not know"),
#C1067 = FACT, (see choices below), (Useless answer-MultipleCombined, use the following columns)
#Animal manure
#Compost (heap)
#Bokashi (fermented organic matter)
#Legume-based green manure
#Pulses in association and/or rotation with main crop
#Cover crops in association and/or rotation with main crop
#Biochar
#Crop residue maintenance
#Recycling crop waste
#Ramial Wood Chip (RWC) or other wood chips
#Organic agro-industrial waste
#Other methods
#C1068 = FACT,replace label: "Animal manure"
var_label(HouseholdLaos$d181) <- "Animal manure"
#C1069 = FACT, replace label: "Compost (heap)"
var_label(HouseholdLaos$d182) <- "Compost (heap)"
#C1070 = FACT, replace label: "Bokashi (fermented organic matter)"
var_label(HouseholdLaos$d183) <- "Bokashi (fermented organic matter)"
#C1071 = FACT, replace label: "Legume-based green manure"
var_label(HouseholdLaos$d184) <- "Legume-based green manure"
#C1072 = FACT, replace label: "Pulses in association and/or rotation with main crop"
var_label(HouseholdLaos$d185) <- "Pulses in association and/or rotation with main crop"
#C1073 = FACT, replace label: "Cover crops in association and/or rotation with main crop"
var_label(HouseholdLaos$d186) <- "Cover crops in association and/or rotation with main crop"
#C1074 = FACT, replace label: "Biochar"
var_label(HouseholdLaos$d187) <- "Biochar"
#C1075 = FACT, replace label: "Crop residue maintenance"
var_label(HouseholdLaos$d188) <- "Crop residue maintenance"
#C1076 = FACT, replace label: "Recycling crop waste"
var_label(HouseholdLaos$d189) <- "Recycling crop waste"
#C1077 = FACT, replace label: "Ramial Wood Chip (RWC) or other wood chips"
var_label(HouseholdLaos$d1810) <- "Ramial Wood Chip (RWC) or other wood chips"
#C1078 = FACT, replace label: "Organic agro-industrial waste"
var_label(HouseholdLaos$d1811) <- "Organic agro-industrial waste"
#C1079 = FACT, replace label: "Other methods"
var_label(HouseholdLaos$d1899) <- "Other methods"
#C1080 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1081 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_11. for which crop(s) do you use animal manure?"
var_label(HouseholdLaos$d18_11) <- "d18_11. for which crop(s) do you use animal manure?"
#C1082 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_111) <- "Lowland crop n1"
#C1083 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_112) <- "Lowland crop n2"
#C1084 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_113) <- "Lowland crop n3"
#C1085 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_114) <- "Lowland crop n4"
#C1086 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_115) <- "Lowland crop n5"
#C1087 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_116) <- "Upland crop n1"
#C1088 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_117) <- "Upland crop n2"
#C1089 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_118) <- "Upland crop n3"
#C1090 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_119) <- "Upland crop n4"
#C1091 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_1110) <- "Upland crop n5"
#C1092 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_1111) <- "Upland crop n6"
#C1093 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1094 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1095 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1096 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_211) <- "Lowland crop n1"
#C1097 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_212) <- "Lowland crop n2"
#C1098 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_213) <- "Lowland crop n3"
#C1099 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_214) <- "Lowland crop n4"
#C1100 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_215) <- "Lowland crop n5"
#C1101 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_216) <- "Upland crop n1"
#C1102 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_217) <- "Upland crop n2"
#C1103 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_218) <- "Upland crop n3"
#C1104 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_219) <- "Upland crop n4"
#C1105 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_2110) <- "Upland crop n5"
#C1106 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_2111) <- "Upland crop n6"
#C1107 = FACT, (table of correspondence)
#C1108 = OK, (empty)
#C1109 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1110 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_311) <- "Lowland crop n1"
#C1111 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_312) <- "Lowland crop n2"
#C1112 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_313) <- "Lowland crop n3"
#C1113 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_314) <- "Lowland crop n4"
#C1114 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_315) <- "Lowland crop n5"
#C1115 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_316) <- "Upland crop n1"
#C1116 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_317) <- "Upland crop n2"
#C1117 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_318) <- "Upland crop n3"
#C1118 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_319) <- "Upland crop n4"
#C1119 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_3110) <- "Upland crop n5"
#C1120 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_3111) <- "Upland crop n6"
#C1121 = FACT, (table of correspondence)
#C1122 = OK, (empty)
#C1123 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1124 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_411) <- "Lowland crop n1"
#C1125 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_412) <- "Lowland crop n2"
#C1126 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_413) <- "Lowland crop n3"
#C1127 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_414) <- "Lowland crop n4"
#C1128 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_415) <- "Lowland crop n5"
#C1129 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_416) <- "Upland crop n1"
#C1130 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_417) <- "Upland crop n2"
#C1131 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_418) <- "Upland crop n3"
#C1132 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_419) <- "Upland crop n4"
#C1133 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_4110) <- "Upland crop n5"
#C1134 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_4111) <- "Upland crop n6"
#C1135 = FACT, (table of correspondence)
#C1136 = OK, (empty)
#C1137 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1138 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_511) <- "Lowland crop n1"
#C1139 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_512) <- "Lowland crop n2"
#C1140 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_513) <- "Lowland crop n3"
#C1141 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_514) <- "Lowland crop n4"
#C1142 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_515) <- "Lowland crop n5"
#C1143 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_516) <- "Upland crop n1"
#C1144 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_517) <- "Upland crop n2"
#C1145 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_518) <- "Upland crop n3"
#C1146 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_519) <- "Upland crop n4"
#C1147 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_5110) <- "Upland crop n5"
#C1148 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_5111) <- "Upland crop n6"
#C1149 = FACT, (table of correspondence)
#C1150 = OK, (empty)
#C1151 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1152 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_611) <- "Lowland crop n1"
#C1153 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_612) <- "Lowland crop n2"
#C1154 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_613) <- "Lowland crop n3"
#C1155 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_614) <- "Lowland crop n4"
#C1156 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_615) <- "Lowland crop n5"
#C1157 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_616) <- "Upland crop n1"
#C1158 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_617) <- "Upland crop n2"
#C1159 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_618) <- "Upland crop n3"
#C1160 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_619) <- "Upland crop n4"
#C1161 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_6110) <- "Upland crop n5"
#C1162 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_6111) <- "Upland crop n6"
#C1163 = FACT,  (empty), (table of correspondence)
#C1164 = OK, (empty)
#C1165 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1166 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_711) <- "Lowland crop n1"
#C1167 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_712) <- "Lowland crop n2"
#C1168 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_713) <- "Lowland crop n3"
#C1169 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_714) <- "Lowland crop n4"
#C1170 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_715) <- "Lowland crop n5"
#C1171 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_716) <- "Upland crop n1"
#C1172 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_717) <- "Upland crop n2"
#C1173 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_718) <- "Upland crop n3"
#C1174 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_719) <- "Upland crop n4"
#C1175 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_7110) <- "Upland crop n5"
#C1176 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_7111) <- "Upland crop n6"
#C1177 = FACT, (table of correspondence)
#C1178 = OK, (empty)
#C1179 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1180 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_811) <- "Lowland crop n1"
#C1181 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_812) <- "Lowland crop n2"
#C1182 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_813) <- "Lowland crop n3"
#C1183 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_814) <- "Lowland crop n4"
#C1184 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_815) <- "Lowland crop n5"
#C1185 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_816) <- "Upland crop n1"
#C1186 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_817) <- "Upland crop n2"
#C1187 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_818) <- "Upland crop n3"
#C1188 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_819) <- "Upland crop n4"
#C1189 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_8110) <- "Upland crop n5"
#C1190 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_8111) <- "Upland crop n6"
#C1191 = FACT, (table of correspondence)
#C1192 = OK, (empty)
#C1193 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1194 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_911) <- "Lowland crop n1"
#C1195 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_912) <- "Lowland crop n2"
#C1196 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_913) <- "Lowland crop n3"
#C1197 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_914) <- "Lowland crop n4"
#C1198 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_915) <- "Lowland crop n5"
#C1199 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_916) <- "Upland crop n1"
#C1200 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_917) <- "Upland crop n2"
#C1201 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_918) <- "Upland crop n3"
#C1202 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_919) <- "Upland crop n4"
#C1203 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_9110) <- "Upland crop n5"
#C1204 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_9111) <- "Upland crop n6"
#C1205 = FACT, (table of correspondence)
#C1206 = OK, (empty)
#C1207 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1208 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_1011) <- "Lowland crop n1"
#C1209 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_1012) <- "Lowland crop n2"
#C1210 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_1013) <- "Lowland crop n3"
#C1211 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_1014) <- "Lowland crop n4"
#C1212 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_1015) <- "Lowland crop n5"
#C1213 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_1016) <- "Upland crop n1"
#C1214 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_1017) <- "Upland crop n2"
#C1215 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_1018) <- "Upland crop n3"
#C1216 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_1019) <- "Upland crop n4"
#C1217 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_10110) <- "Upland crop n5"
#C1218 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_10111) <- "Upland crop n6"
#C1219 = FACT, (empty), (table of correspondence)
#C1220 = OK, (empty)
#C1221 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_111_a. for which crop(s) do you use organic agro-industrial waste?"
var_label(HouseholdLaos$d18_111_a) <- "d18_111_a. for which crop(s) do you use organic agro-industrial waste?"
#C1222 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_1111_a) <- "Lowland crop n1"
#C1223 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_1112) <- "Lowland crop n2"
#C1224 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_1113) <- "Lowland crop n3"
#C1225 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_1114) <- "Lowland crop n4"
#C1226 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_1115) <- "Lowland crop n5"
#C1227 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_1116) <- "Upland crop n1"
#C1228 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_1117) <- "Upland crop n2"
#C1229 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_1118) <- "Upland crop n3"
#C1230 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_1119) <- "Upland crop n4"
#C1231 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_11110) <- "Upland crop n5"
#C1232 = FACT,  (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_11111) <- "Upland crop n6"
#C1233 = FACT,  (empty), (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdLaos$d18_112_a) <- "d18_112. What was your main motivation to implement this practice?"
#C1234 = OK, (empty)
#C1235 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_991. for which crop(s) do you use d18_oth ?"
var_label(HouseholdLaos$d18_991) <- "d18_991. for which crop(s) do you use d18_oth?"
#C1236 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d18_9911) <- "Lowland crop n1"
#C1237 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d18_9912) <- "Lowland crop n2"
#C1238 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d18_9913) <- "Lowland crop n3"
#C1239 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d18_9914) <- "Lowland crop n4"
#C1240 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d18_9915) <- "Lowland crop n5"
#C1241 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d18_9916) <- "Upland crop n1"
#C1242 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d18_9917) <- "Upland crop n2"
#C1243 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d18_9918) <- "Upland crop n3"
#C1244 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d18_9919) <- "Upland crop n4"
#C1245 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d18_99110) <- "Upland crop n5"
#C1246 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d18_99111) <- "Upland crop n6"
#C1247 = FACT, (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdLaos$d18_992) <- "d18_112. What was your main motivation to implement this practice?"
#C1248 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1249 = FACT, (table of correspondence), (see option below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1250 = FACT, (table of correspondence), (0 = no, 1 = yes, 88 = I don't know)
#C1251 = CHAR, (see choices below), (Useless answer-MultipleCombined, use the following columns)
#Crop rotation / intercropping
#Cover crops
#Mulching / shading
#Sowing date / rate / depth
#Crop spatial arrangement
#Seed cleaning before sowing
#Cultivar choice
#Crop mixtures
#Nutrient placement
#Patch/ban spraying
#Bioherbicide
#Mowing / slashing
#Grazing
#Post harvest weed seed destruction in field
#Any other methods
#C1252 = FACT,replace label: "Crop rotation / intercropping"
var_label(HouseholdLaos$d211) <- "Crop rotation / intercropping"
#C1253 = FACT, replace label: "Cover crops"
var_label(HouseholdLaos$d212) <- "Cover crops"
#C1254 = FACT, replace label: "Mulching / shading"
var_label(HouseholdLaos$d213) <- "Mulching / shading"
#C1255 = FACT, replace label: "Sowing date / rate / depth"
var_label(HouseholdLaos$d214) <- "Sowing date / rate / depth"
#C1256 = FACT, replace label: "Crop spatial arrangement"
var_label(HouseholdLaos$d215) <- "Crop spatial arrangement"
#C1257 = FACT, replace label: "Seed cleaning before sowing"
var_label(HouseholdLaos$d216) <- "Seed cleaning before sowing"
#C1258 = FACT, replace label: "Cultivar choice"
var_label(HouseholdLaos$d217) <- "Cultivar choice"
#C1259 = FACT, replace label: "Crop mixtures"
var_label(HouseholdLaos$d218) <- "Crop mixtures"
#C1260 = FACT, replace label: "Nutrient placement"
var_label(HouseholdLaos$d219) <- "Nutrient placement"
#C1261 = FACT, replace label: "Patch/ban spraying"
var_label(HouseholdLaos$d2110) <- "Patch/ban spraying"
#C1262 = FACT, replace label: "Bioherbicide"
var_label(HouseholdLaos$d2111) <- "Bioherbicide"
#C1263 = FACT, replace label: "Mowing / slashing"
var_label(HouseholdLaos$d2112) <- "Mowing / slashing"
#C1264 = FACT, replace label: "Grazing"
var_label(HouseholdLaos$d2113) <- "Grazing"
#C1265 = FACT, replace label: "Post harvest weed seed destruction in field"
var_label(HouseholdLaos$d2114) <- "Post harvest weed seed destruction in field"
#C1266 = FACT, replace label: "Any other methods"
var_label(HouseholdLaos$d2199) <- "Any other methods"
#C1267 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1268 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1269 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_121) <- "Lowland crop n1"
#C1270 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_122) <- "Lowland crop n2"
#C1271 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_123) <- "Lowland crop n3"
#C1272 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_124) <- "Lowland crop n4"
#C1273 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_125) <- "Lowland crop n5"
#C1274 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_126) <- "Upland crop n1"
#C1275 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_127) <- "Upland crop n2"
#C1276 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_128) <- "Upland crop n3"
#C1277 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_129) <- "Upland crop n4"
#C1278 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_1210) <- "Upland crop n5"
#C1279 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_1211) <- "Upland crop n6"
#C1280 = FACT, (table of correspondence)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1281 = OK, (empty)
#C1282 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1283 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_221) <- "Lowland crop n1"
#C1284 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_222) <- "Lowland crop n2"
#C1285 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_223) <- "Lowland crop n3"
#C1286 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_224) <- "Lowland crop n4"
#C1287 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_225) <- "Lowland crop n5"
#C1288 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_226) <- "Upland crop n1"
#C1289 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_227) <- "Upland crop n2"
#C1290 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_228) <- "Upland crop n3"
#C1291 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_229) <- "Upland crop n4"
#C1292 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_2210) <- "Upland crop n5"
#C1293 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_2211) <- "Upland crop n6"
#C1294 = FACT, (table of correspondence)
#C1295 = OK, (empty)
#C1296 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1297 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_321) <- "Lowland crop n1"
#C1298 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_322) <- "Lowland crop n2"
#C1299 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_323) <- "Lowland crop n3"
#C1300 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_324) <- "Lowland crop n4"
#C1301 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_325) <- "Lowland crop n5"
#C1302 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_326) <- "Upland crop n1"
#C1303 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_327) <- "Upland crop n2"
#C1304 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_328) <- "Upland crop n3"
#C1305 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_329) <- "Upland crop n4"
#C1306 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_3210) <- "Upland crop n5"
#C1307 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_3211) <- "Upland crop n6"
#C1308 = FACT, (table of correspondence)
#C1309 = OK, (empty),
#C1310 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1311 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_421) <- "Lowland crop n1"
#C1312 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_422) <- "Lowland crop n2"
#C1313 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_423) <- "Lowland crop n3"
#C1314 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_424) <- "Lowland crop n4"
#C1315 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_425) <- "Lowland crop n5"
#C1316 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_426) <- "Upland crop n1"
#C1317 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_427) <- "Upland crop n2"
#C1318 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_428) <- "Upland crop n3"
#C1319 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_429) <- "Upland crop n4"
#C1320 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_4210) <- "Upland crop n5"
#C1321 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_4211) <- "Upland crop n6"
#C1322 = FACT, (empty), (table of correspondence)
#C1323 = OK, (empty)
#C1324 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1325 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_521) <- "Lowland crop n1"
#C1326 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_522) <- "Lowland crop n2"
#C1327 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_523) <- "Lowland crop n3"
#C1328 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_524) <- "Lowland crop n4"
#C1329 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_525) <- "Lowland crop n5"
#C1330 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_526) <- "Upland crop n1"
#C1331 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_527) <- "Upland crop n2"
#C1332 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_528) <- "Upland crop n3"
#C1333 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_529) <- "Upland crop n4"
#C1334 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_5210) <- "Upland crop n5"
#C1335 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_5211) <- "Upland crop n6"
#C1336 = FACT, (empty), (table of correspondence)
#C1337 = OK, (empty)
#C1338 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1339 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_621) <- "Lowland crop n1"
#C1340 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_622) <- "Lowland crop n2"
#C1341 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_623) <- "Lowland crop n3"
#C1342 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_624) <- "Lowland crop n4"
#C1343 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_625) <- "Lowland crop n5"
#C1344 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_626) <- "Upland crop n1"
#C1345 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_627) <- "Upland crop n2"
#C1346 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_628) <- "Upland crop n3"
#C1347 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_629) <- "Upland crop n4"
#C1348 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_6210) <- "Upland crop n5"
#C1349 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_6211) <- "Upland crop n6"
#C1350 = FACT, (empty), (table of correspondence)
#C1351 = OK, (empty)
#C1352 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1353 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_721) <- "Lowland crop n1"
#C1354 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_722) <- "Lowland crop n2"
#C1355 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_723) <- "Lowland crop n3"
#C1356 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_724) <- "Lowland crop n4"
#C1357 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_725) <- "Lowland crop n5"
#C1358 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_726) <- "Upland crop n1"
#C1359 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_727) <- "Upland crop n2"
#C1360 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_728) <- "Upland crop n3"
#C1361 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_729) <- "Upland crop n4"
#C1362 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_7210) <- "Upland crop n5"
#C1363 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_7211) <- "Upland crop n6"
#C1364 = FACT, (table of correspondence)
#C1365 = OK, (empty)
#C1366 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1367 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_821) <- "Lowland crop n1"
#C1368 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_822) <- "Lowland crop n2"
#C1369 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_823) <- "Lowland crop n3"
#C1370 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_824) <- "Lowland crop n4"
#C1371 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_825) <- "Lowland crop n5"
#C1372 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_826) <- "Upland crop n1"
#C1373 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_827) <- "Upland crop n2"
#C1374 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_828) <- "Upland crop n3"
#C1375 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_829) <- "Upland crop n4"
#C1376 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_8210) <- "Upland crop n5"
#C1377 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_8211) <- "Upland crop n6"
#C1378 = FACT, (table of correspondence)
#C1379 = OK, (empty)
#C1380 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1381 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_921) <- "Lowland crop n1"
#C1382 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_922) <- "Lowland crop n2"
#C1383 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_923) <- "Lowland crop n3"
#C1384 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_924) <- "Lowland crop n4"
#C1385 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_925) <- "Lowland crop n5"
#C1386 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_926) <- "Upland crop n1"
#C1387 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_927) <- "Upland crop n2"
#C1388 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_928) <- "Upland crop n3"
#C1389 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_929) <- "Upland crop n4"
#C1390 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_9210) <- "Upland crop n5"
#C1391 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_9211) <- "Upland crop n6"
#C1392 = FACT, (empty), (table of correspondence)
#C1393 = OK, (empty)
#C1394 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1395 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_1021) <- "Lowland crop n1"
#C1396 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_1022) <- "Lowland crop n2"
#C1397 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_1023) <- "Lowland crop n3"
#C1398 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_1024) <- "Lowland crop n4"
#C1399 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_1025) <- "Lowland crop n5"
#C1400 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_1026) <- "Upland crop n1"
#C1401 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_1027) <- "Upland crop n2"
#C1402 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_1028) <- "Upland crop n3"
#C1403 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_1029) <- "Upland crop n4"
#C1404 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_10210) <- "Upland crop n5"
#C1405 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_10211) <- "Upland crop n6"
#C1406 = FACT, (table of correspondence)
#C1407 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1408 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1409 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_1121) <- "Lowland crop n1"
#C1410 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_1122) <- "Lowland crop n2"
#C1411 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_1123) <- "Lowland crop n3"
#C1412 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_1124) <- "Lowland crop n4"
#C1413 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_1125) <- "Lowland crop n5"
#C1414 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_1126) <- "Upland crop n1"
#C1415 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_1127) <- "Upland crop n2"
#C1416 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_1128) <- "Upland crop n3"
#C1417 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_1129) <- "Upland crop n4"
#C1418 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_11210) <- "Upland crop n5"
#C1419 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_11211) <- "Upland crop n6"
#C1420 = FACT, (table of correspondence)
#C1421 = OK, (empty)
#C1422 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d21_112. for which crop(s) do you use crop Mowing / slashing"
var_label(HouseholdLaos$d21_122_a) <- "d21_122. for which crop(s) do you use crop Mowing / slashing"
#C1423 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_1221) <- "Lowland crop n1"
#C1424 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_1222) <- "Lowland crop n2"
#C1425 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_1223) <- "Lowland crop n3"
#C1426 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_1224) <- "Lowland crop n4"
#C1427 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_1225) <- "Lowland crop n5"
#C1428 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_1226) <- "Upland crop n1"
#C1429 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_1227) <- "Upland crop n2"
#C1430 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_1228) <- "Upland crop n3"
#C1431 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_1229) <- "Upland crop n4"
#C1432 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_12210) <- "Upland crop n5"
#C1433 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_12211) <- "Upland crop n6"
#C1434 = FACT, (table of correspondence), replace label: "d21_113. what was your main motivation to implement crop Mowing / slashing"
var_label(HouseholdLaos$d21_123_a) <- "d21_123. what was your main motivation to implement crop Mowing / slashing"
#C1435 = OK, (empty)
#C1436 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1437 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_1321) <- "Lowland crop n1"
#C1438 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_1322) <- "Lowland crop n2"
#C1439 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_1323) <- "Lowland crop n3"
#C1440 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_1324) <- "Lowland crop n4"
#C1441 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_1325) <- "Lowland crop n5"
#C1442 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_1326) <- "Upland crop n1"
#C1443 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_1327) <- "Upland crop n2"
#C1444 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_1328) <- "Upland crop n3"
#C1445 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_1329) <- "Upland crop n4"
#C1446 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_13210) <- "Upland crop n5"
#C1447 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_13211) <- "Upland crop n6"
#C1448 = FACT, (table of correspondence)
#C1449 = OK, (empty)
#C1450 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1451 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_1421) <- "Lowland crop n1"
#C1452 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_1422) <- "Lowland crop n2"
#C1453 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_1423) <- "Lowland crop n3"
#C1454 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_1424) <- "Lowland crop n4"
#C1455 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_1425) <- "Lowland crop n5"
#C1456 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_1426) <- "Upland crop n1"
#C1457 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_1427) <- "Upland crop n2"
#C1458 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_1428) <- "Upland crop n3"
#C1459 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_1429) <- "Upland crop n4"
#C1460 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_14210) <- "Upland crop n5"
#C1461 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_14211) <- "Upland crop n6"
#C1462 = FACT, (empty), (table of correspondence)
#C1463 = OK, (empty)
#C1464 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d21_992. for which crop(s) do you use crop d21_oth?"
var_label(HouseholdLaos$d21_992) <- "d21_992. for which crop(s) do you use crop d21_oth?"
#C1465 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d21_9921) <- "Lowland crop n1"
#C1466 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d21_9922) <- "Lowland crop n2"
#C1467 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d21_9923) <- "Lowland crop n3"
#C1468 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d21_9924) <- "Lowland crop n4"
#C1469 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d21_9925) <- "Lowland crop n5"
#C1470 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d21_9926) <- "Upland crop n1"
#C1471 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d21_9927) <- "Upland crop n2"
#C1472 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d21_9928) <- "Upland crop n3"
#C1473 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d21_9929) <- "Upland crop n4"
#C1474 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d21_99210) <- "Upland crop n5"
#C1475 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d21_99211) <- "Upland crop n6"
#C1476 = FACT, (table of correspondence), replace label: "d21_993. what was your main motivation to implement rop d21_oth practice?"
var_label(HouseholdLaos$d21_993) <- "d21_993. what was your main motivation to implement rop d21_oth practice?"
#C1477 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1478 = FACT, (table of correspondence), (see options below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1479 = FACT, (table of correspondence), (see options below)
#Synthetic herbicide without mechanical weeding
#Frequent mechanical weeding (more than three times per year) without synthetic herbicide
#Mixed management using herbicide and mechanical weeding    
#Do not know  
#C1480 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1481 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1482 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1483 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see option below)
#Crop rotation / intercropping
#Flower strips
#Hedgerows
#Soil health maintenance/improvement
#Sanitation practices (removal of damaged/infected plants and fruits)
#Planting date
#Water and nutrient management
#Cultivar choice (tolerant/resistant) / cultivar mixture
#Biopesticide / organic pesticide
#Commercial biological control agents (BCAs)
#Home-made efficient microorganism (EM)
#Commercial efficient microorganism (EM)
#Pheromone traps
#Protein baits
#Any other methods
#C1484 = FACT, replace label: "Crop rotation / intercropping"
var_label(HouseholdLaos$d271) <- "Crop rotation / intercropping"
#C1485 = FACT, replace label: "Flower strips"
var_label(HouseholdLaos$d272) <- "Flower strips"
#C1486 = FACT, replace label: "Hedgerows"
var_label(HouseholdLaos$d273) <- "Hedgerows"
#C1487 = FACT, replace label: "Soil health maintenance/improvement"
var_label(HouseholdLaos$d274) <- "Soil health maintenance/improvement"
#C1488 = FACT, replace label: "Sanitation practices (removal of damaged/infected plants and fruits)"
var_label(HouseholdLaos$d275) <- "Sanitation practices (removal of damaged/infected plants and fruits)"
#C1489 = FACT, replace label: "Planting date"
var_label(HouseholdLaos$d276) <- "Planting date"
#C1490 = FACT, replace label: "Water and nutrient management"
var_label(HouseholdLaos$d277) <- "Water and nutrient management"
#C1491 = FACT, replace label: "Cultivar choice (tolerant/resistant) / cultivar mixture"
var_label(HouseholdLaos$d278) <- "Cultivar choice (tolerant/resistant) / cultivar mixture"
#C1492 = FACT, replace label: "Biopesticide / organic pesticide"
var_label(HouseholdLaos$d279) <- "Biopesticide / organic pesticide"
#C1493 = FACT, replace label: "Commercial biological control agents (BCAs)"
var_label(HouseholdLaos$d2710) <- "Commercial biological control agents (BCAs)"
#C1494 = FACT, replace label: "Home-made efficient microorganism (EM)"
var_label(HouseholdLaos$d2711) <- "Home-made efficient microorganism (EM)"
#C1495 = FACT, replace label: "Commercial efficient microorganism (EM)"
var_label(HouseholdLaos$d2712) <- "Commercial efficient microorganism (EM)"
#C1496 = FACT, replace label: "Pheromone traps"
var_label(HouseholdLaos$d2713) <- "Pheromone traps"
#C1497 = FACT, replace label: "Protein baits"
var_label(HouseholdLaos$d2714) <- "Protein baits"
#C1498 = FACT, replace label: "Any other methods"
var_label(HouseholdLaos$d2799) <- "Any other methods"
#C1499 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1500 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1501 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_111) <- "Lowland crop n1"
#C1502 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_112) <- "Lowland crop n2"
#C1503 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_113) <- "Lowland crop n3"
#C1504 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_114) <- "Lowland crop n4"
#C1505 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_115) <- "Lowland crop n5"
#C1506 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_116) <- "Upland crop n1"
#C1507 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_117) <- "Upland crop n2"
#C1508 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_118) <- "Upland crop n3"
#C1509 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_119) <- "Upland crop n4"
#C1510 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_1110) <- "Upland crop n5"
#C1511 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_1111) <- "Upland crop n6"
#C1512 = FACT, (table of correspondence)
#C1513 = OK, (empty)
#C1514 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1515 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_211) <- "Lowland crop n1"
#C1516 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_212) <- "Lowland crop n2"
#C1517 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_213) <- "Lowland crop n3"
#C1518 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_214) <- "Lowland crop n4"
#C1519 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_215) <- "Lowland crop n5"
#C1520 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_216) <- "Upland crop n1"
#C1521 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_217) <- "Upland crop n2"
#C1522 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_218) <- "Upland crop n3"
#C1523 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_219) <- "Upland crop n4"
#C1524 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_2110) <- "Upland crop n5"
#C1525 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_2111) <- "Upland crop n6"
#C1526 = FACT, (empty), (table of correspondence)
#C1527 = OK, (empty)
#C1528 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1529 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_311) <- "Lowland crop n1"
#C1530 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_312) <- "Lowland crop n2"
#C1531 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_313) <- "Lowland crop n3"
#C1532 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_314) <- "Lowland crop n4"
#C1533 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_315) <- "Lowland crop n5"
#C1534 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_316) <- "Upland crop n1"
#C1535 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_317) <- "Upland crop n2"
#C1536 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_318) <- "Upland crop n3"
#C1537 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_319) <- "Upland crop n4"
#C1538 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_3110) <- "Upland crop n5"
#C1539 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_3111) <- "Upland crop n6"
#C1540 = FACT, (empty), (table of correspondence)
#C1541 = OK, (empty)
#C1542 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1543 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_411) <- "Lowland crop n1"
#C1544 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_412) <- "Lowland crop n2"
#C1545 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_413) <- "Lowland crop n3"
#C1546 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_414) <- "Lowland crop n4"
#C1547 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_415) <- "Lowland crop n5"
#C1548 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_416) <- "Upland crop n1"
#C1549 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_417) <- "Upland crop n2"
#C1550 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_418) <- "Upland crop n3"
#C1551 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_419) <- "Upland crop n4"
#C1552 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_4110) <- "Upland crop n5"
#C1553 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_4111) <- "Upland crop n6"
#C1554 = FACT, (table of correspondence)
#C1555 = OK, (empty)
#C1556 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1557 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_511) <- "Lowland crop n1"
#C1558 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_512) <- "Lowland crop n2"
#C1559 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_513) <- "Lowland crop n3"
#C1560 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_514) <- "Lowland crop n4"
#C1561 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_515) <- "Lowland crop n5"
#C1562 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_516) <- "Upland crop n1"
#C1563 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_517) <- "Upland crop n2"
#C1564 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_518) <- "Upland crop n3"
#C1565 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_519) <- "Upland crop n4"
#C1566 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_5110) <- "Upland crop n5"
#C1567 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_5111) <- "Upland crop n6"
#C1568 = FACT, (empty), (table of correspondence)
#C1569 = OK, (empty)
#C1570 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1571 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_611) <- "Lowland crop n1"
#C1572 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_612) <- "Lowland crop n2"
#C1573 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_613) <- "Lowland crop n3"
#C1574 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_614) <- "Lowland crop n4"
#C1575 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_615) <- "Lowland crop n5"
#C1576 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_616) <- "Upland crop n1"
#C1577 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_617) <- "Upland crop n2"
#C1578 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_618) <- "Upland crop n3"
#C1579 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_619) <- "Upland crop n4"
#C1580 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_6110) <- "Upland crop n5"
#C1581 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_6111) <- "Upland crop n6"
#C1582 = FACT, (empty), (table of correspondence)
#C1583 = OK, (empty)
#C1584 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1585 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_711) <- "Lowland crop n1"
#C1586 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_712) <- "Lowland crop n2"
#C1587 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_713) <- "Lowland crop n3"
#C1588 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_714) <- "Lowland crop n4"
#C1589 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_715) <- "Lowland crop n5"
#C1590 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_716) <- "Upland crop n1"
#C1591 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_717) <- "Upland crop n2"
#C1592 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_718) <- "Upland crop n3"
#C1593 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_719) <- "Upland crop n4"
#C1594 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_7110) <- "Upland crop n5"
#C1595 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_7111) <- "Upland crop n6"
#C1596 = FACT, (empty), (table of correspondence)
#C1597 = OK, (empty)
#C1598 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1599 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_811) <- "Lowland crop n1"
#C1600 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_812) <- "Lowland crop n2"
#C1601 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_813) <- "Lowland crop n3"
#C1602 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_814) <- "Lowland crop n4"
#C1603 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_815) <- "Lowland crop n5"
#C1604 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_816) <- "Upland crop n1"
#C1605 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_817) <- "Upland crop n2"
#C1606 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_818) <- "Upland crop n3"
#C1607 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_819) <- "Upland crop n4"
#C1608 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_8110) <- "Upland crop n5"
#C1609 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_8111) <- "Upland crop n6"
#C1610 = FACT, (empty), (table of correspondence)
#C1611 = OK, (empty)
#C1612 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1613 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_911) <- "Lowland crop n1"
#C1614 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_912) <- "Lowland crop n2"
#C1615 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_913) <- "Lowland crop n3"
#C1616 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_914) <- "Lowland crop n4"
#C1617 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_915) <- "Lowland crop n5"
#C1618 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_916) <- "Upland crop n1"
#C1619 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_917) <- "Upland crop n2"
#C1620 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_918) <- "Upland crop n3"
#C1621 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_919) <- "Upland crop n4"
#C1622 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_9110) <- "Upland crop n5"
#C1623 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_9111) <- "Upland crop n6"
#C1624 = FACT, (table of correspondence)
#C1625 = OK, (empty)
#C1626 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1627 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_1011) <- "Lowland crop n1"
#C1628 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_1012) <- "Lowland crop n2"
#C1629 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_1013) <- "Lowland crop n3"
#C1630 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_1014) <- "Lowland crop n4"
#C1631 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_1015) <- "Lowland crop n5"
#C1632 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_1016) <- "Upland crop n1"
#C1633 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_1017) <- "Upland crop n2"
#C1634 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_1018) <- "Upland crop n3"
#C1635 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_1019) <- "Upland crop n4"
#C1636 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_10110) <- "Upland crop n5"
#C1637 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_10111) <- "Upland crop n6"
#C1638 = FACT, (empty), (table of correspondence)
#C1639 = OK, (empty)
#C1640 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns), replace label: "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
var_label(HouseholdLaos$d27_111_a) <- "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
#C1641 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_1111_a) <- "Lowland crop n1"
#C1642 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_1112) <- "Lowland crop n2"
#C1643 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_1113) <- "Lowland crop n3"
#C1644 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_1114) <- "Lowland crop n4"
#C1645 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_1115) <- "Lowland crop n5"
#C1646 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_1116) <- "Upland crop n1"
#C1647 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_1117) <- "Upland crop n2"
#C1648 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_1118) <- "Upland crop n3"
#C1649 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_1119) <- "Upland crop n4"
#C1650 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_11110) <- "Upland crop n5"
#C1651 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_11111) <- "Upland crop n6"
#C1652 = FACT, (empty), (table of correspondence), replace label: "d27_112. If yes, what was your main motivation to implement this practice?"
var_label(HouseholdLaos$d27_112_a)  <- "d27_112. If yes, what was your main motivation to implement this practice?"
#C1653 = OK, (empty)
#C1654 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1655 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_1211) <- "Lowland crop n1"
#C1656 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_1212) <- "Lowland crop n2"
#C1657 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_1213) <- "Lowland crop n3"
#C1658 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_1214) <- "Lowland crop n4"
#C1659 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_1215) <- "Lowland crop n5"
#C1660 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_1216) <- "Upland crop n1"
#C1661 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_1217) <- "Upland crop n2"
#C1662 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_1218) <- "Upland crop n3"
#C1663 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_1219) <- "Upland crop n4"
#C1664 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_12110) <- "Upland crop n5"
#C1665 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_12111) <- "Upland crop n6"
#C1666 = FACT, (empty), (table of correspondence)
#C1667 = OK, (empty)
#C1668 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1669 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_1311) <- "Lowland crop n1"
#C1670 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_1312) <- "Lowland crop n2"
#C1671 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_1313) <- "Lowland crop n3"
#C1672 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_1314) <- "Lowland crop n4"
#C1673 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_1315) <- "Lowland crop n5"
#C1674 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_1316) <- "Upland crop n1"
#C1675 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_1317) <- "Upland crop n2"
#C1676 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_1318) <- "Upland crop n3"
#C1677 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_1319) <- "Upland crop n4"
#C1678 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_13110) <- "Upland crop n5"
#C1679 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_13111) <- "Upland crop n6"
#C1680 = FACT, (empty), (table of correspondence)
#C1681 = OK, (empty)
#C1682 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1683 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_1411) <- "Lowland crop n1"
#C1684 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_1412) <- "Lowland crop n2"
#C1685 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_1413) <- "Lowland crop n3"
#C1686 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_1414) <- "Lowland crop n4"
#C1687 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_1415) <- "Lowland crop n5"
#C1688 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_1416) <- "Upland crop n1"
#C1689 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_1417) <- "Upland crop n2"
#C1690 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_1418) <- "Upland crop n3"
#C1691 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_1419) <- "Upland crop n4"
#C1692 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_14110) <- "Upland crop n5"
#C1693 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_14111) <- "Upland crop n6"
#C1694 = FACT, (empty), (table of correspondence)
#C1695 = OK, (empty)
#C1696 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d27_991. for which crop(s) do you use d27_oth?"
var_label(HouseholdLaos$d27_991) <- "d27_991. for which crop(s) do you use d27_oth?"
#C1697 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d27_9911) <- "Lowland crop n1"
#C1698 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d27_9912) <- "Lowland crop n2"
#C1699 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d27_9913) <- "Lowland crop n3"
#C1700 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d27_9914) <- "Lowland crop n4"
#C1701 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d27_9915) <- "Lowland crop n5"
#C1702 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d27_9916) <- "Upland crop n1"
#C1703 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d27_9917) <- "Upland crop n2"
#C1704 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d27_9918) <- "Upland crop n3"
#C1705 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d27_9919) <- "Upland crop n4"
#C1706 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d27_99110) <- "Upland crop n5"
#C1707 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d27_99111) <- "Upland crop n6"
#C1708 = FACT, (table of correspondence)
#replace label: "d27_992. if yes, what was your main motivation to implement d27_oth practice"
var_label(HouseholdLaos$d27_992) <- "d27_992. if yes, what was your main motivation to implement d27_oth practice"
#C1709 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1710 = FACT, (table of correspondence), (see answers below)
#I do not know them
#They are too costly
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1711 = FACT, (table of correspondence), (see answers below)
#Synthetic insecticide and fungicide are used regularly and no other system is used
#Mixed use of synthetic and biological/natural pesticides
#Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used
#Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide
#Do not know
#C1712 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1713 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1714 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d30_21) <- "Lowland crop n1"
#C1715 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d30_22) <- "Lowland crop n2"
#C1716 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d30_23) <- "Lowland crop n3"
#C1717 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d30_24) <- "Lowland crop n4"
#C1718 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d30_25) <- "Lowland crop n5"
#C1719 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d30_26) <- "Upland crop n1"
#C1720 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d30_27) <- "Upland crop n2"
#C1721 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d30_28) <- "Upland crop n3"
#C1722 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d30_29) <- "Upland crop n4"
#C1723 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d30_210) <- "Upland crop n5"
#C1724 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d30_211) <- "Upland crop n6"
#C1725 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1726 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1727 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d30_41) <- "Lowland crop n1"
#C1728 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d30_42) <- "Lowland crop n2"
#C1729 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d30_43) <- "Lowland crop n3"
#C1730 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d30_44) <- "Lowland crop n4"
#C1731 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d30_45) <- "Lowland crop n5"
#C1732 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d30_46) <- "Upland crop n1"
#C1733 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d30_47) <- "Upland crop n2"
#C1734 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d30_48) <- "Upland crop n3"
#C1735 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d30_49) <- "Upland crop n4"
#C1736 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d30_410) <- "Upland crop n5"
#C1737 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d30_411) <- "Upland crop n6"
#C1738 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1739 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1740 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d30_61) <- "Lowland crop n1"
#C1741 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d30_62) <- "Lowland crop n2"
#C1742 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d30_63) <- "Lowland crop n3"
#C1743 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d30_64) <- "Lowland crop n4"
#C1744 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d30_65) <- "Lowland crop n5"
#C1745 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d30_66) <- "Upland crop n1"
#C1746 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d30_67) <- "Upland crop n2"
#C1747 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d30_68) <- "Upland crop n3"
#C1748 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d30_69) <- "Upland crop n4"
#C1749 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d30_610) <- "Upland crop n5"
#C1750 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d30_611) <- "Upland crop n6"
#C1751 = FACT, (table of correspondence), (bin)
#C1752 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1753 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdLaos$d32_11) <- "Lowland crop n1"
#C1754 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdLaos$d32_12) <- "Lowland crop n2"
#C1755 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdLaos$d32_13) <- "Lowland crop n3"
#C1756 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdLaos$d32_14) <- "Lowland crop n4"
#C1757 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdLaos$d32_15) <- "Lowland crop n5"
#C1758 = FACT, replace label: "Upland crop n1"
var_label(HouseholdLaos$d32_16) <- "Upland crop n1"
#C1759 = FACT, replace label: "Upland crop n2"
var_label(HouseholdLaos$d32_17) <- "Upland crop n2"
#C1760 = FACT, replace label: "Upland crop n3"
var_label(HouseholdLaos$d32_18) <- "Upland crop n3"
#C1761 = FACT, replace label: "Upland crop n4"
var_label(HouseholdLaos$d32_19) <- "Upland crop n4"
#C1762 = FACT, replace label: "Upland crop n5"
var_label(HouseholdLaos$d32_110) <- "Upland crop n5"
#C1763 = FACT, replace label: "Upland crop n6"
var_label(HouseholdLaos$d32_111) <- "Upland crop n6"
#C1764 = FACT, (table of correspondence), (see answers below)
#From the village seller
#From the cooperative 
#From a trader in town 
#From family & friends
#Do not know
#Other
#C1765 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1766 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#Do not have
#Family members 
#Hired people 
#Mutual help
#C1767 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_10) <- "Do not have"
#C1768 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_11) <- "Family members"
#C1769 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_12) <- "Hired people"
#C1770 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_13) <- "Mutual help"
#C1771 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1772 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_20) <- "Do not have"
#C1773 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_21) <- "Family members"
#C1774 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_22) <- "Hired people"
#C1775 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_23) <- "Mutual help"
#C1776 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1777 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_30) <- "Do not have"
#C1778 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_31) <- "Family members"
#C1779 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_32) <- "Hired people"
#C1780 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_33) <- "Mutual help"
#C1781 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1782 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_40) <- "Do not have"
#C1783 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_41) <- "Family members"
#C1784 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_42) <- "Hired people"
#C1785 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_43) <- "Mutual help"
#C1786 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1787 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_50) <- "Do not have"
#C1788 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_51) <- "Family members"
#C1789 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_52) <- "Hired people"
#C1790 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_53) <- "Mutual help"
#C1791 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1792 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_60) <- "Do not have"
#C1793 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_61) <- "Family members"
#C1794 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_62) <- "Hired people"
#C1795 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_63) <- "Mutual help"
#C1796 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1797 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_70) <- "Do not have"
#C1798 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_71) <- "Family members"
#C1799 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_72) <- "Hired people"
#C1800 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_73) <- "Mutual help"
#C1801 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1802 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_80) <- "Do not have"
#C1803 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_81) <- "Family members"
#C1804 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_82) <- "Hired people"
#C1805 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_83) <- "Mutual help"
#C1806 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1807 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_90) <- "Do not have"
#C1808 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_91) <- "Family members"
#C1809 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_92) <- "Hired people"
#C1810 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_93) <- "Mutual help"
#C1811 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#replace label: "d33_10. Transportation"
var_label(HouseholdLaos$d33_10_a) <- "d33_10. Transportation"
#C1812 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_100) <- "Do not have"
#C1813 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_101) <- "Family members"
#C1814 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_102) <- "Hired people"
#C1815 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_103) <- "Mutual help"
#C1816 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#replace label: "d33_11. Post-harvest processing"
var_label(HouseholdLaos$d33_11_a) <- "d33_11. Post-harvest processing"
#C1817 = FACT, replace label: "Do not have"
var_label(HouseholdLaos$d33_110) <- "Do not have"
#C1818 = FACT, replace label: "Family members"
var_label(HouseholdLaos$d33_111) <- "Family members"
#C1819 = FACT, replace label: "Hired people"
var_label(HouseholdLaos$d33_112) <- "Hired people"
#C1820 = FACT, replace label: "Mutual help"
var_label(HouseholdLaos$d33_113) <- "Mutual help"
#C1821 = OK
#C1822 = OK, (which currency?)
#C1823 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
#Land preparation
#Sowing
#Fertilization
#Weed management
#Pest and disease management
#Pruning
#Water/irrigation management
#Harvest
#Transportation
#Post-harvest processing
#None of above
#Other
#C1824 = FACT, replace label: "Land preparation"
var_label(HouseholdLaos$d361) <- "Land preparation"
#C1825 = FACT, replace label: "Sowing"
var_label(HouseholdLaos$d362) <- "Sowing"
#C1826 = FACT, replace label: "Fertilization"
var_label(HouseholdLaos$d363) <- "Fertilization"
#C1827 = FACT, replace label: "Weed management"
var_label(HouseholdLaos$d364) <- "Weed management"
#C1828 = FACT, replace label: "Pest and disease management"
var_label(HouseholdLaos$d365) <- "Pest and disease management"
#C1829 = FACT, replace label: "Pruning"
var_label(HouseholdLaos$d366) <- "Pruning"
#C1830 = FACT, replace label: "Water/irrigation management"
var_label(HouseholdLaos$d367) <- "Water/irrigation management"
#C1831 = FACT, replace label: "Harvest"
var_label(HouseholdLaos$d368) <- "Harvest"
#C1832 = FACT, replace label: "Transportation"
var_label(HouseholdLaos$d369) <- "Transportation"
#C1833 = FACT, replace label: "Post-harvest processing"
var_label(HouseholdLaos$d3610) <- "Post-harvest processing"
#C1834 = FACT, replace label: "None of above"
var_label(HouseholdLaos$d360) <- "None of above"
#C1835 = FACT, replace label: "Other"
var_label(HouseholdLaos$d3699) <- "Other"
#C1836 = OK, (the answer is still in Lao, Ky will maybe solve it)

# # #e.
#C1837 = FACT, (bin)
#C1838 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
#Buffalo
#Cattle
#Pig
#Goat
#Sheep
#Horse
#Rabbit
#Chicken
#Duck
#Muscovy
#Goose
#Other cattle
#Other poutry
#C1839 = FACT, (bin)
#C1840 = FACT, (bin)
#C1841 = FACT, (bin)
#C1842 = FACT, (bin)
#C1843 = FACT, (bin)
#C1844 = FACT, (bin)
#C1845 = FACT, (bin)
#C1846 = FACT, (bin)
#C1847 = FACT, (bin)
#C1848 = FACT, (bin)
#C1849 = FACT, (bin)
#C1850 = FACT, (bin)
#C1851 = FACT, (bin)
#C1852 = CHAR, (empty)
#C1853 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1854 = OK
#C1855 = OK
#C1856 = OK
#C1857 = OK
#C1858 = OK
#C1859 = OK
#C1860 = OK
#C1861 = OK
#C1862 = OK, (empty)
#C1863 = OK, (empty)
#C1864 = OK
#C1865 = OK
#C1866 = OK
#C1867 = OK
#C1868 = OK
#C1869 = OK
#C1870 = OK, (empty), replace label: "e3_98. e2_oth1"
var_label(HouseholdLaos$e3_98) <- "e3_98. e2_oth1"
#C1871 = OK, (empty), replace label: "e3_99. e2_oth2"
var_label(HouseholdLaos$e3_99) <- "e3_99. e2_oth2"
#C1872 = FACT, (table of correspondence), (see answer below)
#Buffalo
#Cattle
#Pig
#Goat
#Sheep
#Horse
#Rabbit
#Chicken
#Duck
#Muscovy
#Goose
#Other cattle
#Other poutry
#C1873 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1874 = FACT, (table of correspondence)
#C1875 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1876 = FACT, (table of correspondence)
#C1877 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1878 = OK, replace label: "e5_a. how many breeds of e4_1text does your household have"
var_label(HouseholdLaos$e5_a) <- "e5_a. how many breeds of e4_1text does your household have"
#C1879 = FACT, (bin),  replace label: "e5_b. do you have any local breeds of e4_1text"
var_label(HouseholdLaos$e5_b) <- "e5_b. do you have any local breeds of e4_1text"
#C1880 = OK
#C1881 = FACT, (bin),  replace label: "e5_d. do you cross local breeds with other breeds of e4_1text"
var_label(HouseholdLaos$e5_d) <- "e5_d. do you cross local breeds with other breeds of e4_1text"
#C1882 = REMOVE
#C1883 = OK, replace label: "e5_1. how many e4_1text died in the past 1 year (3 years)"
var_label(HouseholdLaos$e5_1) <- "e5_1. how many e4_1text died in the past 1 year (3 years)"
#C1884 = OK, replace label: "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdLaos$e5_2) <- "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1885 = OK, replace label: "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdLaos$e5_3) <- "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
#C1886 = OK, replace label: "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
var_label(HouseholdLaos$e5_4) <- "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
#C1887 = OK, replace label: "e5_41. average selling price/kg for each e4_1text sold (local currency/kg)"
var_label(HouseholdLaos$e5_41) <- "e5_41. average selling price/kg for each e4_1text sold (local currency/kg)"
#C1888 = OK, replace label: "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
var_label(HouseholdLaos$e5_5) <- "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
#C1889 = OK, replace label: "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
var_label(HouseholdLaos$e5_51) <- "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
#C1890 = FACT, (table of correspondence), (see choices below) replace label: "b18_1. what is the main outlet/ buyer for e4_1text?"
var_label(HouseholdLaos$b18_1) <- "b18_1. what is the main outlet/ buyer for e4_1text?"
#Village collector
#Collector outside the village
#Trader in the district
#Trader from the province
#Trader from another province
#Cooperative of which you are a member
#Cooperative of which you are not a member
#Consumers on the farm
#Local markets where you sell your products directly to final consumers
#Consumers through online sales
#Foreign trader (e.g. from China)
#Processors
#Other
#Do not know/ no the second outlet
#C1891 = OK, (the answer is still in Lao, Ky will maybe solve it), (real name)
#C1892 = OK, (the answer is still in Lao, Ky will maybe solve it), (category name)
#C1893 = FACT, (table of correspondence), (see choices below), replace label: "b19_1. what is the proportion of e4_1text that you sell to b18_1"
var_label(HouseholdLaos$b19_1) <- "b19_1. what is the proportion of e4_1text that you sell to b18_1"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C1894 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#replace label: "b20_1. does  b18_1text provide any of the following?"
var_label(HouseholdLaos$b20_1) <- "b20_1. does  b18_1text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C1895 = FACT, replace label: "Nothing"
var_label(HouseholdLaos$b20_10) <- "Nothing"
#C1896 = FACT, replace label: "Inputs (sold)"
var_label(HouseholdLaos$b20_11) <- "Inputs (sold)"
#C1897 = FACT, replace label: "Inputs on credit"
var_label(HouseholdLaos$b20_12) <- "Inputs on credit"
#C1898 = FACT, replace label: "Cash credit"
var_label(HouseholdLaos$b20_13) <- "Cash credit"
#C1899 = FACT, replace label: "Technical advice/training"
var_label(HouseholdLaos$b20_14) <- "Technical advice/training"
#C1900 = FACT, replace label: "Market information"
var_label(HouseholdLaos$b20_15) <- "Market information"
#C1901 = FACT, replace label: "Regular sales"
var_label(HouseholdLaos$b20_16) <- "Regular sales"
#C1902 = FACT, replace label: "Other"
var_label(HouseholdLaos$b20_199) <- "Other"
#C1903 = FACT, replace label: "Do not know"
var_label(HouseholdLaos$b20_188) <- "Do not know"
#C1904 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "b20_1oth. specify other provision from b18_1text"
var_label(HouseholdLaos$b20_1oth) <- "b20_1oth. specify other provision from b18_1text"
#C1905 = FACT, (see choices below), replace label: "b21_1. do you have a contract with b18_1text?"
var_label(HouseholdLaos$b21_1) <- "b21_1. do you have a contract with b18_1text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C1906 = OK, replace label: "e6_a. how many breeds of e4_2text does your household have"
var_label(HouseholdLaos$e6_a) <- "e6_a. how many breeds of e4_2text does your household have"
#C1907 = FACT, (bin),  replace label: "e6_b. do you have any local breeds of e4_2text"
var_label(HouseholdLaos$e6_b) <- "e6_b. do you have any local breeds of e4_2text"
#C1908 = OK
#C1909 = FACT, (bin),  replace label: "e6_d. do you cross local breeds with other breeds of e4_2text"
var_label(HouseholdLaos$e6_d) <- "e6_d. do you cross local breeds with other breeds of e4_2text"
#C1910 = REMOVE
#C1911 = OK, replace label: "e6_1. how many e4_2text died in the past 1 year (3 years)"
var_label(HouseholdLaos$e6_1) <- "e6_1. how many e4_2text died in the past 1 year (3 years)"
#C1912 = OK, replace label: "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdLaos$e6_2) <- "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1913 = OK, replace label: "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdLaos$e6_3) <- "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
#C1914 = OK, replace label: "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
var_label(HouseholdLaos$e6_4) <- "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
#C1915 = OK, replace label: "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
var_label(HouseholdLaos$e6_41) <- "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
#C1916 = OK, replace label: "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
var_label(HouseholdLaos$e6_5) <- "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
#C1917 = OK, replace label: "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
var_label(HouseholdLaos$e6_51) <- "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
#C1918 = FACT, (table of correspondence), (see choices below) replace label: "b18_2. what is the main outlet/ buyer for e4_2text?"
var_label(HouseholdLaos$b18_2) <- "b18_2. what is the main outlet/ buyer for e4_2text?"
#Village collector
#Collector outside the village
#Trader in the district
#Trader from the province
#Trader from another province
#Cooperative of which you are a member
#Cooperative of which you are not a member
#Consumers on the farm
#Local markets where you sell your products directly to final consumers
#Consumers through online sales
#Foreign trader (e.g. from China)
#Processors
#Other
#Do not know/ no the second outlet
#C1919 = OK, (the answer is still in Lao, Ky will maybe solve it), (real name)
#C1920 = OK, (the answer is still in Lao, Ky will maybe solve it), (category name)
#C1921 = FACT, (table of correspondence), (see choices below), replace label: "b19_2. what is the proportion of e4_2text that you sell to b18_2"
var_label(HouseholdLaos$b19_2) <- "b19_2. what is the proportion of e4_2text that you sell to b18_2"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C1922 = FACT, (see choices below)
#replace label: "b20_2. does  b18_2text provide any of the following?"
var_label(HouseholdLaos$b20_2) <- "b20_2. does  b18_2text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C1923 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "b20_2oth. specify other provision from b18_2text"
var_label(HouseholdLaos$b20_2oth) <- "b20_2oth. specify other provision from b18_2text"
#C1924 = FACT, (see choices below), replace label: "b21_2. do you have a contract with b18_2text?"
var_label(HouseholdLaos$b21_2) <- "b21_2. do you have a contract with b18_2text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C1925 = OK, replace label: "e7_a. how many breeds of e4_3text does your household have"
var_label(HouseholdLaos$e7_a) <- "e7_a. how many breeds of e4_3text does your household have"
#C1926 = FACT, (bin),  replace label: "e7_b. do you have any local breeds of e4_3text"
var_label(HouseholdLaos$e7_b) <- "e7_b. do you have any local breeds of e4_3text"
#C1927 = OK
#C1928 = FACT, (bin),  replace label: "e7_d. do you cross local breeds with other breeds of e4_3text"
var_label(HouseholdLaos$e7_d) <- "e7_d. do you cross local breeds with other breeds of e4_3text"
#C1929 = REMOVE
#C1930 = OK, replace label: "e7_1. how many e4_3text died in the past 1 year (3 years)"
var_label(HouseholdLaos$e7_1) <- "e7_1. how many e4_3text died in the past 1 year (3 years)"
#C1931 = OK, replace label: "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdLaos$e7_2) <- "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1932 = OK, replace label: "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdLaos$e7_3) <- "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
#C1933 = OK, replace label: "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
var_label(HouseholdLaos$e7_4) <- "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
#C1934 = OK, replace label: "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
var_label(HouseholdLaos$e7_41) <- "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
#C1935 = OK, replace label: "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
var_label(HouseholdLaos$e7_5) <- "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
#C1936 = OK, replace label: "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
var_label(HouseholdLaos$e7_51) <- "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
#C1937 = FACT, (bin)
#C1938 = OK, (m2 ?)
#C1939 = OK
#C1940 = FACT, (bin)
#C1941 = OK, (m2 ?)
#C1942 = FACT, (bin)
#C1943 = OK, (m2 ?)
#C1944 = FACT, (bin)
#C1945 = OK, (m2 ?)
#C1946 = FACT, (bin)
#C1947 = FACT, (table of correspondence), (see option below)
#Synthetic fertilizer
#Organic manure 
#Both above
#C1948 = FACT, (bin)
#C1949 = FACT, (bin)
#C1950 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C1951 = FACT, (table of correspondence)
#C1952 = FACT, (bin)
#C1953 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C1954 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e191) <- "Grazing in public area"
#C1955 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e192) <- "Grazing in own pasture area"
#C1956 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e193) <- "Cutting and carry natural grass / vegetables from public area"
#C1957 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e194) <- "Cutting and carry natural grass / vegetables from own area"
#C1958 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e195) <- "Cutting and carry of forage"
#C1959 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e196) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C1960 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e197) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C1961 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e198) <- "Silage"
#C1962 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e199) <- "Kitchen waste"
#C1963 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e1988) <- "Do not know"
#C1964 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e1999) <- "Other"
#C1965 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1966 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C1967 = FACT, (table of correspondence)
#C1968 = FACT, (bin)
#C1969 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C1970 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e241) <- "Grazing in public area"
#C1971 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e242) <- "Grazing in own pasture area"
#C1972 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e243) <- "Cutting and carry natural grass / vegetables from public area"
#C1973 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e244) <- "Cutting and carry natural grass / vegetables from own area"
#C1974 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e245) <- "Cutting and carry of forage"
#C1975 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e246) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C1976 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e247) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C1977 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e248) <- "Silage"
#C1978 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e249) <- "Kitchen waste"
#C1979 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e2488) <- "Do not know"
#C1980 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e2499) <- "Other"
#C1981 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C1982 = FACT, (bin)
#C1983 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C1984 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdLaos$e25_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C1985 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdLaos$e25_12) <- "From market: feed / concentrate"
#C1986 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e25_188) <- "Do not know"
#C1987 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e25_199) <- "Other"
#C1988 = FACT, (bin)
#C1989 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C1990 = FACT, (bin), replace label: "Internal parasites"
var_label(HouseholdLaos$e26_11) <- "Internal parasites"
#C1991 = FACT, (bin), replace label: "Tick"
var_label(HouseholdLaos$e26_12) <- "Tick"
#C1992 = FACT, (bin), replace label: "Worms"
var_label(HouseholdLaos$e26_13) <- "Worms"
#C1993 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e26_188) <- "Do not know"
#C1994 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e26_199) <- "Other"
#C1995 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C1996 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdLaos$e270) <- "Nothing"
#C1997 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdLaos$e271) <- "Traditional treatment"
#C1998 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdLaos$e272) <- "Chemicals (define)"
#C1999 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2000 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2001 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2002 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2003 = FACT, (bin)
#C2004 = FACT, (bin)
#C2005 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2006 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdLaos$e29_11) <- "For treatment diseases only"
#C2007 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdLaos$e29_12) <- "For prevention of diseases only"
#C2008 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdLaos$e29_13) <- "For growth promotion"
#C2009 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdLaos$e29_10) <- "I don’t use antibiotics at all"
#C2010 = OK, (empty)
#C2011 = FACT, (table of correspondence), (see answers below)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2012 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2013 = FACT, (table of correspondence), (see answers below)
#Fattening family raising using only local feed stuffs and kitchen waste
#Fattening family raising, but buying feed from market
#Piglet family raising using only local feed stuffs and kitchen waste
#Piglet family raising, but buying feed from market
#Other
#C2014 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2015 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2016 = FACT, (table of correspondence)
#C2017 = FACT, (bin)
#C2018 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C2019 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e341) <- "Grazing in public area"
#C2020 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e342) <- "Grazing in own pasture area"
#C2021 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e343) <- "Cutting and carry natural grass / vegetables from public area"
#C2022 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e344) <- "Cutting and carry natural grass / vegetables from own area"
#C2023 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e345) <- "Cutting and carry of forage"
#C2024 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e346) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2025 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e347) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2026 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e348) <- "Silage"
#C2027 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e349) <- "Kitchen waste"
#C2028 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e3488) <- "Do not know"
#C2029 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e3499) <- "Other"
#C2030 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2031 = FACT, (table of correspondence)
#C2032 = FACT, (bin)
#C2033 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C2034 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e381) <- "Grazing in public area"
#C2035 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e382) <- "Grazing in own pasture area"
#C2036 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e383) <- "Cutting and carry natural grass / vegetables from public area"
#C2037 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e384) <- "Cutting and carry natural grass / vegetables from own area"
#C2038 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e385) <- "Cutting and carry of forage"
#C2039 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e386) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2040 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e387) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2041 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e388) <- "Silage"
#C2042 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e389) <- "Kitchen waste"
#C2043 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e3888) <- "Do not know"
#C2044 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e3899) <- "Other"
#C2045 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2046 = FACT, (bin)

#C2047 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2048 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdLaos$e39_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2049 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdLaos$e39_12) <- "From market: feed / concentrate"
#C2050 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e39_188) <- "Do not know"
#C2051 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e39_199) <- "Other"
#C2052 = FACT, (bin)
#C2053 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2054 = FACT, (table of correspondence), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2055 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdLaos$e410) <- "Nothing"
#C2056 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdLaos$e411) <- "Traditional treatment"
#C2057 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdLaos$e412) <- "Chemicals (define)"
#C2058 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2059 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2060 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2061 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2062 = FACT, (bin)
#C2063 = FACT, (bin)
#C2064 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2065 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdLaos$e43_11) <- "For treatment diseases only"
#C2066 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdLaos$e43_12) <- "For prevention of diseases only"
#C2067 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdLaos$e43_13) <- "For growth promotion"
#C2068 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdLaos$e43_10) <- "I don’t use antibiotics at all"
#C2069 = OK, (empty)
#C2070 = FACT, (empty)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2071 = OK, (empty)
#C2072 = FACT, (table of correspondence), (see answers below)
#Family raising for consumption
#Family raising but larger scale for selling meat
#Family raising but larger scale for selling chick
#Family raising but larger scale for selling eggs
#Industrial (semi-industrial) broiler chicken 
#Industrial (semi-industrial) layer chicken
#Other
#C2073 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2074 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2075 = FACT, (table of correspondence)
#C2076 = FACT, (bin)
#C2077 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C2078 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e481) <- "Grazing in public area"
#C2079 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e482) <- "Grazing in own pasture area"
#C2080 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e483) <- "Cutting and carry natural grass / vegetables from public area"
#C2081 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e484) <- "Cutting and carry natural grass / vegetables from own area"
#C2082 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e485) <- "Cutting and carry of forage"
#C2083 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e486) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2084 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e487) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2085 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e488) <- "Silage"
#C2086 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e489) <- "Kitchen waste"
#C2087 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e4888) <- "Do not know"
#C2088 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e4899) <- "Other"
#C2089 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2090 = FACT, (table of correspondence)
#C2091 = FACT, (bin)
#C2092 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Do not know	
#Other
#C2093 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdLaos$e521) <- "Grazing in public area"
#C2094 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdLaos$e522) <- "Grazing in own pasture area"
#C2095 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdLaos$e523) <- "Cutting and carry natural grass / vegetables from public area"
#C2096 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdLaos$e524) <- "Cutting and carry natural grass / vegetables from own area"
#C2097 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdLaos$e525) <- "Cutting and carry of forage"
#C2098 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdLaos$e526) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2099 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdLaos$e527) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2100 = FACT, (bin), replace label: "Silage"
var_label(HouseholdLaos$e528) <- "Silage"
#C2101 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdLaos$e529) <- "Kitchen waste"
#C2102 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e5288) <- "Do not know"
#C2103 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e5299) <- "Other"
#C2104 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2105 = FACT, (bin)
#C2106 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2107 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdLaos$e53_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2108 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdLaos$e53_12) <- "From market: feed / concentrate"
#C2109 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$e53_188) <- "Do not know"
#C2110 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$e53_199) <- "Other"
#C2111 = FACT, (bin)
#C2112 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2113 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2114 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdLaos$e550) <- "Nothing"
#C2115 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdLaos$e551) <- "Traditional treatment"
#C2116 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdLaos$e552) <- "Chemicals (define)"
#C2117 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2118 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2119 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2120 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2121 = FACT, (bin)
#C2122 = FACT, (bin)
#C2123 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2124 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdLaos$e57_11) <- "For treatment diseases only"
#C2125 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdLaos$e57_12) <- "For prevention of diseases only"
#C2126 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdLaos$e57_13) <- "For growth promotion"
#C2127 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdLaos$e57_10) <- "I don’t use antibiotics at all"
#C2128 = OK, (empty)
#C2129 = FACT, (empty)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2130 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2131 = OK, (bin)
#C2132 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2133 = FACT, (bin), replace label: "Jan"
var_label(HouseholdLaos$e58_11) <- "Jan"
#C2134 = FACT, (bin), replace label: "Feb"
var_label(HouseholdLaos$e58_12) <- "Feb"
#C2135 = FACT, (bin), replace label: "Mar"
var_label(HouseholdLaos$e58_13) <- "Mar"
#C2136 = FACT, (bin), replace label: "Apr"
var_label(HouseholdLaos$e58_14) <- "Apr"
#C2137 = FACT, (bin), replace label: "May"
var_label(HouseholdLaos$e58_15) <- "May"
#C2138 = FACT, (bin), replace label: "Jun"
var_label(HouseholdLaos$e58_16) <- "Jun"
#C2139 = FACT, (bin), replace label: "Jul"
var_label(HouseholdLaos$e58_17) <- "Jul"
#C2140 = FACT, (bin), replace label: "Aug"
var_label(HouseholdLaos$e58_18) <- "Aug"
#C2141 = FACT, (bin), replace label: "Sep"
var_label(HouseholdLaos$e58_19) <- "Sep"
#C2142 = FACT, (bin), replace label: "Oct"
var_label(HouseholdLaos$e58_110) <- "Oct"
#C2143 = FACT, (bin), replace label: "Nov"
var_label(HouseholdLaos$e58_111) <- "Nov"
#C2144 = FACT, (bin), replace label: "Dec"
var_label(HouseholdLaos$e58_112) <- "Dec"
#C2145 = OK, (bin)
#C2146 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2147 = FACT, (bin), replace label: "Jan"
var_label(HouseholdLaos$e59_11) <- "Jan"
#C2148 = FACT, (bin), replace label: "Feb"
var_label(HouseholdLaos$e59_12) <- "Feb"
#C2149 = FACT, (bin), replace label: "Mar"
var_label(HouseholdLaos$e59_13) <- "Mar"
#C2150 = FACT, (bin), replace label: "Apr"
var_label(HouseholdLaos$e59_14) <- "Apr"
#C2151 = FACT, (bin), replace label: "May"
var_label(HouseholdLaos$e59_15) <- "May"
#C2152 = FACT, (bin), replace label: "Jun"
var_label(HouseholdLaos$e59_16) <- "Jun"
#C2153 = FACT, (bin), replace label: "Jul"
var_label(HouseholdLaos$e59_17) <- "Jul"
#C2154 = FACT, (bin), replace label: "Aug"
var_label(HouseholdLaos$e59_18) <- "Aug"
#C2155 = FACT, (bin), replace label: "Sep"
var_label(HouseholdLaos$e59_19) <- "Sep"
#C2156 = FACT, (bin), replace label: "Oct"
var_label(HouseholdLaos$e59_110) <- "Oct"
#C2157 = FACT, (bin), replace label: "Nov"
var_label(HouseholdLaos$e59_111) <- "Nov"
#C2158 = FACT, (bin), replace label: "Dec"
var_label(HouseholdLaos$e59_112) <- "Dec"
#C2159 = FACT, (bin)

# # #f.
#C2160 = FACT, (bin)
#C2161 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2162 = FACT, (table of correspondence), (see answers below)
#No time 
#Very little time
#Moderate amount of time
#Almost enough time
#Sufficient amount of time
#Do not know
#C2163 = FACT, (table of correspondence)
#C2164 = REMOVE, (text from the questionnaire)

# # #g.
#C2165 = FACT, (table of correspondence), (see answers below)
#Myself alone
#Me in consultation with spouse/other family members
#My spouse/other family members
#Do not know
#C2166 = FACT, (table of correspondence)
#C2167 = FACT, (table of correspondence)
#C2168 = FACT, (table of correspondence)
#C2169 = FACT, (bin)
#C2170 = FACT, (bin)
#C2171 = OK, (the answer is still in Lao, Ky will maybe solve it)

# # #h. 
#C2172 = REMOVE, text from the questionnaire
#C2173 = FACT, (table of correspondence), (see answers below)
#Yes, strongly
#Yes, maybe
#They should emigrate if they had the chance
#No, agriculture is not a good job
#Do not know
#C2174 = FACT, (bin)
#C2175 = FACT, (bin)
#C2176 = FACT, (bin)
#C2177 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#Other farmers/farmer group/cooperative
#Technical advisors
#Researchers
#Buyers
#Other
#Do not know
#C2178 = FACT, (bin), replace label: "Other farmers/farmer group/cooperative"
var_label(HouseholdLaos$h4_11) <- "Other farmers/farmer group/cooperative"
#C2179 = FACT, (bin), replace label: "Technical advisors"
var_label(HouseholdLaos$h4_12) <- "Technical advisors"
#C2180 = FACT, (bin), replace label: "Researchers"
var_label(HouseholdLaos$h4_13) <- "Researchers"
#C2181 = FACT, (bin), replace label: "Buyers"
var_label(HouseholdLaos$h4_14) <- "Buyers"
#C2182 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$h4_199) <- "Other"
#C2183 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$h4_188) <- "Do not know"
#C2184 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2185 = FACT, (bin)
#C2186 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2187 = OK, (the answer is still in Lao, Ky will maybe solve it)

# # #i.
#C2188 = REMOVE, (text from questionnaire)
#C2189 = FACT, (table of correspondence), (see answer below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2190 = FACT, (bin)
#C2191 = CHAR, (Useless answer-MultipleCombined, use the following columns),
#C2192 = FACT, (bin), replace label: "Jan"
var_label(HouseholdLaos$i31) <- "Jan"
#C2193 = FACT, (bin), replace label: "Feb"
var_label(HouseholdLaos$i32) <- "Feb"
#C2194 = FACT, (bin), replace label: "Mar"
var_label(HouseholdLaos$i33) <- "Mar"
#C2195 = FACT, (bin), replace label: "Apr"
var_label(HouseholdLaos$i34) <- "Apr"
#C2196 = FACT, (bin), replace label: "May"
var_label(HouseholdLaos$i35) <- "May"
#C2197 = FACT, (bin), replace label: "Jun"
var_label(HouseholdLaos$i36) <- "Jun"
#C2198 = FACT, (bin), replace label: "Jul"
var_label(HouseholdLaos$i37) <- "Jul"
#C2199 = FACT, (bin), replace label: "Aug"
var_label(HouseholdLaos$i38) <- "Aug"
#C2200 = FACT, (bin), replace label: "Sep"
var_label(HouseholdLaos$i39) <- "Sep"
#C2201 = FACT, (bin), replace label: "Oct"
var_label(HouseholdLaos$i310) <- "Oct"
#C2202 = FACT, (bin), replace label: "Nov"
var_label(HouseholdLaos$i311) <- "Nov"
#C2203 = FACT, (bin), replace label: "Dec"
var_label(HouseholdLaos$i312) <- "Dec"
#C2204 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Climate (drought, floods) 
#Pest damages
#Animal disease  
#No buyers for your produce
#Declining selling prices for your produce
#Need to reimburse credits 
#Increasing prices of food (rice…)
#Other
#C2205 = FACT, (bin), replace label: "Climate (drought, floods)"
var_label(HouseholdLaos$i41) <- "Climate (drought, floods)"
#C2206 = FACT, (bin), replace label: "Pest damages"
var_label(HouseholdLaos$i42) <- "Pest damages"
#C2207 = FACT, (bin), replace label: "Animal disease"
var_label(HouseholdLaos$i43) <- "Animal disease"
#C2208 = FACT, (bin), replace label: "No buyers for your produce"
var_label(HouseholdLaos$i44) <- "No buyers for your produce"
#C2209 = FACT, (bin), replace label: "Declining selling prices for your produce"
var_label(HouseholdLaos$i45) <- "Declining selling prices for your produce"
#C2210 = FACT, (bin), replace label: "Need to reimburse credits"
var_label(HouseholdLaos$i46) <- "Need to reimburse credits"
#C2211 = FACT, (bin), replace label: "Increasing prices of food (rice…)"
var_label(HouseholdLaos$i47) <- "Increasing prices of food (rice…)"
#C2212 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$i499) <- "Other"
#C2213 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2214 = FACT, (table of correspondence), (see answers below)
#Happens every year or most years
#Happens sometimes but not regularly
#It was exceptional

# # #j. 
#C2215 = FACT, (table of correspondence), (see answers below)
#Owned
#Rented
#Share cropping 
#Other
#Do not know
#C2216 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2217 = FACT, (table of correspondence), (see answers below)
#Brick wall
#Concrete wall
#Wooden wall
#Bamboo, Thatch/leaves, Grass
#Galvanized iron or aluminium or other metal sheets 
#Wood or logs
#Other
#C2218 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2219 = FACT, (table of correspondence), (see answers below)
#Thatch/leaves/grass 
#Wood
#Fibrous cement 
#Concrete, cement
#Brick tile roof
#Stone tile roof
#Metal/ tin roof
#Other
#C2220 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2221 = FACT, (bin)
#C2222 = FACT, (bin)
#C2223 = FACT, (table of correspondence), (see answers below)
#Private tap water
#Public water
#Drill
#Well
#Rain water
#Natural stream
#Water delivery
#Rain water
#Other
#C2224 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2225 = FACT, (table of correspondence), (see answers below)
#Grid electricity 
#Private electricity
#Small hydroelectricity
#Own generator
#Battery
#Solar panel 
#None, using kerosene/ candles
#Other
#C2226 = OK, (the answer is still in Lao, Ky will maybe solve it)

# # #k.
#C2227 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Motorcycle
#2-wheel hand tractor
#4-wheel tractor
#Car
#Truck
#Combine harvester 
#Land leveler
#Rice planter
#Maize planter
#Cassava disc ridging tool
#Cassava harvesting tool
#Rice thresher
#Rice mill
#Backpack sprayer
#Motor pump sprayer
#Grass cutter
#Grass chopping machine
#Water pump
#Irrigation system  
#Other
#C2228 = FACT, (bin), replace label: "Motorcycle"
var_label(HouseholdLaos$k11) <- "Motorcycle"
#C2229 = FACT, (bin), replace label: "2-wheel hand tractor"
var_label(HouseholdLaos$k12) <- "2-wheel hand tractor"
#C2230 = FACT, (bin), replace label: "4-wheel tractor"
var_label(HouseholdLaos$k13) <- "4-wheel tractor"
#C2231 = FACT, (bin), replace label: "Car"
var_label(HouseholdLaos$k14) <- "Car"
#C2232 = FACT, (bin), replace label: "Truck"
var_label(HouseholdLaos$k15) <- "Truck"
#C2233 = FACT, (bin), replace label: "Combine harvester"
var_label(HouseholdLaos$k16) <- "Combine harvester"
#C2234 = FACT, (bin), replace label: "Land leveler"
var_label(HouseholdLaos$k17) <- "Land leveler"
#C2235 = FACT, (bin), replace label: "Rice planter"
var_label(HouseholdLaos$k18) <- "Rice planter"
#C2236 = FACT, (bin), replace label: "Maize planter"
var_label(HouseholdLaos$k19) <- "Maize planter"
#C2237 = FACT, (bin), replace label: "Cassava disc ridging tool"
var_label(HouseholdLaos$k110) <- "Cassava disc ridging tool"
#C2238 = FACT, (bin), replace label: "Cassava harvesting tool"
var_label(HouseholdLaos$k111) <- "Cassava harvesting tool"
#C2239 = FACT, (bin), replace label: "Rice thresher"
var_label(HouseholdLaos$k112) <- "Rice thresher"
#C2240 = FACT, (bin), replace label: "Rice mill"
var_label(HouseholdLaos$k113) <- "Rice mill"
#C2241 = FACT, (bin), replace label: "Backpack sprayer"
var_label(HouseholdLaos$k114) <- "Backpack sprayer"
#C2242 = FACT, (bin), replace label: "Motor pump sprayer"
var_label(HouseholdLaos$k115) <- "Motor pump sprayer"
#C2243 = FACT, (bin), replace label: "Grass cutter"
var_label(HouseholdLaos$k116) <- "Grass cutter"
#C2244 = FACT, (bin), replace label: "Grass chopping machine"
var_label(HouseholdLaos$k117) <- "Grass chopping machine"
#C2245 = FACT, (bin), replace label: "Water pump"
var_label(HouseholdLaos$k118) <- "Water pump"
#C2246 = FACT, (bin), replace label: "Irrigation system"
var_label(HouseholdLaos$k119) <- "Irrigation system"
#C2247 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$k199) <- "Other"
#C2248 = OK
#C2249 = OK
#C2250 = OK
#C2251 = OK
#C2252 = OK
#C2253 = OK
#C2254 = OK
#C2255 = OK
#C2256 = OK
#C2257 = OK
#C2258 = OK
#C2259 = OK
#C2260 = OK
#C2261 = OK
#C2262 = OK
#C2263 = OK
#C2264 = OK
#C2265 = OK
#C2266 = OK

# # #l.
#C2267 = REMOVE, (text from the questionaire)
#C2268 = FACT, (bin)
#C2269 = FACT, (table of correspondence), (see answers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2270 = FACT, (table of correspondence), (see answers below)
#No debt   
#Debt is higher than income
#Debt is more than half of the income. Capacity to reimburse is limited
#Debt is approximately half of the income
#Debt is low and I am capable to reimburse
#C2271 = FACT, (bin)
#C2272 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#To buy food
#For emergency expenses
#To buy animals
#To buy equipment/machinery for agriculture
#To buy seeds
#To buy chemical farm inputs
#To buy organic farm inputs
#To pay for certification
#Festivals/celebrations
#Child education
#Health care
#To improve the house
#Other
#Do not know
#C2273 = FACT, (bin), replace label: "To buy food"
var_label(HouseholdLaos$l51) <- "To buy food"
#C2274 = FACT, (bin), replace label: "For emergency expenses"
var_label(HouseholdLaos$l52) <- "For emergency expenses"
#C2275 = FACT, (bin), replace label: "To buy animals"
var_label(HouseholdLaos$l53) <- "To buy animals"
#C2276 = FACT, (bin), replace label: "To buy equipment/machinery for agriculture"
var_label(HouseholdLaos$l54) <- "To buy equipment/machinery for agriculture"
#C2277 = FACT, (bin), replace label: "To buy seeds"
var_label(HouseholdLaos$l55) <- "To buy seeds"
#C2278 = FACT, (bin), replace label: "To buy chemical farm inputs"
var_label(HouseholdLaos$l56) <- "To buy chemical farm inputs"
#C2279 = FACT, (bin), replace label: "To buy organic farm inputs"
var_label(HouseholdLaos$l57) <- "To buy organic farm inputs"
#C2280 = FACT, (bin), replace label: "To pay for certification"
var_label(HouseholdLaos$l58) <- "To pay for certification"
#C2281 = FACT, (bin), replace label: "Festivals/celebrations"
var_label(HouseholdLaos$l59) <- "Festivals/celebrations"
#C2282 = FACT, (bin), replace label: "Child education"
var_label(HouseholdLaos$l510) <- "Child education"
#C2283 = FACT, (bin), replace label: "Health care"
var_label(HouseholdLaos$l511) <- "Health care"
#C2284 = FACT, (bin), replace label: "To improve the house"
var_label(HouseholdLaos$l512) <- "To improve the house"
#C2285 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$l599) <- "Other"
#C2286 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$l588) <- "Do not know"
#C2287 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2288 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Producer group
#Union (farmer, women, veteran)
#Bank
#Relatives & friends 
#Collector / trader
#Input seller
#Processor
#Other
#Do not know
#C2289 = FACT, (bin), replace label: "Producer group"
var_label(HouseholdLaos$l61) <- "Producer group"
#C2290 = FACT, (bin), replace label: "Union (farmer, women, veteran)"
var_label(HouseholdLaos$l62) <- "Union (farmer, women, veteran)"
#C2291 = FACT, (bin), replace label: "Bank"
var_label(HouseholdLaos$l63) <- "Bank"
#C2292 = FACT, (bin), replace label: "Relatives & friends"
var_label(HouseholdLaos$l64) <- "Relatives & friends"
#C2293 = FACT, (bin), replace label: "Collector / trader"
var_label(HouseholdLaos$l65) <- "Collector / trader"
#C2294 = FACT, (bin), replace label: "Input seller"
var_label(HouseholdLaos$l66) <- "Input seller"
#C2295 = FACT, (bin), replace label: "Processor"
var_label(HouseholdLaos$l67) <- "Processor"
#C2296 = FACT, (bin), replace label: "Other"
var_label(HouseholdLaos$l699) <- "Other"
#C2297 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdLaos$l688) <- "Do not know"
#C2298 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2299 = FACT, (bin)
#C2300 = FACT, (bin)

# # #m.
#C2301 = FACT, (table of correspondence), (see answers below)
#Complete
#Partially complete
#Refuses to participate
#No household member present
#Household moved to a new location
#C2302 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C2303 = REMOVE, (duplicate)
#C2304 = FACT
#C2305 = FACT
#C2306 = FACT
#C2307 = FACT
#C2308 = REMOVE, (text from questionnaire)
#C2309 = OK, (???)
#C2310 = CHAR, kind of id?
#C2311 = CHAR, kind of id?
# Check if necesary to keep this part
#C2312 = CHAR
#C2313 = CHAR, Useless, (remove?)
#C2314 = CHAR, (empty), (remove?)
#C2315 = OK
#C2316 = CHAR, (empty), (remove?)
#C2317 = OK
#C2318 = CHAR, (empty), (remove?)
#C2319 = CHAR, kind of id?

summary(HouseholdLaos[,2315])


##2.2. Work on the 2nd database: "HouMemberLaos" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, (id of Household member in a household)
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = FACT, (both previous id combined)
#C4 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C5 = FACT, (bin), replace label:"a1_1. is  a1 the survey respondent ?"
var_label(HouMemberLaos$a1_1) <- "a1_1. is  a1 the survey respondent ?"
#C6 = FACT, (table of correspondence), (see answers below),
#replace label:"a2. what is a1’s relationship to household head ?"
var_label(HouMemberLaos$a2) <- "a2. what is a1’s relationship to household head ?"
#Household head
#Spouse/partner of the household head
#Son/daughter
#Son/daughter in law
#Grandchild
#Parent
#Parent-in-law
#Grandparent
#Grandparent-in-law
#Sibling
#Other relative
#Other non-relative
#C7 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C8 = FACT, (table of correspondence), (see answers below), replace label: "a3. what is the sex of a1 ?"
var_label(HouMemberLaos$a3) <- "a3. what is the sex of a1 ?"
#Male
#Female
#C9 = FACT, (table of correspondence), (see answers below), replace label: "a4. what is a1's solar year of birth ?"
var_label(HouMemberLaos$a4) <- "a4. what is a1's solar year of birth ?"
#C10 = NUM, replace label: "Age (years)"
HouMemberLaos$a4_1 <- 2022 - HouMemberLaos$a4
var_label(HouMemberLaos$a4_1) <- "Age (years)"
#C11 = FACT, (table of correspondence), (see answers below), replace label: "a5. what is the highest diploma a1 has obtained ?"
var_label(HouMemberLaos$a5) <- "a5. what is the highest diploma a1 has obtained ?"
#No schooling, illiterate
#Some primary school
#Primary school completed
#Some secondary school
#Secondary school completed
#Some high school
#High school completed
#Some vocational school
#Vocational school completed
#Some college or university
#College or university
#Post-university study/degree
#Do not know
#C12 = FACT, (table of correspondence), (see answers below), replace label: "a6. what is a1's main occupation ?"
var_label(HouMemberLaos$a6) <- "a6. what is a1's main occupation ?"
#Agricultural work on their own farm (including livestock management)
#Permanent salaried employment on someone else’s farm (including livestock management
#Seasonal salaried employment on someone else’s farm (including livestock management)
#Government
#Business (owner)
#Salaried employment (non-agricultural work)
#Junk collector
#Study/training 
#Housewife/ caregiver
#Unable to work 
#Other
#Do not know
#oth. Specify other main occupation of  a1 in the last 12 months
#C13 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "a6_oth. specify other main occupation of a1 in the past 12 months"
var_label(HouMemberLaos$a6_oth) <- "a6_oth. specify other main occupation of a1 in the past 12 months"
#C14 = FACT, (table of correspondence), (see answers below), replace label: "a7. what is a1's secondary occupation ?"
var_label(HouMemberLaos$a7) <- "a7. what is a1's secondary occupation ?"
#No secondary occupation
#Agricultural work on their own farm (including livestock management)
#Permanent salaried employment on someone else’s farm (including livestock management
#Seasonal salaried employment on someone else’s farm (including livestock management)
#Government
#Business (owner)
#Salaried employment (non-agricultural work)
#Junk collector
#Study/training 
#Housewife/ caregiver
#Unable to work 
#Other
#Do not know
#C15 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "a6_oth. specify other second occupation of a1 in the past 12 months"
var_label(HouMemberLaos$a7_oth) <- "a7_oth. specify other second occupation of a1 in the past 12 months"
#C16 = FACT, (table of correspondence), (see answers below)
#Weaving
#Handicraft
#Commerce
#Transportation
#Blacksmithing
#Construction work 
#Factory work
#Agricultural service provision
#Other
#Do not know
#C17 = FACT, (bin), replace label: "Weaving"
var_label(HouMemberLaos$a81) <- "Weaving"
#C18 = FACT, (bin), replace label: "Handicraft"
var_label(HouMemberLaos$a82) <- "Handicraft"
#C19 = FACT, (bin), replace label: "Commerce"
var_label(HouMemberLaos$a83) <- "Commerce"
#C20 = FACT, (bin), replace label: "Transportation"
var_label(HouMemberLaos$a84) <- "Transportation"
#C21 = FACT, (bin), replace label: "Blacksmithing"
var_label(HouMemberLaos$a85) <- "Blacksmithing"
#C22 = FACT, (bin), replace label: "Construction work"
var_label(HouMemberLaos$a86) <- "Construction work"
#C23 = FACT, (bin), replace label: "Factory work"
var_label(HouMemberLaos$a87) <- "Factory work"
#C24 = FACT, (bin), replace label: "Agricultural service provision"
var_label(HouMemberLaos$a88) <- "Agricultural service provision"
#C25 = FACT, (bin), replace label: "Other"
var_label(HouMemberLaos$a899) <- "Other"
#C26 = FACT, (bin), replace label: "Do not know"
var_label(HouMemberLaos$a888) <- "Do not know"
#C27 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C28 = FACT, (table of correspondence), (see answers below), replace label: "a9. reason why a1  was unable to work in the last 12 months ?"
var_label(HouMemberLaos$a9) <- "a9. reason why a1  was unable to work in the last 12 months ?"
#Cannot find employment
#Sick
#Handicapped
#Too old
#Too young
#Other reason
#C29 = OK, (the answer is still in Lao, Ky will maybe solve it), replace label: "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
var_label(HouMemberLaos$a9_oth) <- "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
#C30 = FACT, (bin), replace label: "a10. do a1 migrate seasonally every year ?"
var_label(HouMemberLaos$a10) <- "a10. do a1 migrate seasonally every year ?"
#C31 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C32 = FACT, (table of correspondence), (see answers below), replace label: "a12. if yes, what is a1's main occupation when migrating"
var_label(HouMemberLaos$a12) <- "a12. if yes, what is a1's main occupation when migrating"
#Agricultural work on their own farm (including livestock management)
#Permanent salaried employment on someone else’s farm (including livestock management
#Seasonal salaried employment on someone else’s farm (including livestock management)
#Government
#Business (owner)
#Salaried employment (non-agricultural work)
#Junk collector
#Study/training 
#Housewife/ caregiver
#Unable to work 
#Other
#Do not know
#C33 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "a13. if yes, what months do a1 migrates ?"
var_label(HouMemberLaos$a13) <- "a13. if yes, what months do a1 migrates ?"
#C34 = FACT, (bin), replace label: "Jan"
var_label(HouMemberLaos$a131) <- "Jan"
#C35 = FACT, (bin), replace label: "Feb"
var_label(HouMemberLaos$a132) <- "Feb"
#C36 = FACT, (bin), replace label: "Mar"
var_label(HouMemberLaos$a133) <- "Mar"
#C37 = FACT, (bin), replace label: "Apr"
var_label(HouMemberLaos$a134) <- "Apr"
#C38 = FACT, (bin), replace label: "May"
var_label(HouMemberLaos$a135) <- "May"
#C39 = FACT, (bin), replace label: "Jun"
var_label(HouMemberLaos$a136) <- "Jun"
#C40 = FACT, (bin), replace label: "Jul"
var_label(HouMemberLaos$a137) <- "Jul"
#C41 = FACT, (bin), replace label: "Aug"
var_label(HouMemberLaos$a138) <- "Aug"
#C42 = FACT, (bin), replace label: "Sep"
var_label(HouMemberLaos$a139) <- "Sep"
#C43 = FACT, (bin), replace label: "Oct"
var_label(HouMemberLaos$a1310) <- "Oct"
#C44 = FACT, (bin), replace label: "Nov"
var_label(HouMemberLaos$a1311) <- "Nov"
#C45 = FACT, (bin), replace label: "Dec"
var_label(HouMemberLaos$a1312) <- "Dec"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C46 = OK, (member id and name combined), (useless?)
#C47 = FACT, (bin), ??
#C48 = FACT, (bin), ??
#C49 = FACT, (bin), ??
#C50 = FACT, (bin), ??
#C51 = FACT, (bin), ??
#C52 = FACT, (bin), ??
#C53 = OK, (empty)
#C54 = OK, (empty)
#C55 = OK, (empty)
#C56 = OK, (empty)
#C57 = OK, (empty)
#C58 = OK, (empty)
#C59 = FACT, (index)
#C60 = OK, (useless??)
#C61 = FACT, (parent index)
#C62 = FACT, (id)
#C63 = FACT, (id)
#C64 = FACT, (time)
#C65 = FACT, (time)
#C66 = OK, (empty)
#C67 = OK, (useless?)
#C68 = OK, (empty)
#C69 = OK, (useless?)
#C70 = OK, (empty)

summary(HouMemberLaos[,60])

##2.3. Work on the third database: "ClowlandLaos" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, (id of crop per household)
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C4 = FACT, (number corresponding to crops), replace label: "d2_12. select crop crop1_now with keyword searchcrop1"
var_label(ClowlandLaos$d2_12) <- "d2_12. select crop crop1_now with keyword searchcrop1"
#C5 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C6 = REMOVE, (empty)
#C7 = OK, (crop name in Lao) replace label: "d2_13v. Crop name (Lao)"
var_label(ClowlandLaos$d2_13v) <- "d2_13v. Crop name (Lao)"
#C8 = OK, (crop name in English) replace label: "d2_13e. Crop name (English)"
var_label(ClowlandLaos$d2_13e) <- "d2_13e. Crop name (English)"
#C9 = FACT, ???
#C10 = NUM, replace label: "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(ClowlandLaos$d2_132) <- "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
#C11 = FACT, (table of correspondence), (see option below), replace label: "d2_133. which unit of d2_13v ?"
var_label(ClowlandLaos$d2_133) <- "d2_133. which unit of d2_13v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C12 = OK, replace label: "d2_134. number of seed units of d2_13v that household used?"
var_label(ClowlandLaos$d2_134) <- "d2_134. number of seed units of d2_13v that household used?"
#C13 = OK, replace label: "d2_135. number of kg produced of d2_13v ?"
var_label(ClowlandLaos$d2_135) <- "d2_135. number of kg produced of d2_13v ?"
#C14 = OK, replace label: "d2_136.number of kg sold of d2_13v"
var_label(ClowlandLaos$d2_136) <- "d2_136.number of kg sold of d2_13v"
#C15 = OK, replace label: "d2_137. selling price/ kg of d2_13v"
var_label(ClowlandLaos$d2_137) <- "d2_137. selling price/ kg of d2_13v"
#C16 = OK, replace label: "d2_138. how many species or varieties of d2_13v ?"
var_label(ClowlandLaos$d2_138) <- "d2_138. how many species or varieties of d2_13v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C17 = FACT, (CROP id)
#C18 = OK, (useless??)
#C19 = FACT, (parent index)
#C20 = FACT, (id)
#C21 = FACT, (id)
#C22 = FACT, (time)
#C23 = FACT, (time)
#C24 = OK, (empty)
#C25 = OK, (useless?)
#C26 = OK, (empty)
#C27 = OK, (useless?)
#C28 = OK, (empty)

summary(ClowlandLaos[,15])

##2.4. Work on the fourth database: "CuplandLaos" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, (id of crop per household)
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C4 = FACT, (number corresponding to crops), replace label: "d2_22. select crop crop1_now with keyword searchcrop1"
var_label(CuplandLaos$d2_22) <- "d2_22. select crop crop1_now with keyword searchcrop1"
#C5 = OK, (the answer is still in Lao, Ky will maybe solve it)
#C6 = OK, (crop name in Lao) replace label: "d2_23v. Crop name (Lao)"
var_label(CuplandLaos$d2_23v) <- "d2_23v. Crop name (Lao)"
#C7 = OK, (crop name in English) replace label: "d2_23e. Crop name (English)"
var_label(CuplandLaos$d2_23e) <- "d2_23e. Crop name (English)"
#C8 = FACT, ???
#C9 = NUM, replace label: "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(CuplandLaos$d2_232) <- "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
#C10 = FACT, (table of correspondence), (see option below), replace label: "d2_233. which unit of d2_23v ?"
var_label(CuplandLaos$d2_233) <- "d2_233. which unit of d2_23v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C11 = OK, replace label: "d2_234. number of seed units of d2_23v that household used?"
var_label(CuplandLaos$d2_234) <- "d2_234. number of seed units of d2_23v that household used?"
#C12 = OK, replace label: "d2_235. number of kg produced of d2_23v ?"
var_label(CuplandLaos$d2_235) <- "d2_235. number of kg produced of d2_23v ?"
#C13 = OK, replace label: "d2_236.number of kg sold of d2_23v"
var_label(CuplandLaos$d2_236) <- "d2_236.number of kg sold of d2_23v"
#C14 = OK, replace label: "d2_237. selling price/ kg of d2_23v"
var_label(CuplandLaos$d2_237) <- "d2_237. selling price/ kg of d2_23v"
#C15 = OK, replace label: "d2_238. how many species or varieties of d2_23v ?"
var_label(CuplandLaos$d2_238) <- "d2_238. how many species or varieties of d2_23v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C16 = FACT, (CROP id)
#C17 = OK, (useless??)
#C18 = FACT, (parent index)
#C19 = FACT, (id)
#C20 = FACT, (id)
#C21 = FACT, (time)
#C22 = FACT, (time)
#C23 = OK, (empty)
#C24 = OK, (useless?)
#C25 = OK, (empty)
#C26 = OK, (useless?)
#C27 = OK, (empty)

summary(CuplandLaos[,15])

#var_label(HouseholdLaos$b14_2)


### 3. Columns data type changing and removing
##3.1 Change column data types base on previous Columns ID
#Convert the tbl_df into a dataframe for each database
HouseholdLaos_2 <- as.data.frame(HouseholdLaos)
HouMemberLaos_2 <- as.data.frame(HouMemberLaos)
ClowlandLaos_2 <- as.data.frame(ClowlandLaos)
CuplandLaos_2 <- as.data.frame(CuplandLaos)
#a. FACTOR CONVERSION
#Convert columns to FACTOR - "HouseholdLaos"
for (i in c(1:3,7:9,11,13:30,33:37,42:48,51,62:68,70:76,78:82,85:104,107,109,111,
           119,121:140,143,145:157,159:170,172:182,185:193,196:206,208:222,224,
           226,229:242,244,246,249:262,264,266,269:282,284,286,289:302,304,306,
           309:322,324,326,329:342,344,346,349:362,364,366,369:382,384,386,389:402,
           404,406,409:422,424,426,429:442,444,446,449:462,464,466,469:482,484,486,
           489:502,504,506,509:522,525:527,529:536,538,540:541,543:550,552,554:560,
           562,564,566,569:578,580:584,586:602,605:612,614:615,617:623,643:654,657,
           660:672,674:682,684:697,699:707,709,719:735,738,741,744,747,750,753,756,
           759,762,765,768,771,774,777,780,783,785:787,791,793,795,797,799,801,804,
           806,808,811:816,819:828,831:842,845:856,859:870,873:884,887:898,901:912,
           915:926,929:940,943:951,954:965,968:979,982:993,996:1007,1010:1021,1024:1035,
           1038:1049,1052:1063,1065:1079,1082:1093,1096:1107,1110:1121,1124:1135,
           1138:1149,1152:1163,1166:1177,1180:1191,1194:1205,1208:1219,1222:1233,
           1236:1247,1249:1250,1252:1266,1269:1280,1283:1294,1297:1308,1311:1322,
           1325:1336,1339:1350,1353:1364,1367:1378,1381:1392,1395:1406,1409:1420,
           1423:1434,1437:1448,1451:1462,1465:1476,1478:1480,1482,1484:1498,1501:1512,
           1515:1526,1529:1540,1543:1554,1557:1568,1571:1582,1585:1596,1599:1610,1613:1624,
           1627:1638,1641:1652,1655:1666,1669:1680,1683:1694,1697:1708,1710:1712,1714:1725,
           1727:1738,1740:1751,1753:1764,1767:1770,1772:1775,1777:1780,1782:1785,1787:1790,
           1792:1795,1797:1800,1802:1805,1807:1810,1812:1815,1817:1820,1824:1837,1839:1851,
           1872,1874,1876,1879,1881,1890,1893,1895:1903,1905,1907,1909,1918,1921,1922,1924,
           1926,1928,1937,1940,1942,1944,1946:1952,1954:1964,1966:1968,1970:1980,1982,1984:1988,
           1990:1994,1996:1999,2001,2003:2004,2006:2009,2011,2013,2015:2017,2019:2032,2034:2044,
           2046,2048:2058,2060,2062:2063,2065:2068,2070,2072,2074:2076,2078:2091,2093:2103,2105,
           2107:2112,2114:2117,2119,2121:2122,2124:2127,2129,2133:2144,2147:2160,2162:2163,
           2165:2170,2173:2176,2178:2183,2185,2189:2190,2192:2203,2205:2212,2214:2215,2217,
           2219,2221:2223,2225,2228:2247,2268:2271,2273:2286,2289:2297,
           2299:2301,2304:2307)){
  HouseholdLaos_2[,i] <- as.factor(HouseholdLaos_2[,i])
}
#Convert columns to FACTOR - "HouMemberLaos"
for (i in c(2:3,5:6,8:9,11:12,14,16:26,28,30,32,34:45,47:52,59,61:65)){
  HouMemberLaos_2[,i] <- as.factor(HouMemberLaos_2[,i])
}
#Convert columns to FACTOR - "ClowlandLaos"
for (i in c(2,4,9,11,17,19:23)){
  ClowlandLaos_2[,i] <- as.factor(ClowlandLaos_2[,i])
}
#Convert columns to FACTOR - "CuplandLaos"
for (i in c(2,4,8,10,16,18:22)){
  CuplandLaos_2[,i] <- as.factor(CuplandLaos_2[,i])
}
#b. NUMERIC CONVERSION
#Convert columns to NUMERIC - "HouseholdLaos"
for (i in  c(32,61,114,116,118)){
  HouseholdLaos_2[,i] <- as.numeric(HouseholdLaos_2[,i])
}
#Convert columns to NUMERIC - "HouMemberLaos"
  HouMemberLaos_2[,10] <- as.numeric(HouMemberLaos_2[,10])
#Convert columns to NUMERIC - "ClowlandLaos"
  ClowlandLaos_2[,10] <- as.numeric(ClowlandLaos_2[,10])
#Convert columns to NUMERIC - "CuplandLaos"
  CuplandLaos_2[,9] <- as.numeric(CuplandLaos_2[,9])
#c. CHARACTER CONVERSION
#Convert columns to CHARACTER - "HouseholdLaos"
  for (i in  c(33,49:50,54:60,84,141,144,158,171,184,195,207,228,248,268,288,308,328,348,368,388,
               408,428,448,468,488,508,528,542,553,568,585,604,616,642,673,698,718,
               810,818,830,844,858,872,886,900,914,928,942,953,967,981,995,1009,1023,
               1037,1051,1081,1095,1109,1123,1137,1151,1165,1179,1193,1207,1221,1235,
               1251,1268,1282,1296,1310,1324,1338,1352,1366,1380,1394,1408,1422,1436,
               1450,1464,1483,1500,1514,1528,1542,1556,1570,1584,1598,1612,1626,1640,
               1654,1668,1682,1696,1713,1726,1739,1752,1766,1771,1776,1781,1786,1791,
               1796,1801,1806,1811,1816,1823,1838,1852,1894,1953,1969,1983,1989,1995,
               2005,2018,2033,2047,2064,2077,2092,2106,2113,2123,2132,2146,2177,2191,
               2204,2227,2272,2288,2310:2314,2316,2318:2319)){
    HouseholdLaos_2[,i] <- as.character(HouseholdLaos_2[,i])
  }
#Convert columns to CHARACTER - "HouMemberLaos"
  for (i in c(1,33)){
    HouMemberLaos_2[,i] <- as.character(HouMemberLaos_2[,i])
  }
#Convert columns to CHARACTER - "ClowlandLaos"
  ClowlandLaos_2[,1] <- as.character(ClowlandLaos_2[,1])
#Convert columns to CHARACTER - "CuplandLaos"
  CuplandLaos_2[,1] <- as.character(CuplandLaos_2[,1])

##3.2 REMOVE Unwanted columns (1st Version)
#Remove unwanted columns for each datasets
#HouseholdLaos_2C <- HouseholdLaos_2[,-c(5,6,38:41,52,625,633,641,803,1882,1910,
                                        #1929,2164,2172,2188,2267,2303,2308)]
  
  
#Add the proper labels to each columns
HouseholdLaos_2C <- copy_labels(HouseholdLaos_2, HouseholdLaos)
HouMemberLaos_2C <- copy_labels(HouMemberLaos_2, HouMemberLaos)
ClowlandLaos_2C <- copy_labels(ClowlandLaos_2, ClowlandLaos)
CuplandLaos_2 <- copy_labels(CuplandLaos_2, CuplandLaos)

  
### 4. Data Cleaning 1 (Based on observations and Ky comments)

## 4.1 Data cleaning for "HouseholdLaos_2"

# # #a.Check for household duplicates
  count_if("TRUE",duplicated(HouseholdLaos_2C$o9))
  Dum <- HouseholdLaos_2C[duplicated(HouseholdLaos_2C$o9),]
#3 households are duplicates but it seems to be two members of the same family
  #1st we move the household ID column at 1st column
  HouseholdLaos_2C <- HouseholdLaos_2C %>% relocate(o9 , .before = start_time)
  #Check the number and id of duplicates
  count_if("TRUE",duplicated(HouseholdLaos_2C$o9))
  # Household 3024 = Unknown, Household 3114 = Unknown, 3261 = Unknow
  #, we remove both and will signal it to enumerators for checking
  for (i in c(3024,3114,3261)){ 
  HouseholdLaos_2C <- HouseholdLaos_2C[HouseholdLaos_2C$o9 != i,]
  }
  #Check again the duplicates: 
  count_if("TRUE",duplicated(HouseholdLaos_2C$o9))
  
  
  
# # #b. Outlier part 1
#For sex of respondents, 4 households missed data (3036, 3037, 3041, 3463), NA as gender for some households
#We'll replace it by "both in the same time"
HouseholdLaos_2C$o11 <- as.character(HouseholdLaos_2C$o11)
HouseholdLaos_2C$o11 <- ifelse(HouseholdLaos_2C$o11 == '', "Both man and woman", HouseholdLaos2$o11)
HouseholdLaos_2C$o11 <- as.factor(HouseholdLaos_2C$o11)
HouseholdLaos_2C$o11 <- as.factor(HouseholdLaos_2C$o11)

#Four households have extreme surprised number of plots rented-in (at d9_2), we check
x <- HouseholdLaos_2C[HouseholdLaos_2C$d9_2 >= 100,]
HouseholdLaos_2C$d9_2 <- ifelse(HouseholdLaos_2C$d9_2 > 100, NA,HouseholdLaos_2C$d9_2)

x <- ClowlandLaos_2C[,c(2,8,10:12)]

#For 3rd most important animal, I found selling price/kg (e7_41) of pig, goat, chicken 
#below surprise higher than that at other households. Please check and validate,
#I assume that enumerator asked selling price for a pig or for a flock of chickens
# - - -> Solved through outlier report

#There are some outliers at area of private land for growing forage (e12_1) one household had 
#area around 4 m2 (hhid: 3458) and 2 hhs had area equal 500 000 m2 (hhid: 3509, 3610)
# - - -> Solved through outlier report

#Similarly, we have 6 households with nature pasture area from 0 to 5 m2 (hhid: 3178, 3262,
#3458, 3542, 3546, 3560). Please check and confirm
# - - -> Solved through outlier report


# # #c. Corresponding fields

#There were 27 households who did no selling agri-products (at b1)
#but they still selected 3 main sources of income from crop production and
#livestock raising (b3). Please review list below to know and check record for
#validating data
#We correct it manually as no many Households concerned
HouseholdLaos_2C[,70] <- as.character(HouseholdLaos_2C[,70])
for (i in c(3044,3054,3303,3551)){
  HouseholdLaos_2C[HouseholdLaos_2C$o9 == i,70] <- "Selling own crops/fruits/ vegetable"
}
HouseholdLaos_2C[,70] <- as.factor(HouseholdLaos_2C[,70])

#Do the same for c7_check, c8 check, I also found 71 missing cases at c7 and xx 
#at c8 as followed tables
# Not possible to know the answer so no change is made
x <- HouseholdLaos_2C[,c(562,582,583)]

#One household (ID: 3372) missed all data at module D, so please check and validate if the
#household have info. In the case, the household did not have any kind of lands,
#so please consider it as a missing value case.
#Nothing was corrected by enumerators so we remove this individual for now
HouseholdLaos_2C <- HouseholdLaos_2C[HouseholdLaos_2C$o9 != 3372,]

#Big problem that we missed data for b13_01 (Which crop/vegetables/fruit did you sell to 1st buyer/
#outlet in the last 12 months) and b13_02 (Which crop/vegetables/fruit did you sell to 2nd buyer/
#outlet in the last 12 months)
# b13_01 & b13_02 are empty => ASK ENUM

#Inconsistencies between d18 and d17
#First we pass it under character format to be modified then
HouseholdLaos_2C$d17 <- as.character(HouseholdLaos_2C$d17)
#Then we change unexpected answers by "yes" when they use AE practices
HouseholdLaos_2C$d17 <- ifelse(HouseholdLaos_2C$d18 != '', "Yes", HouseholdLaos_2C$d17)
#And finally we change "yes" answers with no AE practice mentionned
HouseholdLaos_2C$d17 <- ifelse(HouseholdLaos_2C$d18 == '' & HouseholdLaos_2C$d17 == 'Yes', "No", HouseholdLaos_2C$d17)
#Then we convert it as a  factor again
HouseholdLaos_2C$d17 <- as.factor(HouseholdLaos_2C$d17)

# # #d.For answer c8, there are answers “yes” and “Yes” which must be grouped
#First we pass it under character format to be modified then
HouseholdLaos_2C$c8 <- as.character(HouseholdLaos_2C$c8)
#Then we change "unexpected answers"Yes" by "yes" to avoid double counting of positive answer
HouseholdLaos_2C$c8 <- ifelse(HouseholdLaos_2C$c8 == 'Yes', "yes", HouseholdLaos_2C$c8)
#Then we convert it as a  factor again
HouseholdLaos_2C$c8 <- as.factor(HouseholdLaos_2C$c8)

#For all following fields, we just check that answers are corresponding
#a14:a15
sum(HouseholdLaos_2C$a14 >0)-sum(HouseholdLaos_2C$a15 != '')
#b2:b2_1:b2_2
#OK
#b3: b31-b388
#OK
#b4_1: b5_1
sum(HouseholdLaos_2C$b4_1 !='')-sum(!is.na(HouseholdLaos_2C$b5_1))
#b4_2: b5_2
sum(HouseholdLaos_2C$b4_2 !='')-sum(!is.na(HouseholdLaos_2C$b5_2))
#b4_3: b5_3
sum(HouseholdLaos_2C$b4_3 !='')-sum(!is.na(HouseholdLaos_2C$b5_3))
#b9:b9_1-etc:b9_2-etc
#OK
#b1:b10:b11
x <- HouseholdLaos_2C[,c(1,70,193,202)]
#5 households declared that they sold crops products but did not anwered to b10 & b11
#HHID: 3044,3054,3303,3551,3222
#As we cannot know their answer, we cannot solve this issue
#b10:b10_1
#OK
#b1:b16:b17
x <- HouseholdLaos_2C[,c(1,70,203,205)]
#1 household is fulfilled with the wronf information (HHID: 3027)
HouseholdLaos_2C[,70] <- as.character(HouseholdLaos_2C[,70])
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3027, 70] <- "Selling both own crops/fruits/vegetables and livestock products"
HouseholdLaos_2C[,70] <- as.factor(HouseholdLaos_2C[,70])
#Moreover, 2 households declared that they sold crops products but did not anwered to b16 & b17
#HHID: 3222,3231
#As we cannot know their answer, we cannot solve this issue
#b16:b16a
x <- HouseholdLaos_2C[,c(1,203,204)]
#b1:b22
#OK
#b22:b22_1:b22_11-b22_199:b25:b26
x <- HouseholdLaos_2C[,c(1,206,525,526)]
#OK
#(b23_1a-b24_1b)
#b27:b27_1:b27_11-b27:188
#OK
#b28:b28_1
#OK
#c2:c3:c3a:c4:c5:c9:c6:c7:c8:c10:c11
x <- HouseholdLaos_2C[,c(1,562,581:585)]
#For c6, c7 and c8, many unanswered cells, we'll replace it by "no", 
#and also replace "Yes" by "yes" and "No" by "no" to homogenize the answers
for (i in c(581:585)){
  HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- ifelse(HouseholdLaos_2C$c3a != '' & HouseholdLaos_2C[,i] == '', 'no',HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- ifelse(HouseholdLaos_2C[,i] == 'Yes', 'yes',HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- ifelse(HouseholdLaos_2C[,i] == 'No', 'no',HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- as.factor(HouseholdLaos_2C[,i])
}
#d1:d1_1-d1_7
#OK
#d2_sum:d2_sum1-d2_sum11
#OK
#b1:b12_1:b12_2
x <- HouseholdLaos_2C[,c(1,70,654,657)]
#Many households delcared at b1 that they sold agri products but did not answered ar b12_1 and b12_2
#As we cannot know their answer, we cannot solve this issue
#b12_1:b13_1:b13_01-b13_0111:b14_1-b14_188:b15_1
x <- HouseholdLaos_2C[,c(1,654,672:682)]
#For b13_1 and related answers => OK
#For b14_1 and related answers => OK
x <- HouseholdLaos_2C[,c(1,654,684)]
#For b15_1 => OK
#b12_2:b13_02-b13_0211:b13_2:b14_2-b14_288:b15_2
x <- HouseholdLaos_2C[,c(1,657,697:707)]
#For b13_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it
#For b14_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it
x <- HouseholdLaos_2C[,c(1,657,697,698,709)]
#For b15_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it
#d1:d3_2:d3_3
x <- HouseholdLaos_2C[,c(1,616,710,711)]
#OK
#d1:d4_2:d4_3
x <- HouseholdLaos_2C[,c(1,616,712,713)]
#OK
#d1:d5_2:d5_3
x <- HouseholdLaos_2C[,c(1,616,714,715)]
#OK
#d1:d6_2:d6_3
x <- HouseholdLaos_2C[,c(1,616,716,717)]
#OK
#d7-d799
x <- HouseholdLaos_2C[,c(1,718:735)]
#OK
#d71:d7_11:d7_12:d7:13
x <- HouseholdLaos_2C[,c(1,720,737:739)]
#OK
#d72:d7_21:d7_22:d7:23
x <- HouseholdLaos_2C[,c(1,721,740:742)]
#OK
#d73:d7_31:d7_32:d7:33
x <- HouseholdLaos_2C[,c(1,722,743:745)]
#OK
#d74:d7_41:d7_42:d7:43
x <- HouseholdLaos_2C[,c(1,723,746:748)]
#OK
#d75:d7_51:d7_52:d7:53
x <- HouseholdLaos_2C[,c(1,724,749:751)]
#OK
#d76:d7_61:d7_62:d7:63
x <- HouseholdLaos_2C[,c(1,725,752:754)]
#OK
#d77:d7_71:d7_72:d7:73
x <- HouseholdLaos_2C[,c(1,726,755:757)]
#OK
#d78:d7_81:d7_82:d7:83
x <- HouseholdLaos_2C[,c(1,727,758:760)]
#OK
#d79:d7_91:d7_92:d7:93
x <- HouseholdLaos_2C[,c(1,728,761:763)]
#OK
#d710:d7_101:d7_102:d7:103
x <- HouseholdLaos_2C[,c(1,729,764:766)]
#OK
#d711:d7_111:d7_112:d7:113
x <- HouseholdLaos_2C[,c(1,730,767:769)]
#OK
#d712:d7_121:d7_122:d7:123
x <- HouseholdLaos_2C[,c(1,731,770:772)]
#OK
#d713:d7_131:d7_132:d7:133
x <- HouseholdLaos_2C[,c(1,732,773:775)]
#OK
#d714:d7_141:d7_142:d7:143
x <- HouseholdLaos_2C[,c(1,733,776:778)]
#OK
#d715:d7_151:d7_152:d7:153
x <- HouseholdLaos_2C[,c(1,734,779:781)]
#OK
#d799:d7_991:d7_992:d7:993
x <- HouseholdLaos_2C[,c(1,735,782:784)]
#OK
#d2a_1:d2b_1 :d81:d81_1a:d81_1b
x <- HouseholdLaos_2C[,c(1,624,632,785,791,793)]
#OK
#d2a_1:d2b_1 :d82:d82_1a:d82_1b
x <- HouseholdLaos_2C[,c(1,624,632,786,795,797)]
#HHID: 3574, 2 upland crops declared and confirmed but no information for the 2nd crops
#Nothing more to do as we don't have additionnal information
#HHID: 3584, 1 crop declared while information for 2nd most important crop
#we'll add this information
HouseholdLaos_2C[,632] <- as.character(HouseholdLaos_2C[,632])
HouseholdLaos_2C[HouseholdLaos_2C$o9 == '3584',632] <- 1
#d2a_1:d2b_1 :d83:d83_1a:d83_1b
x <- HouseholdLaos_2C[,c(1,624,632,787,799,801)]
#HHID: 3005,3022,3367,3610,3621,3206,3608, at least 3 crops declared and confirmed but no
#information for the 3rd crops, Nothing more to do as we don't have additionnal information
#HHID: 3276,3043,3563,3203, 2 crop declared while information for 3rd most important crop
#we'll add this information
for (i in c(3276,3043)){
  HouseholdLaos_2C[HouseholdLaos_2C$o9 == i,624] <- 3
}
HouseholdLaos_2C[HouseholdLaos_2C$o9 == '3203',632] <- 2
HouseholdLaos_2C[HouseholdLaos_2C$o9 == '3563',632] <- 3
HouseholdLaos_2C[,632] <- as.factor(HouseholdLaos_2C[,632])
#HHID: 3594, 2 crops declared while there are information for the 3rd crop, so we remove it
for (i in c(787,799,801)){
  HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
  HouseholdLaos_2C[HouseholdLaos_2C$o9 == '3594',i] <- ''
  HouseholdLaos_2C[,i] <- as.factor(HouseholdLaos_2C[,i])
}
#d9_1:d9_2
x <- HouseholdLaos_2C[,c(1,804,805)]
#HHID: 3084,3433,3438,3447,3568 declared plots rented out but no information of number of plots
#As we cannot know their answer, we cannot solve this issue
#d10_1:d10_2
x <- HouseholdLaos_2C[,c(1,806,807)]
#OK
#d11_1:d11_2:d11_4-d11_499
#OK
#d2a_1:d2b_1 :d12-d120
x <- HouseholdLaos_2C[,c(1,624,632,818:828)]
#OK
#d121:d131_1:d131_11-d131_111:d131_2
sum(HouseholdLaos_2C$d121 == 1, na.rm = T)-sum(HouseholdLaos_2C$d131_1 != '')
for (i in c(831:841)){
  print(sum(HouseholdLaos_2C$d121 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d121 == 1, na.rm = T)-sum(HouseholdLaos_2C$d131_2 != '')
#d122:d132_1:d132_11-d132_111:d132_2
sum(HouseholdLaos_2C$d122 == 1, na.rm = T)-sum(HouseholdLaos_2C$d132_1 != '')
for (i in c(845:855)){
  print(sum(HouseholdLaos_2C$d122 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d122 == 1, na.rm = T)-sum(HouseholdLaos_2C$d132_2 != '')
#d123:d133_1:d133_11-d133_111:d133_2
sum(HouseholdLaos_2C$d123 == 1, na.rm = T)-sum(HouseholdLaos_2C$d133_1 != '')
for (i in c(859:869)){
  print(sum(HouseholdLaos_2C$d123 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d123 == 1, na.rm = T)-sum(HouseholdLaos_2C$d133_2 != '')
#d124:d134_1:d134_11-d134_111:d134_2
sum(HouseholdLaos_2C$d124 == 1, na.rm = T)-sum(HouseholdLaos_2C$d134_1 != '')
for (i in c(873:883)){
  print(sum(HouseholdLaos_2C$d124 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d124 == 1, na.rm = T)-sum(HouseholdLaos_2C$d134_2 != '')
#d125:d135_1:d135_11-d135_111:d135_2
sum(HouseholdLaos_2C$d125 == 1, na.rm = T)-sum(HouseholdLaos_2C$d135_1 != '')
for (i in c(887:897)){
  print(sum(HouseholdLaos_2C$d125 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d125 == 1, na.rm = T)-sum(HouseholdLaos_2C$d135_2 != '')
#d126:d136_1:d136_11-d136_111:d136_2
sum(HouseholdLaos_2C$d126 == 1, na.rm = T)-sum(HouseholdLaos_2C$d136_1 != '')
for (i in c(901:911)){
  print(sum(HouseholdLaos_2C$d126 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d126 == 1, na.rm = T)-sum(HouseholdLaos_2C$d136_2 != '')
#d127:d137_1:d137_11-d137_111:d137_2
sum(HouseholdLaos_2C$d127 == 1, na.rm = T)-sum(HouseholdLaos_2C$d137_1 != '')
for (i in c(915:925)){
  print(sum(HouseholdLaos_2C$d127 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d127 == 1, na.rm = T)-sum(HouseholdLaos_2C$d137_2 != '')
#d1299:d138_1:d138_11-d138_111:d138_2
sum(HouseholdLaos_2C$d1299 == 1, na.rm = T)-sum(HouseholdLaos_2C$d138_1 != '')
for (i in c(929:939)){
  print(sum(HouseholdLaos_2C$d1299 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d1299 == 1, na.rm = T)-sum(HouseholdLaos_2C$d138_2 != '')
#d2a_1:d2b_1 :d14-d1499
x <- HouseholdLaos_2C[,c(1,624,632,942:951)]
#OK
#d141:d151_1:d151_11-d151_111:d151_2
sum(HouseholdLaos_2C$d141 == 1, na.rm = T)-sum(HouseholdLaos_2C$d151_1 != '')
for (i in c(954:964)){
  print(sum(HouseholdLaos_2C$d141 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d141 == 1, na.rm = T)-sum(HouseholdLaos_2C$d151_2 != '')
#d142:d152_1:d152_11-d152_111:d152_2
sum(HouseholdLaos_2C$d142 == 1, na.rm = T)-sum(HouseholdLaos_2C$d152_1 != '')
for (i in c(968:978)){
  print(sum(HouseholdLaos_2C$d142 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d142 == 1, na.rm = T)-sum(HouseholdLaos_2C$d152_2 != '')
#d143:d153_1:d153_11-d153_111:d153_2
sum(HouseholdLaos_2C$d143 == 1, na.rm = T)-sum(HouseholdLaos_2C$d153_1 != '')
for (i in c(982:992)){
  print(sum(HouseholdLaos_2C$d143 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d143 == 1, na.rm = T)-sum(HouseholdLaos_2C$d153_2 != '')
#d144:d154_1:d154_11-d154_111:d154_2
sum(HouseholdLaos_2C$d144 == 1, na.rm = T)-sum(HouseholdLaos_2C$d154_1 != '')
for (i in c(996:1006)){
  print(sum(HouseholdLaos_2C$d144 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d144 == 1, na.rm = T)-sum(HouseholdLaos_2C$d154_2 != '')
#d145:d155_1:d155_11-d155_111:d155_2
sum(HouseholdLaos_2C$d145 == 1, na.rm = T)-sum(HouseholdLaos_2C$d155_1 != '')
for (i in c(1010:1020)){
  print(sum(HouseholdLaos_2C$d145 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d145 == 1, na.rm = T)-sum(HouseholdLaos_2C$d155_2 != '')
#d146:d156_1:d156_11-d156_111:d156_2
sum(HouseholdLaos_2C$d146 == 1, na.rm = T)-sum(HouseholdLaos_2C$d156_1 != '')
for (i in c(1024:1034)){
  print(sum(HouseholdLaos_2C$d146 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d146 == 1, na.rm = T)-sum(HouseholdLaos_2C$d156_2 != '')
#d147:d157_1:d157_11-d157_111:d157_2
sum(HouseholdLaos_2C$d147 == 1, na.rm = T)-sum(HouseholdLaos_2C$d157_1 != '')
for (i in c(1038:1048)){
  print(sum(HouseholdLaos_2C$d147 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d147 == 1, na.rm = T)-sum(HouseholdLaos_2C$d157_2 != '')
#d1499:d158_1:d158_11-d158_111:d158_2
sum(HouseholdLaos_2C$d1499 == 1, na.rm = T)-sum(HouseholdLaos_2C$d158_1 != '')
for (i in c(1052:1062)){
  print(sum(HouseholdLaos_2C$d1499 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d1499 == 1, na.rm = T)-sum(HouseholdLaos_2C$d158_2 != '')
#d2a_1:d2b_1 :d120:d16
x <- HouseholdLaos_2C[,c(1,624,632,943,1065)]
#OK
#d2a_1:d2b_1 :d17:d18:d181-d1899
x <- HouseholdLaos_2C[,c(17,624,632,1066,1067,1068:1079)]
#For d17, many households answered yes or no while the answer at d18 is not corresponding
#We'll solve this issue
HouseholdLaos_2C$d17 <- as.character(HouseholdLaos_2C$d17)
HouseholdLaos_2C$d17 <- ifelse(!is.na(HouseholdLaos_2C$d181), 'Yes', HouseholdLaos_2C$d17)
HouseholdLaos_2C$d17 <- ifelse(is.na(HouseholdLaos_2C$d181) & HouseholdLaos_2C$d17 == "Yes" , 'No', HouseholdLaos_2C$d17)
HouseholdLaos_2C$d17 <- as.factor(HouseholdLaos_2C$d17)
#d181:d18_11:d18_111-d18_1111:d18_12
sum(HouseholdLaos_2C$d181 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_11 != '')
for (i in c(1082:1092)){
  print(sum(HouseholdLaos_2C$d181 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d181 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_12 != '')
#d182:d18_21:d18_211- d18_2111:d18_22
sum(HouseholdLaos_2C$d182 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_21 != '')
for (i in c(1096:1106)){
  print(sum(HouseholdLaos_2C$d182 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d182 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_22 != '')
#d183:d18_31:d18_311- d18_3111:d18_32
sum(HouseholdLaos_2C$d183 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_31 != '')
for (i in c(1110:1120)){
  print(sum(HouseholdLaos_2C$d183 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d183 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_32 != '')
#d184:d18_41:d18_411- d18_4111:d18_42
sum(HouseholdLaos_2C$d184 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_41 != '')
for (i in c(1124:1134)){
  print(sum(HouseholdLaos_2C$d184 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d184 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_42 != '')
#d185:d18_51:d18_511- d18_5111:d18_52
sum(HouseholdLaos_2C$d185 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_51 != '')
for (i in c(1138:1148)){
  print(sum(HouseholdLaos_2C$d185 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d185 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_52 != '')
#d186:d18_61:d18_611- d18_6111:d18_62
sum(HouseholdLaos_2C$d186 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_61 != '')
for (i in c(1152:1162)){
  print(sum(HouseholdLaos_2C$d186 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d186 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_62 != '')
#d187:d18_71:d18_711- d18_7111:d18_72
sum(HouseholdLaos_2C$d187 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_71 != '')
for (i in c(1166:1176)){
  print(sum(HouseholdLaos_2C$d187 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d187 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_72 != '')
#d188:d18_81:d18_811- d18_8111:d18_82
sum(HouseholdLaos_2C$d188 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_81 != '')
for (i in c(1180:1190)){
  print(sum(HouseholdLaos_2C$d188 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d188 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_82 != '')
#d189:d18_91:d18_911- d18_9111:d18_92
sum(HouseholdLaos_2C$d189 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_91 != '')
for (i in c(1194:1204)){
  print(sum(HouseholdLaos_2C$d189 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d189 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_92 != '')
#d1810:d18_101:d18_1011- d18_10111:d18_102
sum(HouseholdLaos_2C$d1810 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_101 != '')
for (i in c(1208:1218)){
  print(sum(HouseholdLaos_2C$d1810 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d1810 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_102 != '')
#d1811:d18_111:d18_1111- d18_11111:d18_112_a
sum(HouseholdLaos_2C$d1811 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_111_a != '')
for (i in c(1222:1232)){
  print(sum(HouseholdLaos_2C$d1811 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d1811 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_112_a != '')
#d1899:d18_991:d18_9911- d18_99111:d18_992
sum(HouseholdLaos_2C$d1899 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_991 != '')
for (i in c(1236:1246)){
  print(sum(HouseholdLaos_2C$d1899 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d1899 == 1, na.rm = T)-sum(HouseholdLaos_2C$d18_992 != '')
#d2a_1:d2b_1 :d20:d21:d211-d2199
x <- HouseholdLaos_2C[,c(17,624,632,1250,1251,1252:1266)]
#For d120, one household did not answered while he have crops
#We'll solve this issue
HouseholdLaos_2C$d20 <- as.character(HouseholdLaos_2C$d20)
HouseholdLaos_2C[HouseholdLaos_2C$o9 == '3371',1250] <- "No"
HouseholdLaos_2C$d20 <- as.factor(HouseholdLaos_2C$d20)
#d211:d21_12:d21_121-d21_1211:d21_13
sum(HouseholdLaos_2C$d211 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_12 != '')
for (i in c(1269:1279)){
  print(sum(HouseholdLaos_2C$d211 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d211 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_13 != '')
#d212:d21_22:d21_221- d21_2211:d21_23
sum(HouseholdLaos_2C$d212 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_22 != '')
for (i in c(1283:1293)){
  print(sum(HouseholdLaos_2C$d212 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d212 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_23 != '')
#d213:d21_32:d21_321- d21_3211:d21_33
sum(HouseholdLaos_2C$d213 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_32 != '')
for (i in c(1297:1307)){
  print(sum(HouseholdLaos_2C$d213 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d213 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_33 != '')
#d214:d21_42:d21_421- d21_4211:d21_43
sum(HouseholdLaos_2C$d214 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_42 != '')
for (i in c(1311:1321)){
  print(sum(HouseholdLaos_2C$d214 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d214 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_43 != '')
#d215:d21_52:d21_521- d21_5211:d21_53
sum(HouseholdLaos_2C$d215 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_52 != '')
for (i in c(1325:1335)){
  print(sum(HouseholdLaos_2C$d215 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d215 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_53 != '')
#d216:d21_62:d21_621- d21_6211:d21_63
sum(HouseholdLaos_2C$d216 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_62 != '')
for (i in c(1339:1349)){
  print(sum(HouseholdLaos_2C$d216 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d216 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_63 != '')
#d217:d21_72:d21_721- d21_7211:d21_73
sum(HouseholdLaos_2C$d217 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_72 != '')
for (i in c(1353:1363)){
  print(sum(HouseholdLaos_2C$d217 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d217 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_73 != '')
#d218:d21_82:d21_821- d21_8211:d21_83
sum(HouseholdLaos_2C$d218 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_82 != '')
for (i in c(1367:1377)){
  print(sum(HouseholdLaos_2C$d218 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d218 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_83 != '')
#d219:d21_92:d21_921- d21_9211:d21_93
sum(HouseholdLaos_2C$d219 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_92 != '')
for (i in c(1381:1391)){
  print(sum(HouseholdLaos_2C$d219 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d219 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_93 != '')
#d2110:d21_102:d21_1021- d21_10211:d21_103
sum(HouseholdLaos_2C$d2110 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_102 != '')
for (i in c(1395:1405)){
  print(sum(HouseholdLaos_2C$d2110 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2110 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_103 != '')
#d2111:d21_112:d21_1121- d21_11211:d21_113
sum(HouseholdLaos_2C$d2111 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_112 != '')
for (i in c(1409:1419)){
  print(sum(HouseholdLaos_2C$d2111 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2111 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_113 != '')
#d2112:d21_122:d21_1221- d21_12211:d21_123
sum(HouseholdLaos_2C$d2112 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_122_a != '')
for (i in c(1423:1433)){
  print(sum(HouseholdLaos_2C$d2112 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2112 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_123_a != '')
#d2113:d21_132:d21_1321- d21_13211:d21_133
sum(HouseholdLaos_2C$d2113 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_132 != '')
for (i in c(1437:1447)){
  print(sum(HouseholdLaos_2C$d2113 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2113 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_133 != '')
#d2114:d21_142:d21_1421- d21_14211:d21_143
sum(HouseholdLaos_2C$d2114 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_142 != '')
for (i in c(1451:1461)){
  print(sum(HouseholdLaos_2C$d2114 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2114 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_143 != '')
#d2199:d21_992:d21_9921- d21_99211:d21_993
sum(HouseholdLaos_2C$d2199 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_992 != '')
for (i in c(1465:1475)){
  print(sum(HouseholdLaos_2C$d2199 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2199 == 1, na.rm = T)-sum(HouseholdLaos_2C$d21_993 != '')
#d2a_1:d2b_1 :d24:d25
x <- HouseholdLaos_2C[,c(17,624,632,1480,1481)]
#OK
#d2a_1:d2b_1 :d26:d27:d271-d2799
x <- HouseholdLaos_2C[,c(17,624,632,1482:1498)]
#OK
#d271:d27_11:d27_111-d27_1111:d27_12
sum(HouseholdLaos_2C$d271 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_11 != '')
for (i in c(1501:1511)){
  print(sum(HouseholdLaos_2C$d271 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d271 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_12 != '')
#d272:d27_21:d27_211- d27_2111:d27_22
sum(HouseholdLaos_2C$d272 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_21 != '')
for (i in c(1515:1525)){
  print(sum(HouseholdLaos_2C$d272 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d272 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_22 != '')
#d273:d27_31:d27_311- d27_3111:d27_32
sum(HouseholdLaos_2C$d273 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_31 != '')
for (i in c(1529:1539)){
  print(sum(HouseholdLaos_2C$d272 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d273 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_32 != '')
#d274:d27_41:d27_411- d27_4111:d27_42
sum(HouseholdLaos_2C$d274 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_41 != '')
for (i in c(1543:1553)){
  print(sum(HouseholdLaos_2C$d274 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d274 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_42 != '')
#d275:d27_51:d27_511- d27_5111:d27_52
sum(HouseholdLaos_2C$d275 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_51 != '')
for (i in c(1557:1567)){
  print(sum(HouseholdLaos_2C$d275 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d275 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_52 != '')
#d276:d27_61:d27_611- d27_6111:d27_62
sum(HouseholdLaos_2C$d276 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_61 != '')
for (i in c(1570:1580)){
  print(sum(HouseholdLaos_2C$d276 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d276 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_62 != '')
#d277:d27_71:d27_711- d27_7111:d27_72
sum(HouseholdLaos_2C$d277 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_71 != '')
for (i in c(1585:1595)){
  print(sum(HouseholdLaos_2C$d277 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d277 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_72 != '')
#d278:d27_81:d27_811- d27_8111:d27_82
sum(HouseholdLaos_2C$d278 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_81 != '')
for (i in c(1599:1609)){
  print(sum(HouseholdLaos_2C$d278 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d278 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_82 != '')
#d279:d27_91:d27_911- d27_9111:d27_92
sum(HouseholdLaos_2C$d279 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_91 != '')
for (i in c(1613:1623)){
  print(sum(HouseholdLaos_2C$d279 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d279 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_92 != '')
#d2710:d27_101:d27_1011- d27_10111:d27_102
sum(HouseholdLaos_2C$d2710 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_101 != '')
for (i in c(1627:1637)){
  print(sum(HouseholdLaos_2C$d2710 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2710 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_102 != '')
#d2711:d27_111:d27_1111- d27_11111:d27_112
sum(HouseholdLaos_2C$d2711 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_111_a != '')
for (i in c(1641:1651)){
  print(sum(HouseholdLaos_2C$d2711 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2711 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_112_a != '')
#d2712:d27_121:d27_1211- d27_12111:d27_122
sum(HouseholdLaos_2C$d2712 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_121 != '')
for (i in c(1655:1665)){
  print(sum(HouseholdLaos_2C$d2712 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2712 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_122 != '')
#d2713:d27_131:d27_1311- d27_13111:d27_132
sum(HouseholdLaos_2C$d2713 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_131 != '')
for (i in c(1669:1679)){
  print(sum(HouseholdLaos_2C$d2713 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2713 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_132 != '')
#d2714:d27_141:d27_1411- d27_14111:d27_142
sum(HouseholdLaos_2C$d2714 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_141 != '')
for (i in c(1683:1693)){
  print(sum(HouseholdLaos_2C$d2714 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2714 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_142 != '')
#d2799:d27_991:d27_9911- d27_99111:d27_992
sum(HouseholdLaos_2C$d2799 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_991 != '')
for (i in c(1697:1707)){
  print(sum(HouseholdLaos_2C$d2799 == 1, na.rm = T)-sum(!is.na(HouseholdLaos_2C[,i])))
}
sum(HouseholdLaos_2C$d2799 == 1, na.rm = T)-sum(HouseholdLaos_2C$d27_992 != '')
#d2a_1:d2b_1 :d30_1:d30_2:d30_21-d30_211
x <- HouseholdLaos_2C[,c(1,624,632,1712,1713,1714:1724)]
#OK
#d2a_1:d2b_1 :d30_3:d30_4:d30_41-d30_411
x <- HouseholdLaos_2C[,c(1,624,632,1725:1737)]
#OK
#d2a_1:d2b_1 :d30_5:d30_6:d30_61-d30_611
x <- HouseholdLaos_2C[,c(1,624,632,1738:1750)]
#OK
#d2a_1:d2b_1 :d32:d32_1:d32_11-d32_111
x <- HouseholdLaos_2C[,c(1,624,632,1751:1763)]
#OK
#d2a_1:d2b_1:d32:d32_2
x <- HouseholdLaos_2C[,c(1,1751,1764)]
#OK
#d2a_1:d2b_1:d33_1 :d33_10-d33_13
x <- HouseholdLaos_2C[,c(1,624,632,1766:1770)]
#OK
#d2a_1:d2b_1:d33_2 :d33_20:d33_23
x <- HouseholdLaos_2C[,c(1,624,632,1771:1775)]
#OK
#d2a_1:d2b_1:d33_3 :d33_30:d33_33
x <- HouseholdLaos_2C[,c(1,624,632,1776:1780)]
#OK
#d2a_1:d2b_1:d33_4 :d33_40:d33_43
x <- HouseholdLaos_2C[,c(1,624,632,1781:1785)]
#OK
#d2a_1:d2b_1:d33_5 :d33_50:d33_53
x <- HouseholdLaos_2C[,c(1,624,632,1786:1790)]
#OK
#d2a_1:d2b_1:d33_6 :d33_60:d33_63
x <- HouseholdLaos_2C[,c(1,624,632,1791:1795)]
#OK
#d2a_1:d2b_1:d33_7 :d33_70:d33_73
x <- HouseholdLaos_2C[,c(1,624,632,1796:1800)]
#OK
#d2a_1:d2b_1:d33_8 :d33_80:d33_83
x <- HouseholdLaos_2C[,c(1,624,632,1801:1805)]
#OK
#d2a_1:d2b_1:d33_9 :d33_90:d33_93
x <- HouseholdLaos_2C[,c(1,624,632,1806:1810)]
#OK
#d2a_1:d2b_1:d33_10 :d33_100:d33_103
x <- HouseholdLaos_2C[,c(1,624,632,1811:1815)]
#OK
#d2a_1:d2b_1:d33_11 :d33_110:d33_113
x <- HouseholdLaos_2C[,c(1,624,632,1816:1820)]
#OK
#d2a_1:d2b_1:d34 :d35
x <- HouseholdLaos_2C[,c(1,624,632,1821,1822)]
#OK
#d2a_1:d2b_1:d36 :d361-d3699
x <- HouseholdLaos_2C[,c(1,624,632,1823:1835)]
#OK
#e1 :e2 :e2_1 :e2_99
#OK
#e1 :e2_1 :e3_1:e3_2
x <- HouseholdLaos_2C[,c(1,1837,1839,1854,1855)]
#OK
#e1 :e2_2 :e3_3:e3_4
x <- HouseholdLaos_2C[,c(1,1837,1840,1856,1857)]
#OK
#e1 :e2_3 :e3_5:e3_6
x <- HouseholdLaos_2C[,c(1,1837,1841,1858,1859)]
#OK
#e1 :e2_4 :e3_7:e3_8
x <- HouseholdLaos_2C[,c(1,1837,1842,1860,1861)]
#OK
#e1 :e2_5 :e3_9:e3_10
x <- HouseholdLaos_2C[,c(1,1837,1843,1862,1863)]
#OK
#e1 :e2_6 :e3_11
x <- HouseholdLaos_2C[,c(1,1837,1844,1864)]
#OK
#e1 :e2_7 :e3_12
x <- HouseholdLaos_2C[,c(1,1837,1845,1865)]
#OK
#e1 :e2_8 :e3_13
x <- HouseholdLaos_2C[,c(17,1837,1846,1866)]
#OK
#e1 :e2_9 :e3_14
x <- HouseholdLaos_2C[,c(1,1837,1847,1867)]
#OK
#e1 :e2_10 :e3_15
x <- HouseholdLaos_2C[,c(1,1837,1848,1868)]
#OK
#e1 :e2_11 :e3_16
x <- HouseholdLaos_2C[,c(1,1837,1849,1869)]
#OK
#e1 :e2_98 :e3_98
x <- HouseholdLaos_2C[,c(1,1837,1850,1870)]
#OK
#e1 :e2_99 :e3_99
x <- HouseholdLaos_2C[,c(1,1837,1851,1871)]
#OK
#e1:e4_1:e5_a:e5_b:e5_2:e5_1-e5_4:e5_5:b18_1:b19_1:b20_1:b20_10:b20_188:b21_1
x <- HouseholdLaos_2C[,c(1,1837,1872,1878:1881,1883:1886,1888,1890,1893,1894:1903,1905)]
#OK
#e5_4:e5_41
x <- HouseholdLaos_2C[,c(1,1886,1887)]
#OK
#e5_5:e5_51
x <- HouseholdLaos_2C[,c(1,1888,1889)]
#HHID: 3048, 3045, 3035 the selling price is missing, REPLACE
#As we cannot know their answer, we cannot solve this issue
#e1:e4_2:e6_a:e6_b:e6_1-e6_5:b18_2:b19_2:b20_2:b20_20:b20_288:b21_2
x <- HouseholdLaos_2C[,c(1,1837,1874,1906:1909,1911:1914,1916,1918,1921,1922,1924)]
#OK
#e6_4:e6_41
x <- HouseholdLaos_2C[,c(1,1914,1915)]
#OK
#e6_5:e6_51
x <- HouseholdLaos_2C[,c(1,1916,1917)]
#OK
#e1:e4_3:e7_a:e7_b:e7_2:e7_1-e7_4:e7_5
x <- HouseholdLaos_2C[,c(1,1837,1876,1925:1928,1930:1933,1935)]
#OK
#e7_4:e7_41
x <- HouseholdLaos_2C[,c(1,1933,1934)]
#OK
#e7_5:e7_51
x <- HouseholdLaos_2C[,c(1,1935,1936)]
#HHID: 3162,3147,3316 the selling price is missing, REPLACE
#As we cannot know their answer, we cannot solve this issue
#e8:e9:e10
x <- HouseholdLaos_2C[,c(1,1937:1939)]
#OK
#e12_1:e12_2
x <- HouseholdLaos_2C[,c(1,1940:1941)]
#OK
#e12_3:e12_4
x <- HouseholdLaos_2C[,c(1,1942:1943)]
#Some households declared 0,1 or 2 m2 of pasture, we remove it
HouseholdLaos_2C$e12_4 <- ifelse(HouseholdLaos_2C$e12_4 < 3, NA, HouseholdLaos_2C$e12_4)
HouseholdLaos_2C$e12_3 <- as.character(HouseholdLaos_2C$e12_3)
HouseholdLaos_2C$e12_3 <- ifelse(HouseholdLaos_2C$e12_3 == 'Yes' & is.na(HouseholdLaos_2C$e12_4), 'No', HouseholdLaos_2C$e12_3)
HouseholdLaos_2C$e12_3 <- as.factor(HouseholdLaos_2C$e12_3)
#e12_5:e12_6
x <- HouseholdLaos_2C[,c(1,1944:1945)]
#OK
#e13:e13_1
x <- HouseholdLaos_2C[,c(1,1946:1947)]
#OK
#e2_1 :e2_2:e16:e17:e18:e19:e191-e1999
x <- HouseholdLaos_2C[,c(1,1839:1840,1950:1964)]
#OK
#e2_1 :e2_2:e21:e22:e23:e24:e241-e2499
x <- HouseholdLaos_2C[,c(1,1839:1840,1966:1980)]
#OK
#e2_1 :e2_2:e25:e26
x <- HouseholdLaos_2C[,c(1,1839:1840,1982,1988)]
#OK
#e25:e25_1:e25_11-e25_199
x <- HouseholdLaos_2C[,c(1,1839:1840,1982:1987)]
#OK
#e26:e26_1:e26_11-e26_199
x <- HouseholdLaos_2C[,c(1,1839:1840,1988:1994)]
#OK
#e26:e27:e270-e272:e27a_1
x <- HouseholdLaos_2C[,c(1,1839:1840,1988,1995:1999)]
#OK
#e2_1 :e2_2:e28
x <- HouseholdLaos_2C[,c(1,1839:1840,2003)]
#OK
#e2_1 :e2_2:e29:e29_1:e21_11:e29_10
x <- HouseholdLaos_2C[,c(1,1839:1840,2003:2009)]
#OK
#e2_3:e30:e31:e32:e33:e34:e341-e3499
x <- HouseholdLaos_2C[,c(17,1841,1858,1859,2013,2015:2029)]
#For many households who raise pigs, no information about pig raising systems 
#(Probably households with only adult pigs)
#As we cannot know their answer, we cannot solve this issue
#e30:e35:e36:e37:e38:e381-38499
x <- HouseholdLaos_2C[,c(17,2013,2030:2044)]
#OK
#e30 :e39
x <- HouseholdLaos_2C[,c(17,2013,2046)]
#OK
#e39:e39_1:e39_11-e39_199
x <- HouseholdLaos_2C[,c(2046:2051)]
#OK
#e30 :e40
x <- HouseholdLaos_2C[,c(17,2013,2052)]
#OK
#e40 :e40_1:e41:e410:e412
x <- HouseholdLaos_2C[,c(17,2052:2057)]
#OK
#30 :e42
x <- HouseholdLaos_2C[,c(17,2013,2062)]
#OK
#e42 :e43
x <- HouseholdLaos_2C[,c(17,2062,2063)]
#OK
#e43:e43_1:e43_11-e43_10
x <- HouseholdLaos_2C[,c(17,2063:2068)]
#OK
#e2_8 :e44:e45:e46:e47:e48:e481-e4899
x <- HouseholdLaos_2C[,c(17,1846,2072,2074:2088)]
#For some households we have no information about their raising systems
#As we cannot know their answer, we cannot solve this issue
#For other households, there are information but they are no raising chickens, we remove these informations
for (i in c(2072,2074:2088)){
  HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- ifelse(HouseholdLaos_2C$e2_8 == 0, '', HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- as.factor(HouseholdLaos_2C[,i])
}
#e44:e49:e50:e51:e52:e521-e5299:e53:e54:e56
x <- HouseholdLaos_2C[,c(17,2072,2089:2103,2105,2111,2113)]
#For some households we have no information about their raising systems
#As we cannot know their answer, we cannot solve this issue
#For other households, there are information but they are no raising pigs, we remove these informations
for (i in c(2089:2103,2105,2111,2113)){
  HouseholdLaos_2C[,i] <- as.character(HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- ifelse(HouseholdLaos_2C$e2_8 == 0, '', HouseholdLaos_2C[,i])
  HouseholdLaos_2C[,i] <- as.factor(HouseholdLaos_2C[,i])
}
#e53:e53_1:e53_11-e53_199
x <- HouseholdLaos_2C[,c(2105:2110)]
#OK
#e54:e54_1:e55:e550-e552
x <- HouseholdLaos_2C[,c(2111:2116)]
#OK
#e56:e57
x <- HouseholdLaos_2C[,c(2121:2122)]
#OK
#e57:e57_1:e57_11-e57_10
x <- HouseholdLaos_2C[,c(2122:2127)]
#OK
#e2_1,e2_2,e58
x <- HouseholdLaos_2C[,c(1839,1840,2131)]
#OK
#e58:e58_1:e58_11-e58_112
x <- HouseholdLaos_2C[,c(2131:2144)]
#OK
#e1,e59
x <- HouseholdLaos_2C[,c(1837,2145)]
#OK
#e59:e59_1:e59_11:e59_112
x <- HouseholdLaos_2C[,c(2145:2158)]
#OK
#e1:e60
x <- HouseholdLaos_2C[,c(1837,2159)]
#OK
#h4_1:h4_11-h4_188
x <- HouseholdLaos_2C[,c(2176:2183)]
#OK


## 4.2 Data cleaning for "HouseholdMember_2"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(HouMemberLaos_2C$pid))
#Check and remove the real duplicates through automated method
Dum <- HouMemberLaos_2C[duplicated(HouMemberLaos_2C$pid),]
idDup <- unique(Dum$hhid_re1)
DumReal <- HouMemberLaos_2C[HouMemberLaos_2C$hhid_re1 %in% idDup,]
DumReal$check <- paste(DumReal$p_no, DumReal$hhid_re1, DumReal$a1)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
HouMemberLaos_2C <- HouMemberLaos_2C[!rownames(HouMemberLaos_2C) %in% Duplic,]
#For some duplicates, we have to do it manually:
#Re-attribute household members numbers when the household id is duplicated for different household members
Dum <- HouMemberLaos_2C[duplicated(HouMemberLaos_2C$pid),]
idDup <- unique(Dum$hhid_re1)
idDup
for (i in idDup){
  HouMemberLaos_2C$p_no[HouMemberLaos_2C$hhid_re1 == i] <- 1:sum(HouMemberLaos_2C$hhid_re1 == i)
}
HouMemberLaos_2C$pid <- paste(HouMemberLaos_2C$hhid_re1,HouMemberLaos_2C$p_no)
#Check again the duplicates: 
count_if("TRUE",duplicated(HouMemberLaos_2C$pid))

# # #Corresponding field
#Everything is ok in the table, we'll just check if number of household members
#is corresponding with the other table
x <- count(HouMemberLaos_2C, hhid_re1)
y <- HouseholdLaos_2C[,c(1,53)]
G <- merge(x, y, by.x = "hhid_re1", by.y = "o9", all.x = T, all.y= T, sort = TRUE)
#Some households did not answered to a0 while we have the correct number of members
#We will correct it)
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3024, 53] <-  10
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3114, 53] <-  14
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3261, 53] <-  10
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3372, 53] <-  3
# For following households, we know the number of households members but no details on them
#HHID: 3028,3303,3340,3368,3401,3429,3436,3447,3451,3472,3537,3538,3550,3551
#3558,3564,3565,3611

## 4.3 Data cleaning for "ClowlandLaos_2"

# # #a.Check for crops duplicates
ClowlandLaos_2C$pid <- paste(ClowlandLaos_2C$hhid_re2,ClowlandLaos_2C$crop1_now)
ClowlandLaos_2C <- ClowlandLaos_2C %>% relocate(pid , .after = hhid_re2)
#Check the number and id of duplicates
count_if("TRUE",duplicated(ClowlandLaos_2C$pid))
#Check and remove the real duplicates through automated method
Dum <- ClowlandLaos_2C[duplicated(ClowlandLaos_2C$pid),]
idDup <- unique(Dum$hhid_re2)
DumReal <- ClowlandLaos_2C[ClowlandLaos_2C$hhid_re2 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_13e, DumReal$d2_132)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
ClowlandLaos_2C <- ClowlandLaos_2C[!rownames(ClowlandLaos_2C) %in% Duplic,]
#For some duplicates, we have to do it manually:
#Re-attribute household members numbers when the household id is duplicated for different household members
Dum <- ClowlandLaos_2C[duplicated(ClowlandLaos_2C$pid),]
idDup <- unique(Dum$hhid_re2)
idDup
for (i in idDup){
  ClowlandLaos_2C$crop1_now[ClowlandLaos_2C$hhid_re2 == i] <- 1:sum(ClowlandLaos_2C$hhid_re2 == i)
}
ClowlandLaos_2C$pid <- paste(ClowlandLaos_2C$hhid_re2,ClowlandLaos_2C$crop1_now)
#Check again the duplicates: 
count_if("TRUE",duplicated(ClowlandLaos_2C$pid))

# # #b. Wrong writings
#Some words have been written with two different ortographs for lowland crops, let's correct it
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "cabbage", "Cabbages")
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "chineselattuce", "Chinese lettuce")
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "cucumber", "Cucumber")
ClowlandLaos_2C$d2_13e <- ifelse(ClowlandLaos_2C$d2_13e == "Chillies and peppers, dry (Capsicum spp., Pimenta spp.), raw", "Chilli", ClowlandLaos_2C$d2_13e)
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "chili", "Chilli")
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "garlic", "Garlic")
ClowlandLaos_2C$d2_13e <- str_replace(ClowlandLaos_2C$d2_13e, "onion", "Onions")

# # #b. Check for outliers

# d2_132 lowland - hhid 3320 had unbelievable total land areas (700090 m2).
ClowlandLaos_2C$d2_132 <- str_replace(ClowlandLaos_2C$d2_132, '700090', '70000')
ClowlandLaos_2C$d2_132 <- as.numeric(ClowlandLaos_2C$d2_132)

#Please cross-check price of Summer-autumn season rice, some cases are at 10000000 kip/kg but 
#others are 1000 at d2_137.
ClowlandLaos_2C$d2_137 <- ifelse(ClowlandLaos_2C$d2_137 > 1000000, NA,ClowlandLaos_2C$d2_137)

# # #c. Corresponding fields
#Everything is ok in the table, we'll just check if number of crops
#is corresponding with the other table
x <- count(ClowlandLaos_2C, hhid_re2)
y <- HouseholdLaos_2C[,c(1,625)]
G <- merge(x, y, by.x = "hhid_re2", by.y = "o9", all.x = T, all.y= T, sort = TRUE)
#Some households did not answered to a0 while we have the correct number of members
#We will correct it)
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3024, 625] <- 7
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3114, 625] <-  11
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3261, 625] <-  4
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3552, 625] <-  1
# For following households, we know the number of households members but no details on them
#HHID: 3019, 3303, 3340, 3368, 3550, 3558, 3565


## 4.4 Data cleaning for "CuplandLaos_2"

# # #a.Check for crops duplicates
CuplandLaos_2$pid <- paste(CuplandLaos_2$hhid_re3,CuplandLaos_2$crop2_now)
CuplandLaos_2 <- CuplandLaos_2 %>% relocate(pid , .after = hhid_re3)
#Check the number and id of duplicates
count_if("TRUE",duplicated(CuplandLaos_2$pid))
#Check and remove the real duplicates through automated method
Dum <- CuplandLaos_2[duplicated(CuplandLaos_2$pid),]
idDup <- unique(Dum$hhid_re3)
DumReal <- CuplandLaos_2[CuplandLaos_2$hhid_re3 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_23e, DumReal$d2_232)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
CuplandLaos_2 <- CuplandLaos_2[!rownames(CuplandLaos_2) %in% Duplic,]
#For some duplicates, we have to do it manually:
#Re-attribute household members numbers when the household id is duplicated for different household members
Dum <- CuplandLaos_2[duplicated(CuplandLaos_2$pid),]
idDup <- unique(Dum$hhid_re3)
idDup
for (i in idDup){
  CuplandLaos_2$crop2_now[CuplandLaos_2$hhid_re3 == i] <- 1:sum(CuplandLaos_2$hhid_re3 == i)
}
CuplandLaos_2$pid <- paste(CuplandLaos_2$hhid_re3,CuplandLaos_2$crop2_now)
#Check again the duplicates: 
count_if("TRUE",duplicated(CuplandLaos_2$pid))

# # #b. Some words have been written with two different ortographs for upland crops, let's correct it
CuplandLaos_2$d2_23e <- str_replace(CuplandLaos_2$d2_23e, "garlic", "Garlic")
CuplandLaos_2$d2_23e <- str_replace(CuplandLaos_2$d2_23e, "chineselattuce", "Chinese lettuce")
CuplandLaos_2$d2_23e <- str_replace(CuplandLaos_2$d2_23e, "cucumber", "Cucumber")
CuplandLaos_2$d2_23e <- str_replace(CuplandLaos_2$d2_23e, "ginger", "Ginger, rhizome")
CuplandLaos_2$d2_23e <- str_replace(CuplandLaos_2$d2_23e, "Other orchards crop", "Other fruit crop")
summary(as.factor(CuplandLaos_2$d2_23e))

# # #c. Corresponding fields
#Everything is ok in the table, we'll just check if number of crops
#is corresponding with the other table
x <- count(CuplandLaos_2, hhid_re3)
y <- HouseholdLaos_2C[,c(1,633)]
G <- merge(x, y, by.x = "hhid_re3", by.y = "o9", all.x = T, all.y= T, sort = TRUE)
#Some households did not answered to a0 while we have the correct number of members
#We will correct it)
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3471, 633] <- 4
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3292, 633] <-  3
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3542, 633] <-  2
HouseholdLaos_2C[HouseholdLaos_2C$o9 == 3444, 633] <-  1
# For following households, we know the number of households members but no details on them
#HHID: 3447, 3451, 3551, 3564, 3565



#HouMemberLaos_2C <- HouMemberLaos_2[,-6]
#Add the proper labels to each columns
HouseholdLaos_2C <- copy_labels(HouseholdLaos_2, HouseholdLaos)
HouMemberLaos_2C <- copy_labels(HouMemberLaos_2, HouMemberLaos)
ClowlandLaos_2 <- copy_labels(ClowlandLaos_2, ClowlandLaos)
CuplandLaos_2 <- copy_labels(CuplandLaos_2, CuplandLaos)

##3.3 Export of the database under R format
saveRDS(HouseholdLaos_2C, "HouseholdLaos_2C.rds")
saveRDS(HouMemberLaos_2C, "HouMemberLaos_2C.rds")
saveRDS(ClowlandLaos_2, "ClowlandLaos_2.rds")
saveRDS(CuplandLaos_2, "CuplandLaos_2.rds")

