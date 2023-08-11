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
#dta format (from Ky) datasets import
setwd("C:/Users/titou/OneDrive/Bureau/ASSET Stats/WORKDIRECTORY/1.ASSET_data_cleaning-Titouan/ASSET_data_cleaning")
#We import all the dta files transmitted by Ky as *Vietnam* database
HouseholdVietnam <- read_dta("asset_household_survey_vietnam.dta")
ClowlandVietnam <- read_dta("d2_1_lowland_vietnam.dta")
CuplandVietnam <- read_dta("d2_2_upland_vietnam.dta")
HouMemberVietnam <- read_dta("household_rooster_vietnam.dta")

#Importation of corrected database
ClowlandVietnam2 <- read.csv("ClowlandVietnam2.csv", sep = ';')

#We check for columns correspondences and re-adjust when it is necesary


#List creation to check if columns are similar or not
x <- rbind(colnames(ClowlandVietnam),colnames(ClowlandVietnam2))

#We re-attribute names of databases
#For Lowland crops database
colnames(ClowlandVietnam2) <- colnames(ClowlandVietnam)
ClowlandVietnam2 <- copy_labels(ClowlandVietnam2, ClowlandVietnam)
ClowlandVietnam <- ClowlandVietnam2


### 2. Columns and sub-columns name changing
##2.1. Work on the first database: "HouseholdVietnam" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

# # #o.
#C1 = FACT
#C2 = FACT
#C3 = FACT
#C4 = ? (Probably Kobo ID, maybe necessary to replace it later)
#C5 = REMOVE (Text from the questionnaire)
#C6 = REMOVE (Text from the questionnaire)
#C7 = FACT, Change the label: "Consent: 1 = yes, 2 = no"
var_label(HouseholdVietnam$consent) <- "Consent: 1 = yes, 2 = no"
#C8 = FACT 
#C9 = FACT, (Enumerators names = number, correspondence table)
#C10 = OK, (Empty, decide if necessary to remove it or not)
#C11 = FACT, (Supervisors names = number, correspondence table)
#C12 = OK, (Empty, decide if necessary to remove it or not)
#C13 = FACT, (Each country = number, Vietnam = 3)
#C14 = FACT, (Each province = number starting by the country number, only one province in Vietnam)
#C15 = FACT, (Each district = number starting by the country and province number)
#C16 = FACT, (Each commune = specific number)
#C17 = FACT, (Each village = number starting by the country, province and district number)
#C18 = FACT, check duplicates, (starting by country id) = 10 duplicates... 
#Normally it is one of the issue for which Ky should find a solution
count_if("TRUE",duplicated(HouseholdVietnam[,18]))
#C19 = FACT, (same than previous)
#C20 = FACT
#C21 = FACT, (Same than previous)
#C22 = FACT, (Same than "13")
#C23 = FACT
#C24 = FACT
#C25 = FACT, (Same than "14")
#C26 = FACT
#C27 = FACT
#C28 = FACT, (Same than "15")
#C29 = FACT
#C30 = FACT
#C31 = FACT, (Same than "16")
#C32 = FACT
#C33 = FACT
#C34 = FACT, (Same than "17")
#C35 = OK, change label "header's name"
var_label(HouseholdVietnam$header_name_preload) <- "header's name"
#C36 = FACT
#C37 = ASK KY TO GET THE REAL VALUES, NOT POSSIBLE TO GET THEM UNDER THIS FORMAT
#C38 = FACT, (Should replace "20")
#C39 = FACT, (Same than "23")
#C40 = FACT, (Same than "26")
#C41 = FACT, (Same than "29")
#C42 = FACT, (Same than "32")
#C43 = REMOVE (text from the questionnaire)
#C44 = REMOVE (text from the questionnaire)
#C45 = REMOVE (text from the questionnaire)
# # # Good order from this point, re-order previous columns
#C46 = REMOVE (text from the questionnaire)
#C47 = REMOVE (Not really o8)
#C48 = FACT, (Answer = number of HH members included in the interview ??)
#C49 = FACT, (Man or woman, 1 = man, 2 = woman)
#C50 = FACT
#C51 = FACT, (1 = yes, 0 = no ??)
#C52 = FACT, (1 =	Less than 1 year, 2 =	1-5 year, 3 =	5-10 years, 4 = More than 10 years)
#C53 = FACT, (1 =	Other commune within district, 2 = Another district within province, 3 = Another province)
#C54 = FACT (the answer is still in Vietnamese, Ky will maybe solve it)
#C55 = OK
#C56 = OK
#C57 = FACT, (We don't have the correspondence for this question in the questionaire...)
#C58 = REMOVE (Text from the questionnaire)

# # #a.
#C59 = OK
#C60 = NI
#C61 = NI
#C62 = NI
#C63 = NI
#C64 = NI
#C65 = NI
#C66 = NI
#C67 = NUMERIC
#C68 = FACT, (table of correspondence), (see answers below)
#Death or health problem of one household member 
#Harvest failure
#Animal loss
#Unsold harvest or animals
#Other
#Do not know
#C69 = FACT, replace label: "Death or health problem of one household member"
var_label(HouseholdVietnam$a151) <- "Death or health problem of one household member"
#C70 = FACT, replace label: "Harvest failure"
var_label(HouseholdVietnam$a152) <- "Harvest failure"
#C71 = FACT, replace label: "Animal loss"
var_label(HouseholdVietnam$a153) <- "Animal loss"
#C72 = FACT, replace label: "Unsold harvest or animals"
var_label(HouseholdVietnam$a154) <- "Unsold harvest or animals"
#C73 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C74 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C75 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C76 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C77 = FACT, replace label: "Other" 
var_label(HouseholdVietnam$a1599) <- "Other"
#C78 = FACT, replace label: "Do not know"
var_label(HouseholdVietnam$a1588) <- "Do not know"
#C79 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)

# # #b.
#C80 = FACT, (table of correspondence), (see answers below)
#No selling agri-products
#Selling own crops/fruits/ vegetable
#Selling own livestock products
#Selling both own crops/fruits/vegetables and livestock products
#C81 = FACT, (table of correspondence), (see answers below)
#No processed/derived agri-products
#Selling processed products from crops
#Selling processed products from livestocks
#Selling both above processed/derived agri-produc
#C82 = FACT, (table of correspondence), (see answers below)
#Dried/ fermented vegetables (carrot, mustard…)
#Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)
#Dried/ fermented bambooshoot
#Selling other products
#C83 = FACT, replace label: "Dried/ fermented vegetables (carrot, mustard…)"
var_label(HouseholdVietnam$b2_11) <- "Dried/ fermented vegetables (carrot, mustard…)"
#C84 = FACT, replace label: "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
var_label(HouseholdVietnam$b2_12) <- "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
#C85 = FACT, replace label: "Dried/ fermented bambooshoot"
var_label(HouseholdVietnam$b2_13) <- "Dried/ fermented bambooshoot"
#C86 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C87 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C88 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C89 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C90 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C91 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C92 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C93 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C94 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C95 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C96 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C97 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C98 = FACT, replace label: "Selling other products"
var_label(HouseholdVietnam$b2_199) <- "Selling other products"
#C99 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C100 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C101 = FACT, (table of correspondence), (see answers below)
#Dried/fermented meat (pork, beef, etc.)
#Honey
#Cheese
#Selling other products
#C102 = FACT, replace label: "Dried/fermented meat (pork, beef, etc.)"
var_label(HouseholdVietnam$b2_21) <- "Dried/fermented meat (pork, beef, etc.)"
#C103 = FACT, replace label: "Honey"
var_label(HouseholdVietnam$b2_22) <- "Honey"
#C104 = FACT, replace label: "Cheese"
var_label(HouseholdVietnam$b2_23) <- "Cheese"
#C105 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C106 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C107 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C108 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C109 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C110 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C111 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C112 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C113 = FACT, replace label: "Selling other products"
var_label(HouseholdVietnam$b2_299) <- "Selling other products"
#C114 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C115 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C116 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C117 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C118 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C119 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C120 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C121 = FACT, replace label: "Selling own vegetables"
#var_label(HouseholdVietnam$b31) <- "Selling own vegetables"
#C122 = FACT, replace label: "Selling own fruits"
#var_label(HouseholdVietnam$b32) <- "Selling own fruits"
#C123 = FACT, replace label: "Selling rice from the farm"
#var_label(HouseholdVietnam$b33) <- "Selling rice from the farm"
#C124 = FACT, replace label: "Selling coffee from the farm"
#var_label(HouseholdVietnam$b34) <- "Selling coffee from the farm"
#C125 = FACT, replace label: "Selling maize from the farm"
#var_label(HouseholdVietnam$b35) <- "Selling maize from the farm"
#C126 = FACT, replace label: "Selling other crops from the farm"
#var_label(HouseholdVietnam$b36) <- "Selling other crops from the farm"
#C127 = FACT, replace label: "Selling cattle and buffalo"
#var_label(HouseholdVietnam$b37) <- "Selling cattle and buffalo"
#C128 = FACT, replace label: "Selling pigs"
#var_label(HouseholdVietnam$b38) <- "Selling pigs"
#C129 = FACT, replace label: "Selling poultry"
#var_label(HouseholdVietnam$b39) <- "Selling poultry"
#C130 = FACT, replace label: "b3/10 Selling other farm products"
#var_label(HouseholdVietnam$b310) <- "Selling other farm products"
#C131 = FACT, replace label: "Sell derived/processed products"
#var_label(HouseholdVietnam$b311) <- "Sell derived/processed products"
#C132 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
#var_label(HouseholdVietnam$b312) <- "Non-farm wages (salaried work in private or public company)"
#C133 = FACT, replace label: "Other"
#var_label(HouseholdVietnam$b399) <- "Other"
#C134 = FACT, replace label: "Do not know"
#var_label(HouseholdVietnam$b388) <- "Do not know"
#C135 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C136 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdVietnam$b313) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C137 = FACT, replace label: "Selling labor"
var_label(HouseholdVietnam$b314) <- "Selling labor"
#C138 = FACT, replace label: "Remittances"
var_label(HouseholdVietnam$b315) <- "Remittances"
#C139 = FACT, replace label: "Pension"
var_label(HouseholdVietnam$b316) <- "Pension"
#C140 = FACT, replace label: "Rented land"
var_label(HouseholdVietnam$b317) <- "Rented land"
#C141 = FACT, replace label: "Financial support / gift"
var_label(HouseholdVietnam$b318) <- "Financial support / gift"
#C142 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C143 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C144 = FACT, (Table of correspondence)
#C145 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C146 = FACT, (Table of correspondence)
#C147 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C148 = FACT, (Table of correspondence)
#C149 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C150 = OK, change label name for "What is the proportion of total household income from b4_1"
var_label(HouseholdVietnam$b5_1) <- "What is the proportion of total household income from b4_1"
#C151 = NUMERIC, Useless column
#C152 = OK, change label name for "What is the proportion of total household income from b4_2"
var_label(HouseholdVietnam$b5_2) <- "What is the proportion of total household income from b4_2"
#C153 = NUMERIC, Useless column
#C154 = OK, change label name for "What is the proportion of total household income from b4_3"
var_label(HouseholdVietnam$b5_3) <- "What is the proportion of total household income from b4_3"
#C155 = FACT, (bin), (0 = no, 1 = yes)
#C156 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C157 = FACT, replace label: "Selling own vegetables"
var_label(HouseholdVietnam$b71) <- "Selling own vegetables"
#C158 = FACT, replace label: "Selling own fruits"
var_label(HouseholdVietnam$b72) <- "Selling own fruits"
#C159 = FACT, replace label: "Selling rice from the farm"
var_label(HouseholdVietnam$b73) <- "Selling rice from the farm"
#C160 = FACT, replace label: "Selling coffee from the farm"
var_label(HouseholdVietnam$b74) <- "Selling coffee from the farm"
#C161 = FACT, replace label: "Selling maize from the farm"
var_label(HouseholdVietnam$b75) <- "Selling maize from the farm"
#C162 = FACT, replace label: "Selling other crops from the farm"
var_label(HouseholdVietnam$b76) <- "Selling other crops from the farm"
#C163 = FACT, replace label: "Selling cattle and buffalo"
var_label(HouseholdVietnam$b77) <- "Selling cattle and buffalo"
#C164 = FACT, replace label: "Selling pigs"
var_label(HouseholdVietnam$b78) <- "Selling pigs"
#C165 = FACT, replace label: "Selling poultry"
var_label(HouseholdVietnam$b79) <- "Selling poultry"
#C166 = FACT, replace label: "Selling other farm products"
var_label(HouseholdVietnam$b710) <- "Selling other farm products"
#C167 = FACT, replace label: "Sell derived/processed products"
var_label(HouseholdVietnam$b711) <- "Sell derived/processed products"
#C168 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
var_label(HouseholdVietnam$b712) <- "Non-farm wages (salaried work in private or public company)"
#C169 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdVietnam$b713) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C170 = FACT, replace label: "Selling labor"
var_label(HouseholdVietnam$b714) <- "Selling labor"
#C171 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C172 = FACT, replace label: "Remittances"
var_label(HouseholdVietnam$b715) <- "Remittances"
#C173 = FACT, replace label: "Pension"
var_label(HouseholdVietnam$b716) <- "Pension"
#C174 = FACT, replace label: "Rented land"
var_label(HouseholdVietnam$b717) <- "Rented land"
#C175 = FACT, replace label: "Financial support / gift"
var_label(HouseholdVietnam$b718) <- "Financial support / gift"
#C176 = FACT, replace label: "Other"
var_label(HouseholdVietnam$b799) <- "Other"
#C177 = FACT, replace label: "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(b799 , .after = b718)
var_label(HouseholdVietnam$b788) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(b788 , .after = b799)
#C178 = CHAR, (empty)
#C179 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C180 = FACT, (1 = yes, 0 = no)
#C181 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C182 = FACT, replace label: "Jan"
var_label(HouseholdVietnam$b8_11) <- "Jan"
#C183 = FACT, replace label: "Feb"
var_label(HouseholdVietnam$b8_12) <- "Feb"
#C184 = FACT, replace label: "Mar"
var_label(HouseholdVietnam$b8_13) <- "Mar"
#C185 = FACT, replace label: "Apr"
var_label(HouseholdVietnam$b8_14) <- "Apr"
#C186 = FACT, replace label: "May"
var_label(HouseholdVietnam$b8_15) <- "May"
#C187 = FACT, replace label: "Jun"
var_label(HouseholdVietnam$b8_16) <- "Jun"
#C188 = FACT, replace label: "Jul"
var_label(HouseholdVietnam$b8_17) <- "Jul"
#C189 = FACT, replace label: "Aug"
var_label(HouseholdVietnam$b8_18) <- "Aug"
#C190 = FACT, replace label: "Sep"
var_label(HouseholdVietnam$b8_19) <- "Sep"
#C191 = FACT, replace label: "Oct"
var_label(HouseholdVietnam$b8_110) <- "Oct"
#C192 = FACT, replace label: "Nov"
var_label(HouseholdVietnam$b8_111) <- "Nov"
#C193 = FACT, replace label: "Dec"
var_label(HouseholdVietnam$b8_112) <- "Dec"
#C194 = FACT, (bin)
#C195 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C196 = FACT, replace label: "Jan"
var_label(HouseholdVietnam$b9_11) <- "Jan"
#C197 = FACT, replace label: "Feb"
var_label(HouseholdVietnam$b9_12) <- "Feb"
#C198 = FACT, replace label: "Mar"
var_label(HouseholdVietnam$b9_13) <- "Mar"
#C199 = FACT, replace label: "Apr"
var_label(HouseholdVietnam$b9_14) <- "Apr"
#C200 = FACT, replace label: "May"
var_label(HouseholdVietnam$b9_15) <- "May"
#C201 = FACT, replace label: "Jun"
var_label(HouseholdVietnam$b9_16) <- "Jun"
#C202 = FACT, replace label: "Jul"
var_label(HouseholdVietnam$b9_17) <- "Jul"
#C203 = FACT, replace label: "Aug"
var_label(HouseholdVietnam$b9_18) <- "Aug"
#C204 = FACT, replace label: "Sep"
var_label(HouseholdVietnam$b9_19) <- "Sep"
#C205 = FACT, replace label: "Oct"
var_label(HouseholdVietnam$b9_110) <- "Oct"
#C206 = FACT, replace label: "Nov"
var_label(HouseholdVietnam$b9_111) <- "Nov"
#C207 = FACT, replace label: "Dec"
var_label(HouseholdVietnam$b9_112) <- "Dec"
#C208 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C209 = FACT, replace label: "Non-working members went out to look for work"
var_label(HouseholdVietnam$b9_21) <- "Non-working members went out to look for work"
#C210 = FACT, replace label: "b9_2/2 Working members increased work hours"
var_label(HouseholdVietnam$b9_22) <- "Working members increased work hours"
#C211 = FACT, replace label: "One or more members changes residence"
var_label(HouseholdVietnam$b9_23) <- "One or more members changes residence"
#C212 = FACT, replace label: "Spent savings"
var_label(HouseholdVietnam$b9_24) <- "Spent savings"
#C213 = FACT, replace label: "Went into debt"
var_label(HouseholdVietnam$b9_25) <- "Went into debt"
#C214 = FACT, replace label: "Sold property or assets"
var_label(HouseholdVietnam$b9_26) <- "Sold property or assets"
#C215 = FACT, replace label: "Withdrew children from school"
var_label(HouseholdVietnam$b9_27) <- "Withdrew children from school"
#C216 = FACT, replace label: "Decreased food expenses"
var_label(HouseholdVietnam$b9_28) <- "Decreased food expenses"
#C217 = FACT, replace label: "Changed agricultural practices"
var_label(HouseholdVietnam$b9_29) <- "Changed agricultural practices"
#C218 = FACT, replace label: "Did nothing"
var_label(HouseholdVietnam$b9_20) <- "Did nothing"
#C219 = FACT, replace label: "Other"
var_label(HouseholdVietnam$b9_299) <- "Other"
#C220 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C221 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C222 = FACT, replace label: "Community-based tourism or agroecological tourism"
var_label(HouseholdVietnam$b9_31) <- "Community-based tourism or agroecological tourism"
#C223 = FACT, replace label: "Hosting events (e.g. for projects from NGOs, research)"
var_label(HouseholdVietnam$b9_32) <- "Hosting events (e.g. for projects from NGOs, research)"
#C224 = FACT, replace label: "Education and training of others (e.g. training of other farmers, school visits)"
var_label(HouseholdVietnam$b9_33) <- "Education and training of others (e.g. training of other farmers, school visits)"
#C225 = FACT, replace label: "Food processing"
var_label(HouseholdVietnam$b9_34) <- "Food processing"
#C226 = FACT, replace label: "Restaurant"
var_label(HouseholdVietnam$b9_35) <- "Restaurant"
#C227 = FACT, replace label: "Selling products from other farms"
var_label(HouseholdVietnam$b9_36) <- "Selling products from other farms"
#C228 = FACT, replace label: "None of the above"
var_label(HouseholdVietnam$b9_30) <- "None of the above"
#C229 = FACT, replace label: "I do not know"
var_label(HouseholdVietnam$b9_388) <- "I do not know"
#C230 = FACT, (Table of correspondence), (see answers below)
#I do not sell (crops/vegetables/fruits or livestock) = NA
#The local market ( lower or equal to district level market)
#The provincial or national market
#Export
#Other
#Do not know
#C231 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C232 = FACT, (Table of correspondence), (see answers below)
#China
#South East Asia
#East Asia
#Europe
#US
#Other left countries
#C233 = FACT, replace label: "China"
var_label(HouseholdVietnam$b10_11) <- "China"
#C234 = FACT, replace label: "South East Asia"
var_label(HouseholdVietnam$b10_12) <- "South East Asia"
#C235 = FACT, replace label: "East Asia"
var_label(HouseholdVietnam$b10_13) <- "East Asia"
#C236 = FACT, replace label: "Europe"
var_label(HouseholdVietnam$b10_14) <- "Europe"
#C237 = FACT, replace label: "US"
var_label(HouseholdVietnam$b10_15) <- "US"
#C238 = FACT, replace label: "Other left countries"
var_label(HouseholdVietnam$b10_16) <- "Other left countries"
#C239 = FACT, (Table of correspondence), (see answers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C240 = FACT, (Table of correspondence)
#C241 = FACT, (Table of correspondence)
#C242 = FACT, (Table of correspondence)
#C243 = FACT, (0 = no, 1 = yes)
#C244 = FACT, (Table of correspondence), (see answers below)
#Vegetables
#Plum
#Logan
#Mango
#Avocado
#Dragon fruits
#Banana
#Strawberry
#Honey
#Tea
#Rice
#Noodle
#Dried meat (pork, beef, etc.)
#Coffee
#Other
#C245 = FACT, replace label: "Vegetables"
var_label(HouseholdVietnam$b22_11) <- "Vegetables"
#C246 = FACT, replace label: "Plum"
var_label(HouseholdVietnam$b22_12) <- "Plum"
#C247 = FACT, replace label: "Logan"
var_label(HouseholdVietnam$b22_13) <- "Logan"
#C248 = FACT, replace label: "Mango"
var_label(HouseholdVietnam$b22_14) <- "Mango"
#C249 = FACT, replace label: "Avocado"
var_label(HouseholdVietnam$b22_15) <- "Avocado"
#C250 = FACT, replace label: "Dragon fruits"
var_label(HouseholdVietnam$b22_16) <- "Dragon fruits"
#C251 = FACT, replace label: "Banana"
var_label(HouseholdVietnam$b22_17) <- "Banana"
#C252 = FACT, replace label: "Strawberry"
var_label(HouseholdVietnam$b22_18) <- "Strawberry"
#C253 = FACT, replace label: "Honey"
var_label(HouseholdVietnam$b22_19) <- "Honey"
#C254 = FACT, replace label: "Tea"
var_label(HouseholdVietnam$b22_110) <- "Tea"
#C255 = FACT, replace label: "Rice"
var_label(HouseholdVietnam$b22_111) <- "Rice"
#C256 = FACT, replace label: "Noodle"
var_label(HouseholdVietnam$b22_112) <- "Noodle"
#C257 = FACT, replace label: "Dried meat (pork, beef, etc.)"
var_label(HouseholdVietnam$b22_113) <- "Dried meat (pork, beef, etc.)"
#C258 = FACT, replace label: "Coffee"
var_label(HouseholdVietnam$b22_114) <- "Coffee"
#C259 = FACT, replace label: "Other"
var_label(HouseholdVietnam$b22_199) <- "Other"
#C260 = OK, (empty)
#C261 = FACT, (Table of correspondence), (see answers below)
#VietGAP
#Vietnamese organic standard  
#International organic standard
#UTZ certified/ Rainforest alliance standard
#Collective or certification trademark 
#Geographical indication
#Other
#Do not know
#C262 = OK, (empty)
#C263 = FACT, (only NA)
#C264 = OK, (empty)
#C265 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C266 = FACT, replace label: "Village collector"
var_label(HouseholdVietnam$b24_1a1) <- "Village collector"
#C267 = FACT, replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_1a2) <- "Collector outside the village"
#C268 = FACT, replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_1a3) <- "Trader in the district"
#C269 = FACT, replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_1a4) <- "Trader from the province"
#C270 = FACT, replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_1a5) <- "Trader from another province"
#C271 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_1a6) <- "Cooperative of which you are a member"
#C272 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_1a7) <- "Cooperative of which you are not a member"
#C273 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_1a8) <- "Consumers on the farm"
#C274 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_1a9) <- "Local markets where you sell your products directly to final consumers"
#C275 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_1a10) <- "Consumers through online sales"
#C276 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_1a11) <- "Foreign trader (e.g. from China)"
#C277 = FACT, replace label: "Processors"
var_label(HouseholdVietnam$b24_1a12) <- "Processors"
#C278 = FACT, replace label: "Other"
var_label(HouseholdVietnam$b24_1a99) <- "Other"
#C279 = FACT, replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_1a88) <- "Do not know/ no the second outlet"
#C280 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C281 = FACT, (empty)
#C282 = OK, (empty)
#C283 = FACT, (empty)
#C284 = OK, (empty)
#C285 = CHAR, (empty)
#C286 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_2a1) <- "Village collector"
#C287 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_2a2) <- "Collector outside the village"
#C288 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_2a3) <- "Trader in the district"
#C289 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_2a4) <- "Trader from the province"
#C290 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_2a5) <- "Trader from another province"
#C291 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_2a6) <- "Cooperative of which you are a member"
#C292 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_2a7) <- "Cooperative of which you are not a member"
#C293 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_2a8) <- "Consumers on the farm"
#C294 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_2a9) <- "Local markets where you sell your products directly to final consumers"
#C295 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_2a10) <- "Consumers through online sales"
#C296 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_2a11) <- "Foreign trader (e.g. from China)"
#C297 = FACT, replace label: "Processors"
var_label(HouseholdVietnam$b24_2a12) <- "Processors"
#C298 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_2a99) <- "Other"
#C299 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_2a88) <- "Do not know/ no the second outlet"
#C300 = OK, (empty)
#C301 = FACT, (empty)
#C302 = OK, (empty)
#C303 = FACT, (empty)
#C304 = OK, (empty)
#C305 = CHAR, (empty)
#C306 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_3a1) <- "Village collector"
#C307 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_3a2) <- "Collector outside the village"
#C308 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_3a3) <- "Trader in the district"
#C309 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_3a4) <- "Trader from the province"
#C310 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_3a5) <- "Trader from another province"
#C311 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_3a6) <- "Cooperative of which you are a member"
#C312 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_3a7) <- "Cooperative of which you are not a member"
#C313 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_3a8) <- "Consumers on the farm"
#C314 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_3a9) <- "Local markets where you sell your products directly to final consumers"
#C315 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_3a10) <- "Consumers through online sales"
#C316 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_3a11) <- "Foreign trader (e.g. from China)"
#C317 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_3a12) <- "Processors"
#C318 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_3a99) <- "Other"
#C319 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_3a88) <- "Do not know/ no the second outlet"
#C320 = OK, (empty)
#C321 = FACT, (empty)
#C322 = OK, (empty)
#C323 = FACT, (empty)
#C324 = OK, (empty)
#C325 = CHAR, (empty)
#C326 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_4a1) <- "Village collector"
#C327 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_4a2) <- "Collector outside the village"
#C328 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_4a3) <- "Trader in the district"
#C329 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_4a4) <- "Trader from the province"
#C330 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_4a5) <- "Trader from another province"
#C331 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_4a6) <- "Cooperative of which you are a member"
#C332 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_4a7) <- "Cooperative of which you are not a member"
#C333 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_4a8) <- "Consumers on the farm"
#C334 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_4a9) <- "Local markets where you sell your products directly to final consumers"
#C335 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_4a10) <- "Consumers through online sales"
#C336 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_4a11) <- "Foreign trader (e.g. from China)"
#C337 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_4a12) <- "Processors"
#C338 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_4a99) <- "Other"
#C339 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_4a88) <- "Do not know/ no the second outlet"
#C340 = OK, (empty)
#C341 = FACT, (empty)
#C342 = OK, (empty)
#C343 = FACT, (empty)
#C344 = OK, (empty)
#C345 = CHAR, (empty),
#C346 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_5a1) <- "Village collector"
#C347 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_5a2) <- "Collector outside the village"
#C348 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_5a3) <- "Trader in the district"
#C349 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_5a4) <- "Trader from the province"
#C350 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_5a5) <- "Trader from another province"
#C351 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_5a6) <- "Cooperative of which you are a member"
#C352 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_5a7) <- "Cooperative of which you are not a member"
#C353 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_5a8) <- "Consumers on the farm"
#C354 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_5a9) <- "Local markets where you sell your products directly to final consumers"
#C355 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_5a10) <- "Consumers through online sales"
#C356 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_5a11) <- "Foreign trader (e.g. from China)"
#C357 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_5a12) <- "Processors"
#C358 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_5a99) <- "Other"
#C359 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_5a88) <- "Do not know/ no the second outlet"
#C360 = OK, (empty)
#C361 = FACT, (empty)
#C362 = OK, (empty)
#C363 = FACT, (empty)
#C364 = OK, (empty)
#C365 = CHAR, (empty),
#C366 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_6a1) <- "Village collector"
#C367 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_6a2) <- "Collector outside the village"
#C368 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_6a3) <- "Trader in the district"
#C369 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_6a4) <- "Trader from the province"
#C370 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_6a5) <- "Trader from another province"
#C371 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_6a6) <- "Cooperative of which you are a member"
#C372 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_6a7) <- "Cooperative of which you are not a member"
#C373 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_6a8) <- "Consumers on the farm"
#C374 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_6a9) <- "Local markets where you sell your products directly to final consumers"
#C375 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_6a10) <- "Consumers through online sales"
#C376 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_6a11) <- "Foreign trader (e.g. from China)"
#C377 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_6a12) <- "Processors"
#C378 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_6a99) <- "Other"
#C379 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_6a88) <- "Do not know/ no the second outlet"
#C380 = OK, (empty)
#C381 = FACT, (empty)
#C382 = OK, (empty)
#C383 = FACT, (empty)
#C384 = OK, (empty)
#C385 = CHAR, (empty)
#C386 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_7a1) <- "Village collector"
#C387 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_7a2) <- "Collector outside the village"
#C388 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_7a3) <- "Trader in the district"
#C389 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_7a4) <- "Trader from the province"
#C390 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_7a5) <- "Trader from another province"
#C391 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_7a6) <- "Cooperative of which you are a member"
#C392 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_7a7) <- "Cooperative of which you are not a member"
#C393 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_7a8) <- "Consumers on the farm"
#C394 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_7a9) <- "Local markets where you sell your products directly to final consumers"
#C395 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_7a10) <- "Consumers through online sales"
#C396 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_7a11) <- "Foreign trader (e.g. from China)"
#C397 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_7a12) <- "Processors"
#C398 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_7a99) <- "Other"
#C399 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_7a88) <- "Do not know/ no the second outlet"
#C400 = OK, (empty)
#C401 = FACT, (empty)
#C402 = OK, (empty)
#C403 = FACT, (empty)
#C404 = OK, (empty)
#C405 = CHAR, (empty),
#C406 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_8a1) <- "Village collector"
#C407 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_8a2) <- "Collector outside the village"
#C408 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_8a3) <- "Trader in the district"
#C409 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_8a4) <- "Trader from the province"
#C410 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_8a5) <- "Trader from another province"
#C411 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_8a6) <- "Cooperative of which you are a member"
#C412 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_8a7) <- "Cooperative of which you are not a member"
#C413 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_8a8) <- "Consumers on the farm"
#C414 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_8a9) <- "Local markets where you sell your products directly to final consumers"
#C415 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_8a10) <- "Consumers through online sales"
#C416 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_8a11) <- "Foreign trader (e.g. from China)"
#C417 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_8a12) <- "Processors"
#C418 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_8a99) <- "Other"
#C419 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_8a88) <- "Do not know/ no the second outlet"
#C420 = OK, (empty)
#C421 = FACT, (empty)
#C422 = OK, (empty)
#C423 = FACT, (empty)
#C424 = OK, (empty)
#C425 = CHAR, (empty)
#C426 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_9a1) <- "Village collector"
#C427 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_9a2) <- "Collector outside the village"
#C428 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_9a3) <- "Trader in the district"
#C429 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_9a4) <- "Trader from the province"
#C430 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_9a5) <- "Trader from another province"
#C431 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_9a6) <- "Cooperative of which you are a member"
#C432 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_9a7) <- "Cooperative of which you are not a member"
#C433 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_9a8) <- "Consumers on the farm"
#C434 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_9a9) <- "Local markets where you sell your products directly to final consumers"
#C435 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_9a10) <- "Consumers through online sales"
#C436 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_9a11) <- "Foreign trader (e.g. from China)"
#C437 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_9a12) <- "Processors"
#C438 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_9a99) <- "Other"
#C439 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_9a88) <- "Do not know/ no the second outlet"
#C440 = OK, (empty)
#C441 = FACT, (empty)
#C442 = OK, (empty)
#C443 = FACT, (empty)
#C444 = OK, (empty)
#C445 = CHAR, (empty),
#C446 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_10a1) <- "Village collector"
#C447 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_10a2) <- "Collector outside the village"
#C448 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_10a3) <- "Trader in the district"
#C449 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_10a4) <- "Trader from the province"
#C450 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_10a5) <- "Trader from another province"
#C451 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_10a6) <- "Cooperative of which you are a member"
#C452 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_10a7) <- "Cooperative of which you are not a member"
#C453 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_10a8) <- "Consumers on the farm"
#C454 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_10a9) <- "Local markets where you sell your products directly to final consumers"
#C455 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_10a10) <- "Consumers through online sales"
#C456 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_10a11) <- "Foreign trader (e.g. from China)"
#C457 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_10a12) <- "Processors"
#C458 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_10a99) <- "Other"
#C459 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_10a88) <- "Do not know/ no the second outlet"
#C460 = OK, (empty)
#C461 = FACT, (empty)
#C462 = OK, (empty)
#C463 = FACT, (empty)
#C464 = OK, (empty)
#C465 = CHAR, (empty),
#C466 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_11a1) <- "Village collector"
#C467 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_11a2) <- "Collector outside the village"
#C468 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_11a3) <- "Trader in the district"
#C469 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_11a4) <- "Trader from the province"
#C470 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_11a5) <- "Trader from another province"
#C471 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_11a6) <- "Cooperative of which you are a member"
#C472 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_11a7) <- "Cooperative of which you are not a member"
#C473 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_11a8) <- "Consumers on the farm"
#C474 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_11a9) <- "Local markets where you sell your products directly to final consumers"
#C475 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_11a10) <- "Consumers through online sales"
#C476 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_11a11) <- "Foreign trader (e.g. from China)"
#C477 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_11a12) <- "Processors"
#C478 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_11a99) <- "Other"
#C479 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_11a88) <- "Do not know/ no the second outlet"
#C480 = OK, (empty)
#C481 = FACT, (empty)
#C482 = OK, (empty)
#C483 = FACT, (empty)
#C484 = OK, (empty)
#C485 = CHAR, (empty),
#C486 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_12a1) <- "Village collector"
#C487 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_12a2) <- "Collector outside the village"
#C488 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_12a3) <- "Trader in the district"
#C489 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_12a4) <- "Trader from the province"
#C490 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_12a5) <- "Trader from another province"
#C491 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_12a6) <- "Cooperative of which you are a member"
#C492 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_12a7) <- "Cooperative of which you are not a member"
#C493 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_12a8) <- "Consumers on the farm"
#C494 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_12a9) <- "Local markets where you sell your products directly to final consumers"
#C495 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_12a10) <- "Consumers through online sales"
#C496 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_12a11) <- "Foreign trader (e.g. from China)"
#C497 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_12a12) <- "Processors"
#C498 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_12a99) <- "Other"
#C499 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_12a88) <- "Do not know/ no the second outlet"
#C500 = OK, (empty)
#C501 = FACT, (empty)
#C502 = OK, (empty)
#C503 = FACT, (empty)
#C504 = OK, (empty)
#C505 = CHAR, (empty),
#C506 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_13a1) <- "Village collector"
#C507 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_13a2) <- "Collector outside the village"
#C508 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_13a3) <- "Trader in the district"
#C509 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_13a4) <- "Trader from the province"
#C510 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_13a5) <- "Trader from another province"
#C511 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_13a6) <- "Cooperative of which you are a member"
#C512 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_13a7) <- "Cooperative of which you are not a member"
#C513 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_13a8) <- "Consumers on the farm"
#C514 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_13a9) <- "Local markets where you sell your products directly to final consumers"
#C515 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_13a10) <- "Consumers through online sales"
#C516 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_13a11) <- "Foreign trader (e.g. from China)"
#C517 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_13a12) <- "Processors"
#C518 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_13a99) <- "Other"
#C519 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_13a88) <- "Do not know/ no the second outlet"
#C520 = OK, (empty)
#C521 = FACT, (empty)
#C522 = OK, (empty)
#C523 = FACT, (empty)
#C524 = OK, (empty)
#C525 = CHAR, (empty)
#C526 = FACT, replace label: "Village collector"
var_label(HouseholdVietnam$b24_14a1) <- "Village collector"
#C527 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_14a2) <- "Collector outside the village"
#C528 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_14a3) <- "Trader in the district"
#C529 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_14a4) <- "Trader from the province"
#C530 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_14a5) <- "Trader from another province"
#C531 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_14a6) <- "Cooperative of which you are a member"
#C532 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_14a7) <- "Cooperative of which you are not a member"
#C533 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_14a8) <- "Consumers on the farm"
#C534 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_14a9) <- "Local markets where you sell your products directly to final consumers"
#C535 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_14a10) <- "Consumers through online sales"
#C536 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_14a11) <- "Foreign trader (e.g. from China)"
#C537 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_14a12) <- "Processors"
#C538 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_14a99) <- "Other"
#C539 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_14a88) <- "Do not know/ no the second outlet"
#C540 = OK, (empty)
#C541 = FACT, (empty)
#C542 = OK, (empty)
#C543 = FACT, (empty)
#C544 = OK, (empty)
#C545 = CHAR, (empty), replace label: "b24_99a. for certified b22_1oth, which were the buyers/outlets in 2022"
var_label(HouseholdVietnam$b24_99a) <- "b24_99a. for certified b22_1oth, which were the buyers/outlets in 2022"
#C546 = FACT, (empty), replace label: "Village collector"
var_label(HouseholdVietnam$b24_99a1) <- "Village collector"
#C547 = FACT, (empty), replace label: "Collector outside the village"
var_label(HouseholdVietnam$b24_99a2) <- "Collector outside the village"
#C548 = FACT, (empty), replace label: "Trader in the district"
var_label(HouseholdVietnam$b24_99a3) <- "Trader in the district"
#C549 = FACT, (empty), replace label: "Trader from the province"
var_label(HouseholdVietnam$b24_99a4) <- "Trader from the province"
#C550 = FACT, (empty), replace label: "Trader from another province"
var_label(HouseholdVietnam$b24_99a5) <- "Trader from another province"
#C551 = FACT, (empty), replace label: "Cooperative of which you are a member"
var_label(HouseholdVietnam$b24_99a6) <- "Cooperative of which you are a member"
#C552 = FACT, (empty), replace label: "Cooperative of which you are not a member"
var_label(HouseholdVietnam$b24_99a7) <- "Cooperative of which you are not a member"
#C553 = FACT, (empty), replace label: "Consumers on the farm"
var_label(HouseholdVietnam$b24_99a8) <- "Consumers on the farm"
#C554 = FACT, (empty), replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdVietnam$b24_99a9) <- "Local markets where you sell your products directly to final consumers"
#C555 = FACT, (empty), replace label: "Consumers through online sales"
var_label(HouseholdVietnam$b24_99a10) <- "Consumers through online sales"
#C556 = FACT, (empty), replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdVietnam$b24_99a11) <- "Foreign trader (e.g. from China)"
#C557 = FACT, (empty), replace label: "Processors"
var_label(HouseholdVietnam$b24_99a12) <- "Processors"
#C558 = FACT, (empty), replace label: "Other"
var_label(HouseholdVietnam$b24_99a99) <- "Other"
#C559 = FACT, (empty), replace label: "Do not know/ no the second outlet"
var_label(HouseholdVietnam$b24_99a88) <- "Do not know/ no the second outlet"
#C560 = OK, (empty)
#C561 = OK, (empty)
#C562 = FACT, (bin)
#C563 = FACT, (bin)
#C564 = FACT, (bin)
#C565 = FACT, (Table of correspondence), (See answers below)
#Logo
#Card visit 
#Leaflet
#Signboard in the field 
#Signboard at the cooperative/Company
#Social media 
#Internet or other media (TV, Radio, etc.) 
#Do not know
#C566 = FACT, replace label: "Logo"
var_label(HouseholdVietnam$b27_11) <- "Logo"
#C567 = FACT, replace label: "Card visit"
var_label(HouseholdVietnam$b27_12) <- "Card visit"
#C568 = FACT, replace label: "Leaflet"
var_label(HouseholdVietnam$b27_13) <- "Leaflet"
#C569 = FACT, replace label: "Signboard in the field"
var_label(HouseholdVietnam$b27_14) <- "Signboard in the field"
#C570 = FACT, replace label: "Signboard at the cooperative/Company"
var_label(HouseholdVietnam$b27_15) <- "Signboard at the cooperative/Company"
#C571 = FACT, replace label: "Social media"
var_label(HouseholdVietnam$b27_16) <- "Social media"
#C572 = FACT, replace label: "Internet or other media (TV, Radio, etc.)"
var_label(HouseholdVietnam$b27_17) <- "Internet or other media (TV, Radio, etc.)"
#C573 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C574 = FACT, replace label: "Do not know"
var_label(HouseholdVietnam$b27_188) <- "Do not know"
#C575 = OK, (empty)
#C576 = FACT, (bin)
#C577 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C578 = FACT, (bin)
#C579 = FACT, (bin)

# # #c.
#C580 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C581 = FACT, replace label: "Women Union"
#var_label(HouseholdVietnam$c1_1) <- "Women Union"
#C582 = FACT, replace label: "Youth union"
#var_label(HouseholdVietnam$c1_2) <- "Youth union"
#C583 = FACT, replace label: "Veteran union"
#var_label(HouseholdVietnam$c1_3) <- "Veteran union"
#C584 = FACT, replace label: "Farmer union"
#var_label(HouseholdVietnam$c1_4) <- "Farmer union"
#C585 = FACT, replace label: "Elderly Union"
#var_label(HouseholdVietnam$c1_5) <- "Elderly Union"
#C586 = FACT, replace label: "Political party"
#var_label(HouseholdVietnam$c1_6) <- "Political party"
#C587 = FACT, replace label: "Other"
#var_label(HouseholdVietnam$c1_99) <- "Other"
#C588 = FACT, replace label: "None"
#var_label(HouseholdVietnam$c1_0) <- "None"
#C589 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C590 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C591 = FACT, (Table of correspondence), (see answers below)
#No
#Yes, one
#Yes, more than one
#C592 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C593 = FACT, replace label: "Farmer organization on crops"
var_label(HouseholdVietnam$c31) <- "Farmer organization on crops"
#C594 = FACT, replace label: "Farmer organization on fruits"
var_label(HouseholdVietnam$c32) <- "Farmer organization on fruits"
#C595 = FACT, replace label: "Farmer organization on livestock"
var_label(HouseholdVietnam$c33) <- "Farmer organization on livestock"
#C596 = FACT, replace label: "Farmer organization on honey"
var_label(HouseholdVietnam$c34) <- "Farmer organization on honey"
#C597 = FACT, replace label: "Farmer organization on water"
var_label(HouseholdVietnam$c35) <- "Farmer organization on water"
#C598 = FACT, replace label: "Farmer organization on forest"
var_label(HouseholdVietnam$c36) <- "Farmer organization on forest"
#C599 = FACT, replace label: "Other farmer organization"
var_label(HouseholdVietnam$c399) <- "Other farmer organization"
#C600 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C601 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C602 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C603 = FACT, (Table of correspondence), (see answers below)
#Farmer organization on crops
#Farmer organization on fruits
#Farmer organization on livestock
#Farmer organization on honey
#Farmer organization on water
#Farmer organization on forest
#Other
#C604 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "Specify"
var_label(HouseholdVietnam$c3a_text) <- "Specify"
#C605 = FACT, (Table of correspondence), (see answers below)
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
var_label(HouseholdVietnam$c4) <- "c4. what is the legal type of this c3a"
#C606 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C607 = FACT, (Table of correspondence), (see answers below)
#President 
#Treasurer 
#Internal control person
#Trainer 
#Collector 
#Ordinary member
#Other
#Do not know
#replace label: "c5. what is your role / responsibility in the c3a"
var_label(HouseholdVietnam$c5) <- "c5. what is your role / responsibility in the c3a"
#C608 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "c5_oth. specify your other role in the c3a"
var_label(HouseholdVietnam$c5_oth) <- "c5_oth. specify your other role in the c3a"
#C609 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "c9. what are the benefits of belonging to this c3a"
var_label(HouseholdVietnam$c9) <- "c9. what are the benefits of belonging to this c3a"
#C610 = FACT, replace label: "To borrow money "
var_label(HouseholdVietnam$c91) <- "To borrow money"
#C611 = FACT, replace label: "To get advice"
var_label(HouseholdVietnam$c92) <- "To get advice"
#C612 = FACT, replace label: "To save money"
var_label(HouseholdVietnam$c93) <- "To save money"
#C613 = FACT, replace label: "To buy inputs"
var_label(HouseholdVietnam$c94) <- "To buy inputs"
#C614 = FACT, replace label: "To use water"
var_label(HouseholdVietnam$c95) <- "To use water"
#C615 = FACT, replace label: "To get training"
var_label(HouseholdVietnam$c96) <- "To get training"
#C616 = FACT, replace label: "Access to markets"
var_label(HouseholdVietnam$c97) <- "Access to markets"
#C617 = FACT, replace label: "Secure demand from existing markets"
var_label(HouseholdVietnam$c98) <- "Secure demand from existing markets"
#C618 = FACT, replace label: "Sell certified products at a better price "
var_label(HouseholdVietnam$c99) <- "Sell certified products at a better price "
#C619 = FACT, replace label: "Other"
var_label(HouseholdVietnam$c999) <- "Other"
#C620 = FACT, replace label: "Do not know"
var_label(HouseholdVietnam$c988) <- "Do not know"
#C621 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "Specify other"
var_label(HouseholdVietnam$c9_oth) <- "Specify other"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(c9_oth , .after = c999)
#C622 = FACT, (bin), replace label: "Did c3a give you loan/credit in the past 3 years"
var_label(HouseholdVietnam$c6) <- "Did c3a give you loan/credit in the past 3 years"
#C623 = FACT, (bin), replace label: "Did c3a give you technical advice or training in the past 3 years?"
var_label(HouseholdVietnam$c7) <- "Did c3a give you technical advice or training in the past 3 years?"
#C624 = FACT, (bin), replace label: "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
var_label(HouseholdVietnam$c8) <- "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
#C625 = FACT, (bin), replace label: "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
var_label(HouseholdVietnam$c10) <- "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
#C626 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C627 = FACT, replace label: "GAP"
var_label(HouseholdVietnam$c111) <- "GAP"
#C628 = FACT, replace label: "Pest-management"
var_label(HouseholdVietnam$c112) <- "Pest-management"
#C629 = FACT, replace label: "Record-keeping"
var_label(HouseholdVietnam$c113) <- "Record-keeping"
#C630 = FACT, replace label: "Composting"
var_label(HouseholdVietnam$c114) <- "Composting"
#C631 = FACT, replace label: "Markets & prices"
var_label(HouseholdVietnam$c115) <- "Markets & prices"
#C632 = FACT, replace label: "Certification"
var_label(HouseholdVietnam$c116) <- "Certification"
#C633 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C634 = FACT, replace label: "Safe use of chemicals"
var_label(HouseholdVietnam$c117) <- "Safe use of chemicals"
#C635 = FACT, replace label: "Use of fertilizers"
var_label(HouseholdVietnam$c118) <- "Use of fertilizers"
#C636 = FACT, replace label: "IPM"
var_label(HouseholdVietnam$c119) <- "IPM"
#C637 = FACT, replace label: "Weeding"
var_label(HouseholdVietnam$c1110) <- "Weeding"
#C638 = FACT, replace label: "Pheromones"
var_label(HouseholdVietnam$c1111) <- "Pheromones"
#C639 = FACT, replace label: "Forage production"
var_label(HouseholdVietnam$c1112) <- "Forage production"
#C640 = FACT, replace label: "Forage treatment / silage"
var_label(HouseholdVietnam$c1113) <- "Forage treatment / silage"
#C641 = FACT, replace label: "Farmer organization"
var_label(HouseholdVietnam$c1114) <- "Farmer organization"
#C642 = FACT, replace label: "Organic"
var_label(HouseholdVietnam$c1115) <- "Organic"
#C643 = FACT, replace label: "PGS"
var_label(HouseholdVietnam$c1116) <- "PGS"
#C644 = FACT, replace label: "Other"
var_label(HouseholdVietnam$c1199) <- "Other"
#We move this column here:
HouseholdVietnam <- HouseholdVietnam %>% relocate(c1199 , .after = c1116)
#C645 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C646 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "Now I would like to ask you whether you collaborate with other people to do any of the following?"
var_label(HouseholdVietnam$c13_a) <- "Now I would like to ask you whether you collaborate with other people to do any of the following?"
#C647 = FACT, replace label: "Share labor (mutual help, working together on each other farm)"
var_label(HouseholdVietnam$c131) <- "Share labor (mutual help, working together on each other farm)"
#C648 = FACT, replace label: "Manage water/irrigation systems"
var_label(HouseholdVietnam$c132) <- "Manage water/irrigation systems"
#C649 = FACT, replace label: "Raise livestock"
var_label(HouseholdVietnam$c133) <- "Raise livestock"
#C650 = FACT, replace label: "Buy agricultural inputs"
var_label(HouseholdVietnam$c134) <- "Buy agricultural inputs"
#C651 = FACT, replace label: "Selling products to the markets for other farmers "
var_label(HouseholdVietnam$c135) <- "Selling products to the markets for other farmers "
#C652 = FACT, replace label: "Experiment new farming practices"
var_label(HouseholdVietnam$c136) <- "Experiment new farming practices"
#C653 = FACT, replace label: "No collaboration with other people on these issues"
var_label(HouseholdVietnam$c130) <- "No collaboration with other people on these issues"
#We move this column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(c130 , .after = c136)
#C654 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C655 = FACT, replace label: "Other"
var_label(HouseholdVietnam$c1399) <- "Other"
#C656 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C657 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C658 = FACT, (bin), replace label: "Do you exchange your agricultural products, equipment or animals with other farmers?"
var_label(HouseholdVietnam$c14) <- "Do you exchange your agricultural products, equipment or animals with other farmers?"
#C659 = FACT, (bin), replace label: "Are you involved in some form of advocacy work (aiming to influence decision-making within political institutions)?"
var_label(HouseholdVietnam$c15) <- "Are you involved in some form of advocacy work (aiming to influence decision-making within political institutions)?"

# # #d.
#C660 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C661 = FACT, replace label: "Lowland"
#var_label(HouseholdVietnam$d1_1) <- "Lowland"
#C662 = FACT, replace label: "Upland"
#var_label(HouseholdVietnam$d1_2) <- "Upland"
#C663 = FACT, replace label: "Pasture"
#var_label(HouseholdVietnam$d1_3) <- "Pasture"
#C664 = FACT, replace label: "Fallow"
#var_label(HouseholdVietnam$d1_4) <- "Fallow"
#C665 = FACT, replace label: "Forest"
#var_label(HouseholdVietnam$d1_5) <- "Forest"
#C666 = FACT, replace label: "Aquaculture land"
#var_label(HouseholdVietnam$d1_6) <- "Aquaculture land"
#C667 = FACT, replace label: "Homegarden"
#var_label(HouseholdVietnam$d1_7) <- "Homegarden"
#C668 = NUMERIC, replace label: "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdVietnam$no_crop1) <- "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
#C669 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C670 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C671 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C672 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C673 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C674 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C675 = REMOVE, (empty + useless)
#C676 = NUMERIC, replace label: "d2a_1. UPLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdVietnam$no_crop2) <- "d2a_1. UPLAND - how many crop did you that your household planted in the past 12 month"
#C677 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C678 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C679 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C680 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C681 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C682 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C683 = OK, (the answer is still in Vietnamese, Ky will maybe solve it°
#C684 = REMOVE, text from the questionnaire
#C685 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#From 1 to 5 = Lowland crops in order, from 6 to 1x = Upland crops in order
#C686 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d2_sum1) <- "Lowland crop n1"
#C687 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d2_sum2) <- "Lowland crop n2"
#C688 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d2_sum3) <- "Lowland crop n3"
#C689 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d2_sum4) <- "Lowland crop n4"
#C690 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d2_sum5) <- "Lowland crop n5"
#C691 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d2_sum6) <- "Upland crop n1"
#C692 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d2_sum7) <- "Upland crop n2"
#C693 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d2_sum8) <- "Upland crop n3"
#C694 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d2_sum9) <- "Upland crop n4"
#C695 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d2_sum10) <- "Upland crop n5"
#C696 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d2_sum11) <- "Upland crop n6"
#C697 = FACT, (Table of correspondence), (see answers below)
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
#C698 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C699 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C700 = FACT, (Table of correspondence)
#C701 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C702 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C703 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C704 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$b13_011) <- "Lowland crop n1"
#C705 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$b13_012) <- "Lowland crop n2"
#C706 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$b13_013) <- "Lowland crop n3"
#C707 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$b13_014) <- "Lowland crop n4"
#C708 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$b13_015) <- "Lowland crop n5"
#C709 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdVietnam$b13_016) <- "Upland crop n1"
#C710 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdVietnam$b13_017) <- "Upland crop n2"
#C711 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdVietnam$b13_018) <- "Upland crop n3"
#C712 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdVietnam$b13_019) <- "Upland crop n4"
#C713 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdVietnam$b13_0110) <- "Upland crop n5"
#C714 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdVietnam$b13_0111) <- "Upland crop n6"
#C715 = FACT, (Table of correspondence), (see anwsers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C716 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdVietnam$b14_1) <- "does b12_1 provide any of the following"
#C717 = FACT, empty, replace label: "Nothing"
var_label(HouseholdVietnam$b14_10) <- "Nothing"
#We move this column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(b14_10 , .after = b14_1)
#C718 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdVietnam$b14_11) <- "Inputs (sold)"
#C719 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdVietnam$b14_12) <- "Inputs on credit"
#C720 = FACT, empty, replace label: "Gas Credit"
var_label(HouseholdVietnam$b14_13) <- "Gas Credit"
#C721 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdVietnam$b14_14) <- "Technical advice/training"
#C722 = FACT, empty, replace label: "Market information"
var_label(HouseholdVietnam$b14_15) <- "Market information"
#C723 = FACT, empty, UNKNOWN, replace label: "Regular sales"
var_label(HouseholdVietnam$b14_16) <- "Regular sales"
#C724 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$b14_199) <- "Other"
#C725 = FACT, empty, replace label: "Do not know"
var_label(HouseholdVietnam$b14_188) <- "Do not know"
#C726 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "b14_1oth. specify other provision from b12_1"
var_label(HouseholdVietnam$b14_1oth) <- "b14_1oth. specify other provision from b12_1"
#C727 = FACT, (Table of correspondence), (see answers below)
#Formal contract
#Informal contract 
#No contract/ no prior arrangements
#Spot relations 
#Do not know
#replace label: "b15_1. do you have a contract with b12_1"
var_label(HouseholdVietnam$b15_1) <- "b15_1. do you have a contract with b12_1"
#C728 = CHAR, (Useless answer-MultipleCombined, use the following columns),
#C729 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$b13_021) <- "Lowland crop n1"
#C730 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$b13_022) <- "Lowland crop n2"
#C731 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$b13_023) <- "Lowland crop n3"
#C732 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$b13_024) <- "Lowland crop n4"
#C733 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$b13_025) <- "Lowland crop n5"
#C734 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdVietnam$b13_026) <- "Upland crop n1"
#C735 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdVietnam$b13_027) <- "Upland crop n2"
#C736 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdVietnam$b13_028) <- "Upland crop n3"
#C737 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdVietnam$b13_029) <- "Upland crop n4"
#C738 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdVietnam$b13_0210) <- "Upland crop n5"
#C739 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdVietnam$b13_0211) <- "Upland crop n6"
#C740 = FACT, (Table of correspondence, see below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C741 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdVietnam$b14_2) <- "does b12_1 provide any of the following"
#C742 = FACT, empty, replace label: "Nothing"
var_label(HouseholdVietnam$b14_20) <- "Nothing"
#We move this column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(b14_20 , .after = b14_2)
#C743 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdVietnam$b14_21) <- "Inputs (sold)"
#C744 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdVietnam$b14_22) <- "Inputs on credit"
#C745 = FACT, empty, replace label: "Gas Credit"
var_label(HouseholdVietnam$b14_23) <- "Gas Credit"
#C746 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdVietnam$b14_24) <- "Technical advice/training"
#C747 = FACT, empty, replace label: "Market information"
var_label(HouseholdVietnam$b14_25) <- "Market information"
#C748 = FACT, empty, UNKNOWN, replace label: "Regular sales"
var_label(HouseholdVietnam$b14_26) <- "Regular sales"
#C749 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$b14_299) <- "Other"
#C750 = FACT, empty, replace label: "Do not know"
var_label(HouseholdVietnam$b14_288) <- "Do not know"
#C751 = OK, (empty), replace label: "b14_2oth. specify other provision from b12_1"
var_label(HouseholdVietnam$b14_2oth) <- "b14_2oth. specify other provision from b12_2"
#C752 = FACT, (Table of correspondence), (see answers below)
#Formal contract
#Informal contract 
#No contract/ no prior arrangements
#Spot relations 
#Do not know
#replace label: "b15_2. do you have a contract with b12_1"
var_label(HouseholdVietnam$b15_2) <- "b15_2. do you have a contract with b12_2"
#C753 = OK
#C754 = OK
#C755 = OK
#C756 = OK
#C757 = OK
#C758 = OK
#C759 = OK
#C760 = OK
#C761 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C762 = FACT, empty, replace label: "None of forest product"
var_label(HouseholdVietnam$d70) <- "None of forest product"
#C763 = FACT, empty, replace label: "Hunting"
var_label(HouseholdVietnam$d71) <- "Hunting"
#C764 = FACT, empty, replace label: "Fishing"
var_label(HouseholdVietnam$d72) <- "Fishing"
#C765 = FACT, empty, replace label: "Fuelwood"
var_label(HouseholdVietnam$d73) <- "Fuelwood"
#C766 = FACT, empty, replace label: "Mushrooms"
var_label(HouseholdVietnam$d74) <- "Mushrooms"
#C767 = FACT, empty, replace label: "Bamboo shoots"
var_label(HouseholdVietnam$d75) <- "Bamboo shoots"
#C768 = FACT, empty, UNKNOWN, replace label: "Bamboo poles"
var_label(HouseholdVietnam$d76) <- "Bamboo poles"
#C769 = FACT, empty, replace label: "Broom grass"
var_label(HouseholdVietnam$d77) <- "Broom grass"
#C770 = FACT, empty, replace label: "Honey"
var_label(HouseholdVietnam$d78) <- "Honey"
#C771 = FACT, empty, replace label: "Rattan"
var_label(HouseholdVietnam$d79) <- "Rattan"
#C772 = FACT, empty, replace label: "Cardamom"
var_label(HouseholdVietnam$d710) <- "Cardamom"
#C773 = FACT, empty, replace label: "Galangal"
var_label(HouseholdVietnam$d711) <- "Galangal"
#C774 = FACT, empty, replace label: "Dammar gum"
var_label(HouseholdVietnam$d712) <- "Dammar gum"
#C775 = FACT, empty, replace label: "Wild pepper"
var_label(HouseholdVietnam$d713) <- "Wild pepper"
#C776 = FACT, empty, UNKNOWN, replace label: "Medicinal plants"
var_label(HouseholdVietnam$d714) <- "Medicinal plants"
#C777 = FACT, empty, replace label: "Paper mulberry bark"
var_label(HouseholdVietnam$d715) <- "Paper mulberry bark"
#C778 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$d799) <- "Other"
#C779 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C780 = OK, (empty)
#C781 = FACT, (empty)
#C782 = OK, (empty)
#C783 = OK, (empty)
#C784 = FACT, (empty)
#C785 = OK, (empty)
#C786 = OK
#C787 = FACT, (bin)
#C788 = OK
#C789 = OK
#C790 = FACT, (bin)
#C791 = OK, (empty)
#C792 = OK
#C793 = FACT, (bin)
#C794 = OK
#C795 = OK
#C796 = FACT, (bin)
#C797 = OK, (empty)
#C798 = OK
#C799 = FACT, (bin)
#C800 = OK, (empty)
#C801 = OK
#C802 = FACT, (bin)
#C803 = OK
#C804 = OK, (empty)
#C805 = FACT, (empty)
#C806 = OK, (empty)
#C807 = OK
#C808 = FACT, (bin)
#C809 = OK, (empty)
#C810 = OK
#C811 = FACT, (bin)
#C812 = OK
#C813 = OK, (empty)
#C814 = FACT, (empty)
#C815 = OK, (empty)
#C816 = OK
#C817 = FACT, (bin)
#C818 = OK, (empty)
#C819 = OK
#C820 = FACT, (bin)
#C821 = OK
#C822 = OK, (empty)
#C823 = FACT, (empty)
#C824 = OK, (empty)
#C825 = OK, replace label: "d7_991. how many days did your household spend to collect d7_oth over 12 months"
var_label(HouseholdVietnam$d7_991) <- "d7_991. how many days did your household spend to collect d7_oth over 12 months"
#C826 = FACT, (bin), replace label: "d7_992. did your household sell collect d7_oth product in the last 12 month"
var_label(HouseholdVietnam$d7_992) <- "d7_992. did your household sell collect d7_oth product in the last 12 month"
#C827 = OK, replace label: "d7_993. what was your household income from collecting d7_oth in the last 12 months"
var_label(HouseholdVietnam$d7_993) <- "d7_993. what was your household income from collecting d7_oth in the last 12 months"
#C828 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C829 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C830 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C831 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C832 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C833 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C834 = FACT, (table of correspondence), see choices below, replace label: "d81_1a.what was the main reason for planting d81_text"
var_label(HouseholdVietnam$d81_1a) <- "d81_1a.what was the main reason for planting d81_text"
#Market price and demand
#Household consumption preferences; 
#Well adapted to local conditions (soil, climate, …)
#Do not know
#Other
#C835 = OK, replace label: "d81_1a_oth. specify other reason for planting d81_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d81_1a_oth) <- "d81_1a_oth. specify other reason for planting d81_text"
#C836 = FACT, (table of correspondence), see choices below, replace label: "d81_1b. what was the major constraints for d81_text"
var_label(HouseholdVietnam$d81_1b) <- "d81_1b. what was the major constraints for d81_text"
#Water management
#Soil fertility
#Other agronomic constraints
#Insects
#Diseases
#Market price
#Product quality
#Do not know
#Other
#C837 = OK, replace label: "d81_1b_oth. specify other constrain for d81_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d81_1b_oth) <- "d81_1b_oth. specify other constrain for d81_text"
#C838 = FACT, replace label: "d82_1a.what was the main reason for planting d82_text"
var_label(HouseholdVietnam$d82_1a) <- "d82_1a.what was the main reason for planting d82_text"
#C839 = OK, replace label: "d82_1a_oth. specify other reason for planting d82_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d82_1a_oth) <- "d82_1a_oth. specify other reason for planting d82_text"
#C840 = FACT, replace label: "d82_1b. what was the major constraints for d82_text"
var_label(HouseholdVietnam$d82_1b) <- "d82_1b. what was the major constraints for d82_text"
#C841 = OK, replace label: "d82_1b_oth. specify other constrain for d82_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d82_1b_oth) <- "d82_1b_oth. specify other constrain for d82_text"
#C842 = FACT, replace label: "d83_1a.what was the main reason for planting d83_text"
var_label(HouseholdVietnam$d83_1a) <- "d83_1a.what was the main reason for planting d83_text"
#C843 = OK, replace label: "d83_1a_oth. specify other reason for planting d83_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d83_1a_oth) <- "d83_1a_oth. specify other reason for planting d83_text"
#C844 = FACT, see choices below, replace label: "d83_1b. what was the major constraints for d83_text"
var_label(HouseholdVietnam$d83_1b) <- "d83_1b. what was the major constraints for d83_text"
#C845 = OK, replace label: "d83_1b_oth. specify other constrain for d83_text", (the answer is still in Vietnamese, Ky will maybe solve it)
var_label(HouseholdVietnam$d83_1b_oth) <- "d83_1b_oth. specify other constrain for d83_text"
#C846 = REMOVE (Text from the questionnaire)
#C847 = FACT, (bin)
#C848 = OK, High values (>1000)
#C849 = FACT, (bin)
#C850 = OK, High values (>1000)
#C851 = FACT, (bin)
#C852 = OK
#C853 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C854 = FACT, replace label: "Title deed"
var_label(HouseholdVietnam$d11_41) <- "Title deed"
#C855 = FACT, replace label: "Certificate of customary tenure"
var_label(HouseholdVietnam$d11_42) <- "Certificate of customary tenure"
#C856 = FACT, replace label: "Certificate of occupancy"
var_label(HouseholdVietnam$d11_43) <- "Certificate of occupancy"
#C857 = FACT, replace label: "Registered will or registered certificate of hereditary acquisition"
var_label(HouseholdVietnam$d11_44) <- "Registered will or registered certificate of hereditary acquisition"
#C858 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C859 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C860 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C861 = FACT, replace label: "Do not know"
var_label(HouseholdVietnam$d11_488) <- "Do not know"
#C862 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$d11_499) <- "Other"
#C863 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C864 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C865 = FACT, replace label: "Rainwater collection/conservation"
var_label(HouseholdVietnam$d121) <- "Rainwater collection/conservation"
#C866 = FACT, replace label: "Greywater recycling"
var_label(HouseholdVietnam$d122) <- "Greywater recycling"
#C867 = FACT, replace label: "Ponds (for water conservation)"
var_label(HouseholdVietnam$d123) <- "Ponds (for water conservation)"
#C868 = FACT, replace label: "Terraces building"
var_label(HouseholdVietnam$d124) <- "Terraces building"
#C869 = FACT, replace label: "Swales digging"
var_label(HouseholdVietnam$d125) <- "Swales digging"
#C870 = FACT, empty, replace label: "Land levelling"
var_label(HouseholdVietnam$d126) <- "Land levelling"
#C871 = FACT, empty, replace label: "Mulching"
var_label(HouseholdVietnam$d127) <- "Mulching"
#C872 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$d1299) <- "Other"
#C873 = FACT, empty, replace label: "No water conservation practice"
var_label(HouseholdVietnam$d120) <- "No water conservation practice"
#C874 = FACT, empty, replace label: "Do not know"
var_label(HouseholdVietnam$d1288) <- "Do not know"
#C875 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C876 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C877 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d131_11) <- "Lowland crop n1"
#C878 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d131_12) <- "Lowland crop n2"
#C879 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d131_13) <- "Lowland crop n3"
#C880 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d131_14) <- "Lowland crop n4"
#C881 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d131_15) <- "Lowland crop n5"
#C882 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d131_16) <- "Upland crop n1"
#C883 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d131_17) <- "Upland crop n2"
#C884 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d131_18) <- "Upland crop n3"
#C885 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d131_19) <- "Upland crop n4"
#C886 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d131_110) <- "Upland crop n5"
#C887 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d131_111) <- "Upland crop n6"
#C888 = FACT, (table of correspondence), see choices below
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C889 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C890 = CHAR, (empty), (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C891 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d132_11) <- "Lowland crop n1"
#C892 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d132_12) <- "Lowland crop n2"
#C893 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d132_13) <- "Lowland crop n3"
#C894 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d132_14) <- "Lowland crop n4"
#C895 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d132_15) <- "Lowland crop n5"
#C896 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d132_16) <- "Upland crop n1"
#C897 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d132_17) <- "Upland crop n2"
#C898 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d132_18) <- "Upland crop n3"
#C899 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d132_19) <- "Upland crop n4"
#C900 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d132_110) <- "Upland crop n5"
#C901 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d132_111) <- "Upland crop n6"
#C902 = FACT, (empty), (table of correspondence)
#C903 = OK, (empty)
#C904 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C905 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d133_11) <- "Lowland crop n1"
#C906 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d133_12) <- "Lowland crop n2"
#C907 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d133_13) <- "Lowland crop n3"
#C908 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d133_14) <- "Lowland crop n4"
#C909 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d133_15) <- "Lowland crop n5"
#C910 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d133_16) <- "Upland crop n1"
#C911 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d133_17) <- "Upland crop n2"
#C912 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d133_18) <- "Upland crop n3"
#C913 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d133_19) <- "Upland crop n4"
#C914 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d133_110) <- "Upland crop n5"
#C915 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d133_111) <- "Upland crop n6"
#C916 = FACT, (table of correspondence)
#C917 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C918 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C919 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d134_11) <- "Lowland crop n1"
#C920 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d134_12) <- "Lowland crop n2"
#C921 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d134_13) <- "Lowland crop n3"
#C922 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d134_14) <- "Lowland crop n4"
#C923 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d134_15) <- "Lowland crop n5"
#C924 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d134_16) <- "Upland crop n1"
#C925 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d134_17) <- "Upland crop n2"
#C926 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d134_18) <- "Upland crop n3"
#C927 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d134_19) <- "Upland crop n4"
#C928 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d134_110) <- "Upland crop n5"
#C929 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d134_111) <- "Upland crop n6"
#C930 = FACT, (table of correspondence)
#C931 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C932 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C933 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d135_11) <- "Lowland crop n1"
#C934 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d135_12) <- "Lowland crop n2"
#C935 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d135_13) <- "Lowland crop n3"
#C936 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d135_14) <- "Lowland crop n4"
#C937 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d135_15) <- "Lowland crop n5"
#C938 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d135_16) <- "Upland crop n1"
#C939 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d135_17) <- "Upland crop n2"
#C940 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d135_18) <- "Upland crop n3"
#C941 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d135_19) <- "Upland crop n4"
#C942 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d135_110) <- "Upland crop n5"
#C943 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d135_111) <- "Upland crop n6"
#C944 = FACT, (table of correspondence)
#C945 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C946 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C947 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d136_11) <- "Lowland crop n1"
#C948 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d136_12) <- "Lowland crop n2"
#C949 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d136_13) <- "Lowland crop n3"
#C950 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d136_14) <- "Lowland crop n4"
#C951 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d136_15) <- "Lowland crop n5"
#C952 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d136_16) <- "Upland crop n1"
#C953 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d136_17) <- "Upland crop n2"
#C954 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d136_18) <- "Upland crop n3"
#C955 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d136_19) <- "Upland crop n4"
#C956 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d136_110) <- "Upland crop n5"
#C957 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d136_111) <- "Upland crop n6"
#C958 = FACT, (table of correspondence)
#C959 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C960 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C961 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d137_11) <- "Lowland crop n1"
#C962 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d137_12) <- "Lowland crop n2"
#C963 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d137_13) <- "Lowland crop n3"
#C964 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d137_14) <- "Lowland crop n4"
#C965 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d137_15) <- "Lowland crop n5"
#C966 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d137_16) <- "Upland crop n1"
#C967 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d137_17) <- "Upland crop n2"
#C968 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d137_18) <- "Upland crop n3"
#C969 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d137_19) <- "Upland crop n4"
#C970 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d137_110) <- "Upland crop n5"
#C971 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d137_111) <- "Upland crop n6"
#C972 = FACT, (table of correspondence)
#C973 = OK, (empty)
#C974 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C975 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d138_11) <- "Lowland crop n1"
#C976 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d138_12) <- "Lowland crop n2"
#C977 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d138_13) <- "Lowland crop n3"
#C978 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d138_14) <- "Lowland crop n4"
#C979 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d138_15) <- "Lowland crop n5"
#C980 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d138_16) <- "Upland crop n1"
#C981 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d138_17) <- "Upland crop n2"
#C982 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d138_18) <- "Upland crop n3"
#C983 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d138_19) <- "Upland crop n4"
#C984 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d138_110) <- "Upland crop n5"
#C985 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d138_111) <- "Upland crop n6"
#C986 = FACT, (table of correspondence)
#C987 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C988 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C989 = FACT, replace label: "No soil conservation practice"
var_label(HouseholdVietnam$d140) <- "No soil conservation practice"
HouseholdVietnam <- HouseholdVietnam %>% relocate(d140 , .after = d14)
#C990 = FACT, replace label: "Sowing in contour lines"
var_label(HouseholdVietnam$d141) <- "Sowing in contour lines"
#C991 = FACT, replace label: "Natural or planted grass strips"
var_label(HouseholdVietnam$d142) <- "Natural or planted grass strips"
#C992 = FACT, replace label: "Trees conservation in agricultural plots"
var_label(HouseholdVietnam$d143) <- "Trees conservation in agricultural plots"
#C993 = FACT, replace label: "Agroforestry (trees + crops)"
var_label(HouseholdVietnam$d144) <- "Agroforestry (trees + crops)"
#C994 = FACT, empty, replace label: "Crop residues maintained to cover the soil"
var_label(HouseholdVietnam$d145) <- "Crop residues maintained to cover the soil"
#C995 = FACT, empty, replace label: "Use of cover crops"
var_label(HouseholdVietnam$d146) <- "Use of cover crops"
#C996 = FACT, empty, replace label: "Reduced to no-tillage"
var_label(HouseholdVietnam$d147) <- "Reduced to no-tillage"
#C997 = FACT, empty, replace label: "Other"
var_label(HouseholdVietnam$d1499) <- "Other"
#C998 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C999 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1000 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d151_11) <- "Lowland crop n1"
#C1001 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d151_12) <- "Lowland crop n2"
#C1002 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d151_13) <- "Lowland crop n3"
#C1003 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d151_14) <- "Lowland crop n4"
#C1004 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d151_15) <- "Lowland crop n5"
#C1005 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d151_16) <- "Upland crop n1"
#C1006 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d151_17) <- "Upland crop n2"
#C1007 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d151_18) <- "Upland crop n3"
#C1008 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d151_19) <- "Upland crop n4"
#C1009 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d151_110) <- "Upland crop n5"
#C1010 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d151_111) <- "Upland crop n6"
#C1011 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1012 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1013 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1014 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d152_11) <- "Lowland crop n1"
#C1015 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d152_12) <- "Lowland crop n2"
#C1016 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d152_13) <- "Lowland crop n3"
#C1017 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d152_14) <- "Lowland crop n4"
#C1018 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d152_15) <- "Lowland crop n5"
#C1019 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d152_16) <- "Upland crop n1"
#C1020 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d152_17) <- "Upland crop n2"
#C1021 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d152_18) <- "Upland crop n3"
#C1022 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d152_19) <- "Upland crop n4"
#C1023 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d152_110) <- "Upland crop n5"
#C1024 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d152_111) <- "Upland crop n6"
#C1025 = FACT, (table of correspondence)
#C1026 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1027 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1028 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d153_11) <- "Lowland crop n1"
#C1029 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d153_12) <- "Lowland crop n2"
#C1030 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d153_13) <- "Lowland crop n3"
#C1031 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d153_14) <- "Lowland crop n4"
#C1032 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d153_15) <- "Lowland crop n5"
#C1033 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d153_16) <- "Upland crop n1"
#C1034 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d153_17) <- "Upland crop n2"
#C1035 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d153_18) <- "Upland crop n3"
#C1036 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d153_19) <- "Upland crop n4"
#C1037 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d153_110) <- "Upland crop n5"
#C1038 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d153_111) <- "Upland crop n6"
#C1039 = FACT, (table of correspondence)
#C1040 = OK, (empty)
#C1041 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1042 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d154_11) <- "Lowland crop n1"
#C1043 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_12 <- str_extract(HouseholdVietnam$d154_1, "2")
HouseholdVietnam$d154_12 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_12),"0",HouseholdVietnam$d154_12)
HouseholdVietnam$d154_12[HouseholdVietnam$d154_12 == '2'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_12 , .after = d154_11)
#replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d154_12) <- "Lowland crop n2"
#C1044 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_13 <- str_extract(HouseholdVietnam$d154_1, "3")
HouseholdVietnam$d154_13 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_13),"0",HouseholdVietnam$d154_13)
HouseholdVietnam$d154_13[HouseholdVietnam$d154_13 == '3'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_13 , .after = d154_12)
#replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d154_13) <- "Lowland crop n3"
#C1045 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_14 <- str_extract(HouseholdVietnam$d154_1, "4")
HouseholdVietnam$d154_14 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_14),"0",HouseholdVietnam$d154_14)
HouseholdVietnam$d154_14[HouseholdVietnam$d154_14 == '4'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_14 , .after = d154_13)
#replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d154_14) <- "Lowland crop n4"
#C1046 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_15 <- str_extract(HouseholdVietnam$d154_1, "5")
HouseholdVietnam$d154_15 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_15),"0",HouseholdVietnam$d154_15)
HouseholdVietnam$d154_15[HouseholdVietnam$d154_15 == '5'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_15 , .after = d154_14)
#replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d154_15) <- "Lowland crop n5"
#C1047 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_16 <- str_extract(HouseholdVietnam$d154_1, "6")
HouseholdVietnam$d154_16 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_16),"0",HouseholdVietnam$d154_16)
HouseholdVietnam$d154_16[HouseholdVietnam$d154_16 == '6'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_16 , .after = d154_15)
#replace label: "Upland crop n1"
var_label(HouseholdVietnam$d154_16) <- "Upland crop n1"
#C1048 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_17 <- str_extract(HouseholdVietnam$d154_1, "7")
HouseholdVietnam$d154_17 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_17),"0",HouseholdVietnam$d154_17)
HouseholdVietnam$d154_17[HouseholdVietnam$d154_17 == '7'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_17 , .after = d154_16)
#replace label: "Upland crop n2"
var_label(HouseholdVietnam$d154_17) <- "Upland crop n2"
#C1049 = FACT,  non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_18 <- str_extract(HouseholdVietnam$d154_1, "8")
HouseholdVietnam$d154_18 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_18),"0",HouseholdVietnam$d154_18)
HouseholdVietnam$d154_18[HouseholdVietnam$d154_18 == '8'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_18 , .after = d154_17)
#replace label: "Upland crop n3"
var_label(HouseholdVietnam$d154_18) <- "Upland crop n3"
#C1050 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_19 <- str_extract(HouseholdVietnam$d154_1, "9")
HouseholdVietnam$d154_19 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_19),"0",HouseholdVietnam$d154_19)
HouseholdVietnam$d154_19[HouseholdVietnam$d154_19 == '9'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_19 , .after = d154_18)
#replace label: "Upland crop n4"
var_label(HouseholdVietnam$d154_19) <- "Upland crop n4"
#C1051 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_110 <- str_extract(HouseholdVietnam$d154_1, "10")
HouseholdVietnam$d154_110 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_110),"0",HouseholdVietnam$d154_110)
HouseholdVietnam$d154_110[HouseholdVietnam$d154_110 == '10'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_110 , .after = d154_19)
#replace label: "Upland crop n5"
var_label(HouseholdVietnam$d154_110) <- "Upland crop n5"
#C1052 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d154_111 <- str_extract(HouseholdVietnam$d154_1, "11")
HouseholdVietnam$d154_111 <- ifelse(HouseholdVietnam$d154_1 != '' & is.na(HouseholdVietnam$d154_111),"0",HouseholdVietnam$d154_111)
HouseholdVietnam$d154_111[HouseholdVietnam$d154_111 == '11'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d154_111 , .after = d154_110)
#replace label: "Upland crop n6"
var_label(HouseholdVietnam$d154_111) <- "Upland crop n6"
#C1053 = FACT, (table of correspondence)
#C1054 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1055 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1056 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d155_11) <- "Lowland crop n1"
#C1057 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d155_12) <- "Lowland crop n2"
#C1058 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d155_13) <- "Lowland crop n3"
#C1059 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d155_14) <- "Lowland crop n4"
#C1060 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d155_15) <- "Lowland crop n5"
#C1061 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d155_16) <- "Upland crop n1"
#C1062 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d155_17) <- "Upland crop n2"
#C1063 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d155_18) <- "Upland crop n3"
#C1064 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d155_19) <- "Upland crop n4"
#C1065 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d155_110) <- "Upland crop n5"
#C1066 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d155_111) <- "Upland crop n6"
#C1067 = FACT, (table of correspondence)
#C1068 = OK, (empty)
#C1069 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1070 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d156_11) <- "Lowland crop n1"
#C1071 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d156_12) <- "Lowland crop n2"
#C1072 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d156_13) <- "Lowland crop n3"
#C1073 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d156_14) <- "Lowland crop n4"
#C1074 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d156_15) <- "Lowland crop n5"
#C1075 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d156_16) <- "Upland crop n1"
#C1076 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d156_17) <- "Upland crop n2"
#C1077 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d156_18) <- "Upland crop n3"
#C1078 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d156_19) <- "Upland crop n4"
#C1079 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d156_110) <- "Upland crop n5"
#C1080 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d156_111) <- "Upland crop n6"
#C1081 = FACT, (table of correspondence)
#C1082 = OK, (empty)
#C1083 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1084 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d157_11) <- "Lowland crop n1"
#C1085 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d157_12) <- "Lowland crop n2"
#C1086 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d157_13) <- "Lowland crop n3"
#C1087 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d157_14) <- "Lowland crop n4"
#C1088 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d157_15) <- "Lowland crop n5"
#C1089 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d157_16) <- "Upland crop n1"
#C1090 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d157_17) <- "Upland crop n2"
#C1091 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d157_18) <- "Upland crop n3"
#C1092 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d157_19) <- "Upland crop n4"
#C1093 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d157_110) <- "Upland crop n5"
#C1094 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d157_111) <- "Upland crop n6"
#C1095 = FACT, (table of correspondence)
#C1096 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1097 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#replace label: "d158_1.  for which crop(s) do you use d14_oth"
var_label(HouseholdVietnam$d158_1) <- "d158_1.  for which crop(s) do you use d14_oth"
#C1098 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d158_11) <- "Lowland crop n1"
#C1099 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d158_12) <- "Lowland crop n2"
#C1100 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d158_13) <- "Lowland crop n3"
#C1101 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d158_14) <- "Lowland crop n4"
#C1102 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d158_15) <- "Lowland crop n5"
#C1103 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d158_16) <- "Upland crop n1"
#C1104 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d158_17) <- "Upland crop n2"
#C1105 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d158_18) <- "Upland crop n3"
#C1106 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d158_19) <- "Upland crop n4"
#C1107 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d158_110) <- "Upland crop n5"
#C1108 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d158_111) <- "Upland crop n6"
#C1109 = FACT, (table of correspondence), replace label: "d158_2. what was your main motivation to implement d14_oth"
var_label(HouseholdVietnam$d158_2) <- "d158_2. what was your main motivation to implement d14_oth"
#C1110 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1111 = FACT, (table of correspondence), (see choices below),
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1112 = FACT, (table of correspondence), (0 = "no", 1 = "yes", 88 = "Do not know"),
#C1113 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
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
#C1114 = FACT,replace label: "Animal manure"
var_label(HouseholdVietnam$d181) <- "Animal manure"
#C1115 = FACT, replace label: "Compost (heap)"
var_label(HouseholdVietnam$d182) <- "Compost (heap)"
#C1116 = FACT, replace label: "Bokashi (fermented organic matter)"
var_label(HouseholdVietnam$d183) <- "Bokashi (fermented organic matter)"
#C1117 = FACT, replace label: "Legume-based green manure"
var_label(HouseholdVietnam$d184) <- "Legume-based green manure"
#C1118 = FACT, replace label: "Pulses in association and/or rotation with main crop"
var_label(HouseholdVietnam$d185) <- "Pulses in association and/or rotation with main crop"
#C1119 = FACT, replace label: "Cover crops in association and/or rotation with main crop"
var_label(HouseholdVietnam$d186) <- "Cover crops in association and/or rotation with main crop"
#C1120 = FACT, replace label: "Biochar"
var_label(HouseholdVietnam$d187) <- "Biochar"
#C1121 = FACT, replace label: "Crop residue maintenance"
var_label(HouseholdVietnam$d188) <- "Crop residue maintenance"
#C1122 = FACT, replace label: "Recycling crop waste"
var_label(HouseholdVietnam$d189) <- "Recycling crop waste"
#C1123 = FACT, replace label: "Ramial Wood Chip (RWC) or other wood chips"
var_label(HouseholdVietnam$d1810) <- "Ramial Wood Chip (RWC) or other wood chips"
#C1124 = FACT, replace label: "Organic agro-industrial waste"
var_label(HouseholdVietnam$d1811) <- "Organic agro-industrial waste"
#C1125 = FACT, replace label: "Other methods"
var_label(HouseholdVietnam$d1899) <- "Other methods"
#C1126 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1127 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_11. for which crop(s) do you use animal manure?"
var_label(HouseholdVietnam$d18_11) <- "d18_11. for which crop(s) do you use animal manure?"
#C1128 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_111) <- "Lowland crop n1"
#C1129 = FACT, change column name: "d18_112", replace label: "Lowland crop n2"
colnames(HouseholdVietnam)[1129] <- 'd18_112'
var_label(HouseholdVietnam$d18_112) <- "Lowland crop n2"
#C1130 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_113) <- "Lowland crop n3"
#C1131 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_114) <- "Lowland crop n4"
#C1132 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_115) <- "Lowland crop n5"
#C1133 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_116) <- "Upland crop n1"
#C1134 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_117) <- "Upland crop n2"
#C1135 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_118) <- "Upland crop n3"
#C1136 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_119) <- "Upland crop n4"
#C1137 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_1110) <- "Upland crop n5"
#C1138 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_1111) <- "Upland crop n6"
#C1139 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1140 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1141 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1142 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_211) <- "Lowland crop n1"
#C1143 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_212) <- "Lowland crop n2"
#C1144 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_213) <- "Lowland crop n3"
#C1145 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_214) <- "Lowland crop n4"
#C1146 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_215) <- "Lowland crop n5"
#C1147 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_216) <- "Upland crop n1"
#C1148 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_217) <- "Upland crop n2"
#C1149 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_218) <- "Upland crop n3"
#C1150 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_219) <- "Upland crop n4"
#C1151 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_2110) <- "Upland crop n5"
#C1152 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_2111) <- "Upland crop n6"
#C1153 = FACT, (table of correspondence)
#C1154 = OK, (empty)
#C1155 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1156 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_311) <- "Lowland crop n1"
#C1157 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_312) <- "Lowland crop n2"
#C1158 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_313) <- "Lowland crop n3"
#C1159 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_314) <- "Lowland crop n4"
#C1160 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_315) <- "Lowland crop n5"
#C1161 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_316) <- "Upland crop n1"
#C1162 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_317) <- "Upland crop n2"
#C1163 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_318) <- "Upland crop n3"
#C1164 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_319) <- "Upland crop n4"
#C1165 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_3110) <- "Upland crop n5"
#C1166 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_3111) <- "Upland crop n6"
#C1167 = FACT, (table of correspondence)
#C1168 = OK, (empty)
#C1169 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1170 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_411) <- "Lowland crop n1"
#C1171 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_412) <- "Lowland crop n2"
#C1172 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_413) <- "Lowland crop n3"
#C1173 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_414) <- "Lowland crop n4"
#C1174 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_415) <- "Lowland crop n5"
#C1175 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_416) <- "Upland crop n1"
#C1176 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_417) <- "Upland crop n2"
#C1177 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_418) <- "Upland crop n3"
#C1178 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_419) <- "Upland crop n4"
#C1179 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_4110) <- "Upland crop n5"
#C1180 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_4111) <- "Upland crop n6"
#C1181 = FACT, (table of correspondence)
#C1182 = OK, (empty)
#C1183 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1184 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_511) <- "Lowland crop n1"
#C1185 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_512) <- "Lowland crop n2"
#C1186 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_513) <- "Lowland crop n3"
#C1187 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_514) <- "Lowland crop n4"
#C1188 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_515) <- "Lowland crop n5"
#C1189 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_516) <- "Upland crop n1"
#C1190 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_517) <- "Upland crop n2"
#C1191 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_518) <- "Upland crop n3"
#C1192 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_519) <- "Upland crop n4"
#C1193 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_5110) <- "Upland crop n5"
#C1194 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_5111) <- "Upland crop n6"
#C1195 = FACT, (table of correspondence)
#C1196 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1197 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1198 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_611) <- "Lowland crop n1"
#C1199 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_612) <- "Lowland crop n2"
#C1200 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_613) <- "Lowland crop n3"
#C1201 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_614) <- "Lowland crop n4"
#C1202 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_615) <- "Lowland crop n5"
#C1203 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_616) <- "Upland crop n1"
#C1204 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_617) <- "Upland crop n2"
#C1205 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_618) <- "Upland crop n3"
#C1206 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_619) <- "Upland crop n4"
#C1207 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_6110) <- "Upland crop n5"
#C1208 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_6111) <- "Upland crop n6"
#C1209 = FACT, (table of correspondence)
#C1210 = OK, (empty)
#C1211 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1212 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_711) <- "Lowland crop n1"
#C1213 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_712) <- "Lowland crop n2"
#C1214 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_713) <- "Lowland crop n3"
#C1215 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_714) <- "Lowland crop n4"
#C1216 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_715) <- "Lowland crop n5"
#C1217 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_716) <- "Upland crop n1"
#C1218 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_717) <- "Upland crop n2"
#C1219 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_718) <- "Upland crop n3"
#C1220 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_719) <- "Upland crop n4"
#C1221 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_7110) <- "Upland crop n5"
#C1222 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_7111) <- "Upland crop n6"
#C1223 = FACT, (empty), (table of correspondence)
#C1224 = OK, (empty)
#C1225 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1226 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_811) <- "Lowland crop n1"
#C1227 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_812) <- "Lowland crop n2"
#C1228 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_813) <- "Lowland crop n3"
#C1229 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_814) <- "Lowland crop n4"
#C1230 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_815) <- "Lowland crop n5"
#C1231 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_816) <- "Upland crop n1"
#C1232 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_817) <- "Upland crop n2"
#C1233 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_818) <- "Upland crop n3"
#C1234 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_819) <- "Upland crop n4"
#C1235 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_8110) <- "Upland crop n5"
#C1236 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_8111) <- "Upland crop n6"
#C1237 = FACT, (table of correspondence)
#C1238 = OK, (empty)
#C1239 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1240 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_911) <- "Lowland crop n1"
#C1241 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_912) <- "Lowland crop n2"
#C1242 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_913) <- "Lowland crop n3"
#C1243 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_914) <- "Lowland crop n4"
#C1244 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_915) <- "Lowland crop n5"
#C1245 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_916) <- "Upland crop n1"
#C1246 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_917) <- "Upland crop n2"
#C1247 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_918) <- "Upland crop n3"
#C1248 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_919) <- "Upland crop n4"
#C1249 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_9110) <- "Upland crop n5"
#C1250 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_9111) <- "Upland crop n6"
#C1251 = FACT, (empty), (table of correspondence)
#C1252 = OK, (empty)
#C1253 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1254 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_1011) <- "Lowland crop n1"
#C1255 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_1012) <- "Lowland crop n2"
#C1256 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_1013) <- "Lowland crop n3"
#C1257 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_1014) <- "Lowland crop n4"
#C1258 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_1015) <- "Lowland crop n5"
#C1259 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_1016) <- "Upland crop n1"
#C1260 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_1017) <- "Upland crop n2"
#C1261 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_1018) <- "Upland crop n3"
#C1262 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_1019) <- "Upland crop n4"
#C1263 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_10110) <- "Upland crop n5"
#C1264 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_10111) <- "Upland crop n6"
#C1265 = FACT, (table of correspondence)
#C1266 = OK, (empty)
#C1267 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_111_a_a. for which crop(s) do you use organic agro-industrial waste?"
var_label(HouseholdVietnam$d18_111_a) <- "d18_111_a_a. for which crop(s) do you use organic agro-industrial waste?"
#C1268 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_1111_a) <- "Lowland crop n1"
#C1269 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_1112_a) <- "Lowland crop n2"
#C1270 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_1113_a) <- "Lowland crop n3"
#C1271 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_1114_a) <- "Lowland crop n4"
#C1272 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_1115_a) <- "Lowland crop n5"
#C1273 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_1116_a) <- "Upland crop n1"
#C1274 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_1117_a) <- "Upland crop n2"
#C1275 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_1118_a) <- "Upland crop n3"
#C1276 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_1119_a) <- "Upland crop n4"
#C1277 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_11110_a) <- "Upland crop n5"
#C1278 = FACT,  (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_11111_a) <- "Upland crop n6"
#C1279 = FACT,  (empty), (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdVietnam$d18_112) <- "d18_112. What was your main motivation to implement this practice?"
#C1280 = OK, (empty)
#C1281 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_991. for which crop(s) do you use d18_oth ?"
var_label(HouseholdVietnam$d18_991) <- "d18_991. for which crop(s) do you use d18_oth?"
#C1282 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d18_9911) <- "Lowland crop n1"
#C1283 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d18_9912) <- "Lowland crop n2"
#C1284 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d18_9913) <- "Lowland crop n3"
#C1285 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d18_9914) <- "Lowland crop n4"
#C1286 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d18_9915) <- "Lowland crop n5"
#C1287 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d18_9916) <- "Upland crop n1"
#C1288 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d18_9917) <- "Upland crop n2"
#C1289 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d18_9918) <- "Upland crop n3"
#C1290 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d18_9919) <- "Upland crop n4"
#C1291 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d18_99110) <- "Upland crop n5"
#C1292 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d18_99111) <- "Upland crop n6"
#C1293 = FACT, (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdVietnam$d18_992) <- "d18_112. What was your main motivation to implement this practice?"
#C1294 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1295 = FACT, (table of correspondence), (see option below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1296 = FACT, (table of correspondence), (0 = no, 1 = yes, 88 = I don't know)
#C1297 = CHAR, (see choices below), (Useless answer-MultipleCombined, use the following columns)
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
#C1298 = FACT,replace label: "Crop rotation / intercropping"
var_label(HouseholdVietnam$d211) <- "Crop rotation / intercropping"
#C1299 = FACT, replace label: "Cover crops"
var_label(HouseholdVietnam$d212) <- "Cover crops"
#C1300 = FACT, replace label: "Mulching / shading"
var_label(HouseholdVietnam$d213) <- "Mulching / shading"
#C1301 = FACT, replace label: "Sowing date / rate / depth"
var_label(HouseholdVietnam$d214) <- "Sowing date / rate / depth"
#C1302 = FACT, replace label: "Crop spatial arrangement"
var_label(HouseholdVietnam$d215) <- "Crop spatial arrangement"
#C1303 = FACT, replace label: "Seed cleaning before sowing"
var_label(HouseholdVietnam$d216) <- "Seed cleaning before sowing"
#C1304 = FACT, replace label: "Cultivar choice"
var_label(HouseholdVietnam$d217) <- "Cultivar choice"
#C1305 = FACT, replace label: "Crop mixtures"
var_label(HouseholdVietnam$d218) <- "Crop mixtures"
#C1306 = FACT, replace label: "Nutrient placement"
var_label(HouseholdVietnam$d219) <- "Nutrient placement"
#C1307 = FACT, replace label: "Patch/ban spraying"
var_label(HouseholdVietnam$d2110) <- "Patch/ban spraying"
#C1308 = FACT, replace label: "Bioherbicide"
var_label(HouseholdVietnam$d2111) <- "Bioherbicide"
#C1309 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d2112 <- str_extract(HouseholdVietnam$d21, "12")
HouseholdVietnam$d2112 <- ifelse(HouseholdVietnam$d21 != '' & is.na(HouseholdVietnam$d2112),"0",HouseholdVietnam$d2112)
HouseholdVietnam$d2112[HouseholdVietnam$d2112 == '12'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d2112 , .after = d2111)
#replace label: "Mowing / slashing"
var_label(HouseholdVietnam$d2112) <- "Mowing / slashing"
#C1310 = FACT non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d2113 <- str_extract(HouseholdVietnam$d21, "13")
HouseholdVietnam$d2113 <- ifelse(HouseholdVietnam$d21 != '' & is.na(HouseholdVietnam$d2113),"0",HouseholdVietnam$d2113)
HouseholdVietnam$d2113[HouseholdVietnam$d2113 == '13'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d2113 , .after = d2112)
#replace label: "Grazing"
var_label(HouseholdVietnam$d2113) <- "Grazing"
#C1311 = FACT, non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d2114 <- str_extract(HouseholdVietnam$d21, "14")
HouseholdVietnam$d2114 <- ifelse(HouseholdVietnam$d21 != '' & is.na(HouseholdVietnam$d2114),"0",HouseholdVietnam$d2114)
HouseholdVietnam$d2114[HouseholdVietnam$d2114 == '14'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d2114 , .after = d2113)
#replace label: "Post harvest weed seed destruction in field"
var_label(HouseholdVietnam$d2114) <- "Post harvest weed seed destruction in field"
#C1312 = FACT,  non-existing column, we create it and move it at the right place:
#Extract factor level values, write "yes" and "no" (1 and 0)
HouseholdVietnam$d2199 <- str_extract(HouseholdVietnam$d21, "99")
HouseholdVietnam$d2199 <- ifelse(HouseholdVietnam$d21 != '' & is.na(HouseholdVietnam$d2199),"0",HouseholdVietnam$d2199)
HouseholdVietnam$d2199[HouseholdVietnam$d2199 == '99'] <- '1'
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d2199 , .after = d2114)
#replace label: "Any other methods"
var_label(HouseholdVietnam$d2199) <- "Any other methods"
#C1313 = FACT, (table of correspondence)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1314 = OK, (empty)
#C1315 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1316 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_221) <- "Lowland crop n1"
#C1317 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_222) <- "Lowland crop n2"
#C1318 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_223) <- "Lowland crop n3"
#C1319 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_224) <- "Lowland crop n4"
#C1320 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_225) <- "Lowland crop n5"
#C1321 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_226) <- "Upland crop n1"
#C1322 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_227) <- "Upland crop n2"
#C1323 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_228) <- "Upland crop n3"
#C1324 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_229) <- "Upland crop n4"
#C1325 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_2210) <- "Upland crop n5"
#C1326 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_2211) <- "Upland crop n6"
#C1327 = FACT, (table of correspondence)
#C1328 = OK, (empty)
#C1329 = FACT, (table of correspondence, see crop corresponence)
#C1330 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_321) <- "Lowland crop n1"
#C1331 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_322) <- "Lowland crop n2"
#C1332 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_323) <- "Lowland crop n3"
#C1333 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_324) <- "Lowland crop n4"
#C1334 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_325) <- "Lowland crop n5"
#C1335 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_326) <- "Upland crop n1"
#C1336 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_327) <- "Upland crop n2"
#C1337 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_328) <- "Upland crop n3"
#C1338 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_329) <- "Upland crop n4"
#C1339 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_3210) <- "Upland crop n5"
#C1340 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_3211) <- "Upland crop n6"
#C1341 = FACT, (table of correspondence)
#C1342 = OK, (empty),
#C1343 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1344 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_421) <- "Lowland crop n1"
#C1345 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_422) <- "Lowland crop n2"
#C1346 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_423) <- "Lowland crop n3"
#C1347 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_424) <- "Lowland crop n4"
#C1348 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_425) <- "Lowland crop n5"
#C1349 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_426) <- "Upland crop n1"
#C1350 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_427) <- "Upland crop n2"
#C1351 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_428) <- "Upland crop n3"
#C1352 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_429) <- "Upland crop n4"
#C1353 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_4210) <- "Upland crop n5"
#C1354 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_4211) <- "Upland crop n6"
#C1355 = FACT, (empty), (table of correspondence)
#C1356 = OK, (empty)
#C1357 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1358 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_521) <- "Lowland crop n1"
#C1359 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_522) <- "Lowland crop n2"
#C1360 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_523) <- "Lowland crop n3"
#C1361 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_524) <- "Lowland crop n4"
#C1362 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_525) <- "Lowland crop n5"
#C1363 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_526) <- "Upland crop n1"
#C1364 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_527) <- "Upland crop n2"
#C1365 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_528) <- "Upland crop n3"
#C1366 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_529) <- "Upland crop n4"
#C1367 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_5210) <- "Upland crop n5"
#C1368 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_5211) <- "Upland crop n6"
#C1369 = FACT, (empty), (table of correspondence)
#C1370 = OK, (empty)
#C1371 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1372 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_621) <- "Lowland crop n1"
#C1373 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_622) <- "Lowland crop n2"
#C1374 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_623) <- "Lowland crop n3"
#C1375 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_624) <- "Lowland crop n4"
#C1376 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_625) <- "Lowland crop n5"
#C1377 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_626) <- "Upland crop n1"
#C1378 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_627) <- "Upland crop n2"
#C1379 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_628) <- "Upland crop n3"
#C1380 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_629) <- "Upland crop n4"
#C1381 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_6210) <- "Upland crop n5"
#C1382 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_6211) <- "Upland crop n6"
#C1383 = FACT, (table of correspondence)
#C1384 = OK, (empty)
#C1385 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1386 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_721) <- "Lowland crop n1"
#C1387 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_722) <- "Lowland crop n2"
#C1388 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_723) <- "Lowland crop n3"
#C1389 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_724) <- "Lowland crop n4"
#C1390 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_725) <- "Lowland crop n5"
#C1391 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_726) <- "Upland crop n1"
#C1392 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_727) <- "Upland crop n2"
#C1393 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_728) <- "Upland crop n3"
#C1394 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_729) <- "Upland crop n4"
#C1395 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_7210) <- "Upland crop n5"
#C1396 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_7211) <- "Upland crop n6"
#C1397 = FACT, (table of correspondence)
#C1398 = OK, (empty)
#C1399 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1400 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_821) <- "Lowland crop n1"
#C1401 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_822) <- "Lowland crop n2"
#C1402 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_823) <- "Lowland crop n3"
#C1403 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_824) <- "Lowland crop n4"
#C1404 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_825) <- "Lowland crop n5"
#C1405 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_826) <- "Upland crop n1"
#C1406 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_827) <- "Upland crop n2"
#C1407 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_828) <- "Upland crop n3"
#C1408 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_829) <- "Upland crop n4"
#C1409 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_8210) <- "Upland crop n5"
#C1410 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_8211) <- "Upland crop n6"
#C1411 = FACT, (table of correspondence)
#C1412 = OK, (empty)
#C1413 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1414 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_921) <- "Lowland crop n1"
#C1415 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_922) <- "Lowland crop n2"
#C1416 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_923) <- "Lowland crop n3"
#C1417 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_924) <- "Lowland crop n4"
#C1418 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_925) <- "Lowland crop n5"
#C1419 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_926) <- "Upland crop n1"
#C1420 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_927) <- "Upland crop n2"
#C1421 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_928) <- "Upland crop n3"
#C1422 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_929) <- "Upland crop n4"
#C1423 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_9210) <- "Upland crop n5"
#C1424 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_9211) <- "Upland crop n6"
#C1425 = FACT, (table of correspondence)
#C1426 = OK, (empty)
#C1427 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1428 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_1021) <- "Lowland crop n1"
#C1429 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_1022) <- "Lowland crop n2"
#C1430 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_1023) <- "Lowland crop n3"
#C1431 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_1024) <- "Lowland crop n4"
#C1432 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_1025) <- "Lowland crop n5"
#C1433 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_1026) <- "Upland crop n1"
#C1434 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_1027) <- "Upland crop n2"
#C1435 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_1028) <- "Upland crop n3"
#C1436 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_1029) <- "Upland crop n4"
#C1437 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_10210) <- "Upland crop n5"
#C1438 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_10211) <- "Upland crop n6"
#C1439 = FACT, (table of correspondence)
#C1440 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1441 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1442 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_1121) <- "Lowland crop n1"
#C1443 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_1122) <- "Lowland crop n2"
#C1444 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_1123) <- "Lowland crop n3"
#C1445 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_1124) <- "Lowland crop n4"
#C1446 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_1125) <- "Lowland crop n5"
#C1447 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_1126) <- "Upland crop n1"
#C1448 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_1127) <- "Upland crop n2"
#C1449 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_1128) <- "Upland crop n3"
#C1450 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_1129) <- "Upland crop n4"
#C1451 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_11210) <- "Upland crop n5"
#C1452 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_11211) <- "Upland crop n6"
#C1453 = FACT, (table of correspondence)
#C1454 = OK, (empty)
#C1455 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d21_122. for which crop(s) do you use crop Mowing / slashing"
var_label(HouseholdVietnam$d21_122_a) <- "d21_122. for which crop(s) do you use crop Mowing / slashing"
#C1456 = FACT, remove "a" from column name, replace label: "Lowland crop n1"
colnames(HouseholdVietnam)[1456] <- 'd21_1221'
var_label(HouseholdVietnam$d21_1221) <- "Lowland crop n1"
#C1457 = FACT, remove "a" from column name, replace label: "Lowland crop n2"
colnames(HouseholdVietnam)[1457] <- 'd21_1222'
var_label(HouseholdVietnam$d21_1222) <- "Lowland crop n2"
#C1458 = FACT, remove "a" from column name, replace label: "Lowland crop n3"
colnames(HouseholdVietnam)[1458] <- 'd21_1223'
var_label(HouseholdVietnam$d21_1223) <- "Lowland crop n3"
#C1459 = FACT, remove "a" from column name, replace label: "Lowland crop n4"
colnames(HouseholdVietnam)[1459] <- 'd21_1224'
var_label(HouseholdVietnam$d21_1224) <- "Lowland crop n4"
#C1460 = FACT, remove "a" from column name, replace label: "Lowland crop n5"
colnames(HouseholdVietnam)[1460] <- 'd21_1225'
var_label(HouseholdVietnam$d21_1225) <- "Lowland crop n5"
#C1461 = FACT, remove "a" from column name, replace label: "Upland crop n1"
colnames(HouseholdVietnam)[1461] <- 'd21_1226'
var_label(HouseholdVietnam$d21_1226) <- "Upland crop n1"
#C1462 = FACT, remove "a" from column name, replace label: "Upland crop n2"
colnames(HouseholdVietnam)[1462] <- 'd21_1227'
var_label(HouseholdVietnam$d21_1227) <- "Upland crop n2"
#C1463 = FACT, remove "a" from column name, replace label: "Upland crop n3"
colnames(HouseholdVietnam)[1463] <- 'd21_1228'
var_label(HouseholdVietnam$d21_1228) <- "Upland crop n3"
#C1464 = FACT, remove "a" from column name, replace label: "Upland crop n4"
colnames(HouseholdVietnam)[1464] <- 'd21_1229'
var_label(HouseholdVietnam$d21_1229) <- "Upland crop n4"
#C1465 = FACT, remove "a" from column name, replace label: "Upland crop n5"
colnames(HouseholdVietnam)[1465] <- 'd21_12210'
var_label(HouseholdVietnam$d21_12210) <- "Upland crop n5"
#C1466 = FACT, remove "a" from column name, replace label: "Upland crop n6"
colnames(HouseholdVietnam)[1466] <- 'd21_12211'
var_label(HouseholdVietnam$d21_12211) <- "Upland crop n6"
#C1467 = FACT, (table of correspondence), replace label: "d21_123. what was your main motivation to implement crop Mowing / slashing"
var_label(HouseholdVietnam$d21_123_a) <- "d21_123. what was your main motivation to implement crop Mowing / slashing"
#C1468 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1469 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1470 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_1321) <- "Lowland crop n1"
#C1471 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_1322) <- "Lowland crop n2"
#C1472 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_1323) <- "Lowland crop n3"
#C1473 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_1324) <- "Lowland crop n4"
#C1474 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_1325) <- "Lowland crop n5"
#C1475 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_1326) <- "Upland crop n1"
#C1476 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_1327) <- "Upland crop n2"
#C1477 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_1328) <- "Upland crop n3"
#C1478 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_1329) <- "Upland crop n4"
#C1479 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_13210) <- "Upland crop n5"
#C1480 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_13211) <- "Upland crop n6"
#C1481 = FACT, (table of correspondence)
#C1482 = OK, (empty)
#C1483 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1484 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_1421) <- "Lowland crop n1"
#C1485 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_1422) <- "Lowland crop n2"
#C1486 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_1423) <- "Lowland crop n3"
#C1487 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_1424) <- "Lowland crop n4"
#C1488 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_1425) <- "Lowland crop n5"
#C1489 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_1426) <- "Upland crop n1"
#C1490 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_1427) <- "Upland crop n2"
#C1491 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_1428) <- "Upland crop n3"
#C1492 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_1429) <- "Upland crop n4"
#C1493 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_14210) <- "Upland crop n5"
#C1494 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_14211) <- "Upland crop n6"
#C1495 = FACT, (table of correspondence)
#C1496 = OK, (empty)
#C1497 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d21_992. for which crop(s) do you use crop d21_oth?"
var_label(HouseholdVietnam$d21_992) <- "d21_992. for which crop(s) do you use crop d21_oth?"
#C1498 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d21_9921) <- "Lowland crop n1"
#C1499 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d21_9922) <- "Lowland crop n2"
#C1500 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d21_9923) <- "Lowland crop n3"
#C1501 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d21_9924) <- "Lowland crop n4"
#C1502 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d21_9925) <- "Lowland crop n5"
#C1503 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d21_9926) <- "Upland crop n1"
#C1504 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d21_9927) <- "Upland crop n2"
#C1505 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d21_9928) <- "Upland crop n3"
#C1506 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d21_9929) <- "Upland crop n4"
#C1507 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d21_99210) <- "Upland crop n5"
#C1508 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d21_99211) <- "Upland crop n6"
#C1509 = FACT, (table of correspondence), replace label: "d21_993. what was your main motivation to implement rop d21_oth practice?"
var_label(HouseholdVietnam$d21_993) <- "d21_993. what was your main motivation to implement rop d21_oth practice?"
#C1510 = OK, (empty)
#C1511 = FACT, (table of correspondence), (see options below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1512 = FACT, (table of correspondence), (see options below)
#Synthetic herbicide without mechanical weeding
#Frequent mechanical weeding (more than three times per year) without synthetic herbicide
#Mixed management using herbicide and mechanical weeding    
#Do not know  
#C1513 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1514 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1515 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1516 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see option below)
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
#C1517 = FACT, replace label: "Crop rotation / intercropping"
var_label(HouseholdVietnam$d271) <- "Crop rotation / intercropping"
#C1518 = FACT, replace label: "Flower strips"
var_label(HouseholdVietnam$d272) <- "Flower strips"
#C1519 = FACT, replace label: "Hedgerows"
var_label(HouseholdVietnam$d273) <- "Hedgerows"
#C1520 = FACT, replace label: "Soil health maintenance/improvement"
var_label(HouseholdVietnam$d274) <- "Soil health maintenance/improvement"
#C1521 = FACT, replace label: "Sanitation practices (removal of damaged/infected plants and fruits)"
var_label(HouseholdVietnam$d275) <- "Sanitation practices (removal of damaged/infected plants and fruits)"
#C1522 = FACT, replace label: "Planting date"
var_label(HouseholdVietnam$d276) <- "Planting date"
#C1523 = FACT, replace label: "Water and nutrient management"
var_label(HouseholdVietnam$d277) <- "Water and nutrient management"
#C1524 = FACT, replace label: "Cultivar choice (tolerant/resistant) / cultivar mixture"
var_label(HouseholdVietnam$d278) <- "Cultivar choice (tolerant/resistant) / cultivar mixture"
#C1525 = FACT, replace label: "Biopesticide / organic pesticide"
var_label(HouseholdVietnam$d279) <- "Biopesticide / organic pesticide"
#C1526 = FACT, replace label: "Commercial biological control agents (BCAs)"
var_label(HouseholdVietnam$d2710) <- "Commercial biological control agents (BCAs)"
#C1527 = FACT, replace label: "Home-made efficient microorganism (EM)"
var_label(HouseholdVietnam$d2711) <- "Home-made efficient microorganism (EM)"
#C1528 = FACT, replace label: "Commercial efficient microorganism (EM)"
var_label(HouseholdVietnam$d2712) <- "Commercial efficient microorganism (EM)"
#C1529 = FACT, replace label: "Pheromone traps"
var_label(HouseholdVietnam$d2713) <- "Pheromone traps"
#C1530 = FACT, replace label: "Protein baits"
var_label(HouseholdVietnam$d2714) <- "Protein baits"
#C1531 = FACT, replace label: "Any other methods"
var_label(HouseholdVietnam$d2799) <- "Any other methods"
#C1532 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1533 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1534 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_111) <- "Lowland crop n1"
#C1535 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_112) <- "Lowland crop n2"
#C1536 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_113) <- "Lowland crop n3"
#C1537 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_114) <- "Lowland crop n4"
#C1538 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_115) <- "Lowland crop n5"
#C1539 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_116) <- "Upland crop n1"
#C1540 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_117) <- "Upland crop n2"
#C1541 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_118) <- "Upland crop n3"
#C1542 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_119) <- "Upland crop n4"
#C1543 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_1110) <- "Upland crop n5"
#C1544 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_1111) <- "Upland crop n6"
#C1545 = FACT, (table of correspondence)
#C1546 = OK, (empty)
#C1547 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1548 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_211) <- "Lowland crop n1"
#C1549 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_212) <- "Lowland crop n2"
#C1550 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_213) <- "Lowland crop n3"
#C1551 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_214) <- "Lowland crop n4"
#C1552 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_215) <- "Lowland crop n5"
#C1553 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_216) <- "Upland crop n1"
#C1554 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_217) <- "Upland crop n2"
#C1555 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_218) <- "Upland crop n3"
#C1556 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_219) <- "Upland crop n4"
#C1557 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_2110) <- "Upland crop n5"
#C1558 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_2111) <- "Upland crop n6"
#C1559 = FACT, (empty), (table of correspondence)
#C1560 = OK, (empty)
#C1561 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1562 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_311) <- "Lowland crop n1"
#C1563 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_312) <- "Lowland crop n2"
#C1564 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_313) <- "Lowland crop n3"
#C1565 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_314) <- "Lowland crop n4"
#C1566 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_315) <- "Lowland crop n5"
#C1567 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_316) <- "Upland crop n1"
#C1568 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_317) <- "Upland crop n2"
#C1569 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_318) <- "Upland crop n3"
#C1570 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_319) <- "Upland crop n4"
#C1571 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_3110) <- "Upland crop n5"
#C1572 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_3111) <- "Upland crop n6"
#C1573 = FACT, (empty), (table of correspondence)
#C1574 = OK, (empty)
#C1575 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1576 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_411) <- "Lowland crop n1"
#C1577 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_412) <- "Lowland crop n2"
#C1578 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_413) <- "Lowland crop n3"
#C1579 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_414) <- "Lowland crop n4"
#C1580 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_415) <- "Lowland crop n5"
#C1581 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_416) <- "Upland crop n1"
#C1582 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_417) <- "Upland crop n2"
#C1583 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_418) <- "Upland crop n3"
#C1584 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_419) <- "Upland crop n4"
#C1585 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_4110) <- "Upland crop n5"
#C1586 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_4111) <- "Upland crop n6"
#C1587 = FACT, (table of correspondence)
#C1588 = OK, (empty)
#C1589 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1590 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_511) <- "Lowland crop n1"
#C1591 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_512) <- "Lowland crop n2"
#C1592 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_513) <- "Lowland crop n3"
#C1593 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_514) <- "Lowland crop n4"
#C1594 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_515) <- "Lowland crop n5"
#C1595 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_516) <- "Upland crop n1"
#C1596 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_517) <- "Upland crop n2"
#C1597 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_518) <- "Upland crop n3"
#C1598 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_519) <- "Upland crop n4"
#C1599 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_5110) <- "Upland crop n5"
#C1600 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_5111) <- "Upland crop n6"
#C1601 = FACT, (table of correspondence)
#C1602 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1603 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1604 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_611) <- "Lowland crop n1"
#C1605 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_612) <- "Lowland crop n2"
#C1606 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_613) <- "Lowland crop n3"
#C1607 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_614) <- "Lowland crop n4"
#C1608 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_615) <- "Lowland crop n5"
#C1609 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_616) <- "Upland crop n1"
#C1610 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_617) <- "Upland crop n2"
#C1611 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_618) <- "Upland crop n3"
#C1612 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_619) <- "Upland crop n4"
#C1613 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_6110) <- "Upland crop n5"
#C1614 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_6111) <- "Upland crop n6"
#C1615 = FACT, (table of correspondence)
#C1616 = OK, (empty)
#C1617 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1618 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_711) <- "Lowland crop n1"
#C1619 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_712) <- "Lowland crop n2"
#C1620 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_713) <- "Lowland crop n3"
#C1621 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_714) <- "Lowland crop n4"
#C1622 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_715) <- "Lowland crop n5"
#C1623 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_716) <- "Upland crop n1"
#C1624 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_717) <- "Upland crop n2"
#C1625 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_718) <- "Upland crop n3"
#C1626 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_719) <- "Upland crop n4"
#C1627 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_7110) <- "Upland crop n5"
#C1628 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_7111) <- "Upland crop n6"
#C1629 = FACT, (table of correspondence)
#C1630 = OK, (empty)
#C1631 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1632 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_811) <- "Lowland crop n1"
#C1633 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_812) <- "Lowland crop n2"
#C1634 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_813) <- "Lowland crop n3"
#C1635 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_814) <- "Lowland crop n4"
#C1636 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_815) <- "Lowland crop n5"
#C1637 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_816) <- "Upland crop n1"
#C1638 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_817) <- "Upland crop n2"
#C1639 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_818) <- "Upland crop n3"
#C1640 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_819) <- "Upland crop n4"
#C1641 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_8110) <- "Upland crop n5"
#C1642 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_8111) <- "Upland crop n6"
#C1643 = FACT, (table of correspondence)
#C1644 = OK, (empty)
#C1645 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1646 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_911) <- "Lowland crop n1"
#C1647 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_912) <- "Lowland crop n2"
#C1648 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_913) <- "Lowland crop n3"
#C1649 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_914) <- "Lowland crop n4"
#C1650 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_915) <- "Lowland crop n5"
#C1651 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_916) <- "Upland crop n1"
#C1652 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_917) <- "Upland crop n2"
#C1653 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_918) <- "Upland crop n3"
#C1654 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_919) <- "Upland crop n4"
#C1655 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_9110) <- "Upland crop n5"
#C1656 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_9111) <- "Upland crop n6"
#C1657 = FACT, (table of correspondence)
#C1658 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1659 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1660 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_1011) <- "Lowland crop n1"
#C1661 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_1012) <- "Lowland crop n2"
#C1662 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_1013) <- "Lowland crop n3"
#C1663 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_1014) <- "Lowland crop n4"
#C1664 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_1015) <- "Lowland crop n5"
#C1665 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_1016) <- "Upland crop n1"
#C1666 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_1017) <- "Upland crop n2"
#C1667 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_1018) <- "Upland crop n3"
#C1668 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_1019) <- "Upland crop n4"
#C1669 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_10110) <- "Upland crop n5"
#C1670 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_10111) <- "Upland crop n6"
#C1671 = FACT, (table of correspondence)
#C1672 = OK, (empty)
#C1673 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns), replace label: "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
var_label(HouseholdVietnam$d27_111_a) <- "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
#C1674 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_1111_a) <- "Lowland crop n1"
#C1675 = FACT, (empty), remove a "a" from colname, replace label: "Lowland crop n2"
colnames(HouseholdVietnam)[1675] <- 'd27_1112'
var_label(HouseholdVietnam$d27_1112) <- "Lowland crop n2"
#C1676 = FACT, (empty), remove a "a" from colname, replace label: "Lowland crop n3"
colnames(HouseholdVietnam)[1676] <- 'd27_1113'
var_label(HouseholdVietnam$d27_1113) <- "Lowland crop n3"
#C1677 = FACT, (empty), remove a "a" from colname, replace label: "Lowland crop n4"
colnames(HouseholdVietnam)[1677] <- 'd27_1114'
var_label(HouseholdVietnam$d27_1114) <- "Lowland crop n4"
#C1678 = FACT, (empty), remove a "a" from colname, replace label: "Lowland crop n5"
colnames(HouseholdVietnam)[1678] <- 'd27_1115'
var_label(HouseholdVietnam$d27_1115) <- "Lowland crop n5"
#C1679 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n1"
colnames(HouseholdVietnam)[1679] <- 'd27_1116'
var_label(HouseholdVietnam$d27_1116) <- "Upland crop n1"
#C1680 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n2"
colnames(HouseholdVietnam)[1680] <- 'd27_1117'
var_label(HouseholdVietnam$d27_1117) <- "Upland crop n2"
#C1681 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n3"
colnames(HouseholdVietnam)[1681] <- 'd27_1118'
var_label(HouseholdVietnam$d27_1118) <- "Upland crop n3"
#C1682 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n4"
colnames(HouseholdVietnam)[1682] <- 'd27_1119'
var_label(HouseholdVietnam$d27_1119) <- "Upland crop n4"
#C1683 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n5"
colnames(HouseholdVietnam)[1683] <- 'd27_11110'
var_label(HouseholdVietnam$d27_11110) <- "Upland crop n5"
#C1684 = FACT, (empty), remove a "a" from colname, replace label: "Upland crop n6"
colnames(HouseholdVietnam)[1684] <- 'd27_11111'
var_label(HouseholdVietnam$d27_11111) <- "Upland crop n6"
#C1685 = FACT, (empty), (table of correspondence), replace label: "d27_112. If yes, what was your main motivation to implement this practice?"
var_label(HouseholdVietnam$d27_112_a)  <- "d27_112. If yes, what was your main motivation to implement this practice?"
#C1686 = OK, (empty)
#C1687 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1688 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_1211) <- "Lowland crop n1"
#C1689 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_1212) <- "Lowland crop n2"
#C1690 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_1213) <- "Lowland crop n3"
#C1691 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_1214) <- "Lowland crop n4"
#C1692 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_1215) <- "Lowland crop n5"
#C1693 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_1216) <- "Upland crop n1"
#C1694 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_1217) <- "Upland crop n2"
#C1695 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_1218) <- "Upland crop n3"
#C1696 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_1219) <- "Upland crop n4"
#C1697 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_12110) <- "Upland crop n5"
#C1698 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_12111) <- "Upland crop n6"
#C1699 = FACT, (empty), (table of correspondence)
#C1700 = OK, (empty)
#C1701 = FACT, (correspond to crops numbers)
#C1702 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_1311) <- "Lowland crop n1"
#C1703 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_1312) <- "Lowland crop n2"
#C1704 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_1313) <- "Lowland crop n3"
#C1705 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_1314) <- "Lowland crop n4"
#C1706 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_1315) <- "Lowland crop n5"
#C1707 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_1316) <- "Upland crop n1"
#C1708 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_1317) <- "Upland crop n2"
#C1709 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_1318) <- "Upland crop n3"
#C1710 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_1319) <- "Upland crop n4"
#C1711 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_13110) <- "Upland crop n5"
#C1712 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_13111) <- "Upland crop n6"
#C1713 = FACT, (table of correspondence)
#C1714 = OK, (empty)
#C1715 = FACT, (correspond to crops numbers)
#C1716 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_1411) <- "Lowland crop n1"
#C1717 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_1412) <- "Lowland crop n2"
#C1718 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_1413) <- "Lowland crop n3"
#C1719 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_1414) <- "Lowland crop n4"
#C1720 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_1415) <- "Lowland crop n5"
#C1721 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_1416) <- "Upland crop n1"
#C1722 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_1417) <- "Upland crop n2"
#C1723 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_1418) <- "Upland crop n3"
#C1724 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_1419) <- "Upland crop n4"
#C1725 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_14110) <- "Upland crop n5"
#C1726 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_14111) <- "Upland crop n6"
#C1727 = FACT, (table of correspondence)
#C1728 = OK, (empty)
#C1729 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d27_991. for which crop(s) do you use d27_oth?"
var_label(HouseholdVietnam$d27_991) <- "d27_991. for which crop(s) do you use d27_oth?"
#C1730 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d27_9911) <- "Lowland crop n1"
#C1731 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d27_9912) <- "Lowland crop n2"
#C1732 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d27_9913) <- "Lowland crop n3"
#C1733 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d27_9914) <- "Lowland crop n4"
#C1734 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d27_9915) <- "Lowland crop n5"
#C1735 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d27_9916) <- "Upland crop n1"
#C1736 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d27_9917) <- "Upland crop n2"
#C1737 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d27_9918) <- "Upland crop n3"
#C1738 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d27_9919) <- "Upland crop n4"
#C1739 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d27_99110) <- "Upland crop n5"
#C1740 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d27_99111) <- "Upland crop n6"
#C1741 = FACT, (table of correspondence)
#replace label: "d27_992. if yes, what was your main motivation to implement d27_oth practice"
var_label(HouseholdVietnam$d27_992) <- "d27_992. if yes, what was your main motivation to implement d27_oth practice"
#C1742 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1743 = FACT, (table of correspondence), (see answers below)
#I do not know them
#They are too costly
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1744 = FACT, (table of correspondence), (see answers below)
#Synthetic insecticide and fungicide are used regularly and no other system is used
#Mixed use of synthetic and biological/natural pesticides
#Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used
#Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide
#Do not know
#C1745 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1746 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1747 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d30_21) <- "Lowland crop n1"
#C1748 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d30_22) <- "Lowland crop n2"
#C1749 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d30_23) <- "Lowland crop n3"
#C1750 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d30_24) <- "Lowland crop n4"
#C1751 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d30_25) <- "Lowland crop n5"
#C1752 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d30_26) <- "Upland crop n1"
#C1753 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d30_27) <- "Upland crop n2"
#C1754 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d30_28) <- "Upland crop n3"
#C1755 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d30_29) <- "Upland crop n4"
#C1756 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d30_210) <- "Upland crop n5"
#C1757 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d30_211) <- "Upland crop n6"
#C1758 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1759 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1760 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d30_41) <- "Lowland crop n1"
#C1761 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d30_42) <- "Lowland crop n2"
#C1762 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d30_43) <- "Lowland crop n3"
#C1763 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d30_44) <- "Lowland crop n4"
#C1764 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d30_45) <- "Lowland crop n5"
#C1765 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d30_46) <- "Upland crop n1"
#C1766 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d30_47) <- "Upland crop n2"
#C1767 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d30_48) <- "Upland crop n3"
#C1768 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d30_49) <- "Upland crop n4"
#C1769 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d30_410) <- "Upland crop n5"
#C1770 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d30_411) <- "Upland crop n6"
#C1771 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1772 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1773 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d30_61) <- "Lowland crop n1"
#C1774 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d30_62) <- "Lowland crop n2"
#C1775 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d30_63) <- "Lowland crop n3"
#C1776 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d30_64) <- "Lowland crop n4"
#C1777 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d30_65) <- "Lowland crop n5"
#C1778 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d30_66) <- "Upland crop n1"
#C1779 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d30_67) <- "Upland crop n2"
#C1780 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d30_68) <- "Upland crop n3"
#C1781 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d30_69) <- "Upland crop n4"
#C1782 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d30_610) <- "Upland crop n5"
#C1783 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d30_611) <- "Upland crop n6"
#C1784 = FACT, (table of correspondence), (bin)
#C1785 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1786 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdVietnam$d32_11) <- "Lowland crop n1"
#C1787 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdVietnam$d32_12) <- "Lowland crop n2"
#C1788 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdVietnam$d32_13) <- "Lowland crop n3"
#C1789 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdVietnam$d32_14) <- "Lowland crop n4"
#C1790 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdVietnam$d32_15) <- "Lowland crop n5"
#C1791 = FACT, replace label: "Upland crop n1"
var_label(HouseholdVietnam$d32_16) <- "Upland crop n1"
#C1792 = FACT, replace label: "Upland crop n2"
var_label(HouseholdVietnam$d32_17) <- "Upland crop n2"
#C1793 = FACT, replace label: "Upland crop n3"
var_label(HouseholdVietnam$d32_18) <- "Upland crop n3"
#C1794 = FACT, replace label: "Upland crop n4"
var_label(HouseholdVietnam$d32_19) <- "Upland crop n4"
#C1795 = FACT, replace label: "Upland crop n5"
var_label(HouseholdVietnam$d32_110) <- "Upland crop n5"
#C1796 = FACT, replace label: "Upland crop n6"
var_label(HouseholdVietnam$d32_111) <- "Upland crop n6"
#C1797 = FACT, (table of correspondence), (see answers below)
#From the village seller
#From the cooperative 
#From a trader in town 
#From family & friends
#Do not know
#Other
#C1798 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1799 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#Do not have
#Family members 
#Hired people 
#Mutual help
#C1800 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_10) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_10 , .after = d33_1)
#C1801 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_11) <- "Family members"
#C1802 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_12) <- "Hired people"
#C1803 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_13) <- "Mutual help"
#C1804 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1805 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_20) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_20 , .after = d33_2)
#C1806 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_21) <- "Family members"
#C1807 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_22) <- "Hired people"
#C1808 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_23) <- "Mutual help"
#C1809 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1810 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_30) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_30 , .after = d33_3)
#C1811 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_31) <- "Family members"
#C1812 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_32) <- "Hired people"
#C1813 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_33) <- "Mutual help"
#C1814 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1815 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_40) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_40 , .after = d33_4)
#C1816 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_41) <- "Family members"
#C1817 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_42) <- "Hired people"
#C1818 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_43) <- "Mutual help"
#C1819 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1820 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_50) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_50 , .after = d33_5)
#C1821 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_51) <- "Family members"
#C1822 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_52) <- "Hired people"
#C1823 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_53) <- "Mutual help"
#C1824 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1825 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_60) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_60 , .after = d33_6)
#C1826 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_61) <- "Family members"
#C1827 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_62) <- "Hired people"
#C1828 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_63) <- "Mutual help"
#C1829 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1830 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_70) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_70 , .after = d33_7)
#C1831 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_71) <- "Family members"
#C1832 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_72) <- "Hired people"
#C1833 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_73) <- "Mutual help"
#C1834 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1835 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_80) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_80 , .after = d33_8)
#C1836 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_81) <- "Family members"
#C1837 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_82) <- "Hired people"
#C1838 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_83) <- "Mutual help"
#C1839 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1840 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_90) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_90 , .after = d33_9)
#C1841 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_91) <- "Family members"
#C1842 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_92) <- "Hired people"
#C1843 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_93) <- "Mutual help"
#C1844 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#replace label: "d33_10. Transportation"
var_label(HouseholdVietnam$d33_10_a) <- "d33_10. Transportation"
#C1845 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_100) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_100 , .after = d33_10_a)
#C1846 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_101) <- "Family members"
#C1847 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_102) <- "Hired people"
#C1848 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_103) <- "Mutual help"
#C1849 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#replace label: "d33_11. Post-harvest processing"
var_label(HouseholdVietnam$d33_11_a) <- "d33_11. Post-harvest processing"
#C1850 = FACT, replace label: "Do not have"
var_label(HouseholdVietnam$d33_110) <- "Do not have"
#Move the column at the right place
HouseholdVietnam <- HouseholdVietnam %>% relocate(d33_110 , .after = d33_11_a)
#C1851 = FACT, replace label: "Family members"
var_label(HouseholdVietnam$d33_111) <- "Family members"
#C1852 = FACT, replace label: "Hired people"
var_label(HouseholdVietnam$d33_112) <- "Hired people"
#C1853 = FACT, replace label: "Mutual help"
var_label(HouseholdVietnam$d33_113) <- "Mutual help"
#C1854 = OK
#C1855 = OK, (which currency?)
#C1856 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
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
#C1857 = FACT, replace label: "Land preparation"
var_label(HouseholdVietnam$d361) <- "Land preparation"
#C1858 = FACT, replace label: "Sowing"
var_label(HouseholdVietnam$d362) <- "Sowing"
#C1859 = FACT, replace label: "Fertilization"
var_label(HouseholdVietnam$d363) <- "Fertilization"
#C1860 = FACT, replace label: "Weed management"
var_label(HouseholdVietnam$d364) <- "Weed management"
#C1861 = FACT, replace label: "Pest and disease management"
var_label(HouseholdVietnam$d365) <- "Pest and disease management"
#C1862 = FACT, replace label: "Pruning"
var_label(HouseholdVietnam$d366) <- "Pruning"
#C1863 = FACT, replace label: "Water/irrigation management"
var_label(HouseholdVietnam$d367) <- "Water/irrigation management"
#C1864 = FACT, replace label: "Harvest"
var_label(HouseholdVietnam$d368) <- "Harvest"
#C1865 = FACT, replace label: "Transportation"
var_label(HouseholdVietnam$d369) <- "Transportation"
#C1866 = FACT, replace label: "None of above"
var_label(HouseholdVietnam$d360) <- "None of above"
#C1867 = FACT, replace label: "Other"
var_label(HouseholdVietnam$d3699) <- "Other"
#C1868 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)

# # #e.
#C1869 = FACT, (bin)
#C1870 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
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
#C1871 = FACT, (bin)
#C1872 = FACT, (bin)
#C1873 = FACT, (bin)
#C1874 = FACT, (bin)
#C1875 = FACT, (bin)
#C1876 = FACT, (bin)
#C1877 = FACT, (bin)
#C1878 = FACT, (bin)
#C1879 = FACT, (bin)
#C1880 = FACT, (bin)
#C1881 = FACT, (bin)
#C1882 = FACT, (bin)
#C1883 = FACT, (bin)
#C1884 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1885 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1886 = OK
#C1887 = OK
#C1888 = OK
#C1889 = OK
#C1890 = OK
#C1891 = OK
#C1892 = OK
#C1893 = OK
#C1894 = OK, (empty)
#C1895 = OK, (empty)
#C1896 = OK
#C1897 = OK
#C1898 = OK
#C1899 = OK
#C1900 = OK
#C1901 = OK
#C1902 = OK, replace label: "e3_98. e2_oth1"
var_label(HouseholdVietnam$e3_98) <- "e3_98. e2_oth1"
#C1903 = OK, replace label: "e3_99. e2_oth2"
var_label(HouseholdVietnam$e3_99) <- "e3_99. e2_oth2"
#C1904 = FACT, (table of correspondence), (see answer below)
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
#C1905 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1906 = FACT, (table of correspondence)
#C1907 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1908 = FACT, (table of correspondence)
#C1909 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1910 = OK, replace label: "e5_a. how many breeds of e4_1text does your household have"
var_label(HouseholdVietnam$e5_a) <- "e5_a. how many breeds of e4_1text does your household have"
#C1911 = FACT, (bin),  replace label: "e5_b. do you have any local breeds of e4_1text"
var_label(HouseholdVietnam$e5_b) <- "e5_b. do you have any local breeds of e4_1text"
#C1912 = OK
#C1913 = FACT, (bin),  replace label: "e5_d. do you cross local breeds with other breeds of e4_1text"
var_label(HouseholdVietnam$e5_d) <- "e5_d. do you cross local breeds with other breeds of e4_1text"
#C1914 = REMOVE
#C1915 = OK, replace label: "e5_1. how many e4_1text died in the past 1 year (3 years)"
var_label(HouseholdVietnam$e5_1) <- "e5_1. how many e4_1text died in the past 1 year (3 years)"
#C1916 = OK, replace label: "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e5_2) <- "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1917 = OK, replace label: "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e5_3) <- "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
#C1918 = OK, replace label: "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
var_label(HouseholdVietnam$e5_4) <- "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
#C1919 = OK, replace label: "e5_41. average selling price/kg for each e4_1text sold (local currency/kg)"
var_label(HouseholdVietnam$e5_41) <- "e5_41. average selling price/kg for each e4_1text sold (local currency/kg)"
#C1920 = OK, replace label: "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e5_5) <- "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
#C1921 = OK, replace label: "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
var_label(HouseholdVietnam$e5_51) <- "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
#C1922 = FACT, (table of correspondence), (see choices below) replace label: "b18_1. what is the main outlet/ buyer for e4_1text?"
var_label(HouseholdVietnam$b18_1) <- "b18_1. what is the main outlet/ buyer for e4_1text?"
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
#C1923 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), (real name)
#C1924 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), (category name)
#C1925 = FACT, (table of correspondence), (see choices below), replace label: "b19_1. what is the proportion of e4_1text that you sell to b18_1"
var_label(HouseholdVietnam$b19_1) <- "b19_1. what is the proportion of e4_1text that you sell to b18_1"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C1926 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#replace label: "b20_1. does  b18_1text provide any of the following?"
var_label(HouseholdVietnam$b20_1) <- "b20_1. does  b18_1text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C1927 = FACT, replace label: "Nothing"
var_label(HouseholdVietnam$b20_10) <- "Nothing"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(b20_10 , .after = b20_1)
#C1928 = FACT, replace label: "Inputs (sold)"
var_label(HouseholdVietnam$b20_11) <- "Inputs (sold)"
#C1929 = FACT, replace label: "Inputs on credit"
var_label(HouseholdVietnam$b20_12) <- "Inputs on credit"
#C1930 = FACT, replace label: "Cash credit"
var_label(HouseholdVietnam$b20_13) <- "Cash credit"
#C1931 = FACT, replace label: "Technical advice/training"
var_label(HouseholdVietnam$b20_14) <- "Technical advice/training"
#C1932 = FACT, replace label: "Market information"
var_label(HouseholdVietnam$b20_15) <- "Market information"
#C1933 = FACT, replace label: "Regular sales"
var_label(HouseholdVietnam$b20_16) <- "Regular sales"
#C1934 = FACT, replace label: "Other"
var_label(HouseholdVietnam$b20_199) <- "Other"
#C1935 = FACT, replace label: "Do not know"
var_label(HouseholdVietnam$b20_188) <- "Do not know"
#C1936 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "b20_1oth. specify other provision from b18_1text"
var_label(HouseholdVietnam$b20_1oth) <- "b20_1oth. specify other provision from b18_1text"
#C1937 = FACT, (see choices below), replace label: "b21_1. do you have a contract with b18_1text?"
var_label(HouseholdVietnam$b21_1) <- "b21_1. do you have a contract with b18_1text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C1938 = OK, replace label: "e6_a. how many breeds of e4_2text does your household have"
var_label(HouseholdVietnam$e6_a) <- "e6_a. how many breeds of e4_2text does your household have"
#C1939 = FACT, (bin),  replace label: "e6_b. do you have any local breeds of e4_2text"
var_label(HouseholdVietnam$e6_b) <- "e6_b. do you have any local breeds of e4_2text"
#C1940 = OK
#C1941 = FACT, (bin),  replace label: "e6_d. do you cross local breeds with other breeds of e4_2text"
var_label(HouseholdVietnam$e6_d) <- "e6_d. do you cross local breeds with other breeds of e4_2text"
#C1942 = REMOVE
#C1943 = OK, replace label: "e6_1. how many e4_2text died in the past 1 year (3 years)"
var_label(HouseholdVietnam$e6_1) <- "e6_1. how many e4_2text died in the past 1 year (3 years)"
#C1944 = OK, replace label: "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e6_2) <- "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1945 = OK, replace label: "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e6_3) <- "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
#C1946 = OK, replace label: "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
var_label(HouseholdVietnam$e6_4) <- "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
#C1947 = OK, replace label: "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
var_label(HouseholdVietnam$e6_41) <- "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
#C1948 = OK, replace label: "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e6_5) <- "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
#C1949 = OK, replace label: "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
var_label(HouseholdVietnam$e6_51) <- "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
#C1950 = FACT, (table of correspondence), (see choices below) replace label: "b18_2. what is the main outlet/ buyer for e4_2text?"
var_label(HouseholdVietnam$b18_2) <- "b18_2. what is the main outlet/ buyer for e4_2text?"
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
#C1951 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), (real name)
#C1952 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), (category name)
#C1953 = FACT, (table of correspondence), (see choices below), replace label: "b19_2. what is the proportion of e4_2text that you sell to b18_2"
var_label(HouseholdVietnam$b19_2) <- "b19_2. what is the proportion of e4_2text that you sell to b18_2"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C1954 = FACT, (see choices below)
#replace label: "b20_2. does  b18_2text provide any of the following?"
var_label(HouseholdVietnam$b20_2) <- "b20_2. does  b18_2text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C1955 = OK, (empty), replace label: "b20_2oth. specify other provision from b18_2text"
var_label(HouseholdVietnam$b20_2oth) <- "b20_2oth. specify other provision from b18_2text"
#C1956 = FACT, (see choices below), replace label: "b21_2. do you have a contract with b18_2text?"
var_label(HouseholdVietnam$b21_2) <- "b21_2. do you have a contract with b18_2text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C1957 = OK, replace label: "e7_a. how many breeds of e4_3text does your household have"
var_label(HouseholdVietnam$e7_a) <- "e7_a. how many breeds of e4_3text does your household have"
#C1958 = FACT, (bin),  replace label: "e7_b. do you have any local breeds of e4_3text"
var_label(HouseholdVietnam$e7_b) <- "e7_b. do you have any local breeds of e4_3text"
#C1959 = OK
#C1960 = FACT, (bin),  replace label: "e7_d. do you cross local breeds with other breeds of e4_3text"
var_label(HouseholdVietnam$e7_d) <- "e7_d. do you cross local breeds with other breeds of e4_3text"
#C1961 = REMOVE
#C1962 = OK, replace label: "e7_1. how many e4_3text died in the past 1 year (3 years)"
var_label(HouseholdVietnam$e7_1) <- "e7_1. how many e4_3text died in the past 1 year (3 years)"
#C1963 = OK, replace label: "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e7_2) <- "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
#C1964 = OK, replace label: "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e7_3) <- "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
#C1965 = OK, replace label: "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
var_label(HouseholdVietnam$e7_4) <- "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
#C1966 = OK, replace label: "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
var_label(HouseholdVietnam$e7_41) <- "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
#C1967 = OK, replace label: "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
var_label(HouseholdVietnam$e7_5) <- "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
#C1968 = OK, replace label: "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
var_label(HouseholdVietnam$e7_51) <- "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
#C1969 = FACT, (bin)
#C1970 = OK, (m2 ?)
#C1971 = OK
#C1972 = FACT, (bin)
#C1973 = OK, (m2 ?)
#C1974 = FACT, (bin)
#C1975 = OK, (m2 ?)
#C1976 = FACT, (bin)
#C1977 = OK, (m2 ?)
#C1978 = FACT, (bin)
#C1979 = FACT, (table of correspondence), (see option below)
#Synthetic fertilizer
#Organic manure 
#Both above
#C1980 = FACT, (bin)
#C1981 = FACT, (bin)
#C1982 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C1983 = FACT, (table of correspondence)
#C1984 = FACT, (bin)
#C1985 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C1986 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e191) <- "Grazing in public area"
#C1987 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e192) <- "Grazing in own pasture area"
#C1988 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e193) <- "Cutting and carry natural grass / vegetables from public area"
#C1989 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e194) <- "Cutting and carry natural grass / vegetables from own area"
#C1990 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e195) <- "Cutting and carry of forage"
#C1991 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e196) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C1992 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e197) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C1993 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e198) <- "Silage"
#C1994 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e199) <- "Kitchen waste"
#C1995 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e1988) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(e1988, .after = e199)
#C1996 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e1999) <- "Other"
#C1997 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C1998 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C1999 = FACT, (table of correspondence)
#C2000 = FACT, (bin)
#C2001 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2002 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e241) <- "Grazing in public area"
#C2003 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e242) <- "Grazing in own pasture area"
#C2004 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e243) <- "Cutting and carry natural grass / vegetables from public area"
#C2005 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e244) <- "Cutting and carry natural grass / vegetables from own area"
#C2006 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e245) <- "Cutting and carry of forage"
#C2007 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e246) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2008 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e247) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2009 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e248) <- "Silage"
#C2010 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e249) <- "Kitchen waste"
#C2011 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e2488) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(e2488, .after = e249)
#C2012 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e2499) <- "Other"
#C2013 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2014 = FACT, (bin)
#C2015 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2016 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdVietnam$e25_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2017 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdVietnam$e25_12) <- "From market: feed / concentrate"
#C2018 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2019 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e25_188) <- "Do not know"
#C2020 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e25_199) <- "Other"
#C2021 = FACT, (bin)
#C2022 = FACT, (Table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2023 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2024 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdVietnam$e270) <- "Nothing"
#C2025 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdVietnam$e271) <- "Traditional treatment"
#C2026 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdVietnam$e272) <- "Chemicals (define)"
#C2027 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2028 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2029 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2030 = OK, (empty)
#C2031 = FACT, (bin)
#C2032 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2033 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdVietnam$e290) <- "I don’t use antibiotics at all"
#C2034 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdVietnam$e291) <- "For treatment diseases only"
#C2035 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdVietnam$e292) <- "For prevention of diseases only"
#C2036 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdVietnam$e293) <- "For growth promotion"
#C2037 = REMOVE, (empty, correspond to nothing in the questionnaire)
#c2038 = FACT, (table of correspondence), (see answers below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2039 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#c2040 = FACT, (table of correspondence), (see answers below)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2041 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2042 = FACT, (table of correspondence), (see answers below)
#Fattening family raising using only local feed stuffs and kitchen waste
#Fattening family raising, but buying feed from market
#Piglet family raising using only local feed stuffs and kitchen waste
#Piglet family raising, but buying feed from market
#Other
#C2043 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2044 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2045 = FACT, (table of correspondence)
#C2046 = FACT, (bin)
#C2047 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2048 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e341) <- "Grazing in public area"
#C2049 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e342) <- "Grazing in own pasture area"
#C2050 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e343) <- "Cutting and carry natural grass / vegetables from public area"
#C2051 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e344) <- "Cutting and carry natural grass / vegetables from own area"
#C2052 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e345) <- "Cutting and carry of forage"
#C2053 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e346) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2054 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e347) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2055 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e348) <- "Silage"
#C2056 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e349) <- "Kitchen waste"
#C2057 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e3488) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(e3488, .after = e349)
#C2058 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e3499) <- "Other"
#C2059 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2060 = FACT, (table of correspondence)
#C2061 = FACT, (bin)
#C2062 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2063 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e381) <- "Grazing in public area"
#C2064 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e382) <- "Grazing in own pasture area"
#C2065 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e383) <- "Cutting and carry natural grass / vegetables from public area"
#C2066 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e384) <- "Cutting and carry natural grass / vegetables from own area"
#C2067 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e385) <- "Cutting and carry of forage"
#C2068 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e386) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2069 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e387) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2070 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e388) <- "Silage"
#C2071 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e389) <- "Kitchen waste"
#C2072 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e3888) <- "Do not know"
#C2073 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e3899) <- "Other"
#C2074 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2075 = FACT, (bin)
#C2076 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2077 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdVietnam$e39_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2078 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdVietnam$e39_12) <- "From market: feed / concentrate"
#C2079 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2080 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e39_188) <- "Do not know"
#C2081 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e39_199) <- "Other"
#C2082 = FACT, (bin)
#C2083 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2084 = FACT, (table of correspondence), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2085 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdVietnam$e410) <- "Nothing"
#C2086 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdVietnam$e411) <- "Traditional treatment"
#C2087 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdVietnam$e412) <- "Chemicals (define)"
#C2088 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2089 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2090 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2091 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2092 = FACT, (bin)
#C2093 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2094 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdVietnam$e430) <- "I don’t use antibiotics at all"
#C2095 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdVietnam$e431) <- "For treatment diseases only"
#C2096 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdVietnam$e432) <- "For prevention of diseases only"
#C2097 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdVietnam$e433) <- "For growth promotion"
#C2098 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2099 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2100 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2101 = FACT, (table of correspondence), (see table below)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2102 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2103 = FACT, (table of correspondence), (see answers below)
#Family raising for consumption
#Family raising but larger scale for selling meat
#Family raising but larger scale for selling chick
#Family raising but larger scale for selling eggs
#Industrial (semi-industrial) broiler chicken 
#Industrial (semi-industrial) layer chicken
#Other
#C2104 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2105 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2106 = FACT, (table of correspondence)
#C2107 = FACT, (bin)
#C2108 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2109 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e481) <- "Grazing in public area"
#C2110 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e482) <- "Grazing in own pasture area"
#C2111 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e483) <- "Cutting and carry natural grass / vegetables from public area"
#C2112 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e484) <- "Cutting and carry natural grass / vegetables from own area"
#C2113 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e485) <- "Cutting and carry of forage"
#C2114 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e486) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2115 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e487) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2116 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e488) <- "Silage"
#C2117 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e489) <- "Kitchen waste"
#C2118 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e4888) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(e4888, .after = e489)
#C2119 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e4899) <- "Other"
#C2120 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2121 = FACT, (table of correspondence)
#C2122 = FACT, (bin)
#C2123 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2124 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdVietnam$e521) <- "Grazing in public area"
#C2125 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdVietnam$e522) <- "Grazing in own pasture area"
#C2126 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdVietnam$e523) <- "Cutting and carry natural grass / vegetables from public area"
#C2127 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdVietnam$e524) <- "Cutting and carry natural grass / vegetables from own area"
#C2128 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdVietnam$e525) <- "Cutting and carry of forage"
#C2129 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdVietnam$e526) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2130 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdVietnam$e527) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2131 = FACT, (bin), replace label: "Silage"
var_label(HouseholdVietnam$e528) <- "Silage"
#C2132 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdVietnam$e529) <- "Kitchen waste"
#C2133 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e5288) <- "Do not know"
#Move this column at this place:
HouseholdVietnam <- HouseholdVietnam %>% relocate(e5288, .after = e529)
#C2134 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e5299) <- "Other"
#C2135 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2136 = FACT, (bin)
#C2137 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2138 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdVietnam$e53_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2139 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdVietnam$e53_12) <- "From market: feed / concentrate"
#C2140 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2141 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$e53_188) <- "Do not know"
#C2142 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$e53_199) <- "Other"
#C2143 = FACT, (bin)
#C2144 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2145 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2146 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdVietnam$e550) <- "Nothing"
#C2147 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdVietnam$e551) <- "Traditional treatment"
#C2148 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdVietnam$e552) <- "Chemicals (define)"
#C2149 = FACT, (bin)
#C2150 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2151 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2152 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2153 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2154 = FACT, (bin)
#C2155 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2156 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdVietnam$e570) <- "I don’t use antibiotics at all"
#C2157 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdVietnam$e571) <- "For treatment diseases only"
#C2158 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdVietnam$e572) <- "For prevention of diseases only"
#C2159 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdVietnam$e573) <- "For growth promotion"
#C2160 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2161 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2162 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2163 = FACT, (empty)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2164 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2165 = OK, (bin)
#C2166 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2167 = FACT, (bin), replace label: "Jan"
var_label(HouseholdVietnam$e58_11) <- "Jan"
#C2168 = FACT, (bin), replace label: "Feb"
var_label(HouseholdVietnam$e58_12) <- "Feb"
#C2169 = FACT, (bin), replace label: "Mar"
var_label(HouseholdVietnam$e58_13) <- "Mar"
#C2170 = FACT, (bin), replace label: "Apr"
var_label(HouseholdVietnam$e58_14) <- "Apr"
#C2171 = FACT, (bin), replace label: "May"
var_label(HouseholdVietnam$e58_15) <- "May"
#C2172 = FACT, (bin), replace label: "Jun"
var_label(HouseholdVietnam$e58_16) <- "Jun"
#C2173 = FACT, (bin), replace label: "Jul"
var_label(HouseholdVietnam$e58_17) <- "Jul"
#C2174 = FACT, (bin), replace label: "Aug"
var_label(HouseholdVietnam$e58_18) <- "Aug"
#C2175 = FACT, (bin), replace label: "Sep"
var_label(HouseholdVietnam$e58_19) <- "Sep"
#C2176 = FACT, (bin), replace label: "Oct"
var_label(HouseholdVietnam$e58_110) <- "Oct"
#C2177 = FACT, (bin), replace label: "Nov"
var_label(HouseholdVietnam$e58_111) <- "Nov"
#C2178 = FACT, (bin), replace label: "Dec"
var_label(HouseholdVietnam$e58_112) <- "Dec"
#C2179 = OK, (bin)
#C2180 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2181 = FACT, (bin), replace label: "Jan"
var_label(HouseholdVietnam$e59_11) <- "Jan"
#C2182 = FACT, (bin), replace label: "Feb"
var_label(HouseholdVietnam$e59_12) <- "Feb"
#C2183 = FACT, (bin), replace label: "Mar"
var_label(HouseholdVietnam$e59_13) <- "Mar"
#C2184 = FACT, (bin), replace label: "Apr"
var_label(HouseholdVietnam$e59_14) <- "Apr"
#C2185 = FACT, (bin), replace label: "May"
var_label(HouseholdVietnam$e59_15) <- "May"
#C2186 = FACT, (bin), replace label: "Jun"
var_label(HouseholdVietnam$e59_16) <- "Jun"
#C2187 = FACT, (bin), replace label: "Jul"
var_label(HouseholdVietnam$e59_17) <- "Jul"
#C2188 = FACT, (bin), replace label: "Aug"
var_label(HouseholdVietnam$e59_18) <- "Aug"
#C2189 = FACT, (bin), replace label: "Sep"
var_label(HouseholdVietnam$e59_19) <- "Sep"
#C2190 = FACT, (bin), replace label: "Oct"
var_label(HouseholdVietnam$e59_110) <- "Oct"
#C2191 = FACT, (bin), replace label: "Nov"
var_label(HouseholdVietnam$e59_111) <- "Nov"
#C2192 = FACT, (bin), replace label: "Dec"
var_label(HouseholdVietnam$e59_112) <- "Dec"
#C2193 = FACT, (bin)

# # #f.
#C2194 = FACT, (bin)
#C2195 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2196 = FACT, (table of correspondence), (see answers below)
#No time 
#Very little time
#Moderate amount of time
#Almost enough time
#Sufficient amount of time
#Do not know
#C2197 = FACT, (table of correspondence)
#C2198 = REMOVE, (text from the questionnaire)

# # #g.
#C2199 = FACT, (table of correspondence), (see answers below)
#Myself alone
#Me in consultation with spouse/other family members
#My spouse/other family members
#Do not know
#C2200 = FACT, (table of correspondence)
#C2201 = FACT, (table of correspondence)
#C2202 = FACT, (table of correspondence)
#C2203 = FACT, (bin)
#C2204 = FACT, (bin)


# # #h. 
#C2205 = REMOVE, text from the questionnaire
#C2206 = FACT, (table of correspondence), (see answers below)
#Yes, strongly
#Yes, maybe
#They should emigrate if they had the chance
#No, agriculture is not a good job
#Do not know
#C2207 = FACT, (bin)
#C2208 = FACT, (bin)
#C2209 = FACT, (bin)
#C2210 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#Other farmers/farmer group/cooperative
#Technical advisors
#Researchers
#Buyers
#Other
#Do not know
#C2211 = FACT, (bin), replace label: "Other farmers/farmer group/cooperative"
var_label(HouseholdVietnam$h4_11) <- "Other farmers/farmer group/cooperative"
#C2212 = FACT, (bin), replace label: "Technical advisors"
var_label(HouseholdVietnam$h4_12) <- "Technical advisors"
#C2213 = FACT, (bin), replace label: "Researchers"
var_label(HouseholdVietnam$h4_13) <- "Researchers"
#C2214 = FACT, (bin), replace label: "Buyers"
var_label(HouseholdVietnam$h4_14) <- "Buyers"
#C2215 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$h4_199) <- "Other"
#C2216 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$h4_188) <- "Do not know"
#C2217 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2218 = FACT, (bin)
#C2219 = OK, (empty)
#C2220 = OK, (empty)

# # #i.
#C2221 = REMOVE, (text from questionnaire)
#C2222 = FACT, (table of correspondence), (see answer below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2223 = FACT, (bin)
#C2224 = CHAR, (Useless answer-MultipleCombined, use the following columns),
#C2225 = FACT, (bin), replace label: "Jan"
var_label(HouseholdVietnam$i31) <- "Jan"
#C2226 = FACT, (bin), replace label: "Feb"
var_label(HouseholdVietnam$i32) <- "Feb"
#C2227 = FACT, (bin), replace label: "Mar"
var_label(HouseholdVietnam$i33) <- "Mar"
#C2228 = FACT, (bin), replace label: "Apr"
var_label(HouseholdVietnam$i34) <- "Apr"
#C2229 = FACT, (bin), replace label: "May"
var_label(HouseholdVietnam$i35) <- "May"
#C2230 = FACT, (bin), replace label: "Jun"
var_label(HouseholdVietnam$i36) <- "Jun"
#C2231 = FACT, (bin), replace label: "Jul"
var_label(HouseholdVietnam$i37) <- "Jul"
#C2232 = FACT, (bin), replace label: "Aug"
var_label(HouseholdVietnam$i38) <- "Aug"
#C2233 = FACT, (bin), replace label: "Sep"
var_label(HouseholdVietnam$i39) <- "Sep"
#C2234 = FACT, (bin), replace label: "Oct"
var_label(HouseholdVietnam$i310) <- "Oct"
#C2235 = FACT, (bin), replace label: "Nov"
var_label(HouseholdVietnam$i311) <- "Nov"
#C2236 = FACT, (bin), replace label: "Dec"
var_label(HouseholdVietnam$i312) <- "Dec"
#C2237 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Climate (drought, floods) 
#Pest damages
#Animal disease  
#No buyers for your produce
#Declining selling prices for your produce
#Need to reimburse credits 
#Increasing prices of food (rice…)
#Other
#C2238 = FACT, (bin), replace label: "Climate (drought, floods)"
var_label(HouseholdVietnam$i41) <- "Climate (drought, floods)"
#C2239 = FACT, (bin), replace label: "Pest damages"
var_label(HouseholdVietnam$i42) <- "Pest damages"
#C2240 = FACT, (bin), replace label: "Animal disease"
var_label(HouseholdVietnam$i43) <- "Animal disease"
#C2241 = FACT, (bin), replace label: "No buyers for your produce"
var_label(HouseholdVietnam$i44) <- "No buyers for your produce"
#C2242 = FACT, (bin), replace label: "Declining selling prices for your produce"
var_label(HouseholdVietnam$i45) <- "Declining selling prices for your produce"
#C2243 = FACT, (bin), replace label: "Need to reimburse credits"
var_label(HouseholdVietnam$i46) <- "Need to reimburse credits"
#C2244 = FACT, (bin), replace label: "Increasing prices of food (rice…)"
var_label(HouseholdVietnam$i47) <- "Increasing prices of food (rice…)"
#C2245 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$i499) <- "Other"
#C2246 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2247 = FACT, (table of correspondence), (see answers below)
#Happens every year or most years
#Happens sometimes but not regularly
#It was exceptional

# # #j. 
#C2248 = FACT, (table of correspondence), (see answers below)
#Owned
#Rented
#Share cropping 
#Other
#Do not know
#C2249 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2250 = FACT, (table of correspondence), (see answers below)
#Brick wall
#Concrete wall
#Wooden wall
#Bamboo, Thatch/leaves, Grass
#Galvanized iron or aluminium or other metal sheets 
#Wood or logs
#Other
#C2251 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2252 = FACT, (table of correspondence), (see answers below)
#Thatch/leaves/grass 
#Wood
#Fibrous cement 
#Concrete, cement
#Brick tile roof
#Stone tile roof
#Metal/ tin roof
#Other
#C2253 = OK, (empty)
#C2254 = FACT, (bin)
#C2255 = FACT, (bin)
#C2256 = FACT, (table of correspondence), (see answers below)
#Private tap water
#Public water
#Drill
#Well
#Rain water
#Natural stream
#Water delivery
#Rain water
#Other
#C2257 = OK, (empty)
#C2258 = FACT, (table of correspondence), (see answers below)
#Grid electricity 
#Private electricity
#Small hydroelectricity
#Own generator
#Battery
#Solar panel 
#None, using kerosene/ candles
#Other
#C2259 = OK, (other)

# # #k.
#C2260 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
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
#C2261 = FACT, (bin), replace label: "Motorcycle"
var_label(HouseholdVietnam$k11) <- "Motorcycle"
#C2262 = FACT, (bin), replace label: "2-wheel hand tractor"
var_label(HouseholdVietnam$k12) <- "2-wheel hand tractor"
#C2263 = FACT, (bin), replace label: "4-wheel tractor"
var_label(HouseholdVietnam$k13) <- "4-wheel tractor"
#C2264 = FACT, (bin), replace label: "Car"
var_label(HouseholdVietnam$k14) <- "Car"
#C2265 = FACT, (bin), replace label: "Truck"
var_label(HouseholdVietnam$k15) <- "Truck"
#C2266 = FACT, (bin), replace label: "Combine harvester"
var_label(HouseholdVietnam$k16) <- "Combine harvester"
#C2267 = FACT, (bin), replace label: "Land leveler"
var_label(HouseholdVietnam$k17) <- "Land leveler"
#C2268 = FACT, (bin), replace label: "Rice planter"
var_label(HouseholdVietnam$k18) <- "Rice planter"
#C2269 = FACT, (bin), replace label: "Maize planter"
var_label(HouseholdVietnam$k19) <- "Maize planter"
#C2270 = FACT, (bin), replace label: "Cassava disc ridging tool"
var_label(HouseholdVietnam$k110) <- "Cassava disc ridging tool"
#C2271 = FACT, (bin), replace label: "Cassava harvesting tool"
var_label(HouseholdVietnam$k111) <- "Cassava harvesting tool"
#C2272 = FACT, (bin), replace label: "Rice thresher"
var_label(HouseholdVietnam$k112) <- "Rice thresher"
#C2273 = FACT, (bin), replace label: "Rice mill"
var_label(HouseholdVietnam$k113) <- "Rice mill"
#C2274 = FACT, (bin), replace label: "Backpack sprayer"
var_label(HouseholdVietnam$k114) <- "Backpack sprayer"
#C2275 = FACT, (bin), replace label: "Motor pump sprayer"
var_label(HouseholdVietnam$k115) <- "Motor pump sprayer"
#C2276 = FACT, (bin), replace label: "Grass cutter"
var_label(HouseholdVietnam$k116) <- "Grass cutter"
#C2277 = FACT, (bin), replace label: "Grass chopping machine"
var_label(HouseholdVietnam$k117) <- "Grass chopping machine"
#C2278 = FACT, (bin), replace label: "Water pump"
var_label(HouseholdVietnam$k118) <- "Water pump"
#C2279 = FACT, (bin), replace label: "Irrigation system"
var_label(HouseholdVietnam$k119) <- "Irrigation system"
#C2280 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$k199) <- "Other"
#C2281 = OK
#C2282 = OK
#C2283 = OK
#C2284 = OK
#C2285 = OK
#C2286 = OK
#C2287 = OK
#C2288 = OK
#C2289 = OK
#C2290 = OK
#C2291 = OK
#C2292 = OK
#C2293 = OK
#C2294 = OK
#C2295 = OK
#C2296 = OK
#C2297 = OK
#C2298 = OK
#C2299 = OK

# # #l.
#C2300 = REMOVE, (text from the questionaire)
#C2301 = FACT, (bin)
#C2302 = FACT, (table of correspondence), (see answers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2303 = FACT, (table of correspondence), (see answers below)
#No debt   
#Debt is higher than income
#Debt is more than half of the income. Capacity to reimburse is limited
#Debt is approximately half of the income
#Debt is low and I am capable to reimburse
#C2304 = FACT, (bin)
#C2305 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
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
#C2306 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2307 = FACT, (bin), replace label: "To buy food"
var_label(HouseholdVietnam$l51) <- "To buy food"
#C2308 = FACT, (bin), replace label: "For emergency expenses"
var_label(HouseholdVietnam$l52) <- "For emergency expenses"
#C2309 = FACT, (bin), replace label: "To buy animals"
var_label(HouseholdVietnam$l53) <- "To buy animals"
#C2310 = FACT, (bin), replace label: "To buy equipment/machinery for agriculture"
var_label(HouseholdVietnam$l54) <- "To buy equipment/machinery for agriculture"
#C2311 = FACT, (bin), replace label: "To buy seeds"
var_label(HouseholdVietnam$l55) <- "To buy seeds"
#C2312 = FACT, (bin), replace label: "To buy chemical farm inputs"
var_label(HouseholdVietnam$l56) <- "To buy chemical farm inputs"
#C2313 = FACT, (bin), replace label: "To buy organic farm inputs"
var_label(HouseholdVietnam$l57) <- "To buy organic farm inputs"
#C2314 = FACT, (bin), replace label: "To pay for certification"
var_label(HouseholdVietnam$l58) <- "To pay for certification"
#C2315 = FACT, (bin), replace label: "Festivals/celebrations"
var_label(HouseholdVietnam$l59) <- "Festivals/celebrations"
#C2316 = FACT, (bin), replace label: "Child education"
var_label(HouseholdVietnam$l510) <- "Child education"
#C2317 = FACT, (bin), replace label: "Health care"
var_label(HouseholdVietnam$l511) <- "Health care"
#C2318 = FACT, (bin), replace label: "To improve the house"
var_label(HouseholdVietnam$l512) <- "To improve the house"
#C2319 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$l599) <- "Other"
#C2320 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$l588) <- "Do not know"
#C2321 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2322 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Producer group
#Union (farmer, women, veteran)
#Bank
#Relatives & friends 
#Collector / trader
#Input seller
#Processor
#Other
#Do not know
#C2323 = FACT, (bin), replace label: "Producer group"
var_label(HouseholdVietnam$l61) <- "Producer group"
#C2324 = FACT, (bin), replace label: "Union (farmer, women, veteran)"
var_label(HouseholdVietnam$l62) <- "Union (farmer, women, veteran)"
#C2325 = FACT, (bin), replace label: "Bank"
var_label(HouseholdVietnam$l63) <- "Bank"
#C2326 = FACT, (bin), replace label: "Relatives & friends"
var_label(HouseholdVietnam$l64) <- "Relatives & friends"
#C2327 = FACT, (bin), replace label: "Collector / trader"
var_label(HouseholdVietnam$l65) <- "Collector / trader"
#C2328 = FACT, (bin), replace label: "Input seller"
var_label(HouseholdVietnam$l66) <- "Input seller"
#C2329 = FACT, (bin), replace label: "Processor"
var_label(HouseholdVietnam$l67) <- "Processor"
#C2330 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2331 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2332 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2333 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2334 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2335 = FACT, (bin), replace label: "Other"
var_label(HouseholdVietnam$l699) <- "Other"
#C2336 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdVietnam$l688) <- "Do not know"
#C2337 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2338 = FACT, (bin)
#C2339 = FACT, (bin)

# # #m.
#C2340 = FACT, (table of correspondence), (see answers below)
#Complete
#Partially complete
#Refuses to participate
#No household member present
#Household moved to a new location
#C2341 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C2342 = REMOVE, (duplicate)
#C2343 = OK
#C2344 = OK
#C2345 = OK
#C2346 = OK

## For each column we determine if necessary to change the name, label or remove it
#C2347 = REMOVE, (text from questionnaire)
#C2348 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2349 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2350 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2351 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2352 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2353 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2354 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2355 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2356 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2357 = REMOVE, (empty, correspond to nothing in the questionnaire) 
#C2358 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2359 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2360 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2361 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2362 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2363 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2364 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2365 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2366 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2367 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2368 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2369 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2370 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2371 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2372 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2373 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2374 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2375 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2376 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2377 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2378 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2379 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2380 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2381 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2382 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2383 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2384 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2385 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2386 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2387 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2388 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2389 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2390 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2391 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2392 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2393 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2394 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2395 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2396 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2397 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2398 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2399 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2400 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2401 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2402 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2403 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2404 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2405 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2406 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2407 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2408 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2409 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2410 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2411 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2412 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2413 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2414 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2415 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2416 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2417 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2418 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2419 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2420 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2421 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2422 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2423 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2424 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2425 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2426 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2427 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2428 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2429 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2430 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2431 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2432 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2433 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2434 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2435 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2436 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2437 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2438 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2439 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2440 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2441 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2442 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2443 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2444 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2445 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2446 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2447 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2448 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2449 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2450 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2451 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2452 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2453 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2454 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2455 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2456 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2457 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2458 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2459 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2460 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2461 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2462 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2463 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2464 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2465 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2466 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2467 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2468 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2469 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2470 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2471 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2472 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2473 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2474 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2475 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2476 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2477 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2478 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2479 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2480 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2481 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2482 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2483 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2484 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2485 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2486 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2487 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2488 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2489 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2490 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2491 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2492 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2493 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2494 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2495 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2496 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2497 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2498 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2499 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2500 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2501 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2502 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2503 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2504 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2505 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2506 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2507 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2508 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2509 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2510 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2511 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2512 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2513 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2514 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2515 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2516 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2517 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2518 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2519 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2520 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2521 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2522 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2523 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2524 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2525 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2526 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2527 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2528 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2529 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2530 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2531 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2532 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2533 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2534 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2535 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2536 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2537 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2538 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2539 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2540 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2541 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2542 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2543 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2544 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2545 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2546 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2547 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2548 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2549 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2550 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2551 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2552 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2553 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2554 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2555 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2556 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2557 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2558 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2559 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2560 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2561 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2562 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2563 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2564 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2565 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2566 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2567 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2568 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2569 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2570 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2571 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2572 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2573 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2574 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2575 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2576 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2577 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2578 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2579 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2580 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2581 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2582 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2583 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2584 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2585 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2586 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2587 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2588 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2589 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2590 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2591 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2592 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2593 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2594 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2595 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2596 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2597 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2598 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2599 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2600 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2601 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2602 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2603 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2604 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2605 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2606 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2607 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2608 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2609 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2610 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2611 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2612 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2613 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2614 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2615 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2616 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2617 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2618 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2619 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2620 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2621 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2622 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2623 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2624 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2625 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2626 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2627 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2628 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2629 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2630 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2631 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2632 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2633 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2634 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2635 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2636 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2637 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2638 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2639 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2640 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2641 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2642 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2643 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2644 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2645 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2646 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2647 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2648 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2649 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2650 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2651 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2652 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2653 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2654 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2655 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2656 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2657 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2658 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2659 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2660 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2661 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2662 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2663 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2664 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2665 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2666 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2667 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2668 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2669 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2670 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2671 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2672 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2673 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2674 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2675 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2676 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2677 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2678 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2679 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2680 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2681 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2682 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2683 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2684 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2685 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2686 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2687 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2688 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2689 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2690 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2691 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2692 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2693 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2694 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2695 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2696 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2697 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2698 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2699 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2700 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2701 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2702 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2703 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2704 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2705 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2706 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2707 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2708 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2709 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C2710 = CHAR, kind of id?
#C2711 = CHAR, kind of id?
#C2712 = CHAR
#C2713 = OK, (empty)
#C2714 = OK, (empty)
#C2715 = OK
#C2716 = OK
#C2717 = OK
#C2718 = OK, (empty)
#C2719 = CHAR, kind of id?
#C2720 = OK, (???)
#C2721 = OK, (???)
#C2722 = OK, (???)
#C2723 = OK, (???)
#C2724 = OK, (???)
#C2725 = OK, (???)
#C2726 = OK, (???)
#C2727 = OK, (???)
#C2728 = OK, (???)
#C2729 = OK, (???)



##2.2. Work on the 2nd database: "HouMemberVietnam" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#ColViet <- colnames(HouMemberVietnam)
#ColVietnameses <- colnames(HouMemberVietnameses)

#Colcheck <- cbind(ColViet,ColVietnameses)

#C1 = FACT, (id of household, to link with other dataframes)
#C2 = FACT, (id of Household member in a household)
#C3 = FACT, (both previous id combined)
#C4 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C5 = FACT, (bin), replace label:"a1_1. is  a1 the survey respondent ?"
var_label(HouMemberVietnam$a1_1) <- "a1_1. is  a1 the survey respondent ?"
#C6 = FACT, (table of correspondence), (see answers below),
#replace label:"a2. what is a1’s relationship to household head ?"
var_label(HouMemberVietnam$a2) <- "a2. what is a1’s relationship to household head ?"
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
#C7 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C8 = FACT, (table of correspondence), (see answers below), replace label: "a3. what is the sex of a1 ?"
var_label(HouMemberVietnam$a3) <- "a3. what is the sex of a1 ?"
#Male
#Female
#C9 = FACT, (table of correspondence), (see answers below), replace label: "a4. what is a1's solar year of birth ?"
var_label(HouMemberVietnam$a4) <- "a4. what is a1's solar year of birth ?"
#C10 = NUM, replace label: "Age (years)"
var_label(HouMemberVietnam$a4_1) <- "Age (years)"
#C11 = FACT, (table of correspondence), (see answers below), replace label: "a5. what is the highest diploma a1 has obtained ?"
var_label(HouMemberVietnam$a5) <- "a5. what is the highest diploma a1 has obtained ?"
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
var_label(HouMemberVietnam$a6) <- "a6. what is a1's main occupation ?"
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
#C13 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "a6_oth. specify other main occupation of a1 in the past 12 months"
var_label(HouMemberVietnam$a6_oth) <- "a6_oth. specify other main occupation of a1 in the past 12 months"
#C14 = FACT, (table of correspondence), (see answers below), replace label: "a7. what is a1's secondary occupation ?"
var_label(HouMemberVietnam$a7) <- "a7. what is a1's secondary occupation ?"
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
#C15 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "a6_oth. specify other second occupation of a1 in the past 12 months"
var_label(HouMemberVietnam$a7_oth) <- "a7_oth. specify other second occupation of a1 in the past 12 months"
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
var_label(HouMemberVietnam$a81) <- "Weaving"
#C18 = FACT, (bin), replace label: "Handicraft"
var_label(HouMemberVietnam$a82) <- "Handicraft"
#C19 = FACT, (bin), replace label: "Commerce"
var_label(HouMemberVietnam$a83) <- "Commerce"
#C20 = FACT, (bin), replace label: "Transportation"
var_label(HouMemberVietnam$a84) <- "Transportation"
#C21 = FACT, (bin), replace label: "Blacksmithing"
var_label(HouMemberVietnam$a85) <- "Blacksmithing"
#C22 = FACT, (bin), replace label: "Construction work"
var_label(HouMemberVietnam$a86) <- "Construction work"
#C23 = FACT, (bin), replace label: "Factory work"
var_label(HouMemberVietnam$a87) <- "Factory work"
#C24 = FACT, (bin), replace label: "Agricultural service provision"
var_label(HouMemberVietnam$a88) <- "Agricultural service provision"
#C25 = FACT, (bin), replace label: "Other"
var_label(HouMemberVietnam$a899) <- "Other"
#C26 = FACT, (bin), replace label: "Do not know"
HouMemberVietnam <- HouMemberVietnam %>% relocate(a899, .after = a88)
var_label(HouMemberVietnam$a888) <- "Do not know"
#C27 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C28 = FACT, (table of correspondence), (see answers below), replace label: "a9. reason why a1  was unable to work in the last 12 months ?"
var_label(HouMemberVietnam$a9) <- "a9. reason why a1  was unable to work in the last 12 months ?"
#Cannot find employment
#Sick
#Handicapped
#Too old
#Too young
#Other reason
#C29 = OK, (the answer is still in Vietnamese, Ky will maybe solve it), replace label: "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
var_label(HouMemberVietnam$a9_oth) <- "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
#C30 = FACT, (bin), replace label: "a10. do a1 migrate seasonally every year ?"
var_label(HouMemberVietnam$a10) <- "a10. do a1 migrate seasonally every year ?"
#C31 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C32 = FACT, (table of correspondence), (see answers below), replace label: "a12. if yes, what is a1's main occupation when migrating"
var_label(HouMemberVietnam$a12) <- "a12. if yes, what is a1's main occupation when migrating"
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
var_label(HouMemberVietnam$a13) <- "a13. if yes, what months do a1 migrates ?"
#C34 = FACT, (bin), replace label: "Jan"
var_label(HouMemberVietnam$a131) <- "Jan"
#C35 = FACT, (bin), replace label: "Feb"
var_label(HouMemberVietnam$a132) <- "Feb"
#C36 = FACT, (bin), replace label: "Mar"
var_label(HouMemberVietnam$a133) <- "Mar"
#C37 = FACT, (bin), replace label: "Apr"
var_label(HouMemberVietnam$a134) <- "Apr"
#C38 = FACT, (bin), replace label: "May"
var_label(HouMemberVietnam$a135) <- "May"
#C39 = FACT, (bin), replace label: "Jun"
var_label(HouMemberVietnam$a136) <- "Jun"
#C40 = FACT, (bin), replace label: "Jul"
var_label(HouMemberVietnam$a137) <- "Jul"
#C41 = FACT, (bin), replace label: "Aug"
var_label(HouMemberVietnam$a138) <- "Aug"
#C42 = FACT, (bin), replace label: "Sep"
var_label(HouMemberVietnam$a139) <- "Sep"
#C43 = FACT, (bin), replace label: "Oct"
var_label(HouMemberVietnam$a1310) <- "Oct"
#C44 = FACT, (bin), replace label: "Nov"
var_label(HouMemberVietnam$a1311) <- "Nov"
#C45 = FACT, (bin), replace label: "Dec"
var_label(HouMemberVietnam$a1312) <- "Dec"

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

#C59 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C60 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C61 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C62 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C63 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C64 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C65 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C66 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C67 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C68 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C69 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C70 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C71 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C72 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C73 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C74 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C75 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C76 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C77 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C78 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C79 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C80 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C81 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C82 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C83 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C84 = FACT, (index)
#C85 = OK, (useless??)
#C86 = FACT, (parent index)
#C87 = FACT, (id)
#C88 = FACT, (id)
#C89 = FACT, (time)
#C90 = FACT, (time)
#C91 = OK, (empty)
#C92 = OK, (useless?)
#C93 = OK, (empty)
#C94 = OK, (useless?)
#C95 = OK, (empty)

summary(HouMemberVietnam[,60])

##2.3. Work on the third database: "ClowlandVietnam" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#ColViet <- colnames(ClowlandVietnam)
#ColVietnameses <- colnames(ClowlandVietnameses)

#Colcheck <- cbind(ColViet,ColVietnameses)

#C1 = FACT, (id of household, to link with other dataframes)
#C2 = FACT, (id of crop per household)
#C3 = FACT, We create a new column here as the combination of crop and household id
ClowlandVietnam$pid <- paste(ClowlandVietnam$hhid_re2,ClowlandVietnam$crop1_now)
#We move this column at the right place
ClowlandVietnam <- ClowlandVietnam %>% relocate(pid, .after = hhid_re2)
#C4 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C5 = FACT, (number corresponding to crops), replace label: "d2_12. select crop crop1_now with keyword searchcrop1"
var_label(ClowlandVietnam$d2_12) <- "d2_12. select crop crop1_now with keyword searchcrop1"
#C6 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C7 = REMOVE, (empty)
#C8 = OK, (crop name in Vietnamese) replace label: "d2_13v. Crop name (Vietnamese)"
var_label(ClowlandVietnam$d2_13v) <- "d2_13v. Crop name (Vietnamese)"
#C9 = OK, (crop name in English) replace label: "d2_13e. Crop name (English)"
var_label(ClowlandVietnam$d2_13e) <- "d2_13e. Crop name (English)"
#C10 = FACT, ???
#C11 = NUM, replace label: "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(ClowlandVietnam$d2_132) <- "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
#C12 = FACT, (table of correspondence), (see option below), replace label: "d2_133. which unit of d2_13v ?"
var_label(ClowlandVietnam$d2_133) <- "d2_133. which unit of d2_13v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C13 = OK, replace label: "d2_134. number of seed units of d2_13v that household used?"
var_label(ClowlandVietnam$d2_134) <- "d2_134. number of seed units of d2_13v that household used?"
#C14 = OK, replace label: "d2_135. number of kg produced of d2_13v ?"
var_label(ClowlandVietnam$d2_135) <- "d2_135. number of kg produced of d2_13v ?"
#C15 = OK, replace label: "d2_136.number of kg sold of d2_13v"
var_label(ClowlandVietnam$d2_136) <- "d2_136.number of kg sold of d2_13v"
#C16 = OK, replace label: "d2_137. selling price/ kg of d2_13v"
var_label(ClowlandVietnam$d2_137) <- "d2_137. selling price/ kg of d2_13v"
#C17 = OK, replace label: "d2_138. how many species or varieties of d2_13v ?"
var_label(ClowlandVietnam$d2_138) <- "d2_138. how many species or varieties of d2_13v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C18 = FACT, (CROP id)
#C19 = OK, (useless??)
#C20 = FACT, (parent index)
#C21 = FACT, (id)
#C22 = FACT, (id)
#C23 = FACT, (time)
#C24 = FACT, (empty)
#C25 = OK, (empty)
#C26 = OK, (empty)
#C27 = OK, (useless?)
#C28 = OK, (useless?)
#C29 = OK, (empty)

summary(ClowlandVietnam[,15])

##2.4. Work on the fourth database: "CuplandVietnam" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#ColViet <- colnames(CuplandVietnam)
#ColVietnameses <- colnames(CuplandVietnameses)

#Colcheck <- cbind(ColViet,ColVietnameses)

#C1 = FACT, (id of household, to link with other dataframes)
#C2 = FACT, (id of crop per household)
#C3 = FACT, We create a new column here as the combination of crop and household id
CuplandVietnam$pid <- paste(CuplandVietnam$hhid_re3,CuplandVietnam$crop2_now)
#We move this column at the right place
CuplandVietnam <- CuplandVietnam %>% relocate(pid, .after = hhid_re3)
#C4 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C5 = FACT, (number corresponding to crops), replace label: "d2_22. select crop crop1_now with keyword searchcrop1"
var_label(CuplandVietnam$d2_22) <- "d2_22. select crop crop1_now with keyword searchcrop1"
#C6 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C7 = OK, (crop name in Vietnamese) replace label: "d2_23v. Crop name (Vietnamese)"
var_label(CuplandVietnam$d2_23v) <- "d2_23v. Crop name (Vietnamese)"
#C8 = OK, (crop name in English) replace label: "d2_23e. Crop name (English)"
var_label(CuplandVietnam$d2_23e) <- "d2_23e. Crop name (English)"
#C9 = FACT, ???
#C10 = NUM, replace label: "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(CuplandVietnam$d2_232) <- "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
#C11 = FACT, (table of correspondence), (see option below), replace label: "d2_233. which unit of d2_23v ?"
var_label(CuplandVietnam$d2_233) <- "d2_233. which unit of d2_23v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C12 = OK, replace label: "d2_234. number of seed units of d2_23v that household used?"
var_label(CuplandVietnam$d2_234) <- "d2_234. number of seed units of d2_23v that household used?"
#C13 = OK, replace label: "d2_235. number of kg produced of d2_23v ?"
var_label(CuplandVietnam$d2_235) <- "d2_235. number of kg produced of d2_23v ?"
#C14 = OK, replace label: "d2_236.number of kg sold of d2_23v"
var_label(CuplandVietnam$d2_236) <- "d2_236.number of kg sold of d2_23v"
#C15 = OK, replace label: "d2_237. selling price/ kg of d2_23v"
var_label(CuplandVietnam$d2_237) <- "d2_237. selling price/ kg of d2_23v"
#C16 = OK, replace label: "d2_238. how many species or varieties of d2_23v ?"
var_label(CuplandVietnam$d2_238) <- "d2_238. how many species or varieties of d2_23v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C17 = ??
#C18 = FACT, (CROP id)
#C19 = OK, (useless??)
#C20 = FACT, (parent index)
#C21 = FACT, (id)
#C22 = FACT, (id)
#C23 = FACT, (time)
#C24 = FACT, (time)
#C25 = OK, (empty)
#C26 = OK, (useless?)
#C27 = OK, (empty)
#C28 = OK, (useless?)
#C29 = OK, (empty)

summary(CuplandVietnam[,15])

#var_label(HouseholdVietnam$b14_2)


### 3. Columns data type changing and removing


##3.1 Change column data types base on previous Columns ID
#Convert the tbl_df into a dataframe for each database
HouseholdVietnam_2 <- as.data.frame(HouseholdVietnam)
HouMemberVietnam_2 <- as.data.frame(HouMemberVietnam)
ClowlandVietnam_2 <- as.data.frame(ClowlandVietnam)
CuplandVietnam_2 <- as.data.frame(CuplandVietnam)
#a. FACTOR CONVERSION
#Convert columns to FACTOR - "HouseholdVietnam"
for (i in c(1:3,7:9,11,13:34,36,38:42,48:54,57,68:76,77:78,80:99,101:104,113,
            121:141,144,146,148,155,157:177,180,182:194,196:207,
            209:219,222:230,232:259,261,263,266:279,281,283,286:299,301,303,306:319,
            321,323,326:339,341,343,346:359,361,363,366:379,381,383,386:399,401,403,
            406:419,421,423,426:439,441,443,446:459,461,463,466:479,481,483,486:499,
            501,503,506:519,521,523,526:539,541,543,546:559,562:574,576,578:579,
            581:589,591,593:601,603,605,607,610:625,627:644,647:656,658:659,661:667,686:697,700,704:715,717:725,727,729:740,742:750,752,
            762:778,781,784,787,790,793,796,799,802,805,808,811,814,817,820,823,826,
            828:830,834,836,838,840,842,844,847,849,851,854:858,860:862,865:874,
            877:888,891:902,905:916,919:930,933:944,947:958,961:972,975:986,989:997,
            1000:1011,1014:1025,1028:1039,1042:1053,1056:1067,1070:1081,1084:1095,
            1098:1109,1111:1112,1114:1125,1128:1139,1142:1153,1156:1167,1170:1181,
            1184:1195,1198:1209,1212:1223,1226:1237,1240:1251,1254:1265,1268:1279,
            1282:1293,1295:1296,1298:1313,1316:1327,1329:1341,1344:1355,1358:1369,
            1372:1383,1386:1397,1400:1411,1414:1425,1428:1439,1442:1453,1456:1467,
            1470:1481,1484:1495,1498:1509,1511:1513,1515,1517:1531,1534:1545,
            1548:1559,1562:1573,1576:1587,1590:1601,1604:1615,1618:1629,1632:1643,
            1646:1657,1660:1671,1674:1685,1688:1699,1701:1713,1715:1727,1730:1741,
            1743:1745,1747:1758,1760:1771,1773:1784,1786:1797,1800:1803,1805:1808,
            1810:1813,1815:1818,1820:1823,1825:1828,1830:1833,1835:1838,1840:1843,
            1845:1848,1850:1853,1857:1867,1869,1871:1883,1904,1906,1908,1911,1913,
            1922,1925,1927:1935,1937,1939,1941,1950,1953,1954,1956,1958,1960,1969,
            1972,1974,1976,1978:1984,1986:1996,1998:2000,2002:2012,2014,2016:2017,
            2019:2022,2024:2027,2029,2031,2033:2036,2038,2040,2042,2044,2045,2046,
            2048:2061,2063:2073,2075,2077:2078,2080:2088,2090,2092,2094:2097,2099,
            2101,2103,2105:2107,2109:2122,2124:2134,2136,2138:2144,2146:2150,
            2152,2154,2156:2161,2163,2165,2167:2179,2181:2194,2196:2197,2199:2204,
            2206:2209,2211:2216,2218,2222:2223,2225:2236,2238:2245,2247:2248,2250,
            2252,2254:2256,2258,2261:2280,2301:2304,2307:2320,2323:2329,2335:2336,
            2338:2340)){
  HouseholdVietnam_2[,i] <- as.factor(HouseholdVietnam_2[,i])
}
#Convert columns to FACTOR - "HouMemberVietnam"
for (i in c(1:3,5:6,8:9,11:12,14,16:26,28,30,32,34:45,47:52,84,86:90)){
  HouMemberVietnam_2[,i] <- as.factor(HouMemberVietnam_2[,i])
}
#Convert columns to FACTOR - "ClowlandVietnam"
for (i in c(1:2,4,10:12,17,19:23)){
  ClowlandVietnam_2[,i] <- as.factor(ClowlandVietnam_2[,i])
}
#Convert columns to FACTOR - "CuplandVietnam"
for (i in c(1:5,9:11,17,19:23)){
  CuplandVietnam_2[,i] <- as.factor(CuplandVietnam_2[,i])
}
#b. NUMERIC CONVERSION
#Convert columns to NUMERIC - "HouseholdVietnam"
for (i in  c(67,151,153,668,676)){
  HouseholdVietnam_2[,i] <- as.numeric(HouseholdVietnam_2[,i])
}
#Convert columns to NUMERIC - "HouMemberVietnam"
  HouMemberVietnam_2[,10] <- as.numeric(HouMemberVietnam_2[,10])
#Convert columns to NUMERIC - "ClowlandVietnam"
  ClowlandVietnam_2[,10] <- as.numeric(ClowlandVietnam_2[,10])
#Convert columns to NUMERIC - "CuplandVietnam"
  CuplandVietnam_2[,9] <- as.numeric(CuplandVietnam_2[,9])
#c. CHARACTER CONVERSION
#Convert columns to CHARACTER - "HouseholdVietnam"
  for (i in  c(120,156,178,181,195,208,221,265,285,305,325,345,365,385,405,425,
               445,465,485,505,525,545,580,592,609,626,646,660,685,703,716,728,
               741,761,853,864,876,890,904,918,932,946,960,974,988,999,1013,1027,
               1041,1055,1069,1083,1097,1113,1127,1141,1155,1169,1183,1197,1211,
               1225,1239,1253,1267,1281,1297,1315,1343,1357,1371,1385,1399,1413,
               1427,1441,1455,1469,1483,1497,1516,1533,1547,1561,1575,1589,1603,
               1617,1631,1645,1659,1673,1687,1729,1746,1759,1772,1785,1799,1804,
               1809,1814,1819,1824,1829,1834,1839,1844,1849,1856,1870,1926,1985,
               2001,2015,2023,2032,2047,2062,2076,2093,2108,2123,2137,2145,2155,
               2166,2180,2198,2210,2224,2237,2260,2305,2322,2710:2712,2719)){
    HouseholdVietnam_2[,i] <- as.character(HouseholdVietnam_2[,i])
  }
#Convert columns to CHARACTER - "HouMemberVietnam"
HouMemberVietnam_2[,33] <- as.factor(HouMemberVietnam_2[,33])
    


##3.2 REMOVE Unwanted columns (1st Version)
#Remove unwanted columns for each datasets
#HouseholdVietnam_2C <- HouseholdVietnam_2[,-c(5:6,43:47,58,73:76,86:97,99,105:112,
                                              #114:118,135,171,573,589,600:601,633,
                                              #654,656,675,684,846,858:860,1914,1942,
                                              #1961,2018,2037,2079,2098,2140,2160,2205,
                                              #2221,2300,2306,2330:2334,2342,2347:2709)]
#HouMemberVietnam_2C <- HouMemberVietnam_2[,-c(59:83)]
#ClowlandVietnam_2C <- ClowlandVietnam_2C[,-6]

#Add the proper labels to each columns
HouseholdVietnam_2C <- copy_labels(HouseholdVietnam_2, HouseholdVietnam)
HouMemberVietnam_2C <- copy_labels(HouMemberVietnam_2, HouMemberVietnam)
ClowlandVietnam_2C <- copy_labels(ClowlandVietnam_2, ClowlandVietnam)
CuplandVietnam_2 <- copy_labels(CuplandVietnam_2, CuplandVietnam)


### 4. Data Cleaning 1 (Based on Ky comments)

## 4.1 Data cleaning for "HouseholdVietnam_2"


# # #a. Duplicates
#Check if all households agreed to participate on the survey (% of "yes" answer)
count_if("1",HouseholdVietnam_2C$consent)/nrow(HouseholdVietnam_2C)
# Check household duplicates, for some of them it correspond to answer from the man
#and from the woman to the survey
#1st we move the household ID column at 1st column
HouseholdVietnam_2C <- HouseholdVietnam_2C %>% relocate(o9 , .before = start_time)
#Check the number and id of duplicates
count_if("TRUE",duplicated(HouseholdVietnam_2C$o9))
Dum <- HouseholdVietnam_2C[duplicated(HouseholdVietnam_2C$o9),]
IdDup <- Dum$o9
#Create a loop to merge all the household values in a same column
for (i in (IdDup)){
  Temp <- subset(HouseholdVietnam_2C, o9 == i)
  for (j in 1:ncol(HouseholdVietnam_2C)){
    ifelse(is.na(Temp[1,j]), Temp[1,j] <- Temp[2,j], Temp[1,j] <- Temp[1,j])
  }
  HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == i,] <- Temp[1,]
}
#Select all duplicates rows and delete the unnecessary
Dupli <- rownames(HouseholdVietnam_2C[HouseholdVietnam_2C$o9 %in% IdDup,])
row_odd <- seq_len(length(Dupli)) %% 2
Dupli <- Dupli[row_odd == 0]
HouseholdVietnam_2C <- HouseholdVietnam_2C[! rownames(HouseholdVietnam_2C) %in% Dupli,]
#Check again the duplicates: 
count_if("TRUE",duplicated(HouseholdVietnam_2C$o9))

# # #b Outliers part 1 - Ky

#Corrections from enumetators updated base (07/26/2023)

#b1
HouseholdVietnam_2C[,80] <- as.character(HouseholdVietnam_2C[,80])
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 119, 80] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 148, 80] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 165, 80] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 176, 80] <- 2
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 195, 80] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 210, 80] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 648, 80] <- 1
HouseholdVietnam_2C[,80] <- as.factor(HouseholdVietnam_2C[,80])

#b17
HouseholdVietnam_2C[,242] <- as.character(HouseholdVietnam_2C[,242])
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 515, 242] <- NA
HouseholdVietnam_2C[,242] <- as.factor(HouseholdVietnam_2C[,242])

#e5_a - Nb of breeds, 1st animal
HouseholdVietnam_2C[,1910] <- as.character(HouseholdVietnam_2C[,1910])
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 526, 1910] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 551, 1910] <- 1
HouseholdVietnam_2C[,1910] <- as.factor(HouseholdVietnam_2C[,1910])

#e6_a - Nb of breeds, 2nd animal
HouseholdVietnam_2C[,1938] <- as.character(HouseholdVietnam_2C[,1938])
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 186, 1938] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 551, 1938] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 557, 1938] <- 1
HouseholdVietnam_2C[,1938] <- as.factor(HouseholdVietnam_2C[,1938])

#e7_a - Nb of breeds, 3rd animal
HouseholdVietnam_2C[,1957] <- as.character(HouseholdVietnam_2C[,1957])
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 139, 1957] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 577, 1957] <- 1
HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == 653, 1957] <- 1
HouseholdVietnam_2C[,1957] <- as.factor(HouseholdVietnam_2C[,1957])

### CHECK b5_1, b5_2, b5_3

#HHid 397 (row 318) declared no land according to KY, we will check this assumption
sum(is.na(HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == "397",660:1868]))
#The row is empty, we delete this row
HouseholdVietnam_2C <- HouseholdVietnam_2C[!HouseholdVietnam_2C$o9 == "397", ]

#HHid 199 (row 161) declared 2,940,000 m2 of forest land (Ky), we check: 
h199 <- HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == "199", ]
#@The value will be checked with enumerators, but removed for now
HouseholdVietnam_2C$d5_3 <- ifelse(HouseholdVietnam_2C$d5_3 > 100000, 29400,HouseholdVietnam_2C$d5_3)

#High number of plots for some Households, number of plots rented-in and owned
HouseholdVietnam_2C$d9_2 <- ifelse(HouseholdVietnam_2C$d9_2 > 100, NA,HouseholdVietnam_2C$d9_2)
HouseholdVietnam_2C$d10_2 <- ifelse(HouseholdVietnam_2C$d10_2 > 100, NA,HouseholdVietnam_2C$d10_2)
HouseholdVietnam_2C$d11_2 <- ifelse(HouseholdVietnam_2C$d11_2 > 100, NA,HouseholdVietnam_2C$d11_2)

#Some sensitive figure for number of breed (e5_a) of the 1st, 2nd and 3rd most important
#livestock as table below
HouseholdVietnam_2C$e5_a <- ifelse(HouseholdVietnam_2C$e5_a > 6, NA,HouseholdVietnam_2C$e5_a)
HouseholdVietnam_2C$e6_a <- ifelse(HouseholdVietnam_2C$e6_a > 6, NA,HouseholdVietnam_2C$e6_a)
HouseholdVietnam_2C$e7_a <- ifelse(HouseholdVietnam_2C$e7_a > 6, NA,HouseholdVietnam_2C$e7_a)

#please do cross-check number of Motor pump sprayer" (k2_15) and Irrigation system (k2_19).
#It seems to me that they are outliers or mistaken by typing errors
HouseholdVietnam_2C$k2_15 <- ifelse(HouseholdVietnam_2C$k2_15 > 10, NA,HouseholdVietnam_2C$k2_15)
HouseholdVietnam_2C$k2_19 <- ifelse(HouseholdVietnam_2C$k2_19 > 10, NA,HouseholdVietnam_2C$k2_19)


# # #c. Corresponding fields - Ky remarks
#-	There were 13 households who did no selling agri-products (at b1) but they still selected 3
#main sources of income from crop production and livestock raising (b3). Please review list below
#to know and check record for validating data
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C$b1 <- as.character(HouseholdVietnam_2C$b1)
HouseholdVietnam_2C[c(65,96,119,132,154,166,492),80] <- "1"
HouseholdVietnam_2C[c(93,174),80] <- "2"
HouseholdVietnam_2C[c(67,318),80] <- "3"

#For selling livestock, we based on b1 & b2 to find 308 households selling fresh or processed
#livestock product, but there were 318 ones answered at b17. So, we should delete 10 households
#through check_b10inc_liv (0=no sell livestock products, 1=sold livestock products). Households
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C[424,80] <- "3"
HouseholdVietnam_2C[177,80] <- "0"
HouseholdVietnam_2C$b1 <- as.factor(HouseholdVietnam_2C$b1)
HouseholdVietnam_2C$b17 <- as.character(HouseholdVietnam_2C$b17)
HouseholdVietnam_2C[c(67,93,174,319),242] <- "88"
HouseholdVietnam_2C$b17 <- as.factor(HouseholdVietnam_2C$b17)

#Another problem that we missed data for 45 households at b13_01, we had 362 households sold
#crop products for the 1st buyers (at b12_1), but only 317 households listed crop they sold at
#b13_01. 
#NOTHING TO DO HERE FOR NOW

#Need validate unlogic between d81 (576 respondents) and d81_1a (578 respondents)
HouseholdVietnam_2C$d81_1a <- as.character(HouseholdVietnam_2C$d81_1a)
HouseholdVietnam_2C$d81_1a <- ifelse(is.na(HouseholdVietnam_2C$d81) & !is.na(HouseholdVietnam_2C$d81_1a), NA, HouseholdVietnam_2C$d81_1a)
HouseholdVietnam_2C$d81_1a <- as.factor(HouseholdVietnam_2C$d81_1a)

#Need validate unlogic between d82 (521 respondents) and d82_1a (533 respondents)
HouseholdVietnam_2C$d82_1a <- as.character(HouseholdVietnam_2C$d82_1a)
HouseholdVietnam_2C$d82_1a <- ifelse(is.na(HouseholdVietnam_2C$d82) & !is.na(HouseholdVietnam_2C$d82_1a), NA, HouseholdVietnam_2C$d82_1a)
HouseholdVietnam_2C$d82_1a  <- as.factor(HouseholdVietnam_2C$d82_1a)

#Need validate unlogic between d83 (397 respondents) and d83_1a (414 respondents)
HouseholdVietnam_2C$d83_1a <- as.character(HouseholdVietnam_2C$d83_1a)
HouseholdVietnam_2C$d83_1a <- ifelse(is.na(HouseholdVietnam_2C$d83) & !is.na(HouseholdVietnam_2C$d83_1a), NA, HouseholdVietnam_2C$d83_1a)
HouseholdVietnam_2C$d83_1a  <- as.factor(HouseholdVietnam_2C$d83_1a)

#We have 578 hhs with lowland or upland but d12 on water conservation practice have 581
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C$d12 <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C$d12)
for (i in c(865:874)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}

#Check for d131_1 & d131_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d131_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d131_2))
#Check for d133_1 & d133_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d133_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d133_2))
#Check for d134_1 & d134_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d134_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d134_2))
#Check for d135_1 & d135_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d135_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d135_2))
#Check for d136_1 & d136_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d136_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d136_2))
#Check for d137_1 & d137_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d137_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d137_2))
#Check for d138_1 & d138_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d138_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d138_2))

#We have 578 hhs with lowland or upland but d14 on soil conservation practice have 575
#MANUAL, NEED TO MODIFY IT
#1st, there are 2 '0' values for households with no crops, we replace it by ''
HouseholdVietnam_2C$d14 <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C$d14)
for (i in c(989:997)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}

#Then, there are 7 values with answers to column corresponding to each practices,
#but not on column 13, we replace '' by '0':
HouseholdVietnam_2C$d14 <- ifelse(HouseholdVietnam_2C$d14 == '' & HouseholdVietnam_2C$d140 == 2, '0', HouseholdVietnam_2C$d14)

#Check for d151_1 & d151_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d151_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d151_2))
#Check for d152_1 & d152_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d152_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d152_2))
#Check for d153_1 & d153_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d153_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d153_2))
#Check for d154_1 & d154_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d154_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d154_2))
#Check for d155_1 & d155_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d155_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d155_2))
#Check for d156_1 & d156_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d156_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d156_2))
#Check for d157_1 & d157_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d157_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d157_2))
#Check for d158_1 & d158_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d158_1 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d158_2))

#We have 3 answers for d16 for hhs without lowland or upland
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C$d16 <- as.character(HouseholdVietnam_2C$d16)
HouseholdVietnam_2C$d16 <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C$d16)
HouseholdVietnam_2C$d16 <- as.factor(HouseholdVietnam_2C$d16)

#For d18, there are different issues solved below
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C$d18 <- ifelse(HouseholdVietnam_2C$d181 == '1' & HouseholdVietnam_2C$d18 == '', '1', HouseholdVietnam_2C$d18)
HouseholdVietnam_2C$d18 <- ifelse(HouseholdVietnam_2C$d1899 == '1' & HouseholdVietnam_2C$d18 == '', as.character('99'), HouseholdVietnam_2C$d18)
#For d17, there are 3 '0' values for households with no crops, we replace it by ''
HouseholdVietnam_2C$d17 <- as.character(HouseholdVietnam_2C$d17)
HouseholdVietnam_2C$d17 <- ifelse(HouseholdVietnam_2C$d17 != '2' & !is.na(HouseholdVietnam_2C$d18), '2', HouseholdVietnam_2C$d17)
HouseholdVietnam_2C$d17 <- ifelse(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2), NA, HouseholdVietnam_2C$d17)
HouseholdVietnam_2C$d17 <- as.factor(HouseholdVietnam_2C$d17)

## 3 NA remains for households with crops, see later if necesary to change it
#for "no practice"

#Check for d18_11 & d18_12, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_11 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_12))
#Check for d18_21 & d18_22, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_21 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_22))
#Check for d18_31 & d18_32, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_31 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_32))
#Check for d18_41 & d18_42, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_41 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_42))
#Check for d18_51 & d18_52, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_51 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_52))
#Check for d18_61 & d18_62, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_61 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_62))
#Check for d18_71 & d18_72, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_71 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_72))
#Check for d18_81 & d18_82, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_81 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_82))
#Check for d18_91 & d18_92, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_91 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_92))
#Check for d18_101 & d18_102, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_101 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_102))
#Check for d18_111_a & d18_112, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d18_111_a != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d18_112))

#For d19, one hh answered while he mentionned to have implemented this practice
HouseholdVietnam_2C$d19 <- as.character(HouseholdVietnam_2C$d19)
HouseholdVietnam_2C$d19 <- ifelse(HouseholdVietnam_2C$d17 == '2' & !is.na(HouseholdVietnam_2C$d19), NA, HouseholdVietnam_2C$d19)
HouseholdVietnam_2C$d19 <- as.factor(HouseholdVietnam_2C$d19)

#For d20 & d21, there are different issues solved below
#MANUAL, NEED TO MODIFY IT
HouseholdVietnam_2C$d21 <- ifelse(HouseholdVietnam_2C$d2110 == '1' & HouseholdVietnam_2C$d21 == '', '10', HouseholdVietnam_2C$d21)
#For d20, different issues solved below
HouseholdVietnam_2C$d20 <- as.character(HouseholdVietnam_2C$d20)
HouseholdVietnam_2C$d20 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d20)
HouseholdVietnam_2C$d20 <- ifelse(!is.na(HouseholdVietnam_2C$no_crop1) & !is.na(HouseholdVietnam_2C$no_crop2) & is.na(HouseholdVietnam_2C$d20), '0', HouseholdVietnam_2C$d20)
HouseholdVietnam_2C$d20 <- as.factor(HouseholdVietnam_2C$d20)
x <- cbind(HouseholdVietnam_2C$no_crop1,HouseholdVietnam_2C$no_crop2,HouseholdVietnam_2C$d20)

#Check for d21_12 & d21_13, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_12 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_13))
#Check for d21_22 & d21_23, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_22 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_23))
#Check for d21_32 & d21_33, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_32 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_33))
#Check for d21_42 & d21_43, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_42 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_43))
#Check for d21_52 & d21_53, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_52 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_53))
#Check for d21_62 & d21_63, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_62 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_63))
#Check for d21_72 & d21_73, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_72 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_73))
#Check for d21_82 & d21_83, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_82 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_83))
#Check for d21_92 & d21_93, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_92 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_93))
#Check for d21_102 & d21_103, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_102 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_103))
#Check for d21_112 & d21_113, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_112 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_113))
#Check for d21_122 & d21_123, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_122 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_123))
#Check for d21_132 & d21_133, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_132 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_133))
#Check for d21_992 & d21_993, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d21_992 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d21_993))

#For d22, one hh answered while he mentionned to have implemented this practice
HouseholdVietnam_2C$d22 <- as.character(HouseholdVietnam_2C$d22)
HouseholdVietnam_2C$d22 <- ifelse(HouseholdVietnam_2C$d20 == '2' & !is.na(HouseholdVietnam_2C$d22), NA, HouseholdVietnam_2C$d22)
HouseholdVietnam_2C$d22 <- as.factor(HouseholdVietnam_2C$d22)

#One household didn't answered to d22 but we don't fulfil it as we don't know his answer

#For d24, different issues solved below
HouseholdVietnam_2C$d24 <- as.character(HouseholdVietnam_2C$d24)
HouseholdVietnam_2C$d24 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d24)
HouseholdVietnam_2C$d24 <- as.factor(HouseholdVietnam_2C$d24)

#For d26, different issues solved below
HouseholdVietnam_2C$d26 <- as.character(HouseholdVietnam_2C$d26)
HouseholdVietnam_2C$d26 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d26)
HouseholdVietnam_2C$d26 <- as.factor(HouseholdVietnam_2C$d26)

#Check for d27_11 & d27_12, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_11 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_12))
#Check for d27_21 & d27_22, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_21 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_22))
#Check for d27_31 & d27_32, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_31 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_32))
#Check for d27_41 & d27_42, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_41 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_42))
#Check for d27_51 & d27_52, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_51 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_52))
#Check for d27_61 & d27_62, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_61 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_62))
#Check for d27_71 & d27_72, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_71 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_72))
#Check for d27_81 & d27_82, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_81 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_82))
#Check for d27_91 & d27_92, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_91 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_92))
#Check for d27_101 & d27_102, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_101 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_102))
#Check for d27_111_a & d27_112_a, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_111_a != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_112_a))
#Check for d27_121 & d27_122, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_121 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_122))
#Check for d27_131 & d27_132, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_131 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_132))
#Check for d27_141 & d27_142, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_141 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_142))
#Check for d27_991 & d27_992, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & HouseholdVietnam_2C$d27_991 != '')
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d27_992))

#For d28, different issues solved below
HouseholdVietnam_2C$d28 <- as.character(HouseholdVietnam_2C$d28)
HouseholdVietnam_2C$d28 <- ifelse(is.na(HouseholdVietnam_2C$d26) & !is.na(HouseholdVietnam_2C$d28), NA, HouseholdVietnam_2C$d28)
HouseholdVietnam_2C$d28 <- as.factor(HouseholdVietnam_2C$d28)

#For d30_1, different issues solved below
HouseholdVietnam_2C$d30_1 <- as.character(HouseholdVietnam_2C$d30_1)
HouseholdVietnam_2C$d30_1 <- ifelse(HouseholdVietnam_2C$d30_2 == '' & HouseholdVietnam_2C$d30_1 == '1' , '0', HouseholdVietnam_2C$d30_1)
HouseholdVietnam_2C$d30_1 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d30_1)
HouseholdVietnam_2C$d30_1 <- as.factor(HouseholdVietnam_2C$d30_1)

#Check for d30_3 & d30_4, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d30_3))
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d30_4))
#Check for d30_5 & d30_6, Crops for which you use this practice and main motivation
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d30_5))
sum(is.na(HouseholdVietnam_2C$no_crop1) & is.na(HouseholdVietnam_2C$no_crop2) & !is.na(HouseholdVietnam_2C$d30_6))

#For d32 & d32_2, different issues solved below
HouseholdVietnam_2C$d32 <- as.character(HouseholdVietnam_2C$d32)
HouseholdVietnam_2C$d32 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d32)
HouseholdVietnam_2C$d32 <- ifelse(HouseholdVietnam_2C$d32_1 == '' & HouseholdVietnam_2C$d32 == '2' , '1', HouseholdVietnam_2C$d32)
HouseholdVietnam_2C$d32 <- as.factor(HouseholdVietnam_2C$d32)
HouseholdVietnam_2C$d32_2 <- as.character(HouseholdVietnam_2C$d32_2)
HouseholdVietnam_2C$d32_2 <- ifelse(HouseholdVietnam_2C$no_crop1 == '' & HouseholdVietnam_2C$no_crop2 == '' , NA, HouseholdVietnam_2C$d32_2)
HouseholdVietnam_2C$d32_2 <- as.factor(HouseholdVietnam_2C$d32_2)

#For module E3. Cattle and buffalo system, households having other kind of cattle 
#didn't fulfil this part

#One household is raising cattle/buffalo and didn't fulfil E3 = Nothing more to do...

#There were 317 households raised buffalo and cattle as important animals. However, I found 328 
#households participated in the module E3. In fact, these households raise pig and the data
#In module E3 is probably the data supposed to be in module E4, so we'll transfer these data at the right place
#First we set columns in the same order
HouseholdVietnam_2C <- HouseholdVietnam_2C %>% relocate(e3899 , .after = e3888)
#Then we extract household id of concerned households
x <- HouseholdVietnam_2C[HouseholdVietnam_2C$e2_1 == '0' & HouseholdVietnam_2C$e2_2 == '0' & !is.na(HouseholdVietnam_2C$e16),]
hid <- x$o9
#And finally we create a loop to replace the values in E4 and remove them from E3
for (i in hid){
  HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == i,c(2044:2102)] <- HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == i,c(1982:1996,1998:2041)]
  HouseholdVietnam_2C[HouseholdVietnam_2C$o9 == i,c(1982:1996,1998:2041)] <- 'NA'
}

#For module E4, There are 11 households raising pigs without information at E4,
#nothing more to do here

#For module E5, A lot of households are raising poultry (mainly chicken), but didn't fulfiled E5
#Nothing more to do here for now
#There were 391 households raised poultries as important animals, but 378 households participated
#in the module E5. So please check and impute missing data for 13 households (in fact more than 13)

#Only households raised buffalo and cattle as important animals (317) will be asked e58,
#but we have 338 ones replied the question in reality. So please check and redundant data
#We create a loop to solve this issue
for (i in 2165:2178){
HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C$e2_1 == '0' & HouseholdVietnam_2C$e2_2 == '0',
                                             NA,HouseholdVietnam_2C[,i])
HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}
summary(HouseholdVietnam_2C$b1)
# # #d. Corresponding fields - Additionnal checks
#The following part is based on Laos script but we removed all parts for which
#no issue was highligted for Laos, assuming no issue was permitted by Kobo for 
#these fields

#b1:b10:b11
x <- HouseholdVietnam_2C[,c(1,80,230,239)]

#5 households declared that they sold crops products but did not anwered to b10 & b11
#HHID: 3044,3054,3303,3551,3222
#As we cannot know their answer, we cannot solve this issue

#c2:c3:c3a:c4:c5:c9:c6:c7:c8:c10:c11
x <- HouseholdVietnam_2C[,c(1,591,592,603,605,607,609,622:626)]

#For c6, c7 and c8, many unanswered cells, we'll replace it by "no", 
#and also replace "Yes" by "yes" and "No" by "no" to homogenize the answers
for (i in c(581:585)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C$c3a != '' & HouseholdVietnam_2C[,i] == '', 'no',HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C[,i] == 'Yes', 'yes',HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C[,i] == 'No', 'no',HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}

#b1:b12_1:b12_2
x <- HouseholdVietnam_2C[,c(1,80,697,700)]

#Many households declared at b1 that they sold agri products but did not answered ar b12_1 and b12_2
#As we cannot know their answer, we cannot solve this issue

#b12_2:b13_02-b13_0211:b13_2:b14_2-b14_288:b15_2
x <- HouseholdVietnam_2C[,c(1,700,728:750,752)]

#For b13_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it
#For b14_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it
#For b15_2 and related answers => Many answers are missing but we don't have this
#information so we cannot solve it

#d9_1:d9_2
x <- HouseholdVietnam_2C[,c(1,847,848)]

#HHID: 3084,3433,3438,3447,3568 declared plots rented out but no information of number of plots
#As we cannot know their answer, we cannot solve this issue

#e5_5:e5_51
x <- HouseholdVietnam_2C[,c(1,1920,1921)]

#HHID: 3048, 3045, 3035 the selling price is missing, REPLACE
#As we cannot know their answer, we cannot solve this issue

#e7_5:e7_51
x <- HouseholdVietnam_2C[,c(1,1967,1968)]

#HHID: 3162,3147,3316 the selling price is missing, REPLACE
#As we cannot know their answer, we cannot solve this issue

#e12_3:e12_4
x <- HouseholdVietnam_2C[,c(1,1974:1975)]

#Some households declared 0,1 or 2 m2 of pasture, we remove it
HouseholdVietnam_2C$e12_4 <- ifelse(HouseholdVietnam_2C$e12_4 < 3, NA, HouseholdVietnam_2C$e12_4)
HouseholdVietnam_2C$e12_3 <- as.character(HouseholdVietnam_2C$e12_3)
HouseholdVietnam_2C$e12_3 <- ifelse(HouseholdVietnam_2C$e12_3 == 'Yes' & is.na(HouseholdVietnam_2C$e12_4), 'No', HouseholdVietnam_2C$e12_3)
HouseholdVietnam_2C$e12_3 <- as.factor(HouseholdVietnam_2C$e12_3)

#e2_3:e30:e31:e32:e33:e34:e341-e3499
x <- HouseholdVietnam_2C[,c(17,1873,2042,2044:2058)]

#For many households who raise pigs, no information about pig raising systems 
#(Probably households with only adult pigs)
#As we cannot know their answer, we cannot solve this issue

#e2_8 :e44:e45:e46:e47:e48:e481-e4899
x <- HouseholdVietnam_2C[,c(17,1878,2103,2105:2119)]

#For some households we have no information about their raising systems
#As we cannot know their answer, we cannot solve this issue
#For other households, there are information but they are no raising chickens, we remove these informations
for (i in c(2072,2074:2088)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C$e2_8 == 0, '', HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}

#e44:e49:e50:e51:e52:e521-e5299:e53:e54:e56
x <- HouseholdVietnam_2C[,c(17,2103,2120:2134,2136,2143,2154)]

#For some households we have no information about their raising systems
#As we cannot know their answer, we cannot solve this issue
#For other households, there are information but they are no raising pigs, we remove these informations
for (i in c(2089:2103,2105,2111,2113)){
  HouseholdVietnam_2C[,i] <- as.character(HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- ifelse(HouseholdVietnam_2C$e2_8 == 0, '', HouseholdVietnam_2C[,i])
  HouseholdVietnam_2C[,i] <- as.factor(HouseholdVietnam_2C[,i])
}




## 4.2C Data cleaning for "HouMemberVietnam_2"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(HouMemberVietnam_2C$pid))
#Check and remove the real duplicates through automated method
Dum <- HouMemberVietnam_2C[duplicated(HouMemberVietnam_2C$pid),]
idDup <- unique(Dum$hhid_re1)
DumReal <- HouMemberVietnam_2C[HouMemberVietnam_2C$hhid_re1 %in% idDup,]
DumReal$check <- paste(DumReal$p_no, DumReal$hhid_re1, DumReal$a2, DumReal$a4_1)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
HouMemberVietnam_2C <- HouMemberVietnam_2C[!rownames(HouMemberVietnam_2C) %in% Duplic,]
#For some duplicates, we have to do it manually:
#TO ADAPT TO EACH DATABASE
HouMemberVietnam_2C <- HouMemberVietnam_2C[-c(21,2443),]
#Re-attribute household members numbers when the household id is duplicated for different household members
Dum <- HouMemberVietnam_2C[duplicated(HouMemberVietnam_2C$pid),]
idDup <- unique(Dum$hhid_re1)
for (i in idDup){
  HouMemberVietnam_2C$p_no[HouMemberVietnam_2C$hhid_re1 == i] <- 1:sum(HouMemberVietnam_2C$hhid_re1 == i)
}
HouMemberVietnam_2C$pid <- paste(HouMemberVietnam_2C$hhid_re1,HouMemberVietnam_2C$p_no)
#Check again the duplicates: 
count_if("TRUE",duplicated(HouMemberVietnam_2C$pid))

## 4.3 Data cleaning for "ClowlandVietnam_2C"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(ClowlandVietnam_2C$pid))
#Check and remove the real duplicates through automated method
Dum <- ClowlandVietnam_2C[duplicated(ClowlandVietnam_2C$pid),]
idDup <- unique(Dum$hhid_re2)
DumReal <- ClowlandVietnam_2C[ClowlandVietnam_2C$hhid_re2 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_13v, DumReal$d2_132)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
Dupli$pid
ClowlandVietnam_2C <- ClowlandVietnam_2C[!rownames(ClowlandVietnam_2C) %in% Duplic,]
#Check again the duplicates:

count_if("TRUE",duplicated(ClowlandVietnam_2C$pid))
Dum <- ClowlandVietnam_2C[duplicated(ClowlandVietnam_2C$pid),]
idDup <- unique(Dum$pid)
DumReal <- ClowlandVietnam_2C[ClowlandVietnam_2C$pid %in% idDup,]

# # #b Outliers part 1
ClowlandVietnam_2C$d2_133 <- as.character(ClowlandVietnam_2C$d2_133)
ClowlandVietnam_2C$d2_133 <- ifelse(ClowlandVietnam_2C$d2_13e == "Spring onion", "Tuber", ClowlandVietnam_2C$d2_133)
ClowlandVietnam_2C$d2_133 <- as.factor(ClowlandVietnam_2C$d2_133)

#Check amount of seeds according to Ky excel file


## 4.4 Data cleaning for "CuplandVietnam_2"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(CuplandVietnam_2$pid))
#Check and remove the real duplicates through automated method
Dum <- CuplandVietnam_2[duplicated(CuplandVietnam_2$pid),]
idDup <- unique(Dum$hhid_re3)
DumReal <- CuplandVietnam_2[CuplandVietnam_2$hhid_re3 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_13v, DumReal$d2_132)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
CuplandVietnam_2 <- CuplandVietnam_2[!rownames(CuplandVietnam_2) %in% Duplic,]
#Check again the duplicates: 
count_if("TRUE",duplicated(CuplandVietnam_2$pid))


#We add the labels

HouseholdVietnam_2C <- copy_labels(HouseholdVietnam_2C, HouseholdVietnam)
HouMemberVietnam_2C <- copy_labels(HouMemberVietnam_2C, HouMemberVietnam)
ClowlandVietnam_2C <- copy_labels(ClowlandVietnam_2C, ClowlandVietnam)
CuplandVietnam_2 <- copy_labels(CuplandVietnam_2, CuplandVietnam)


###5. Data export


##3.3 Export of the database under dta format
saveRDS(HouseholdVietnam_2C, "HouseholdVietnam_2C.rds")
saveRDS(HouMemberVietnam_2C, "HouMemberVietnam_2C.rds")
saveRDS(ClowlandVietnam_2C, "ClowlandVietnam_2C.rds")
saveRDS(CuplandVietnam_2, "CuplandVietnam_2.rds")


#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 30L)
