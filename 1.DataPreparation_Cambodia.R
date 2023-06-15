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
#We import all the dta files transmitted by Ky as *Cambodia* database
HouseholdCambodia <- read_dta("asset_household_survey_Cambodia.dta")
ClowlandCambodia <- read_dta("d2_1_lowland_Cambodia.dta")
CuplandCambodia <- read_dta("d2_2_upland_Cambodia.dta")
HouMemberCambodia <- read_dta("household_rooster_Cambodia.dta")
HomegardenCambodia <- read_dta("d2_3_homegarden_cambodia.dta")

### 2. Columns and sub-columns name changing
##2.1. Work on the first database: "HouseholdCambodia" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

# # #o.
#C1 = FACT
#C2 = FACT
#C3 = FACT
#C4 = ? (Probably Kobo ID, maybe necessary to replace it later)
#C5 = REMOVE (Text from the questionnaire)
#C6 = REMOVE (Text from the questionnaire)
#C7 = FACT, Change the label: "Consent: 1 = yes, 2 = no"
var_label(HouseholdCambodia$consent) <- "Consent: 1 = yes, 2 = no"
#C8 = FACT 
#C9 = FACT, (Enumerators names = number, correspondence table)
#C10 = OK, (Empty, decide if necessary to remove it or not)
#C11 = FACT, (Supervisors names = number, correspondence table)
#C12 = OK, (Empty, decide if necessary to remove it or not)
#C13 = FACT, (Each country = number, Cambodia = 3)
#C14 = FACT, (Each province = number starting by the country number, only one province in Cambodia)
#C15 = FACT, (Each district = number starting by the country and province number)
#C16 = FACT, (Each commune = specific number)
#C17 = FACT, (Each village = number starting by the country, province and district number)
#C18 = FACT, check duplicates, (starting by country id) = 10 duplicates... 
#Normally it is one of the issue for which Ky should find a solution
count_if("TRUE",duplicated(HouseholdCambodia[,18]))
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
var_label(HouseholdCambodia$header_name_preload) <- "header's name"
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
#C54 = FACT (the answer is still in Khmer, Ky will maybe solve it)
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
var_label(HouseholdCambodia$a151) <- "Death or health problem of one household member"
#C70 = FACT, replace label: "Harvest failure"
var_label(HouseholdCambodia$a152) <- "Harvest failure"
#C71 = FACT, replace label: "Animal loss"
var_label(HouseholdCambodia$a153) <- "Animal loss"
#C72 = FACT, replace label: "Unsold harvest or animals"
var_label(HouseholdCambodia$a154) <- "Unsold harvest or animals"
#C73 = FACT, replace label: "More labor saved from mechanization"
var_label(HouseholdCambodia$a155) <- "More labor saved from mechanization"
#C74 = FACT, replace label: "Less or no longer available non-agricultural work"
var_label(HouseholdCambodia$a156) <- "Less or no longer available non-agricultural work"
#C75 = FACT, replace label: "Need an extra income for the family's expenses"
var_label(HouseholdCambodia$a157) <- "Need an extra income for the family's expenses"
#C76 = FACT, replace label: "Need an extra income for paying loan / debt"
HouseholdCambodia <- HouseholdCambodia %>% relocate(a158 , .after = a157)
var_label(HouseholdCambodia$a158) <- "Need an extra income for paying loan / debt"
#C77 = FACT, replace label: "Other" 
var_label(HouseholdCambodia$a1599) <- "Other"
#C78 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$a1588) <- "Do not know"
#C79 = OK, (empty)

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
var_label(HouseholdCambodia$b2_11) <- "Dried/ fermented vegetables (carrot, mustard…)"
#C84 = FACT, replace label: "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
var_label(HouseholdCambodia$b2_12) <- "Dried/ fermented fruits or tuber (apricot, plum, banana, sweet potatoes…)"
#C85 = FACT, replace label: "Dried/ fermented bambooshoot"
var_label(HouseholdCambodia$b2_13) <- "Dried/ fermented bambooshoot"
#C86 = FACT, replace label: "Selling other products"
var_label(HouseholdCambodia$b2_199) <- "Selling other products"
#C87 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C88 = FACT, (table of correspondence), (see answers below)
#Dried/fermented meat (pork, beef, etc.)
#Honey
#Cheese
#Selling other products
#C89 = FACT, replace label: "Dried/fermented meat (pork, beef, etc.)"
var_label(HouseholdCambodia$b2_21) <- "Dried/fermented meat (pork, beef, etc.)"
#C90 = FACT, replace label: "Honey"
var_label(HouseholdCambodia$b2_22) <- "Honey"
#C91 = FACT, replace label: "Cheese"
var_label(HouseholdCambodia$b2_23) <- "Cheese"
#C92 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C93 = FACT, replace label: "Selling other products"
var_label(HouseholdCambodia$b2_299) <- "Selling other products"
#C94 = OK, (empty)
#C95 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C96 = FACT, replace label: "Selling own vegetables"
var_label(HouseholdCambodia$b31) <- "Selling own vegetables"
#C97 = FACT, replace label: "Selling own fruits"
var_label(HouseholdCambodia$b32) <- "Selling own fruits"
#C98 = FACT, replace label: "Selling rice from the farm"
var_label(HouseholdCambodia$b33) <- "Selling rice from the farm"
#C99 = FACT, replace label: "Selling cassava from the farm"
var_label(HouseholdCambodia$b34) <- "Selling cassava from the farm"
#C100 = FACT, replace label: "Selling cashew from the farm"
var_label(HouseholdCambodia$b35) <- "Selling cashew from the farm"
#C101 = FACT, replace label: "Selling soybean from the farm"
var_label(HouseholdCambodia$b36) <- "Selling soybean from the farm"
#C102 = FACT, replace label: "Selling cover crop seed from the farm"
var_label(HouseholdCambodia$b37) <- "Selling cover crop seed from the farm"
#C103 = FACT, replace label: "Selling other crops from the farm"
var_label(HouseholdCambodia$b38) <- "Selling other crops from the farm"
#C104 = FACT, replace label: "Selling cattle and buffalo"
var_label(HouseholdCambodia$b39) <- "Selling cattle and buffalo"
#C105 = FACT, replace label: "Selling pigs"
var_label(HouseholdCambodia$b310) <- "Selling pigs"
#C106 = FACT, replace label: "Selling poultry"
var_label(HouseholdCambodia$b311) <- "Selling poultry"
#C107 = FACT, replace label: "Selling other farm products"
var_label(HouseholdCambodia$b312) <- "Selling other farm products"
#C108 = FACT, replace label: "Sell derived/processed products"
var_label(HouseholdCambodia$b313) <- "Sell derived/processed products"
#C109 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
var_label(HouseholdCambodia$b314) <- "Non-farm wages (salaried work in private or public company)"
#C110 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdCambodia$b315) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C111 = FACT, replace label: "Selling labor"
var_label(HouseholdCambodia$b316) <- "Selling labor"
#C112 = FACT, replace label: "Remittances"
var_label(HouseholdCambodia$b317) <- "Remittances"
#C113 = FACT, replace label: "Pension"
var_label(HouseholdCambodia$b318) <- "Pension"
#C114 = FACT, replace label: "Rented land"
var_label(HouseholdCambodia$b319) <- "Rented land"
#C115 = FACT, replace label: "Financial support / gift"
var_label(HouseholdCambodia$b320) <- "Financial support / gift"
#C116 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b399) <- "Other"
#C117 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$b388) <- "Do not know"
#C118 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C119 = OK, (the answer is still in Vietnamese, Ky will maybe solve it)
#C120 = FACT, (Table of correspondence)
#C121 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C122 = FACT, (Table of correspondence)
#C123 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C124 = FACT, (Table of correspondence)
#C125 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C126 = OK, change label name for "What is the proportion of total household income from b4_1"
var_label(HouseholdCambodia$b5_1) <- "What is the proportion of total household income from b4_1"
#C127 = NUMERIC, Useless column
#C128 = OK, change label name for "What is the proportion of total household income from b4_2"
var_label(HouseholdCambodia$b5_2) <- "What is the proportion of total household income from b4_2"
#C129 = NUMERIC, Useless column
#C130 = OK, change label name for "What is the proportion of total household income from b4_3"
var_label(HouseholdCambodia$b5_3) <- "What is the proportion of total household income from b4_3"
#C131 = FACT, (bin), (0 = no, 1 = yes)
#C132 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C133 = FACT, replace label: "Selling own vegetables"
var_label(HouseholdCambodia$b71) <- "Selling own vegetables"
#C134 = FACT, replace label: "Selling own fruits"
var_label(HouseholdCambodia$b72) <- "Selling own fruits"
#C135 = FACT, replace label: "Selling rice from the farm"
var_label(HouseholdCambodia$b73) <- "Selling rice from the farm"
#C136 = FACT, replace label: "Selling cassava from the farm"
var_label(HouseholdCambodia$b74) <- "Selling cassava from the farm"
#C137 = FACT, replace label: "Selling cashew from the farm"
var_label(HouseholdCambodia$b75) <- "Selling cashew from the farm"
#C138 = FACT, replace label: "Selling soybean from the farm"
var_label(HouseholdCambodia$b76) <- "Selling soybean from the farm"
#C139 = FACT, replace label: "Selling cover crop seed from the farm"
var_label(HouseholdCambodia$b77) <- "Selling cover crop seed from the farm"
#C140 = FACT, replace label: "Selling other crops from the farm"
var_label(HouseholdCambodia$b78) <- "Selling other crops from the farm"
#C141 = FACT, replace label: "Selling cattle and buffalo"
var_label(HouseholdCambodia$b79) <- "Selling cattle and buffalo"
#C142 = FACT, replace label: "Selling pigs"
var_label(HouseholdCambodia$b710) <- "Selling pigs"
#C143 = FACT, replace label: "Selling poultry"
var_label(HouseholdCambodia$b711) <- "Selling poultry"
#C144 = FACT, replace label: "Selling other farm products"
var_label(HouseholdCambodia$b712) <- "Selling other farm products"
#C145 = FACT, replace label: "Sell derived/processed products"
var_label(HouseholdCambodia$b713) <- "Sell derived/processed products"
#C146 = FACT, replace label: "Non-farm wages (salaried work in private or public company)"
var_label(HouseholdCambodia$b714) <- "Non-farm wages (salaried work in private or public company)"
#C147 = FACT, replace label: "Non-farm income (own business: shop, trader/collector etc.)"
var_label(HouseholdCambodia$b715) <- "Non-farm income (own business: shop, trader/collector etc.)"
#C148 = FACT, replace label: "Selling labor"
var_label(HouseholdCambodia$b716) <- "Selling labor"
#C149 = FACT, replace label: "Remittances"
var_label(HouseholdCambodia$b717) <- "Remittances"
#C150 = FACT, replace label: "Pension"
var_label(HouseholdCambodia$b718) <- "Pension"
#C151 = FACT, replace label: "Rented land"
var_label(HouseholdCambodia$b719) <- "Rented land"
#C152 = FACT, replace label: "Financial support / gift"
var_label(HouseholdCambodia$b720) <- "Financial support / gift"
#C153 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b799) <- "Other"
#C154 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$b788) <- "Do not know"
#C155 = CHAR, (empty)
#C156 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C157 = FACT, (1 = yes, 0 = no)
#C158 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C159 = FACT, replace label: "Jan"
var_label(HouseholdCambodia$b8_11) <- "Jan"
#C160 = FACT, replace label: "Feb"
var_label(HouseholdCambodia$b8_12) <- "Feb"
#C161 = FACT, replace label: "Mar"
var_label(HouseholdCambodia$b8_13) <- "Mar"
#C162 = FACT, replace label: "Apr"
var_label(HouseholdCambodia$b8_14) <- "Apr"
#C163 = FACT, replace label: "May"
var_label(HouseholdCambodia$b8_15) <- "May"
#C164 = FACT, replace label: "Jun"
var_label(HouseholdCambodia$b8_16) <- "Jun"
#C165 = FACT, replace label: "Jul"
var_label(HouseholdCambodia$b8_17) <- "Jul"
#C166 = FACT, replace label: "Aug"
var_label(HouseholdCambodia$b8_18) <- "Aug"
#C167 = FACT, replace label: "Sep"
var_label(HouseholdCambodia$b8_19) <- "Sep"
#C168 = FACT, replace label: "Oct"
var_label(HouseholdCambodia$b8_110) <- "Oct"
#C169 = FACT, replace label: "Nov"
var_label(HouseholdCambodia$b8_111) <- "Nov"
#C170 = FACT, replace label: "Dec"
var_label(HouseholdCambodia$b8_112) <- "Dec"
#C171 = FACT, (bin)
#C172 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C173 = FACT, replace label: "Jan"
var_label(HouseholdCambodia$b9_11) <- "Jan"
#C174 = FACT, replace label: "Feb"
var_label(HouseholdCambodia$b9_12) <- "Feb"
#C175 = FACT, replace label: "Mar"
var_label(HouseholdCambodia$b9_13) <- "Mar"
#C176 = FACT, replace label: "Apr"
var_label(HouseholdCambodia$b9_14) <- "Apr"
#C177 = FACT, replace label: "May"
var_label(HouseholdCambodia$b9_15) <- "May"
#C178 = FACT, replace label: "Jun"
var_label(HouseholdCambodia$b9_16) <- "Jun"
#C179 = FACT, replace label: "Jul"
var_label(HouseholdCambodia$b9_17) <- "Jul"
#C180 = FACT, replace label: "Aug"
var_label(HouseholdCambodia$b9_18) <- "Aug"
#C181 = FACT, replace label: "Sep"
var_label(HouseholdCambodia$b9_19) <- "Sep"
#C182 = FACT, replace label: "Oct"
var_label(HouseholdCambodia$b9_110) <- "Oct"
#C183 = FACT, replace label: "Nov"
var_label(HouseholdCambodia$b9_111) <- "Nov"
#C184 = FACT, replace label: "Dec"
var_label(HouseholdCambodia$b9_112) <- "Dec"
#C185 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C186 = FACT, replace label: "Non-working members went out to look for work"
var_label(HouseholdCambodia$b9_21) <- "Non-working members went out to look for work"
#C187 = FACT, replace label: "b9_2/2 Working members increased work hours"
var_label(HouseholdCambodia$b9_22) <- "Working members increased work hours"
#C188 = FACT, replace label: "Reducing / postpone own farm work and sale labor to the others"
var_label(HouseholdCambodia$b9_23) <- "Reducing / postpone own farm work and sale labor to the others"
#C189 = FACT, replace label: "One or more members changes residence"
var_label(HouseholdCambodia$b9_24) <- "One or more members changes residence"
#C190 = FACT, replace label: "Spent savings"
var_label(HouseholdCambodia$b9_25) <- "Spent savings"
#C191 = FACT, replace label: "Taking loan"
var_label(HouseholdCambodia$b9_26) <- "Taking loan"
#C192 = FACT, replace label: "Increase loan"
var_label(HouseholdCambodia$b9_27) <- "Increase loan"
#C193 = FACT, replace label: "Went into debt"
var_label(HouseholdCambodia$b9_28) <- "Went into debt"
#C194 = FACT, replace label: "Sold property or assets"
var_label(HouseholdCambodia$b9_29) <- "Sold property or assets"
#C195 = FACT, replace label: "Withdrew children from school"
var_label(HouseholdCambodia$b9_210) <- "Withdrew children from school"
#C196 = FACT, replace label: "Decreased food expenses"
var_label(HouseholdCambodia$b9_211) <- "Decreased food expenses"
#C197 = FACT, replace label: "Changed agricultural practices"
var_label(HouseholdCambodia$b9_212) <- "Changed agricultural practices"
#C198 = FACT, replace label: "Sold animal (cattle, buffalos…)"
var_label(HouseholdCambodia$b9_213) <- "Sold animal (cattle, buffalos…)"
#C199 = FACT, replace label: "Did nothing"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(b9_20 , .after = b9_213)
var_label(HouseholdCambodia$b9_20) <- "Did nothing"
#C200 = FACT, replace label: "Other"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(b9_299 , .after = b9_20)
var_label(HouseholdCambodia$b9_299) <- "Other"
#C201 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C202 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C203 = FACT, replace label: "Community-based tourism or agroecological tourism"
var_label(HouseholdCambodia$b9_31) <- "Community-based tourism or agroecological tourism"
#C204 = FACT, replace label: "Hosting events (e.g. for projects from NGOs, research)"
var_label(HouseholdCambodia$b9_32) <- "Hosting events (e.g. for projects from NGOs, research)"
#C205 = FACT, replace label: "Education and training of others (e.g. training of other farmers, school visits)"
var_label(HouseholdCambodia$b9_33) <- "Education and training of others (e.g. training of other farmers, school visits)"
#C206 = FACT, replace label: "Food processing"
var_label(HouseholdCambodia$b9_34) <- "Food processing"
#C207 = FACT, replace label: "Restaurant"
var_label(HouseholdCambodia$b9_35) <- "Restaurant"
#C208 = FACT, replace label: "Selling products from other farms"
var_label(HouseholdCambodia$b9_36) <- "Selling products from other farms"
#C209 = FACT, replace label: "None of the above"
var_label(HouseholdCambodia$b9_30) <- "None of the above"
#C210 = FACT, replace label: "I do not know"
var_label(HouseholdCambodia$b9_388) <- "I do not know"
#C211 = FACT, (Table of correspondence), (see answers below)
#I do not sell (crops/vegetables/fruits or livestock) = NA
#The local market ( lower or equal to district level market)
#The provincial or national market
#Export
#Other
#Do not know
#C212 = OK, (empty)
#C213 = FACT, (Table of correspondence), (see answers below)
#China
#South East Asia
#East Asia
#Europe
#US
#Other left countries
#C214 = FACT, replace label: "China"
var_label(HouseholdCambodia$b10_11) <- "China"
#C215 = FACT, replace label: "South East Asia"
var_label(HouseholdCambodia$b10_12) <- "South East Asia"
#C216 = FACT, replace label: "East Asia"
var_label(HouseholdCambodia$b10_13) <- "East Asia"
#C217 = FACT, replace label: "Europe"
var_label(HouseholdCambodia$b10_14) <- "Europe"
#C218 = FACT, replace label: "US"
var_label(HouseholdCambodia$b10_15) <- "US"
#C219 = FACT, replace label: "Other left countries"
var_label(HouseholdCambodia$b10_16) <- "Other left countries"
#C220 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$b10_188) <- "Do not know"
#C221 = FACT, (Table of correspondence), (see answers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C222 = FACT, (Table of correspondence)
#C223 = FACT, (Table of correspondence)
#C224 = FACT, (Table of correspondence)
#C225 = FACT, (0 = no, 1 = yes)
#C226 = FACT, (0 = no, 1 = yes)
#C227 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C228 = FACT, replace label: "Vegetables"
var_label(HouseholdCambodia$b22_0b1) <- "Vegetables"
#C229 = FACT, replace label: "Honey"
var_label(HouseholdCambodia$b22_0b9) <- "Honey"
#C230 = FACT, replace label: "Rice"
var_label(HouseholdCambodia$b22_0b11) <- "Rice"
#C231 = FACT, replace label: "Dried meat (pork, beef, etc.)"
var_label(HouseholdCambodia$b22_0b13) <- "Dried meat (pork, beef, etc.)"
#C232 = FACT, replace label: "Cassava"
var_label(HouseholdCambodia$b22_0b15) <- "Cassava"
#C233 = FACT, replace label: "Cashew nut"
var_label(HouseholdCambodia$b22_0b16) <- "Cashew nut"
#C234 = FACT, replace label: "Peanut"
var_label(HouseholdCambodia$b22_0b17) <- "Peanut"
#C235 = FACT, replace label: "Pepper"
var_label(HouseholdCambodia$b22_0b18) <- "Pepper"
#C236 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b22_0b99) <- "Other"
#C237 = OK, (empty)
#C238 =FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#Do not know
#C239 =FACT, (Table of correspondence), (same answer than previous)
#C240 =FACT, (Table of correspondence), (same answer than previous)
#C241 =FACT, (Table of correspondence), (same answer than previous)
#C242 =FACT, (Table of correspondence), (same answer than previous)
#C243 =FACT, (Table of correspondence), (same answer than previous)
#C244 =FACT, (Table of correspondence), (same answer than previous)
#C245 =FACT, (Table of correspondence), (same answer than previous)
#C246 =FACT, (Table of correspondence), (same answer than previous)
#C247 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C248 = FACT, replace label: "Reduced / no more demand from the cooperative / union or contracted company"
var_label(HouseholdCambodia$b22_0d1) <- "Reduced / no more demand from the cooperative / union or contracted company"
#C249 = FACT, replace label: "Technical standard is too complicated to follow"
var_label(HouseholdCambodia$b22_0d2) <- "Technical standard is too complicated to follow"
#C250 = FACT, replace label: "The pests are too much that need uses pesticides"
var_label(HouseholdCambodia$b22_0d3) <- "The pests are too much that need uses pesticides"
#C251 = FACT, replace label: "The soil fertility / yield drop too much that need to use chemical fertilizers"
var_label(HouseholdCambodia$b22_0d4) <- "The soil fertility / yield drop too much that need to use chemical fertilizers"
#C252 = FACT, replace label: "The neighbored plots are used the chemical inputs"
var_label(HouseholdCambodia$b22_0d5) <- "The neighbored plots are used the chemical inputs"
#C253 = FACT, replace label: "The premium price is too low"
var_label(HouseholdCambodia$b22_0d6) <- "The premium price is too low"
#C254 = FACT, replace label: "Reduced / no longer trust on the transparency of contract farming management /"
var_label(HouseholdCambodia$b22_0d7) <- "Reduced / no longer trust on the transparency of contract farming management /"
#C255 = FACT, replace label: "The payment is often delayed or too long, need quick payment"
var_label(HouseholdCambodia$b22_0d8) <- "The payment is often delayed or too long, need quick payment"
#C256 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b22_0d99) <- "Other"
#C257 = FACT, replace label: "Do not know"
HouseholdCambodia <- HouseholdCambodia %>% relocate(b22_0d88 , .after = b22_0d99)
var_label(HouseholdCambodia$b22_0d88) <- "Do not know"
#C258 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Other. Specify"
var_label(HouseholdCambodia$b22_0d_oth) <- "Other. Specify"
#C259 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C260 = FACT, replace label: "Vegetables"
var_label(HouseholdCambodia$b22_11) <- "Vegetables"
#C261 = FACT, replace label: "Honey"
var_label(HouseholdCambodia$b22_19) <- "Honey"
#C262 = FACT, replace label: "Rice"
var_label(HouseholdCambodia$b22_111) <- "Rice"
#C263 = FACT, replace label: "Dried meat (pork, beef, etc.)"
var_label(HouseholdCambodia$b22_113) <- "Dried meat (pork, beef, etc.)"
#C264 = FACT, replace label: "Cassava"
var_label(HouseholdCambodia$b22_115) <- "Cassava"
#C265 = FACT, replace label: "Cashew nut"
var_label(HouseholdCambodia$b22_116) <- "Cashew nut"
#C266 = FACT, replace label: "Peanut"
var_label(HouseholdCambodia$b22_117) <- "Peanut"
#C267 = FACT, replace label: "Pepper"
var_label(HouseholdCambodia$b22_118) <- "Pepper"
#C268 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b22_199) <- "Other"
#C269 = OK, (empty)
#C270 = FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#Do not know
#C271 = OK, (empty)
#C272 = OK, (empty)
#C273 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C274 = FACT, replace label: "Village collector"
var_label(HouseholdCambodia$b24_1a1) <- "Village collector"
#C275 = FACT, replace label: "Collector outside the village"
var_label(HouseholdCambodia$b24_1a2) <- "Collector outside the village"
#C276 = FACT, replace label: "Trader in the district"
var_label(HouseholdCambodia$b24_1a3) <- "Trader in the district"
#C277 = FACT, replace label: "Trader from the province"
var_label(HouseholdCambodia$b24_1a4) <- "Trader from the province"
#C278 = FACT, replace label: "Trader from another province"
var_label(HouseholdCambodia$b24_1a5) <- "Trader from another province"
#C279 = FACT, replace label: "Contractor-company from the province"
var_label(HouseholdCambodia$b24_1a6) <- "Contractor-company from the province"
#C280 = FACT, replace label: "Contractor-company from another province"
var_label(HouseholdCambodia$b24_1a7) <- "Contractor-company from another province"
#C281 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdCambodia$b24_1a8) <- "Cooperative of which you are a member"
#C282 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdCambodia$b24_1a9) <- "Cooperative of which you are not a member"
#C283 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdCambodia$b24_1a10) <- "Consumers on the farm"
#C284 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdCambodia$b24_1a11) <- "Local markets where you sell your products directly to final consumers"
#C285 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdCambodia$b24_1a12) <- "Consumers through online sales"
#C286 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdCambodia$b24_1a13) <- "Foreign trader (e.g. from China)"
#C287 = FACT, replace label: "Processors"
var_label(HouseholdCambodia$b24_1a14) <- "Processors"
#C288 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b24_1a99) <- "Other"
#C289 = FACT, replace label: "Do not know/No selling/no the second outlet"
var_label(HouseholdCambodia$b24_1a88) <- "Do not know/No selling/no the second outlet"
#C290 = OK, (empty)
#C291 = OK, (empty)
#C292 = OK, (empty)
#C293 = OK, (empty)
#C294 = OK, (empty)
#C295 = FACT, replace label: "Village collector", (empty)
var_label(HouseholdCambodia$b24_9a1) <- "Village collector"
#C296 = FACT, replace label: "Collector outside the village", (empty)
var_label(HouseholdCambodia$b24_9a2) <- "Collector outside the village"
#C297 = FACT, replace label: "Trader in the district", (empty)
var_label(HouseholdCambodia$b24_9a3) <- "Trader in the district"
#C298 = FACT, replace label: "Trader from the province", (empty)
var_label(HouseholdCambodia$b24_9a4) <- "Trader from the province"
#C299 = FACT, replace label: "Trader from another province", (empty)
var_label(HouseholdCambodia$b24_9a5) <- "Trader from another province"
#C300 = FACT, replace label: "Contractor-company from the province", (empty)
var_label(HouseholdCambodia$b24_9a6) <- "Contractor-company from the province"
#C301 = FACT, replace label: "Contractor-company from another province", (empty)
var_label(HouseholdCambodia$b24_9a7) <- "Contractor-company from another province"
#C302 = FACT, replace label: "Cooperative of which you are a member", (empty)
var_label(HouseholdCambodia$b24_9a8) <- "Cooperative of which you are a member"
#C303 = FACT, replace label: "Cooperative of which you are not a member", (empty)
var_label(HouseholdCambodia$b24_9a9) <- "Cooperative of which you are not a member"
#C304 = FACT, replace label: "Consumers on the farm", (empty)
var_label(HouseholdCambodia$b24_9a10) <- "Consumers on the farm"
#C305 = FACT, replace label: "Local markets where you sell your products directly to final consumers", (empty)
var_label(HouseholdCambodia$b24_9a11) <- "Local markets where you sell your products directly to final consumers"
#C306 = FACT, replace label: "Consumers through online sales", (empty)
var_label(HouseholdCambodia$b24_9a12) <- "Consumers through online sales"
#C307 = FACT, replace label: "Foreign trader (e.g. from China)", (empty)
var_label(HouseholdCambodia$b24_9a13) <- "Foreign trader (e.g. from China)"
#C308 = FACT, replace label: "Processors", (empty)
var_label(HouseholdCambodia$b24_9a14) <- "Processors"
#C309 = FACT, replace label: "Other", (empty)
var_label(HouseholdCambodia$b24_9a99) <- "Other"
#C310 = FACT, replace label: "Do not know/No selling/no the second outlet", (empty)
var_label(HouseholdCambodia$b24_9a88) <- "Do not know/No selling/no the second outlet"
#C311 = OK, (empty)
#C312 =FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C313 = OK, (empty)
#C314 = OK, (empty)
#C315 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C316 = FACT, replace label: "Village collector"
var_label(HouseholdCambodia$b24_11a1) <- "Village collector"
#C317 = FACT, replace label: "Collector outside the village"
var_label(HouseholdCambodia$b24_11a2) <- "Collector outside the village"
#C318 = FACT, replace label: "Trader in the district"
var_label(HouseholdCambodia$b24_11a3) <- "Trader in the district"
#C319 = FACT, replace label: "Trader from the province"
var_label(HouseholdCambodia$b24_11a4) <- "Trader from the province"
#C320 = FACT, replace label: "Trader from another province"
var_label(HouseholdCambodia$b24_11a5) <- "Trader from another province"
#C321 = FACT, replace label: "Contractor-company from the province"
var_label(HouseholdCambodia$b24_11a6) <- "Contractor-company from the province"
#C322 = FACT, replace label: "Contractor-company from another province"
var_label(HouseholdCambodia$b24_11a7) <- "Contractor-company from another province"
#C323 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdCambodia$b24_11a8) <- "Cooperative of which you are a member"
#C324 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdCambodia$b24_11a9) <- "Cooperative of which you are not a member"
#C325 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdCambodia$b24_11a10) <- "Consumers on the farm"
#C326 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdCambodia$b24_11a11) <- "Local markets where you sell your products directly to final consumers"
#C327 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdCambodia$b24_11a12) <- "Consumers through online sales"
#C328 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdCambodia$b24_11a13) <- "Foreign trader (e.g. from China)"
#C329 = FACT, replace label: "Processors"
var_label(HouseholdCambodia$b24_11a14) <- "Processors"
#C330 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b24_11a99) <- "Other"
#C331 = FACT, replace label: "Do not know/No selling/no the second outlet"
var_label(HouseholdCambodia$b24_11a88) <- "Do not know/No selling/no the second outlet"
#C332 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Other. Specify"
#C333 = OK, (empty)
#C334 = OK, (empty)
#C335 = CHAR, (empty)
#C336 = CHAR, (Useless answer-MultipleCombined, use the following columns), (empty)
#C337 = FACT, replace label: "Village collector", (empty)
var_label(HouseholdCambodia$b24_9a1) <- "Village collector"
#C338 = FACT, replace label: "Collector outside the village", (empty)
var_label(HouseholdCambodia$b24_9a2) <- "Collector outside the village"
#C339 = FACT, replace label: "Trader in the district", (empty)
var_label(HouseholdCambodia$b24_9a3) <- "Trader in the district"
#C340 = FACT, replace label: "Trader from the province", (empty)
var_label(HouseholdCambodia$b24_9a4) <- "Trader from the province"
#C341 = FACT, replace label: "Trader from another province", (empty)
var_label(HouseholdCambodia$b24_9a5) <- "Trader from another province"
#C342 = FACT, replace label: "Contractor-company from the province", (empty)
var_label(HouseholdCambodia$b24_9a6) <- "Contractor-company from the province"
#C343 = FACT, replace label: "Contractor-company from another province", (empty)
var_label(HouseholdCambodia$b24_9a7) <- "Contractor-company from another province"
#C344 = FACT, replace label: "Cooperative of which you are a member", (empty)
var_label(HouseholdCambodia$b24_9a8) <- "Cooperative of which you are a member"
#C345 = FACT, replace label: "Cooperative of which you are not a member", (empty)
var_label(HouseholdCambodia$b24_9a9) <- "Cooperative of which you are not a member"
#C346 = FACT, replace label: "Consumers on the farm", (empty)
var_label(HouseholdCambodia$b24_9a10) <- "Consumers on the farm"
#C347 = FACT, replace label: "Local markets where you sell your products directly to final consumers", (empty)
var_label(HouseholdCambodia$b24_9a11) <- "Local markets where you sell your products directly to final consumers"
#C348 = FACT, replace label: "Consumers through online sales", (empty)
var_label(HouseholdCambodia$b24_9a12) <- "Consumers through online sales"
#C349 = FACT, replace label: "Foreign trader (e.g. from China)", (empty)
var_label(HouseholdCambodia$b24_9a13) <- "Foreign trader (e.g. from China)"
#C350 = FACT, replace label: "Processors", (empty)
var_label(HouseholdCambodia$b24_9a14) <- "Processors"
#C351 = FACT, replace label: "Other", (empty)
var_label(HouseholdCambodia$b24_9a99) <- "Other"
#C352 = FACT, replace label: "Do not know/No selling/no the second outlet", (empty)
var_label(HouseholdCambodia$b24_9a88) <- "Do not know/No selling/no the second outlet"
#C353 = OK, (empty)
#C354 =FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C355 = OK, (empty)
#C356 = OK, (empty)
#C357 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C358 = FACT, replace label: "Village collector"
var_label(HouseholdCambodia$b24_15a1) <- "Village collector"
#C359 = FACT, replace label: "Collector outside the village"
var_label(HouseholdCambodia$b24_15a2) <- "Collector outside the village"
#C360 = FACT, replace label: "Trader in the district"
var_label(HouseholdCambodia$b24_15a3) <- "Trader in the district"
#C361 = FACT, replace label: "Trader from the province"
var_label(HouseholdCambodia$b24_15a4) <- "Trader from the province"
#C362 = FACT, replace label: "Trader from another province"
var_label(HouseholdCambodia$b24_15a5) <- "Trader from another province"
#C363 = FACT, replace label: "Contractor-company from the province"
var_label(HouseholdCambodia$b24_15a6) <- "Contractor-company from the province"
#C364 = FACT, replace label: "Contractor-company from another province"
var_label(HouseholdCambodia$b24_15a7) <- "Contractor-company from another province"
#C365 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdCambodia$b24_15a8) <- "Cooperative of which you are a member"
#C366 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdCambodia$b24_15a9) <- "Cooperative of which you are not a member"
#C367 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdCambodia$b24_15a10) <- "Consumers on the farm"
#C368 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdCambodia$b24_15a11) <- "Local markets where you sell your products directly to final consumers"
#C369 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdCambodia$b24_15a12) <- "Consumers through online sales"
#C370 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdCambodia$b24_15a13) <- "Foreign trader (e.g. from China)"
#C371 = FACT, replace label: "Processors"
var_label(HouseholdCambodia$b24_15a14) <- "Processors"
#C372 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b24_15a99) <- "Other"
#C373 = FACT, replace label: "Do not know/No selling/no the second outlet"
#We move this column
HouseholdCambodia <- HouseholdCambodia %>% relocate(b24_15a88 , .after = b24_15a99)
var_label(HouseholdCambodia$b24_15a88) <- "Do not know/No selling/no the second outlet"
#C374 = OK, ???, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Other. Specify"
#C375 = OK, (empty)
#C376 =FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C377 = OK, (empty)
#C378 = OK, (empty)
#C379 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C380 = FACT, replace label: "Village collector"
var_label(HouseholdCambodia$b24_16a1) <- "Village collector"
#C381 = FACT, replace label: "Collector outside the village"
var_label(HouseholdCambodia$b24_16a2) <- "Collector outside the village"
#C382 = FACT, replace label: "Trader in the district"
var_label(HouseholdCambodia$b24_16a3) <- "Trader in the district"
#C383 = FACT, replace label: "Trader from the province"
var_label(HouseholdCambodia$b24_16a4) <- "Trader from the province"
#C384 = FACT, replace label: "Trader from another province"
var_label(HouseholdCambodia$b24_16a5) <- "Trader from another province"
#C385 = FACT, replace label: "Contractor-company from the province"
var_label(HouseholdCambodia$b24_16a6) <- "Contractor-company from the province"
#C386 = FACT, replace label: "Contractor-company from another province"
var_label(HouseholdCambodia$b24_16a7) <- "Contractor-company from another province"
#C387 = FACT, replace label: "Cooperative of which you are a member"
var_label(HouseholdCambodia$b24_16a8) <- "Cooperative of which you are a member"
#C388 = FACT, replace label: "Cooperative of which you are not a member"
var_label(HouseholdCambodia$b24_16a9) <- "Cooperative of which you are not a member"
#C389 = FACT, replace label: "Consumers on the farm"
var_label(HouseholdCambodia$b24_16a10) <- "Consumers on the farm"
#C390 = FACT, replace label: "Local markets where you sell your products directly to final consumers"
var_label(HouseholdCambodia$b24_16a11) <- "Local markets where you sell your products directly to final consumers"
#C391 = FACT, replace label: "Consumers through online sales"
var_label(HouseholdCambodia$b24_16a12) <- "Consumers through online sales"
#C392 = FACT, replace label: "Foreign trader (e.g. from China)"
var_label(HouseholdCambodia$b24_16a13) <- "Foreign trader (e.g. from China)"
#C393 = FACT, replace label: "Processors"
var_label(HouseholdCambodia$b24_16a14) <- "Processors"
#C394 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b24_16a99) <- "Other"
#C395 = FACT, replace label: "Do not know/No selling/no the second outlet"
var_label(HouseholdCambodia$b24_16a88) <- "Do not know/No selling/no the second outlet"
#C396 = OK, ???, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Other. Specify"
#C397 = OK, (empty)
#C398 = FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C399 = OK, (empty)
#C400 = OK, (empty)
#C401 = CHAR, (Useless answer-MultipleCombined, use the following columns), (empty)
#C402 = FACT, replace label: "Village collector", (empty)
var_label(HouseholdCambodia$b24_17a1) <- "Village collector"
#C403 = FACT, replace label: "Collector outside the village", (empty)
var_label(HouseholdCambodia$b24_17a2) <- "Collector outside the village"
#C404 = FACT, replace label: "Trader in the district", (empty)
var_label(HouseholdCambodia$b24_17a3) <- "Trader in the district"
#C405 = FACT, replace label: "Trader from the province", (empty)
var_label(HouseholdCambodia$b24_17a4) <- "Trader from the province"
#C406 = FACT, replace label: "Trader from another province", (empty)
var_label(HouseholdCambodia$b24_17a5) <- "Trader from another province"
#C407 = FACT, replace label: "Contractor-company from the province", (empty)
var_label(HouseholdCambodia$b24_17a6) <- "Contractor-company from the province"
#C408 = FACT, replace label: "Contractor-company from another province", (empty)
var_label(HouseholdCambodia$b24_17a7) <- "Contractor-company from another province"
#C409 = FACT, replace label: "Cooperative of which you are a member", (empty)
var_label(HouseholdCambodia$b24_17a8) <- "Cooperative of which you are a member"
#C410 = FACT, replace label: "Cooperative of which you are not a member", (empty)
var_label(HouseholdCambodia$b24_17a9) <- "Cooperative of which you are not a member"
#C411 = FACT, replace label: "Consumers on the farm", (empty)
var_label(HouseholdCambodia$b24_17a10) <- "Consumers on the farm"
#C412 = FACT, replace label: "Local markets where you sell your products directly to final consumers", (empty)
var_label(HouseholdCambodia$b24_17a11) <- "Local markets where you sell your products directly to final consumers"
#C413 = FACT, replace label: "Consumers through online sales", (empty)
var_label(HouseholdCambodia$b24_17a12) <- "Consumers through online sales"
#C414 = FACT, replace label: "Foreign trader (e.g. from China)", (empty)
var_label(HouseholdCambodia$b24_17a13) <- "Foreign trader (e.g. from China)"
#C415 = FACT, replace label: "Processors", (empty)
var_label(HouseholdCambodia$b24_17a14) <- "Processors"
#C416 = FACT, replace label: "Other", (empty)
var_label(HouseholdCambodia$b24_17a99) <- "Other"
#C417 = FACT, replace label: "Do not know/No selling/no the second outlet", (empty)
var_label(HouseholdCambodia$b24_17a88) <- "Do not know/No selling/no the second outlet"
#C418 = OK, (empty)
#C419 = FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C420 = OK, (empty)
#C421 = OK, (empty)
#C422 = CHAR, (Useless answer-MultipleCombined, use the following columns), (empty)
#C423 = FACT, replace label: "Village collector", (empty)
var_label(HouseholdCambodia$b24_17a1) <- "Village collector"
#C424 = FACT, replace label: "Collector outside the village", (empty)
var_label(HouseholdCambodia$b24_17a2) <- "Collector outside the village"
#C425 = FACT, replace label: "Trader in the district", (empty)
var_label(HouseholdCambodia$b24_17a3) <- "Trader in the district"
#C426 = FACT, replace label: "Trader from the province", (empty)
var_label(HouseholdCambodia$b24_17a4) <- "Trader from the province"
#C427 = FACT, replace label: "Trader from another province", (empty)
var_label(HouseholdCambodia$b24_17a5) <- "Trader from another province"
#C428 = FACT, replace label: "Contractor-company from the province", (empty)
var_label(HouseholdCambodia$b24_17a6) <- "Contractor-company from the province"
#C429 = FACT, replace label: "Contractor-company from another province", (empty)
var_label(HouseholdCambodia$b24_17a7) <- "Contractor-company from another province"
#C430 = FACT, replace label: "Cooperative of which you are a member", (empty)
var_label(HouseholdCambodia$b24_17a8) <- "Cooperative of which you are a member"
#C431 = FACT, replace label: "Cooperative of which you are not a member", (empty)
var_label(HouseholdCambodia$b24_17a9) <- "Cooperative of which you are not a member"
#C432 = FACT, replace label: "Consumers on the farm", (empty)
var_label(HouseholdCambodia$b24_17a10) <- "Consumers on the farm"
#C433 = FACT, replace label: "Local markets where you sell your products directly to final consumers", (empty)
var_label(HouseholdCambodia$b24_17a11) <- "Local markets where you sell your products directly to final consumers"
#C434 = FACT, replace label: "Consumers through online sales", (empty)
var_label(HouseholdCambodia$b24_17a12) <- "Consumers through online sales"
#C435 = FACT, replace label: "Foreign trader (e.g. from China)", (empty)
var_label(HouseholdCambodia$b24_17a13) <- "Foreign trader (e.g. from China)"
#C436 = FACT, replace label: "Processors", (empty)
var_label(HouseholdCambodia$b24_17a14) <- "Processors"
#C437 = FACT, replace label: "Other", (empty)
var_label(HouseholdCambodia$b24_17a99) <- "Other"
#C438 = FACT, replace label: "Do not know/No selling/no the second outlet", (empty)
var_label(HouseholdCambodia$b24_17a88) <- "Do not know/No selling/no the second outlet"
#C439 = OK, (empty)
#C440 = FACT, (Table of correspondence), (see answers below)
#CamGAP standard
#CamOrganic standard
#International organic standard, EU and US
#Organic Wildlife-Friendly
#Organic Fairtrade
#Collective or certification trademark
#Geographical indication
#Other
#C441 = OK, (empty)
#C442 = OK, (empty)
#C443 = CHAR, (Useless answer-MultipleCombined, use the following columns), (empty)
#C444 = FACT, replace label: "Village collector", (empty)
var_label(HouseholdCambodia$b24_17a1) <- "Village collector"
#C445 = FACT, replace label: "Collector outside the village", (empty)
var_label(HouseholdCambodia$b24_17a2) <- "Collector outside the village"
#C446 = FACT, replace label: "Trader in the district", (empty)
var_label(HouseholdCambodia$b24_17a3) <- "Trader in the district"
#C447 = FACT, replace label: "Trader from the province", (empty)
var_label(HouseholdCambodia$b24_17a4) <- "Trader from the province"
#C448 = FACT, replace label: "Trader from another province", (empty)
var_label(HouseholdCambodia$b24_17a5) <- "Trader from another province"
#C449 = FACT, replace label: "Contractor-company from the province", (empty)
var_label(HouseholdCambodia$b24_17a6) <- "Contractor-company from the province"
#C450 = FACT, replace label: "Contractor-company from another province", (empty)
var_label(HouseholdCambodia$b24_17a7) <- "Contractor-company from another province"
#C451 = FACT, replace label: "Cooperative of which you are a member", (empty)
var_label(HouseholdCambodia$b24_17a8) <- "Cooperative of which you are a member"
#C452 = FACT, replace label: "Cooperative of which you are not a member", (empty)
var_label(HouseholdCambodia$b24_17a9) <- "Cooperative of which you are not a member"
#C453 = FACT, replace label: "Consumers on the farm", (empty)
var_label(HouseholdCambodia$b24_17a10) <- "Consumers on the farm"
#C454 = FACT, replace label: "Local markets where you sell your products directly to final consumers", (empty)
var_label(HouseholdCambodia$b24_17a11) <- "Local markets where you sell your products directly to final consumers"
#C455 = FACT, replace label: "Consumers through online sales", (empty)
var_label(HouseholdCambodia$b24_17a12) <- "Consumers through online sales"
#C456 = FACT, replace label: "Foreign trader (e.g. from China)", (empty)
var_label(HouseholdCambodia$b24_17a13) <- "Foreign trader (e.g. from China)"
#C457 = FACT, replace label: "Processors", (empty)
var_label(HouseholdCambodia$b24_17a14) <- "Processors"
#C458 = FACT, replace label: "Other", (empty)
var_label(HouseholdCambodia$b24_17a99) <- "Other"
#C459 = FACT, replace label: "Do not know/No selling/no the second outlet", (empty)
var_label(HouseholdCambodia$b24_17a88) <- "Do not know/No selling/no the second outlet"
#C460 = OK, (empty)
#C461 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C462 = FACT, (bin)
#C463 = FACT, (bin)
#C464 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C465 = FACT, (empty)
#C466 = FACT, (Table of correspondence), (See answers below), (empty)
#Logo
#Card visit 
#Leaflet
#Signboard in the field 
#Signboard at the cooperative/Company
#Social media 
#Internet or other media (TV, Radio, etc.) 
#Do not know
#C467 = FACT, replace label: "Logo"
var_label(HouseholdCambodia$b27_11) <- "Logo"
#C468 = FACT, replace label: "Card visit"
var_label(HouseholdCambodia$b27_12) <- "Card visit"
#C469 = FACT, replace label: "Leaflet"
var_label(HouseholdCambodia$b27_13) <- "Leaflet"
#C470 = FACT, replace label: "Signboard in the field"
var_label(HouseholdCambodia$b27_14) <- "Signboard in the field"
#C471 = FACT, replace label: "Signboard at the cooperative/Company"
var_label(HouseholdCambodia$b27_15) <- "Signboard at the cooperative/Company"
#C472 = FACT, replace label: "Social media"
var_label(HouseholdCambodia$b27_16) <- "Social media"
#C473 = FACT, replace label: "Internet or other media (TV, Radio, etc.)"
var_label(HouseholdCambodia$b27_17) <- "Internet or other media (TV, Radio, etc.)"
#C474 = REMOVE, (empty, correspond to nothing in the questionnaire)
#C475 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$b27_188) <- "Do not know"
#C476 = OK, (empty)
#C477 = FACT, (bin)
#C478 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C479 = FACT, (bin)
#C480 = FACT, (bin)

# # #c.
#C481 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C482 = FACT
#C483 = FACT
#C484 = FACT
#C485 = FACT
#C486 = FACT
#C487 = FACT
#C488 = FACT
#C489 = FACT
#C490 = FACT
#C491 = FACT
#C492 = FACT
#C493 = FACT
#C494 = FACT
#C495 = FACT
#C496 = FACT
#C497 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C498 = FACT, (Table of correspondence), (see answers below)
#No
#Yes, one
#Yes, more than one
#C499 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C500 = FACT, replace label: "Farmer organization on crops"
var_label(HouseholdCambodia$c31) <- "Farmer organization on crops"
#C501 = FACT, replace label: "Farmer organization on fruits"
var_label(HouseholdCambodia$c32) <- "Farmer organization on fruits"
#C502 = FACT, replace label: "Farmer organization on livestock"
var_label(HouseholdCambodia$c33) <- "Farmer organization on livestock"
#C503 = FACT, replace label: "Farmer organization on water"
var_label(HouseholdCambodia$c34) <- "Farmer organization on water"
#C504 = FACT, replace label: "Farmer organization on forest"
var_label(HouseholdCambodia$c35) <- "Farmer organization on forest"
#C505 = FACT, replace label: "Farmer organization on market"
var_label(HouseholdCambodia$c36) <- "Farmer organization on market"
#C506 = FACT, replace label: "Farmer organization on credit"
var_label(HouseholdCambodia$c37) <- "Farmer organization on credit"
#C507 = FACT, replace label: "Farmer organization on any type of mutual help"
var_label(HouseholdCambodia$c38) <- "Farmer organization on any type of mutual help"
#C508 = FACT, replace label: "Farmer organization on diversified activities: crop, livestock, market, credit…etc."
var_label(HouseholdCambodia$c39) <- "Farmer organization on diversified activities: crop, livestock, market, credit…etc."
#C509 = FACT, replace label: "??"
var_label(HouseholdCambodia$c310) <- "??"
#C510 = FACT, replace label: "Other farmer organization"
var_label(HouseholdCambodia$c399) <- "Other farmer organization"
#We move this column
HouseholdCambodia <- HouseholdCambodia %>% relocate(c399 , .after = c310)
#C511 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C512 = FACT, (Table of correspondence), (see answers below)
#Farmer organization on crops
#Farmer organization on fruits
#Farmer organization on livestock
#Farmer organization on water
#Farmer organization on forest
#Farmer organization on market
#Farmer organization on credit
#Farmer organization on any type of mutual help
#Farmer organization on diversified activities: crop, livestock, market, credit…etc.
#Other farmer organization
#C513 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Specify"
var_label(HouseholdCambodia$c3a_text) <- "Specify"
#C514 = FACT, (Table of correspondence), (see answers below)
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
var_label(HouseholdCambodia$c4) <- "c4. what is the legal type of this c3a"
#C515 = OK, (empty)
#C516 = FACT, (Table of correspondence), (see answers below)
#President 
#Treasurer 
#Internal control person
#Trainer 
#Collector 
#Ordinary member
#Other
#Do not know
#replace label: "c5. what is your role / responsibility in the c3a"
var_label(HouseholdCambodia$c5) <- "c5. what is your role / responsibility in the c3a"
#C517 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "c5_oth. specify your other role in the c3a"
var_label(HouseholdCambodia$c5_oth) <- "c5_oth. specify your other role in the c3a"
#C518 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "c9. what are the benefits of belonging to this c3a"
var_label(HouseholdCambodia$c9) <- "c9. what are the benefits of belonging to this c3a"
#C519 = FACT, replace label: "To borrow money "
var_label(HouseholdCambodia$c91) <- "To borrow money"
#C520 = FACT, replace label: "To get advice"
var_label(HouseholdCambodia$c92) <- "To get advice"
#C521 = FACT, replace label: "To save money"
var_label(HouseholdCambodia$c93) <- "To save money"
#C522 = FACT, replace label: "To buy inputs"
var_label(HouseholdCambodia$c94) <- "To buy inputs"
#C523 = FACT, replace label: "To use water"
var_label(HouseholdCambodia$c95) <- "To use water"
#C524 = FACT, replace label: "To get training"
var_label(HouseholdCambodia$c96) <- "To get training"
#C525 = FACT, replace label: "Access to markets"
var_label(HouseholdCambodia$c97) <- "Access to markets"
#C526 = FACT, replace label: "Secure demand from existing markets"
var_label(HouseholdCambodia$c98) <- "Secure demand from existing markets"
#C527 = FACT, replace label: "Sell certified products at a better price "
var_label(HouseholdCambodia$c99) <- "Sell certified products at a better price "
#C528 = FACT, replace label: "Contribute to conservation purpose e.g. forest, wildlife, water, fish…etc."
var_label(HouseholdCambodia$c910) <- "Contribute to conservation purpose e.g. forest, wildlife, water, fish…etc."
#We move this column
HouseholdCambodia <- HouseholdCambodia %>% relocate(c910 , .after = c99)
#C529 = FACT, replace label: "Other"
var_label(HouseholdCambodia$c999) <- "Other"
#C530 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "Specify other"
var_label(HouseholdCambodia$c9_oth) <- "Specify other"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(c9_oth , .after = c999)
#C531 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$c988) <- "Do not know"
#C532 = FACT, (bin), replace label: "Did c3a give you loan/credit in the past 3 years"
var_label(HouseholdCambodia$c6) <- "Did c3a give you loan/credit in the past 3 years"
#C533 = FACT, (bin), replace label: "Did c3a give you technical advice or training in the past 3 years?"
var_label(HouseholdCambodia$c7) <- "Did c3a give you technical advice or training in the past 3 years?"
#C534 = FACT, (bin), replace label: "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
var_label(HouseholdCambodia$c8) <- "Did you sign a contract whereby the c3a commits to buy from you following some at specific conditions (price, volume, quality, time...)?"
#C535 = FACT, (bin), replace label: "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
var_label(HouseholdCambodia$c10) <- "Apart from the c3a, did you receive training in the past 1 year for any farming activity (e.g. crop/trees/livestock)?"
#C536 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C537 = FACT, replace label: "Standard - CamGAP"
var_label(HouseholdCambodia$c111) <- "Standard - CamGAP"
#C538 = FACT, replace label: "Standard - CamOrganic"
var_label(HouseholdCambodia$c112) <- "Standard - CamOrganic"
#C539 = FACT, replace label: "Standard - International organic"
var_label(HouseholdCambodia$c113) <- "Standard - International organic"
#C540 = FACT, replace label: "Standard - Organic and fairtrade"
var_label(HouseholdCambodia$c114) <- "Standard - Organic and fairtrade"
#C541 = FACT, replace label: "Standard - Organic Wildlife-Friendly"
var_label(HouseholdCambodia$c115) <- "Standard - Organic Wildlife-Friendly"
#C542 = FACT, replace label: "Farming techniques in general for vegetables"
var_label(HouseholdCambodia$c116) <- "Farming techniques in general for vegetables"
#C543 = FACT, replace label: "Farming techniques in general for rice"
var_label(HouseholdCambodia$c117) <- "Farming techniques in general for rice"
#C544 = FACT, replace label: "Farming techniques in general for cassava"
var_label(HouseholdCambodia$c118) <- "Farming techniques in general for cassava"
#C545 = FACT, replace label: "Farming techniques in general for peanut"
var_label(HouseholdCambodia$c119) <- "Farming techniques in general for peanut"
#C546 = FACT, replace label: "Farming techniques in general for cashew"
var_label(HouseholdCambodia$c1110) <- "Farming techniques in general for cashew"
#C547 = FACT, replace label: "Crop protection - pest/disease and safe use of pesticides"
var_label(HouseholdCambodia$c1111) <- "Crop protection - pest/disease and safe use of pesticides"
#C548 = FACT, replace label: "Crop protection - IPM"
var_label(HouseholdCambodia$c1112) <- "Crop protection - IPM"
#C549 = FACT, replace label: "Crop protection - ACP on Pheromones"
var_label(HouseholdCambodia$c1113) <- "Crop protection - ACP on Pheromones"
#C550 = FACT, replace label: "Farming techniques in general"
var_label(HouseholdCambodia$c1114) <- "Farming techniques in general"
#C551 = FACT, replace label: "Farming - weed management"
var_label(HouseholdCambodia$c1115) <- "Farming - weed management"
#C552 = FACT, replace label: "Farming - use of fertilizers"
var_label(HouseholdCambodia$c1116) <- "Farming - use of fertilizers"
#C553 = FACT, replace label: "Farming - composting"
var_label(HouseholdCambodia$c1117) <- "Farming - composting"
#C554 = FACT, replace label: "Livestock - raising technique in general for cattle/buffalo"
var_label(HouseholdCambodia$c1118) <- "Livestock - raising technique in general for cattle/buffalo"
#C555 = FACT, replace label: "Livestock - raising technique in general for pig"
var_label(HouseholdCambodia$c1119) <- "Livestock - raising technique in general for pig"
#C556 = FACT, replace label: "Livestock - raising technique in general for poultry"
var_label(HouseholdCambodia$c1120) <- "Livestock - raising technique in general for poultry"
#C557 = FACT, replace label: "Livestock - Forage production and improved pasture"
var_label(HouseholdCambodia$c1121) <- "Livestock - Forage production and improved pasture"
#C558 = FACT, replace label: "Livestock - Forage treatment / silage"
var_label(HouseholdCambodia$c1122) <- "Livestock - Forage treatment / silage"
#C559 = FACT, replace label: "Organization - management / finance"
var_label(HouseholdCambodia$c1123) <- "Organization - management / finance"
#C560 = FACT, replace label: "Organization - business plan"
var_label(HouseholdCambodia$c1124) <- "Organization - business plan"
#C561 = FACT, replace label: "Organization - contract farming, market price"
var_label(HouseholdCambodia$c1125) <- "Organization - contract farming, market price"
#C562 = FACT, replace label: "Organization - FWUC on irrigation scheme maintenance and operation"
var_label(HouseholdCambodia$c1126) <- "Organization - FWUC on irrigation scheme maintenance and operation"
#C563 = FACT, replace label: "Organization - community forestry"
var_label(HouseholdCambodia$c1127) <- "Organization - community forestry"
#C564 = FACT, replace label: "Organization - internal control system and record keeping"
var_label(HouseholdCambodia$c1128) <- "Organization - internal control system and record keeping"
#C565 = FACT, replace label: "Regulation / conservation practices / roles of rangers / reporting"
var_label(HouseholdCambodia$c1129) <- "Regulation / conservation practices / roles of rangers / reporting"
#C566 = FACT, replace label: "Other"
var_label(HouseholdCambodia$c1199) <- "Other"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(c1199 , .after = c1129)
#C567 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C568 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C569 = FACT, replace label: "Share labor (mutual help, working together on each other farm)"
var_label(HouseholdCambodia$c131) <- "Share labor (mutual help, working together on each other farm)"
#C570 = FACT, replace label: "Manage water/irrigation systems"
var_label(HouseholdCambodia$c132) <- "Manage water/irrigation systems"
#C571 = FACT, replace label: "Raise livestock"
var_label(HouseholdCambodia$c133) <- "Raise livestock"
#C572 = FACT, replace label: "Buy agricultural inputs"
var_label(HouseholdCambodia$c134) <- "Buy agricultural inputs"
#C573 = FACT, replace label: "Selling products to the markets for other farmers "
var_label(HouseholdCambodia$c135) <- "Selling products to the markets for other farmers "
#C574 = FACT, replace label: "Experiment new farming practices"
var_label(HouseholdCambodia$c136) <- "Experiment new farming practices"
#C575 = FACT, replace label: "No collaboration with other people on these issues"
var_label(HouseholdCambodia$c130) <- "No collaboration with other people on these issues"
#C576 = FACT, replace label: "Other"
var_label(HouseholdCambodia$c1399) <- "Other"
#C577 = OK, (empty)
#C578 = FACT, (bin)
#C579 = FACT, (bin)

# # #d.
#C580 = FACT, (bin)
#C581 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C582 = FACT
#C583 = FACT
#C584 = FACT
#C585 = FACT
#C586 = FACT
#C587 = FACT
#C588 = FACT
#C589 = FACT
#C590 = NUMERIC, replace label: "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdCambodia$no_crop1) <- "d2a_1. LOWLAND - how many crop did you that your household planted in the past 12 month"
#C591 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C592 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C593 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C594 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C595 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C596 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C597 = REMOVE, (empty + useless)
#C598 = NUMERIC,
#C599 = NUMERIC,
#C600 = NUMERIC, replace label: "d2a_1. UPLAND - how many crop did you that your household planted in the past 12 month"
var_label(HouseholdCambodia$no_crop2) <- "d2a_1. UPLAND - how many crop did you that your household planted in the past 12 month"
#C601 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C602 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C603 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C604 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C605 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C606 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C607 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C608 = REMOVE, text from the questionnaire
#C609 = NUMERIC,
#C610 = NUMERIC,
#C611 = NUMERIC, replace label: "d2_3. The crops that your household planted on HOMEGARDEN in the last 12 months were:"
var_label(HouseholdCambodia$no_crop3) <- "d2_3. The crops that your household planted on HOMEGARDEN in the last 12 months were"
#C612 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C613 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C614 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C615 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C616 = OK, (empty)
#C617 = NUMERIC,
#C618 = NUMERIC,
#C619 = CHAR, (Useless answer- MultipleCombined, use the following columns)
#From 1 to 5 = Lowland crops in order, from 6 to 1x = Upland crops in order
#C620 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d2_sum1) <- "Lowland crop n1"
#C621 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d2_sum2) <- "Lowland crop n2"
#C622 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d2_sum3) <- "Lowland crop n3"
#C623 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d2_sum4) <- "Lowland crop n4"
#C624 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d2_sum5) <- "Lowland crop n5"
#C625 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d2_sum6) <- "Upland crop n1"
#C626 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d2_sum7) <- "Upland crop n2"
#C627 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d2_sum8) <- "Upland crop n3"
#C628 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d2_sum9) <- "Upland crop n4"
#C629 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d2_sum10) <- "Upland crop n5"
#C630 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d2_sum11) <- "Upland crop n6"
#C631 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d2_sum12) <- "Homegarden crop n1"
#C632 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d2_sum13) <- "Homegarden crop n2"
#C633 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d2_sum14) <- "Homegarden crop n3"
#C634 = FACT, (Table of correspondence), (see answers below)
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
#C635 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C636 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C637 = CHAR, (the answer is still in Khmer, Ky will maybe solve it)
#C638 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C639 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$b13_011) <- "Lowland crop n1"
#C640 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$b13_012) <- "Lowland crop n2"
#C641 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$b13_013) <- "Lowland crop n3"
#C642 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$b13_014) <- "Lowland crop n4"
#C643 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$b13_015) <- "Lowland crop n5"
#C644 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdCambodia$b13_016) <- "Upland crop n1"
#C645 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdCambodia$b13_017) <- "Upland crop n2"
#C646 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdCambodia$b13_018) <- "Upland crop n3"
#C647 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdCambodia$b13_019) <- "Upland crop n4"
#C648 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdCambodia$b13_0110) <- "Upland crop n5"
#C649 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdCambodia$b13_0111) <- "Upland crop n6"
#C650 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$b13_0112) <- "Homegarden crop n1"
#C651 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$b13_0113) <- "Homegarden crop n2"
#C652 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$b13_0114) <- "Homegarden crop n3"
#C653 = FACT, (Table of correspondence), (see anwsers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C654 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdCambodia$b14_1) <- "does b12_1 provide any of the following"
#C655 = FACT, empty, replace label: "Nothing"
var_label(HouseholdCambodia$b14_10) <- "Nothing"
#C656 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdCambodia$b14_11) <- "Inputs (sold)"
#C657 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdCambodia$b14_12) <- "Inputs on credit"
#C658 = FACT, empty, replace label: "Gas Credit"
var_label(HouseholdCambodia$b14_13) <- "Gas Credit"
#C659 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdCambodia$b14_14) <- "Technical advice/training"
#C660 = FACT, empty, replace label: "Market information"
var_label(HouseholdCambodia$b14_15) <- "Market information"
#C661 = FACT, empty, UNKNOWN, replace label: "Regular sales"
var_label(HouseholdCambodia$b14_16) <- "Regular sales"
#C662 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$b14_199) <- "Other"
#C663 = FACT, empty, replace label: "Do not know"
var_label(HouseholdCambodia$b14_188) <- "Do not know"
#C664 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "b14_1oth. specify other provision from b12_1"
var_label(HouseholdCambodia$b14_1oth) <- "b14_1oth. specify other provision from b12_1"
#C665 = FACT, (Table of correspondence), (see answers below)
#Formal contract
#Informal contract 
#No contract/ no prior arrangements
#Spot relations 
#Do not know
#replace label: "b15_1. do you have a contract with b12_1"
var_label(HouseholdCambodia$b15_1) <- "b15_1. do you have a contract with b12_1"
#C666 = FACT, (Table of correspondence), (see answers below)
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
#C667 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C668 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C669 = CHAR, (the answer is still in Khmer, Ky will maybe solve it)
#C670 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C671 = FACT, empty, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$b13_021) <- "Lowland crop n1"
#C672 = FACT, empty, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$b13_022) <- "Lowland crop n2"
#C673 = FACT, empty, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$b13_023) <- "Lowland crop n3"
#C674 = FACT, empty, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$b13_024) <- "Lowland crop n4"
#C675 = FACT, empty, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$b13_025) <- "Lowland crop n5"
#C676 = FACT, empty, replace label: "Upland crop n1"
var_label(HouseholdCambodia$b13_026) <- "Upland crop n1"
#C677 = FACT, empty, replace label: "Upland crop n2"
var_label(HouseholdCambodia$b13_027) <- "Upland crop n2"
#C678 = FACT, empty, replace label: "Upland crop n3"
var_label(HouseholdCambodia$b13_028) <- "Upland crop n3"
#C679 = FACT, empty, replace label: "Upland crop n4"
var_label(HouseholdCambodia$b13_029) <- "Upland crop n4"
#C680 = FACT, empty, replace label: "Upland crop n5"
var_label(HouseholdCambodia$b13_0210) <- "Upland crop n5"
#C681 = FACT, empty, replace label: "Upland crop n6"
var_label(HouseholdCambodia$b13_0211) <- "Upland crop n6"
#C682 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$b13_0212) <- "Homegarden crop n1"
#C683 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$b13_0213) <- "Homegarden crop n2"
#C684 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$b13_0214) <- "Homegarden crop n3"
#C685 = FACT, (Table of correspondence), (see anwsers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C686 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "does b12_1 provide any of the following"
var_label(HouseholdCambodia$b14_2) <- "does b12_1 provide any of the following"
#C687 = FACT, empty, replace label: "Nothing"
var_label(HouseholdCambodia$b14_20) <- "Nothing"
#C688 = FACT, empty, replace label: "Inputs (sold)"
var_label(HouseholdCambodia$b14_21) <- "Inputs (sold)"
#C689 = FACT, empty, replace label: "Inputs on credit"
var_label(HouseholdCambodia$b14_22) <- "Inputs on credit"
#C690 = FACT, empty, replace label: "Gas Credit"
var_label(HouseholdCambodia$b14_23) <- "Gas Credit"
#C691 = FACT, empty, replace label: "Technical advice/training"
var_label(HouseholdCambodia$b14_24) <- "Technical advice/training"
#C692 = FACT, empty, replace label: "Market information"
var_label(HouseholdCambodia$b14_25) <- "Market information"
#C693 = FACT, empty, UNKNOWN, replace label: "Regular sales"
var_label(HouseholdCambodia$b14_26) <- "Regular sales"
#C694 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$b14_299) <- "Other"
#C695 = FACT, empty, replace label: "Do not know"
var_label(HouseholdCambodia$b14_288) <- "Do not know"
#C696 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "b14_2oth. specify other provision from b12_2"
var_label(HouseholdCambodia$b14_2oth) <- "b14_2oth. specify other provision from b12_2"
#C697 = FACT, (Table of correspondence), (see answers below)
#Formal contract
#Informal contract 
#No contract/ no prior arrangements
#Spot relations 
#Do not know
#replace label: "b15_2. do you have a contract with b12_1"
var_label(HouseholdCambodia$b15_2) <- "b15_2. do you have a contract with b12_2"
#C698 = OK
#C699 = OK
#C700 = OK
#C701 = OK
#C702 = NUMERIC,
#C703 = NUMERIC,
#C704 = OK
#C705 = OK
#C706 = OK
#C707 = OK
#C708 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C709 = FACT, empty, replace label: "None of forest product"
var_label(HouseholdCambodia$d70) <- "None of forest product"
#C710 = FACT, empty, replace label: "Hunting"
var_label(HouseholdCambodia$d71) <- "Hunting"
#C711 = FACT, empty, replace label: "Fishing"
var_label(HouseholdCambodia$d72) <- "Fishing"
#C712 = FACT, empty, replace label: "Fuelwood"
var_label(HouseholdCambodia$d73) <- "Fuelwood"
#C713 = FACT, empty, replace label: "Mushrooms"
var_label(HouseholdCambodia$d74) <- "Mushrooms"
#C714 = FACT, empty, replace label: "Bamboo shoots"
var_label(HouseholdCambodia$d75) <- "Bamboo shoots"
#C715 = FACT, empty, UNKNOWN, replace label: "Bamboo poles"
var_label(HouseholdCambodia$d76) <- "Bamboo poles"
#C716 = FACT, empty, replace label: "Broom grass"
var_label(HouseholdCambodia$d77) <- "Broom grass"
#C717 = FACT, empty, replace label: "Honey"
var_label(HouseholdCambodia$d78) <- "Honey"
#C718 = FACT, empty, replace label: "Rattan"
var_label(HouseholdCambodia$d79) <- "Rattan"
#C719 = FACT, empty, replace label: "Galangal"
var_label(HouseholdCambodia$d711) <- "Galangal"
#C720 = FACT, empty, replace label: "Medicinal plants"
var_label(HouseholdCambodia$d714) <- "Medicinal plants"
#C721 = FACT, empty, replace label: "Wooden poles"
var_label(HouseholdCambodia$d716) <- "Wooden poles"
#C722 = FACT, empty, replace label: "Collect wood oil"
var_label(HouseholdCambodia$d717) <- "Collect wood oil"
#C723 = FACT, empty, UNKNOWN, replace label: "Leave for thatch roof"
var_label(HouseholdCambodia$d718) <- "Leave for thatch roof"
#C724 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$d799) <- "Other"
#C725 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C726 = NUMERIC
#C727 = FACT, (bin)
#C728 = OK, (empty)
#C729 = NUMERIC
#C730 = FACT, (bin)
#C731 = NUMERIC
#C732 = NUMERIC
#C733 = FACT, (bin)
#C734 = NUMERIC
#C735 = NUMERIC
#C736 = FACT, (bin)
#C737 = NUMERIC
#C738 = NUMERIC
#C739 = FACT, (bin)
#C740 = NUMERIC
#C741 = NUMERIC
#C742 = FACT, (bin)
#C743 = NUMERIC
#C744 = NUMERIC
#C745 = FACT, (bin)
#C746 = OK, (empty)
#C747 = NUMERIC
#C748 = FACT, (bin)
#C749 = NUMERIC
#C750 = NUMERIC
#C751 = FACT, (bin)
#C752 = NUMERIC
#C753 = OK, (empty)
#C754 = OK, (empty)
#C755 = OK, (empty)
#C756 = NUMERIC
#C757 = FACT, (bin)
#C758 = OK, (empty)
#C759 = NUMERIC
#C760 = FACT, (bin)
#C761 = NUMERIC
#C762 = OK, (empty)
#C763 = OK, (empty)
#C764 = OK, (empty)
#C765 = NUMERIC
#C766 = FACT, (bin)
#C767 = NUMERIC
#C768 = NUMERIC, replace label: "d7_991. how many days did your household spend to collect d7_oth over 12 months"
var_label(HouseholdCambodia$d7_991) <- "d7_991. how many days did your household spend to collect d7_oth over 12 months"
#C769 = FACT, (bin), replace label: "d7_992. did your household sell collect d7_oth product in the last 12 month"
var_label(HouseholdCambodia$d7_992) <- "d7_992. did your household sell collect d7_oth product in the last 12 month"
#C770 = NUMERIC, replace label: "d7_993. what was your household income from collecting d7_oth in the last 12 months"
var_label(HouseholdCambodia$d7_993) <- "d7_993. what was your household income from collecting d7_oth in the last 12 months"
#C771 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C772 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C773 = FACT, (Table of correspondence, correspond to previous mentioned crops)
#C774 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C775 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C776 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C777 = FACT, (table of correspondence), see choices below, replace label: "d81_1a.what was the main reason for planting d81_text"
var_label(HouseholdCambodia$d81_1a) <- "d81_1a.what was the main reason for planting d81_text"
#Market price and demand
#Household consumption preferences; 
#Well adapted to local conditions (soil, climate, …)
#Do not know
#Other
#C778 = OK, replace label: "d81_1a_oth. specify other reason for planting d81_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d81_1a_oth) <- "d81_1a_oth. specify other reason for planting d81_text"
#C779 = FACT, (table of correspondence), see choices below, replace label: "d81_1b. what was the major constraints for d81_text"
var_label(HouseholdCambodia$d81_1b) <- "d81_1b. what was the major constraints for d81_text"
#Water management
#Soil fertility
#Other agronomic constraints
#Insects
#Diseases
#Market price
#Product quality
#Do not know
#Other
#C780 = OK, replace label: "d81_1b_oth. specify other constrain for d81_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d81_1b_oth) <- "d81_1b_oth. specify other constrain for d81_text"
#C781 = FACT, replace label: "d82_1a.what was the main reason for planting d82_text"
var_label(HouseholdCambodia$d82_1a) <- "d82_1a.what was the main reason for planting d82_text"
#C782 = OK, replace label: "d82_1a_oth. specify other reason for planting d82_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d82_1a_oth) <- "d82_1a_oth. specify other reason for planting d82_text"
#C783 = FACT, replace label: "d82_1b. what was the major constraints for d82_text"
var_label(HouseholdCambodia$d82_1b) <- "d82_1b. what was the major constraints for d82_text"
#C784 = OK, replace label: "d82_1b_oth. specify other constrain for d82_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d82_1b_oth) <- "d82_1b_oth. specify other constrain for d82_text"
#C785 = FACT, replace label: "d83_1a.what was the main reason for planting d83_text"
var_label(HouseholdCambodia$d83_1a) <- "d83_1a.what was the main reason for planting d83_text"
#C786 = OK, replace label: "d83_1a_oth. specify other reason for planting d83_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d83_1a_oth) <- "d83_1a_oth. specify other reason for planting d83_text"
#C787 = FACT, see choices below, replace label: "d83_1b. what was the major constraints for d83_text"
var_label(HouseholdCambodia$d83_1b) <- "d83_1b. what was the major constraints for d83_text"
#C788 = OK, replace label: "d83_1b_oth. specify other constrain for d83_text", (the answer is still in Khmer, Ky will maybe solve it)
var_label(HouseholdCambodia$d83_1b_oth) <- "d83_1b_oth. specify other constrain for d83_text"
#C789 = REMOVE (Text from the questionnaire)
#C790 = FACT, (bin)
#C791 = OK, High values (>1000)
#C792 = NUMERIC, High values
#C793 = FACT, (bin)
#C794 = OK, High values (>1000)
#C795 = NUMERIC, High values
#C796 = FACT, (bin)
#C797 = OK
#C798 = NUMERIC, High values
#C799 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C800 = FACT, replace label: "Title deed"
var_label(HouseholdCambodia$d11_41) <- "Title deed"
#C801 = FACT, replace label: "Certificate of customary tenure"
var_label(HouseholdCambodia$d11_42) <- "Certificate of customary tenure"
#C802 = FACT, replace label: "Certificate of occupancy"
var_label(HouseholdCambodia$d11_43) <- "Certificate of occupancy"
#C803 = FACT, replace label: "Registered will or registered certificate of hereditary acquisition"
var_label(HouseholdCambodia$d11_44) <- "Registered will or registered certificate of hereditary acquisition"
#C804 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$d11_488) <- "Do not know"
#C805 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$d11_499) <- "Other"
#C806 = FACT, empty, replace label: "None of above"
var_label(HouseholdCambodia$d11_40) <- "None of above"
#C807 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C808 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C809 = FACT, replace label: "Rainwater collection/conservation"
var_label(HouseholdCambodia$d121) <- "Rainwater collection/conservation"
#C810 = FACT, replace label: "Greywater recycling"
var_label(HouseholdCambodia$d122) <- "Greywater recycling"
#C811 = FACT, replace label: "Ponds (for water conservation)"
var_label(HouseholdCambodia$d123) <- "Ponds (for water conservation)"
#C812 = FACT, replace label: "Terraces building"
var_label(HouseholdCambodia$d124) <- "Terraces building"
#C813 = FACT, replace label: "Swales digging"
var_label(HouseholdCambodia$d125) <- "Swales digging"
#C814 = FACT, empty, replace label: "Land levelling"
var_label(HouseholdCambodia$d126) <- "Land levelling"
#C815 = FACT, empty, replace label: "Mulching"
var_label(HouseholdCambodia$d127) <- "Mulching"
#C816 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$d1299) <- "Other"
#C817 = FACT, empty, replace label: "Do not know"
var_label(HouseholdCambodia$d1288) <- "Do not know"
#C818 = FACT, empty, replace label: "No water conservation practice"
var_label(HouseholdCambodia$d120) <- "No water conservation practice"
#C819 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C820 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C821 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d131_11) <- "Lowland crop n1"
#C822 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d131_12) <- "Lowland crop n2"
#C823 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d131_13) <- "Lowland crop n3"
#C824 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d131_14) <- "Lowland crop n4"
#C825 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d131_15) <- "Lowland crop n5"
#C826 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d131_16) <- "Upland crop n1"
#C827 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d131_17) <- "Upland crop n2"
#C828 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d131_18) <- "Upland crop n3"
#C829 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d131_19) <- "Upland crop n4"
#C830 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d131_110) <- "Upland crop n5"
#C831 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d131_111) <- "Upland crop n6"
#C832 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d131_112) <- "Homegarden crop n1"
#C833 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d131_113) <- "Homegarden crop n2"
#C834 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d131_114) <- "Homegarden crop n3"
#C835 = FACT, (table of correspondence), see choices below
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C836 = OK, (empty)
#C837 = CHAR, (empty), (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C838 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d132_11) <- "Lowland crop n1"
#C839 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d132_12) <- "Lowland crop n2"
#C840 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d132_13) <- "Lowland crop n3"
#C841 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d132_14) <- "Lowland crop n4"
#C842 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d132_15) <- "Lowland crop n5"
#C843 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d132_16) <- "Upland crop n1"
#C844 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d132_17) <- "Upland crop n2"
#C845 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d132_18) <- "Upland crop n3"
#C846 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d132_19) <- "Upland crop n4"
#C847 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d132_110) <- "Upland crop n5"
#C848 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d132_111) <- "Upland crop n6"
#C849 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d131_112) <- "Homegarden crop n1"
#C850 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d132_113) <- "Homegarden crop n2"
#C851 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d132_114) <- "Homegarden crop n3"
#C852 = FACT, (empty), (table of correspondence)
#C853 = OK, (empty)
#C854 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C855 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d133_11) <- "Lowland crop n1"
#C856 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d133_12) <- "Lowland crop n2"
#C857 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d133_13) <- "Lowland crop n3"
#C858 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d133_14) <- "Lowland crop n4"
#C859 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d133_15) <- "Lowland crop n5"
#C860 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d133_16) <- "Upland crop n1"
#C861 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d133_17) <- "Upland crop n2"
#C862 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d133_18) <- "Upland crop n3"
#C863 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d133_19) <- "Upland crop n4"
#C864 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d133_110) <- "Upland crop n5"
#C865 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d133_111) <- "Upland crop n6"
#C866 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d133_112) <- "Homegarden crop n1"
#C867 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d133_113) <- "Homegarden crop n2"
#C868 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d133_114) <- "Homegarden crop n3"
#C869 = FACT, (table of correspondence)
#C870 = OK, (empty)
#C871 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C872 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d134_11) <- "Lowland crop n1"
#C873 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d134_12) <- "Lowland crop n2"
#C874 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d134_13) <- "Lowland crop n3"
#C875 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d134_14) <- "Lowland crop n4"
#C876 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d134_15) <- "Lowland crop n5"
#C877 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d134_16) <- "Upland crop n1"
#C878 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d134_17) <- "Upland crop n2"
#C879 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d134_18) <- "Upland crop n3"
#C880 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d134_19) <- "Upland crop n4"
#C881 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d134_110) <- "Upland crop n5"
#C882 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d134_111) <- "Upland crop n6"
#C883 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d134_112) <- "Homegarden crop n1"
#C884 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d134_113) <- "Homegarden crop n2"
#C885 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d134_114) <- "Homegarden crop n3"
#C886 = FACT, (table of correspondence)
#C887 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C888 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C889 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d135_11) <- "Lowland crop n1"
#C890 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d135_12) <- "Lowland crop n2"
#C891 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d135_13) <- "Lowland crop n3"
#C892 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d135_14) <- "Lowland crop n4"
#C893 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d135_15) <- "Lowland crop n5"
#C894 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d135_16) <- "Upland crop n1"
#C895 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d135_17) <- "Upland crop n2"
#C896 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d135_18) <- "Upland crop n3"
#C897 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d135_19) <- "Upland crop n4"
#C898 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d135_110) <- "Upland crop n5"
#C899 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d135_111) <- "Upland crop n6"
#C900 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d135_112) <- "Homegarden crop n1"
#C901 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d135_113) <- "Homegarden crop n2"
#C902 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d135_114) <- "Homegarden crop n3"
#C903 = FACT, (table of correspondence)
#C904 = OK
#C905 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C906 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d136_11) <- "Lowland crop n1"
#C907 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d136_12) <- "Lowland crop n2"
#C908 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d136_13) <- "Lowland crop n3"
#C909 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d136_14) <- "Lowland crop n4"
#C910 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d136_15) <- "Lowland crop n5"
#C911 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d136_16) <- "Upland crop n1"
#C912 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d136_17) <- "Upland crop n2"
#C913 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d136_18) <- "Upland crop n3"
#C914 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d136_19) <- "Upland crop n4"
#C915 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d136_110) <- "Upland crop n5"
#C916 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d136_111) <- "Upland crop n6"
#C917 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d136_112) <- "Homegarden crop n1"
#C918 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d136_113) <- "Homegarden crop n2"
#C919 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d136_114) <- "Homegarden crop n3"
#C920 = FACT, (table of correspondence)
#C921 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C922 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C923 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d137_11) <- "Lowland crop n1"
#C924 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d137_12) <- "Lowland crop n2"
#C925 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d137_13) <- "Lowland crop n3"
#C926 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d137_14) <- "Lowland crop n4"
#C927 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d137_15) <- "Lowland crop n5"
#C928 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d137_16) <- "Upland crop n1"
#C929 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d137_17) <- "Upland crop n2"
#C930 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d137_18) <- "Upland crop n3"
#C931 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d137_19) <- "Upland crop n4"
#C932 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d137_110) <- "Upland crop n5"
#C933 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d137_111) <- "Upland crop n6"
#C934 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d137_112) <- "Homegarden crop n1"
#C935 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d137_113) <- "Homegarden crop n2"
#C936 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d137_114) <- "Homegarden crop n3"
#C937 = FACT, (table of correspondence)
#C938 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C939 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C940 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d138_11) <- "Lowland crop n1"
#C941 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d138_12) <- "Lowland crop n2"
#C942 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d138_13) <- "Lowland crop n3"
#C943 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d138_14) <- "Lowland crop n4"
#C944 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d138_15) <- "Lowland crop n5"
#C945 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d138_16) <- "Upland crop n1"
#C946 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d138_17) <- "Upland crop n2"
#C947 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d138_18) <- "Upland crop n3"
#C948 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d138_19) <- "Upland crop n4"
#C949 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d138_110) <- "Upland crop n5"
#C950 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d138_111) <- "Upland crop n6"
#C951 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d138_112) <- "Homegarden crop n1"
#C952 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d138_113) <- "Homegarden crop n2"
#C953 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d138_114) <- "Homegarden crop n3"
#C954 = FACT, (table of correspondence)
#C955 = OK, (empty)
#C956 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C957 = FACT, replace label: "No soil conservation practice"
var_label(HouseholdCambodia$d140) <- "No soil conservation practice"
HouseholdCambodia <- HouseholdCambodia %>% relocate(d140 , .after = d14)
#C958 = FACT, replace label: "Sowing in contour lines"
var_label(HouseholdCambodia$d141) <- "Sowing in contour lines"
#C959 = FACT, replace label: "Natural or planted grass strips"
var_label(HouseholdCambodia$d142) <- "Natural or planted grass strips"
#C960 = FACT, replace label: "Trees conservation in agricultural plots"
var_label(HouseholdCambodia$d143) <- "Trees conservation in agricultural plots"
#C961 = FACT, replace label: "Agroforestry (trees + crops)"
var_label(HouseholdCambodia$d144) <- "Agroforestry (trees + crops)"
#C962 = FACT, empty, replace label: "Crop residues maintained to cover the soil"
var_label(HouseholdCambodia$d145) <- "Crop residues maintained to cover the soil"
#C963 = FACT, empty, replace label: "Use of cover crops"
var_label(HouseholdCambodia$d146) <- "Use of cover crops"
#C964 = FACT, empty, replace label: "Reduced to no-tillage"
var_label(HouseholdCambodia$d147) <- "Reduced to no-tillage"
#C965 = FACT, empty, replace label: "Other"
var_label(HouseholdCambodia$d1499) <- "Other"
#C966 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C967 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C968 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d151_11) <- "Lowland crop n1"
#C969 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d151_12) <- "Lowland crop n2"
#C970 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d151_13) <- "Lowland crop n3"
#C971 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d151_14) <- "Lowland crop n4"
#C972 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d151_15) <- "Lowland crop n5"
#C973 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d151_16) <- "Upland crop n1"
#C974 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d151_17) <- "Upland crop n2"
#C975 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d151_18) <- "Upland crop n3"
#C976 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d151_19) <- "Upland crop n4"
#C977 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d151_110) <- "Upland crop n5"
#C978 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d151_111) <- "Upland crop n6"
#C979 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d151_112) <- "Homegarden crop n1"
#C980 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d151_113) <- "Homegarden crop n2"
#C981 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d151_114) <- "Homegarden crop n3"
#C982 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C983 = OK, (empty)
#C984 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C985 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d152_11) <- "Lowland crop n1"
#C986 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d152_12) <- "Lowland crop n2"
#C987 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d152_13) <- "Lowland crop n3"
#C988 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d152_14) <- "Lowland crop n4"
#C989 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d152_15) <- "Lowland crop n5"
#C990 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d152_16) <- "Upland crop n1"
#C991 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d152_17) <- "Upland crop n2"
#C992 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d152_18) <- "Upland crop n3"
#C993 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d152_19) <- "Upland crop n4"
#C994 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d152_110) <- "Upland crop n5"
#C995 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d152_111) <- "Upland crop n6"
#C996 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d152_112) <- "Homegarden crop n1"
#C997 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d152_113) <- "Homegarden crop n2"
#C998 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d152_114) <- "Homegarden crop n3"
#C999 = FACT, (table of correspondence)
#C1000 = OK, (empty)
#C1001 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1002 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d153_11) <- "Lowland crop n1"
#C1003 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d153_12) <- "Lowland crop n2"
#C1004 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d153_13) <- "Lowland crop n3"
#C1005 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d153_14) <- "Lowland crop n4"
#C1006 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d153_15) <- "Lowland crop n5"
#C1007 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d153_16) <- "Upland crop n1"
#C1008 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d153_17) <- "Upland crop n2"
#C1009 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d153_18) <- "Upland crop n3"
#C1010 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d153_19) <- "Upland crop n4"
#C1011 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d153_110) <- "Upland crop n5"
#C1012 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d153_111) <- "Upland crop n6"
#C1013 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d153_112) <- "Homegarden crop n1"
#C1014 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d153_113) <- "Homegarden crop n2"
#C1015 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d153_114) <- "Homegarden crop n3"
#C1016 = FACT, (table of correspondence)
#C1017 = OK, (empty)
#C1018 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1019 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d154_11) <- "Lowland crop n1"
#C1020 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d154_12) <- "Lowland crop n2"
#C1021 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d154_13) <- "Lowland crop n3"
#C1022 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d154_14) <- "Lowland crop n4"
#C1023 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d154_15) <- "Lowland crop n5"
#C1024 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d154_16) <- "Upland crop n1"
#C1025 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d154_17) <- "Upland crop n2"
#C1026 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d154_18) <- "Upland crop n3"
#C1027 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d154_19) <- "Upland crop n4"
#C1028 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d154_110) <- "Upland crop n5"
#C1029 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d154_111) <- "Upland crop n6"
#C1030 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d154_112) <- "Homegarden crop n1"
#C1031 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d154_113) <- "Homegarden crop n2"
#C1032 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d154_114) <- "Homegarden crop n3"
#C1033 = FACT, (table of correspondence)
#C1034 = OK, (empty)
#C1035 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1036 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d155_11) <- "Lowland crop n1"
#C1037 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d155_12) <- "Lowland crop n2"
#C1038 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d155_13) <- "Lowland crop n3"
#C1039 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d155_14) <- "Lowland crop n4"
#C1040 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d155_15) <- "Lowland crop n5"
#C1041 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d155_16) <- "Upland crop n1"
#C1042 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d155_17) <- "Upland crop n2"
#C1043 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d155_18) <- "Upland crop n3"
#C1044 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d155_19) <- "Upland crop n4"
#C1045 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d155_110) <- "Upland crop n5"
#C1046 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d155_111) <- "Upland crop n6"
#C1047 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d155_112) <- "Homegarden crop n1"
#C1048 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d155_113) <- "Homegarden crop n2"
#C1049 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d155_114) <- "Homegarden crop n3"
#C1050 = FACT, (table of correspondence)
#C1051 = OK, (empty)
#C1052 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1053 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d156_11) <- "Lowland crop n1"
#C1054 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d156_12) <- "Lowland crop n2"
#C1055 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d156_13) <- "Lowland crop n3"
#C1056 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d156_14) <- "Lowland crop n4"
#C1057 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d156_15) <- "Lowland crop n5"
#C1058 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d156_16) <- "Upland crop n1"
#C1059 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d156_17) <- "Upland crop n2"
#C1060 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d156_18) <- "Upland crop n3"
#C1061 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d156_19) <- "Upland crop n4"
#C1062 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d156_110) <- "Upland crop n5"
#C1063 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d156_111) <- "Upland crop n6"
#C1064 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d156_112) <- "Homegarden crop n1"
#C1065 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d156_113) <- "Homegarden crop n2"
#C1066 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d156_114) <- "Homegarden crop n3"
#C1067 = FACT, (table of correspondence)
#C1068 = OK, (empty)
#C1069 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#C1070 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d157_11) <- "Lowland crop n1"
#C1071 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d157_12) <- "Lowland crop n2"
#C1072 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d157_13) <- "Lowland crop n3"
#C1073 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d157_14) <- "Lowland crop n4"
#C1074 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d157_15) <- "Lowland crop n5"
#C1075 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d157_16) <- "Upland crop n1"
#C1076 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d157_17) <- "Upland crop n2"
#C1077 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d157_18) <- "Upland crop n3"
#C1078 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d157_19) <- "Upland crop n4"
#C1079 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d157_110) <- "Upland crop n5"
#C1080 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d157_111) <- "Upland crop n6"
#C1081 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d157_112) <- "Homegarden crop n1"
#C1082 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d157_113) <- "Homegarden crop n2"
#C1083 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d157_114) <- "Homegarden crop n3"
#C1084 = FACT, (table of correspondence)
#C1085 = OK, (empty)
#C1086 = CHAR, (Useless answer-MultipleCombined, use the following columns), (correspond to previous mentioned crops)
#replace label: "d158_1.  for which crop(s) do you use d14_oth"
var_label(HouseholdCambodia$d158_1) <- "d158_1.  for which crop(s) do you use d14_oth"
#C1087 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d158_11) <- "Lowland crop n1"
#C1088 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d158_12) <- "Lowland crop n2"
#C1089 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d158_13) <- "Lowland crop n3"
#C1090 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d158_14) <- "Lowland crop n4"
#C1091 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d158_15) <- "Lowland crop n5"
#C1092 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d158_16) <- "Upland crop n1"
#C1093 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d158_17) <- "Upland crop n2"
#C1094 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d158_18) <- "Upland crop n3"
#C1095 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d158_19) <- "Upland crop n4"
#C1096 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d158_110) <- "Upland crop n5"
#C1097 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d158_111) <- "Upland crop n6"
#C1098 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d157_112) <- "Homegarden crop n1"
#C1099 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d157_113) <- "Homegarden crop n2"
#C1100 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d157_114) <- "Homegarden crop n3"
#C1101 = FACT, (table of correspondence), replace label: "d158_2. what was your main motivation to implement d14_oth"
var_label(HouseholdCambodia$d158_2) <- "d158_2. what was your main motivation to implement d14_oth"
#C1102 = OK, (empty)
#C1103 = FACT, (table of correspondence), (see choices below),
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1104 = FACT, (table of correspondence), (0 = "no", 1 = "yes", 88 = "Do not know"),
#C1105 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
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
#C1106 = FACT,replace label: "Animal manure"
var_label(HouseholdCambodia$d181) <- "Animal manure"
#C1107 = FACT, replace label: "Compost (heap)"
var_label(HouseholdCambodia$d182) <- "Compost (heap)"
#C1108 = FACT, replace label: "Bokashi (fermented organic matter)"
var_label(HouseholdCambodia$d183) <- "Bokashi (fermented organic matter)"
#C1109 = FACT, replace label: "Legume-based green manure"
var_label(HouseholdCambodia$d184) <- "Legume-based green manure"
#C1110 = FACT, replace label: "Pulses in association and/or rotation with main crop"
var_label(HouseholdCambodia$d185) <- "Pulses in association and/or rotation with main crop"
#C1111 = FACT, replace label: "Cover crops in association and/or rotation with main crop"
var_label(HouseholdCambodia$d186) <- "Cover crops in association and/or rotation with main crop"
#C1112 = FACT, replace label: "Biochar"
var_label(HouseholdCambodia$d187) <- "Biochar"
#C1113 = FACT, replace label: "Crop residue maintenance"
var_label(HouseholdCambodia$d188) <- "Crop residue maintenance"
#C1114 = FACT, replace label: "Recycling crop waste"
var_label(HouseholdCambodia$d189) <- "Recycling crop waste"
#C1115 = FACT, replace label: "Ramial Wood Chip (RWC) or other wood chips"
var_label(HouseholdCambodia$d1810) <- "Ramial Wood Chip (RWC) or other wood chips"
#C1116 = FACT, replace label: "Organic agro-industrial waste"
var_label(HouseholdCambodia$d1811) <- "Organic agro-industrial waste"
#C1117 = FACT, replace label: "Other methods"
var_label(HouseholdCambodia$d1899) <- "Other methods"
#C1118 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1119 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_11. for which crop(s) do you use animal manure?"
var_label(HouseholdCambodia$d18_11) <- "d18_11. for which crop(s) do you use animal manure?"
#C1120 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_111) <- "Lowland crop n1"
#C1121 = FACT, change column name: "d18_112", replace label: "Lowland crop n2"
colnames(HouseholdCambodia)[1129] <- 'd18_112'
var_label(HouseholdCambodia$d18_112) <- "Lowland crop n2"
#C1122 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_113) <- "Lowland crop n3"
#C1123 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_114) <- "Lowland crop n4"
#C1124 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_115) <- "Lowland crop n5"
#C1125 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_116) <- "Upland crop n1"
#C1126 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_117) <- "Upland crop n2"
#C1127 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_118) <- "Upland crop n3"
#C1128 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_119) <- "Upland crop n4"
#C1129 = FACT, We change column name
colnames(HouseholdCambodia)[1129] <- 'd18_1110'
# replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_1110) <- "Upland crop n5"
#C1130 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_1111) <- "Upland crop n6"
#C1131 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_1112) <- "Homegarden crop n1"
#C1132 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_1113) <- "Homegarden crop n2"
#C1133 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_1114) <- "Homegarden crop n3"
#C1134 = FACT, (table of correspondence), (see choices below)
#To save money
#To save labor/time
#To improve yields 
#To improve soil/plant health
#Other
#C1135 = OK, (empty)
#C1136 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1137 = FACT,replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_211) <- "Lowland crop n1"
#C1138 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_212) <- "Lowland crop n2"
#C1139 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_213) <- "Lowland crop n3"
#C1140 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_214) <- "Lowland crop n4"
#C1141 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_215) <- "Lowland crop n5"
#C1142 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_216) <- "Upland crop n1"
#C1143 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_217) <- "Upland crop n2"
#C1144 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_218) <- "Upland crop n3"
#C1145 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_219) <- "Upland crop n4"
#C1146 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_2110) <- "Upland crop n5"
#C1147 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_2111) <- "Upland crop n6"
#C1148 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_2112) <- "Homegarden crop n1"
#C1149 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_2113) <- "Homegarden crop n2"
#C1150 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_2114) <- "Homegarden crop n3"
#C1151 = FACT, (table of correspondence)
#C1152 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1153 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1154 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_311) <- "Lowland crop n1"
#C1155 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_312) <- "Lowland crop n2"
#C1156 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_313) <- "Lowland crop n3"
#C1157 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_314) <- "Lowland crop n4"
#C1158 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_315) <- "Lowland crop n5"
#C1159 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_316) <- "Upland crop n1"
#C1160 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_317) <- "Upland crop n2"
#C1161 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_318) <- "Upland crop n3"
#C1162 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_319) <- "Upland crop n4"
#C1163 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_3110) <- "Upland crop n5"
#C1164 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_3111) <- "Upland crop n6"
#C1165 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_3112) <- "Homegarden crop n1"
#C1166 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_3113) <- "Homegarden crop n2"
#C1167 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_3114) <- "Homegarden crop n3"
#C1168 = FACT, (table of correspondence)
#C1169 = OK, (empty)
#C1170 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1171 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_411) <- "Lowland crop n1"
#C1172 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_412) <- "Lowland crop n2"
#C1173 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_413) <- "Lowland crop n3"
#C1174 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_414) <- "Lowland crop n4"
#C1175 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_415) <- "Lowland crop n5"
#C1176 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_416) <- "Upland crop n1"
#C1177 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_417) <- "Upland crop n2"
#C1178 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_418) <- "Upland crop n3"
#C1179 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_419) <- "Upland crop n4"
#C1180 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_4110) <- "Upland crop n5"
#C1181 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_4111) <- "Upland crop n6"
#C1182 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_4112) <- "Homegarden crop n1"
#C1183 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_4113) <- "Homegarden crop n2"
#C1184 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_4114) <- "Homegarden crop n3"
#C1185 = FACT, (table of correspondence)
#C1186 = OK, (empty)
#C1187 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1188 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_511) <- "Lowland crop n1"
#C1189 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_512) <- "Lowland crop n2"
#C1190 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_513) <- "Lowland crop n3"
#C1191 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_514) <- "Lowland crop n4"
#C1192 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_515) <- "Lowland crop n5"
#C1193 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_516) <- "Upland crop n1"
#C1194 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_517) <- "Upland crop n2"
#C1195 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_518) <- "Upland crop n3"
#C1196 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_519) <- "Upland crop n4"
#C1197 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_5110) <- "Upland crop n5"
#C1198 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_5111) <- "Upland crop n6"
#C1199 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_5112) <- "Homegarden crop n1"
#C1200 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_5113) <- "Homegarden crop n2"
#C1201 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_5114) <- "Homegarden crop n3"
#C1202 = FACT, (table of correspondence)
#C1203 = OK, (empty)
#C1204 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1205 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_611) <- "Lowland crop n1"
#C1206 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_612) <- "Lowland crop n2"
#C1207 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_613) <- "Lowland crop n3"
#C1208 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_614) <- "Lowland crop n4"
#C1209 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_615) <- "Lowland crop n5"
#C1210 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_616) <- "Upland crop n1"
#C1211 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_617) <- "Upland crop n2"
#C1212 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_618) <- "Upland crop n3"
#C1213 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_619) <- "Upland crop n4"
#C1214 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_6110) <- "Upland crop n5"
#C1215 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_6111) <- "Upland crop n6"
#C1216 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_6112) <- "Homegarden crop n1"
#C1217 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_6113) <- "Homegarden crop n2"
#C1218 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_6114) <- "Homegarden crop n3"
#C1219 = FACT, (table of correspondence)
#C1220 = OK, (empty)
#C1221 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1222 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_711) <- "Lowland crop n1"
#C1223 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_712) <- "Lowland crop n2"
#C1224 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_713) <- "Lowland crop n3"
#C1225 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_714) <- "Lowland crop n4"
#C1226 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_715) <- "Lowland crop n5"
#C1227 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_716) <- "Upland crop n1"
#C1228 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_717) <- "Upland crop n2"
#C1229 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_718) <- "Upland crop n3"
#C1230 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_719) <- "Upland crop n4"
#C1231 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_7110) <- "Upland crop n5"
#C1232 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_7111) <- "Upland crop n6"
#C1233 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_7112) <- "Homegarden crop n1"
#C1234 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_7113) <- "Homegarden crop n2"
#C1235 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_7114) <- "Homegarden crop n3"
#C1236 = FACT, (empty), (table of correspondence)
#C1237 = OK, (empty)
#C1238 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1239 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_811) <- "Lowland crop n1"
#C1240 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_812) <- "Lowland crop n2"
#C1241 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_813) <- "Lowland crop n3"
#C1242 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_814) <- "Lowland crop n4"
#C1243 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_815) <- "Lowland crop n5"
#C1244 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_816) <- "Upland crop n1"
#C1245 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_817) <- "Upland crop n2"
#C1246 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_818) <- "Upland crop n3"
#C1247 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_819) <- "Upland crop n4"
#C1248 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_8110) <- "Upland crop n5"
#C1249 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_8111) <- "Upland crop n6"
#C1250 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_8112) <- "Homegarden crop n1"
#C1251 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_8113) <- "Homegarden crop n2"
#C1252 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_8114) <- "Homegarden crop n3"
#C1253 = FACT, (table of correspondence)
#C1254 = OK, (empty)
#C1255 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1256 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_911) <- "Lowland crop n1"
#C1257 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_912) <- "Lowland crop n2"
#C1258 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_913) <- "Lowland crop n3"
#C1259 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_914) <- "Lowland crop n4"
#C1260 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_915) <- "Lowland crop n5"
#C1261 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_916) <- "Upland crop n1"
#C1262 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_917) <- "Upland crop n2"
#C1263 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_918) <- "Upland crop n3"
#C1264 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_919) <- "Upland crop n4"
#C1265 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_9110) <- "Upland crop n5"
#C1266 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_9111) <- "Upland crop n6"
#C1267 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_9112) <- "Homegarden crop n1"
#C1268 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_9113) <- "Homegarden crop n2"
#C1269 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_9114) <- "Homegarden crop n3"
#C1270 = FACT, (empty), (table of correspondence)
#C1271 = OK, (empty)
#C1272 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1273 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_1011) <- "Lowland crop n1"
#C1274 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_1012) <- "Lowland crop n2"
#C1275 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_1013) <- "Lowland crop n3"
#C1276 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_1014) <- "Lowland crop n4"
#C1277 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_1015) <- "Lowland crop n5"
#C1278 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_1016) <- "Upland crop n1"
#C1279 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_1017) <- "Upland crop n2"
#C1280 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_1018) <- "Upland crop n3"
#C1281 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_1019) <- "Upland crop n4"
#C1282 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_10110) <- "Upland crop n5"
#C1283 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_10111) <- "Upland crop n6"
#C1284 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_10112) <- "Homegarden crop n1"
#C1285 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_10113) <- "Homegarden crop n2"
#C1286 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_10114) <- "Homegarden crop n3"
#C1287 = FACT, (table of correspondence)
#C1288 = OK, (empty)
#C1289 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_111_a_a. for which crop(s) do you use organic agro-industrial waste?"
var_label(HouseholdCambodia$d18_111_a) <- "d18_111_a_a. for which crop(s) do you use organic agro-industrial waste?"
#C1290 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_111_1) <- "Lowland crop n1"
#C1291 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_111_2) <- "Lowland crop n2"
#C1292 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_111_3) <- "Lowland crop n3"
#C1293 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_111_4) <- "Lowland crop n4"
#C1294 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_111_5) <- "Lowland crop n5"
#C1295 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_111_6) <- "Upland crop n1"
#C1296 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_111_7) <- "Upland crop n2"
#C1297 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_111_8) <- "Upland crop n3"
#C1298 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_111_9) <- "Upland crop n4"
#C1299 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_111_10) <- "Upland crop n5"
#C1300 = FACT,  (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_111_11) <- "Upland crop n6"
#C1301 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_11112) <- "Homegarden crop n1"
#C1302 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_11113) <- "Homegarden crop n2"
#C1303 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_11114) <- "Homegarden crop n3"
#C1304 = FACT,  (empty), (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdCambodia$d18_112) <- "d18_112. What was your main motivation to implement this practice?"
#C1305 = OK, (empty)
#C1306 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d18_991. for which crop(s) do you use d18_oth ?"
var_label(HouseholdCambodia$d18_991) <- "d18_991. for which crop(s) do you use d18_oth?"
#C1307 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d18_9911) <- "Lowland crop n1"
#C1308 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d18_9912) <- "Lowland crop n2"
#C1309 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d18_9913) <- "Lowland crop n3"
#C1310 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d18_9914) <- "Lowland crop n4"
#C1311 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d18_9915) <- "Lowland crop n5"
#C1312 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d18_9916) <- "Upland crop n1"
#C1313 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d18_9917) <- "Upland crop n2"
#C1314 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d18_9918) <- "Upland crop n3"
#C1315 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d18_9919) <- "Upland crop n4"
#C1316 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d18_99110) <- "Upland crop n5"
#C1317 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d18_99111) <- "Upland crop n6"
#C1318 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d18_99112) <- "Homegarden crop n1"
#C1319 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d18_99113) <- "Homegarden crop n2"
#C1320 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d18_99114) <- "Homegarden crop n3"
#C1321 = FACT, (table of correspondence), replace label: "d18_112. What was your main motivation to implement this practice?"
var_label(HouseholdCambodia$d18_992) <- "d18_112. What was your main motivation to implement this practice?"
#C1322 = OK, (empty)
#C1323 = FACT, (table of correspondence), (see option below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don t want to do things differently from my neighbors
#C1324 = FACT, (table of correspondence), (0 = no, 1 = yes, 88 = I don't know)
#C1325 = CHAR, (see choices below), (Useless answer-MultipleCombined, use the following columns)
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
#C1326 = FACT,replace label: "Crop rotation / intercropping"
var_label(HouseholdCambodia$d211) <- "Crop rotation / intercropping"
#C1227 = FACT, replace label: "Cover crops"
var_label(HouseholdCambodia$d212) <- "Cover crops"
#C1328 = FACT, replace label: "Mulching / shading"
var_label(HouseholdCambodia$d213) <- "Mulching / shading"
#C1329 = FACT, replace label: "Sowing date / rate / depth"
var_label(HouseholdCambodia$d214) <- "Sowing date / rate / depth"
#C1330 = FACT, replace label: "Crop spatial arrangement"
var_label(HouseholdCambodia$d215) <- "Crop spatial arrangement"
#C1331 = FACT, replace label: "Seed cleaning before sowing"
var_label(HouseholdCambodia$d216) <- "Seed cleaning before sowing"
#C1332 = FACT, replace label: "Cultivar choice"
var_label(HouseholdCambodia$d217) <- "Cultivar choice"
#C1333 = FACT, replace label: "Crop mixtures"
var_label(HouseholdCambodia$d218) <- "Crop mixtures"
#C1334 = FACT, replace label: "Nutrient placement"
var_label(HouseholdCambodia$d219) <- "Nutrient placement"
#C1335 = FACT, replace label: "Patch/ban spraying"
var_label(HouseholdCambodia$d2110) <- "Patch/ban spraying"
#C1336 = FACT, replace label: "Bioherbicide"
var_label(HouseholdCambodia$d2111) <- "Bioherbicide"
#C1337 = FACT, replace label: "Mowing / slashing"
var_label(HouseholdCambodia$d2112) <- "Mowing / slashing"
#C1338 = FACT, replace label: "Grazing"
var_label(HouseholdCambodia$d2113) <- "Grazing"
#C1339 = FACT, replace label: "Post harvest weed seed destruction in field"
var_label(HouseholdCambodia$d2114) <- "Post harvest weed seed destruction in field"
#C1340 = FACT, replace label: "Any other methods"
var_label(HouseholdCambodia$d2199) <- "Any other methods"
#C1341 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1342 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1343 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_121) <- "Lowland crop n1"
#C1344 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_122) <- "Lowland crop n2"
#C1345 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_123) <- "Lowland crop n3"
#C1346 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_124) <- "Lowland crop n4"
#C1347 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_125) <- "Lowland crop n5"
#C1348 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_126) <- "Upland crop n1"
#C1349 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_127) <- "Upland crop n2"
#C1350 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_128) <- "Upland crop n3"
#C1351 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_129) <- "Upland crop n4"
#C1352 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_1210) <- "Upland crop n5"
#C1353 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_1211) <- "Upland crop n6"
#C1354 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_1212) <- "Homegarden crop n1"
#C1355 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_1213) <- "Homegarden crop n2"
#C1356 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_1214) <- "Homegarden crop n3"
#C1357 = FACT, (table of correspondence)
#C1358 = OK, (empty)
#C1359 = FACT, (table of correspondence, see crop corresponence)
#C1360 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_221) <- "Lowland crop n1"
#C1361 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_222) <- "Lowland crop n2"
#C1362 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_223) <- "Lowland crop n3"
#C1363 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_224) <- "Lowland crop n4"
#C1364 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_225) <- "Lowland crop n5"
#C1365 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_226) <- "Upland crop n1"
#C1366 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_227) <- "Upland crop n2"
#C1367 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_228) <- "Upland crop n3"
#C1368 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_229) <- "Upland crop n4"
#C1369 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_2210) <- "Upland crop n5"
#C1370 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_2211) <- "Upland crop n6"
#C1371 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_2212) <- "Homegarden crop n1"
#C1372 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_2213) <- "Homegarden crop n2"
#C1373 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_2214) <- "Homegarden crop n3"
#C1374 = FACT, (table of correspondence)
#C1375 = OK, (empty),
#C1376 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1377 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_321) <- "Lowland crop n1"
#C1378 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_322) <- "Lowland crop n2"
#C1379 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_323) <- "Lowland crop n3"
#C1380 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_324) <- "Lowland crop n4"
#C1381 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_325) <- "Lowland crop n5"
#C1382 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_326) <- "Upland crop n1"
#C1383 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_327) <- "Upland crop n2"
#C1384 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_328) <- "Upland crop n3"
#C1385 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_329) <- "Upland crop n4"
#C1386 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_3210) <- "Upland crop n5"
#C1387 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_3211) <- "Upland crop n6"
#C1388 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_3212) <- "Homegarden crop n1"
#C1389 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_3213) <- "Homegarden crop n2"
#C1390 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_3214) <- "Homegarden crop n3"
#C1391 = FACT, (empty), (table of correspondence)
#C1392 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1393 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1394 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_421) <- "Lowland crop n1"
#C1395 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_422) <- "Lowland crop n2"
#C1396 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_423) <- "Lowland crop n3"
#C1397 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_424) <- "Lowland crop n4"
#C1398 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_425) <- "Lowland crop n5"
#C1399 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_426) <- "Upland crop n1"
#C1400 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_427) <- "Upland crop n2"
#C1401 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_428) <- "Upland crop n3"
#C1402 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_429) <- "Upland crop n4"
#C1403 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_4210) <- "Upland crop n5"
#C1404 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_4211) <- "Upland crop n6"
#C1405 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_4212) <- "Homegarden crop n1"
#C1406 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_4213) <- "Homegarden crop n2"
#C1407 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_4214) <- "Homegarden crop n3"
#C1408 = FACT, (table of correspondence)
#C1409 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1410 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1411 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_521) <- "Lowland crop n1"
#C1412 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_522) <- "Lowland crop n2"
#C1413 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_523) <- "Lowland crop n3"
#C1414 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_524) <- "Lowland crop n4"
#C1415 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_525) <- "Lowland crop n5"
#C1416 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_526) <- "Upland crop n1"
#C1417 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_527) <- "Upland crop n2"
#C1418 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_528) <- "Upland crop n3"
#C1419 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_529) <- "Upland crop n4"
#C1420 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_5210) <- "Upland crop n5"
#C1421 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_5211) <- "Upland crop n6"
#C1422 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_5212) <- "Homegarden crop n1"
#C1423 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_5213) <- "Homegarden crop n2"
#C1424 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_5214) <- "Homegarden crop n3"
#C1425 = FACT, (table of correspondence), (empty)
#C1426 = OK, (empty)
#C1427 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1428 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_621) <- "Lowland crop n1"
#C1429 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_622) <- "Lowland crop n2"
#C1430 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_623) <- "Lowland crop n3"
#C1431 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_624) <- "Lowland crop n4"
#C1432 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_625) <- "Lowland crop n5"
#C1433 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_626) <- "Upland crop n1"
#C1434 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_627) <- "Upland crop n2"
#C1435 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_628) <- "Upland crop n3"
#C1436 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_629) <- "Upland crop n4"
#C1437 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_6210) <- "Upland crop n5"
#C1438 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_6211) <- "Upland crop n6"
#C1439 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_6212) <- "Homegarden crop n1"
#C1440 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_6213) <- "Homegarden crop n2"
#C1441 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_6214) <- "Homegarden crop n3"
#C1442 = FACT, (table of correspondence)
#C1443 = OK, (empty)
#C1444 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1445 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_721) <- "Lowland crop n1"
#C1446 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_722) <- "Lowland crop n2"
#C1447 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_723) <- "Lowland crop n3"
#C1448 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_724) <- "Lowland crop n4"
#C1449 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_725) <- "Lowland crop n5"
#C1450 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_726) <- "Upland crop n1"
#C1451 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_727) <- "Upland crop n2"
#C1452 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_728) <- "Upland crop n3"
#C1453 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_729) <- "Upland crop n4"
#C1454 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_7210) <- "Upland crop n5"
#C1455 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_7211) <- "Upland crop n6"
#C1456 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_7212) <- "Homegarden crop n1"
#C1457 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_7213) <- "Homegarden crop n2"
#C1458 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_7214) <- "Homegarden crop n3"
#C1459 = FACT, (table of correspondence)
#C1460 = OK, (empty)
#C1461 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1462 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_821) <- "Lowland crop n1"
#C1463 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_822) <- "Lowland crop n2"
#C1464 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_823) <- "Lowland crop n3"
#C1465 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_824) <- "Lowland crop n4"
#C1466 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_825) <- "Lowland crop n5"
#C1467 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_826) <- "Upland crop n1"
#C1468 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_827) <- "Upland crop n2"
#C1469 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_828) <- "Upland crop n3"
#C1470 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_829) <- "Upland crop n4"
#C1471 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_8210) <- "Upland crop n5"
#C1472 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_8211) <- "Upland crop n6"
#C1473 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_8212) <- "Homegarden crop n1"
#C1474 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_8213) <- "Homegarden crop n2"
#C1475 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_8214) <- "Homegarden crop n3"
#C1476 = FACT, (table of correspondence)
#C1477 = OK, (empty)
#C1478 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1479 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_921) <- "Lowland crop n1"
#C1480 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_922) <- "Lowland crop n2"
#C1481 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_923) <- "Lowland crop n3"
#C1482 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_924) <- "Lowland crop n4"
#C1483 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_925) <- "Lowland crop n5"
#C1484 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_926) <- "Upland crop n1"
#C1485 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_927) <- "Upland crop n2"
#C1486 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_928) <- "Upland crop n3"
#C1487 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_929) <- "Upland crop n4"
#C1488 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_9210) <- "Upland crop n5"
#C1489 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_9211) <- "Upland crop n6"
#C1490 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_9212) <- "Homegarden crop n1"
#C1491 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_9213) <- "Homegarden crop n2"
#C1492 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_9214) <- "Homegarden crop n3"
#C1493 = FACT, (table of correspondence)
#C1494 = OK, (empty)
#C1495 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1496 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_1021) <- "Lowland crop n1"
#C1497 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_1022) <- "Lowland crop n2"
#C1498 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_1023) <- "Lowland crop n3"
#C1499 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_1024) <- "Lowland crop n4"
#C1500 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_1025) <- "Lowland crop n5"
#C1501 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_1026) <- "Upland crop n1"
#C1502 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_1027) <- "Upland crop n2"
#C1503 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_1028) <- "Upland crop n3"
#C1504 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_1029) <- "Upland crop n4"
#C1505 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_10210) <- "Upland crop n5"
#C1506 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_10211) <- "Upland crop n6"
#C1507 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_10212) <- "Homegarden crop n1"
#C1508 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_10213) <- "Homegarden crop n2"
#C1509 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_10214) <- "Homegarden crop n3"
#C1510 = FACT, (table of correspondence)
#C1511 = OK, (empty)
#C1512 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1513 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_1121) <- "Lowland crop n1"
#C1514 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_1122) <- "Lowland crop n2"
#C1515 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_1123) <- "Lowland crop n3"
#C1516 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_1124) <- "Lowland crop n4"
#C1517= FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_1125) <- "Lowland crop n5"
#C1518 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_1126) <- "Upland crop n1"
#C1519 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_1127) <- "Upland crop n2"
#C1520 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_1128) <- "Upland crop n3"
#C1521 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_1129) <- "Upland crop n4"
#C1522 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_11210) <- "Upland crop n5"
#C1523 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_11211) <- "Upland crop n6"
#C1524 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_11212) <- "Homegarden crop n1"
#C1525 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_11213) <- "Homegarden crop n2"
#C1526 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_11214) <- "Homegarden crop n3"
#C1527 = FACT, (table of correspondence), replace label: "d21_123. what was your main motivation to implement crop Mowing / slashing"
var_label(HouseholdCambodia$d21_113) <- "d21_123. what was your main motivation to implement bio-herbicide"
#C1528 = OK, (empty)
#C1529 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1530 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_1221) <- "Lowland crop n1"
#C1531 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_1222) <- "Lowland crop n2"
#C1532 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_1223) <- "Lowland crop n3"
#C1533 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_1224) <- "Lowland crop n4"
#C1534 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_1225) <- "Lowland crop n5"
#C1535 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_1226) <- "Upland crop n1"
#C1536 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_1227) <- "Upland crop n2"
#C1537 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_1228) <- "Upland crop n3"
#C1538 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_1229) <- "Upland crop n4"
#C1539 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_12210) <- "Upland crop n5"
#C1540 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_12211) <- "Upland crop n6"
#C1541 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_12212) <- "Homegarden crop n1"
#C1542 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_12213) <- "Homegarden crop n2"
#C1543 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_12214) <- "Homegarden crop n3"
#C1544 = FACT, (table of correspondence)
#C1545 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1546 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1547 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_1321) <- "Lowland crop n1"
#C1548 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_1322) <- "Lowland crop n2"
#C1549 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_1323) <- "Lowland crop n3"
#C1550 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_1324) <- "Lowland crop n4"
#C1551 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_1325) <- "Lowland crop n5"
#C1552 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_1326) <- "Upland crop n1"
#C1553 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_1327) <- "Upland crop n2"
#C1554 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_1328) <- "Upland crop n3"
#C1555 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_1329) <- "Upland crop n4"
#C1556 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_13210) <- "Upland crop n5"
#C1557 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_13211) <- "Upland crop n6"
#C1558 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_13212) <- "Homegarden crop n1"
#C1559 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_13213) <- "Homegarden crop n2"
#C1560 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_13214) <- "Homegarden crop n3"
#C1561 = FACT, (table of correspondence)
#C1562 = OK, (empty)
#C1563 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)#C1564 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_1421) <- "Lowland crop n1"
#C1565 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_1422) <- "Lowland crop n2"
#C1566 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_1423) <- "Lowland crop n3"
#C1567 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_1424) <- "Lowland crop n4"
#C1568 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_1425) <- "Lowland crop n5"
#C1569 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_1426) <- "Upland crop n1"
#C1570 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_1427) <- "Upland crop n2"
#C1571 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_1428) <- "Upland crop n3"
#C1572 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_1429) <- "Upland crop n4"
#C1573 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_14210) <- "Upland crop n5"
#C1574 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_14211) <- "Upland crop n6"
#C1575 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_14212) <- "Homegarden crop n1"
#C1576 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_14213) <- "Homegarden crop n2"
#C1577 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_14214) <- "Homegarden crop n3"
#C1578 = FACT, (table of correspondence),
#C1579 = OK, (empty)
#C1580 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1581 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d21_9921) <- "Lowland crop n1"
#C1582 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d21_9922) <- "Lowland crop n2"
#C1583 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d21_9923) <- "Lowland crop n3"
#C1584 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d21_9924) <- "Lowland crop n4"
#C1585 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d21_9925) <- "Lowland crop n5"
#C1586 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d21_9926) <- "Upland crop n1"
#C1587 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d21_9927) <- "Upland crop n2"
#C1588 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d21_9928) <- "Upland crop n3"
#C1589 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d21_9929) <- "Upland crop n4"
#C1590 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d21_99210) <- "Upland crop n5"
#C1591 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d21_99211) <- "Upland crop n6"
#C1592 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d21_99212) <- "Homegarden crop n1"
#C1593 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d21_99213) <- "Homegarden crop n2"
#C1594 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d21_99214) <- "Homegarden crop n3"
#C1595 = FACT, (table of correspondence),
#C1596 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1597 = FACT, (table of correspondence), (see options below)
#I do not know them 
#They are too costly 
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1598 = FACT, (table of correspondence), (see options below)
#Synthetic herbicide without mechanical weeding
#Frequent mechanical weeding (more than three times per year) without synthetic herbicide
#Mixed management using herbicide and mechanical weeding    
#Do not know  
#C1599 = FACT, (table of correspondence), (0 = "no", 1 = "yes", 88 = "Do not know"),
#C1600 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1601 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1602 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see option below)
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
#Weaver ant
#Any other methods
#C1603 = FACT, replace label: "Crop rotation / intercropping"
var_label(HouseholdCambodia$d271) <- "Crop rotation / intercropping"
#C1604 = FACT, replace label: "Flower strips"
var_label(HouseholdCambodia$d272) <- "Flower strips"
#C1605 = FACT, replace label: "Hedgerows"
var_label(HouseholdCambodia$d273) <- "Hedgerows"
#C1606 = FACT, replace label: "Soil health maintenance/improvement"
var_label(HouseholdCambodia$d274) <- "Soil health maintenance/improvement"
#C1607 = FACT, replace label: "Sanitation practices (removal of damaged/infected plants and fruits)"
var_label(HouseholdCambodia$d275) <- "Sanitation practices (removal of damaged/infected plants and fruits)"
#C1608 = FACT, replace label: "Planting date"
var_label(HouseholdCambodia$d276) <- "Planting date"
#C1609 = FACT, replace label: "Water and nutrient management"
var_label(HouseholdCambodia$d277) <- "Water and nutrient management"
#C1610 = FACT, replace label: "Cultivar choice (tolerant/resistant) / cultivar mixture"
var_label(HouseholdCambodia$d278) <- "Cultivar choice (tolerant/resistant) / cultivar mixture"
#C1611 = FACT, replace label: "Biopesticide / organic pesticide"
var_label(HouseholdCambodia$d279) <- "Biopesticide / organic pesticide"
#C1612 = FACT, replace label: "Commercial biological control agents (BCAs)"
var_label(HouseholdCambodia$d2710) <- "Commercial biological control agents (BCAs)"
#C1613 = FACT, replace label: "Home-made efficient microorganism (EM)"
var_label(HouseholdCambodia$d2711) <- "Home-made efficient microorganism (EM)"
#C1614 = FACT, replace label: "Commercial efficient microorganism (EM)"
var_label(HouseholdCambodia$d2712) <- "Commercial efficient microorganism (EM)"
#C1615 = FACT, replace label: "Pheromone traps"
var_label(HouseholdCambodia$d2713) <- "Pheromone traps"
#C1616 = FACT, replace label: "Protein baits"
var_label(HouseholdCambodia$d2714) <- "Protein baits"
#C1617 = FACT, replace label: "Protein baits"
var_label(HouseholdCambodia$d2715) <- "Protein baits"
#C1618 = FACT, replace label: "Any other methods"
var_label(HouseholdCambodia$d2799) <- "Any other methods"
#C1619 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1620 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1621 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_111) <- "Lowland crop n1"
#C1622 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_112) <- "Lowland crop n2"
#C1623 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_113) <- "Lowland crop n3"
#C1624 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_114) <- "Lowland crop n4"
#C1625 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_115) <- "Lowland crop n5"
#C1626 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_116) <- "Upland crop n1"
#C1627 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_117) <- "Upland crop n2"
#C1628 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_118) <- "Upland crop n3"
#C1629 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_119) <- "Upland crop n4"
#C1630 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_1110) <- "Upland crop n5"
#C1631 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_1111) <- "Upland crop n6"
#C1632 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_1112) <- "Homegarden crop n1"
#C1633 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_1113) <- "Homegarden crop n2"
#C1634 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_1114) <- "Homegarden crop n3"
#C1635 = FACT, (table of correspondence)
#C1636 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1637 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1638 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_211) <- "Lowland crop n1"
#C1639 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_212) <- "Lowland crop n2"
#C1640 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_213) <- "Lowland crop n3"
#C1641 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_214) <- "Lowland crop n4"
#C1642 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_215) <- "Lowland crop n5"
#C1643 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_216) <- "Upland crop n1"
#C1644 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_217) <- "Upland crop n2"
#C1645 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_218) <- "Upland crop n3"
#C1646 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_219) <- "Upland crop n4"
#C1647 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_2110) <- "Upland crop n5"
#C1648 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_2111) <- "Upland crop n6"
#C1649 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_2112) <- "Homegarden crop n1"
#C1650 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_2113) <- "Homegarden crop n2"
#C1651 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_2114) <- "Homegarden crop n3"
#C1652 = FACT, (empty), (table of correspondence)
#C1653 = OK, (empty)
#C1654 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1655 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_311) <- "Lowland crop n1"
#C1656 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_312) <- "Lowland crop n2"
#C1657 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_313) <- "Lowland crop n3"
#C1658 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_314) <- "Lowland crop n4"
#C1659 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_315) <- "Lowland crop n5"
#C1660 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_316) <- "Upland crop n1"
#C1661 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_317) <- "Upland crop n2"
#C1662 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_318) <- "Upland crop n3"
#C1663 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_319) <- "Upland crop n4"
#C1664 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_3110) <- "Upland crop n5"
#C1665 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_3111) <- "Upland crop n6"
#C1666 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_3112) <- "Homegarden crop n1"
#C1667 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_3113) <- "Homegarden crop n2"
#C1668 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_3114) <- "Homegarden crop n3"
#C1669 = FACT, (empty), (table of correspondence)
#C1670 = OK, (empty)
#C1671 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1672 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_411) <- "Lowland crop n1"
#C1673 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_412) <- "Lowland crop n2"
#C1674 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_413) <- "Lowland crop n3"
#C1675 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_414) <- "Lowland crop n4"
#C1676 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_415) <- "Lowland crop n5"
#C1677 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_416) <- "Upland crop n1"
#C1678 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_417) <- "Upland crop n2"
#C1679 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_418) <- "Upland crop n3"
#C1680 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_419) <- "Upland crop n4"
#C1681 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_4110) <- "Upland crop n5"
#C1682 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_4111) <- "Upland crop n6"
#C1683 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_4112) <- "Homegarden crop n1"
#C1684 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_4113) <- "Homegarden crop n2"
#C1685 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_4114) <- "Homegarden crop n3"
#C1686 = FACT, (table of correspondence)
#C1687 = OK, (empty)
#C1688 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1689 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_511) <- "Lowland crop n1"
#C1690 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_512) <- "Lowland crop n2"
#C1691 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_513) <- "Lowland crop n3"
#C1692 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_514) <- "Lowland crop n4"
#C1693 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_515) <- "Lowland crop n5"
#C1694 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_516) <- "Upland crop n1"
#C1695 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_517) <- "Upland crop n2"
#C1696 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_518) <- "Upland crop n3"
#C1697 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_519) <- "Upland crop n4"
#C1698 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_5110) <- "Upland crop n5"
#C1699 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_5111) <- "Upland crop n6"
#C1700 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_5112) <- "Homegarden crop n1"
#C1701 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_5113) <- "Homegarden crop n2"
#C1702 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_5114) <- "Homegarden crop n3"
#C1703 = FACT, (table of correspondence)
#C1704 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1705 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1706 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_611) <- "Lowland crop n1"
#C1707 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_612) <- "Lowland crop n2"
#C1708 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_613) <- "Lowland crop n3"
#C1709 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_614) <- "Lowland crop n4"
#C1710 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_615) <- "Lowland crop n5"
#C1711 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_616) <- "Upland crop n1"
#C1712 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_617) <- "Upland crop n2"
#C1713 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_618) <- "Upland crop n3"
#C1714 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_619) <- "Upland crop n4"
#C1715 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_6110) <- "Upland crop n5"
#C1716 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_6111) <- "Upland crop n6"
#C1717 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_6112) <- "Homegarden crop n1"
#C1718 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_6113) <- "Homegarden crop n2"
#C1719 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_6114) <- "Homegarden crop n3"
#C1720 = FACT, (table of correspondence)
#C1721 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1722 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1723 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_711) <- "Lowland crop n1"
#C1724 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_712) <- "Lowland crop n2"
#C1725 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_713) <- "Lowland crop n3"
#C1726 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_714) <- "Lowland crop n4"
#C1727 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_715) <- "Lowland crop n5"
#C1728 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_716) <- "Upland crop n1"
#C1729 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_717) <- "Upland crop n2"
#C1730 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_718) <- "Upland crop n3"
#C1731 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_719) <- "Upland crop n4"
#C1732 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_7110) <- "Upland crop n5"
#C1733 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_7111) <- "Upland crop n6"
#C1734 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_7112) <- "Homegarden crop n1"
#C1735 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_7113) <- "Homegarden crop n2"
#C1736 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_7114) <- "Homegarden crop n3"
#C1737 = FACT, (table of correspondence)
#C1738 = OK, (empty)
#C1739 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1740 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_811) <- "Lowland crop n1"
#C1741 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_812) <- "Lowland crop n2"
#C1742 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_813) <- "Lowland crop n3"
#C1743 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_814) <- "Lowland crop n4"
#C1744 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_815) <- "Lowland crop n5"
#C1745 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_816) <- "Upland crop n1"
#C1746 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_817) <- "Upland crop n2"
#C1747 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_818) <- "Upland crop n3"
#C1748 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_819) <- "Upland crop n4"
#C1749 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_8110) <- "Upland crop n5"
#C1750 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_8111) <- "Upland crop n6"
#C1751 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_8112) <- "Homegarden crop n1"
#C1752 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_8113) <- "Homegarden crop n2"
#C1753 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_8114) <- "Homegarden crop n3"
#C1754 = FACT, (table of correspondence)
#C1755 = OK, (empty)
#C1756 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1757 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_911) <- "Lowland crop n1"
#C1758 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_912) <- "Lowland crop n2"
#C1759 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_913) <- "Lowland crop n3"
#C1760 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_914) <- "Lowland crop n4"
#C1761 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_915) <- "Lowland crop n5"
#C1762 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_916) <- "Upland crop n1"
#C1763 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_917) <- "Upland crop n2"
#C1764 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_918) <- "Upland crop n3"
#C1765 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_919) <- "Upland crop n4"
#C1766 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_9110) <- "Upland crop n5"
#C1767 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_9111) <- "Upland crop n6"
#C1768 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_9112) <- "Homegarden crop n1"
#C1769 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_9113) <- "Homegarden crop n2"
#C1770 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_9114) <- "Homegarden crop n3"
#C1771 = FACT, (table of correspondence)
#C1772 = OK, (empty)
#C1773 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1774 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_1011) <- "Lowland crop n1"
#C1775 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_1012) <- "Lowland crop n2"
#C1776 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_1013) <- "Lowland crop n3"
#C1777 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_1014) <- "Lowland crop n4"
#C1778 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_1015) <- "Lowland crop n5"
#C1779 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_1016) <- "Upland crop n1"
#C1780 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_1017) <- "Upland crop n2"
#C1781 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_1018) <- "Upland crop n3"
#C1782 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_1019) <- "Upland crop n4"
#C1783 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_10110) <- "Upland crop n5"
#C1784 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_10111) <- "Upland crop n6"
#C1785 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_10112) <- "Homegarden crop n1"
#C1786 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_10113) <- "Homegarden crop n2"
#C1787 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_10114) <- "Homegarden crop n3"
#C1788 = FACT, (table of correspondence)
#C1789 = OK, (empty)
#C1790 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns), replace label: "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
var_label(HouseholdCambodia$d27111) <- "d27_111. for which crop(s) do you use Home-made efficient microorganism (EM)"
#C1791 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27111_1) <- "Lowland crop n1"
#C1792 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27111_2) <- "Lowland crop n2"
#C1793 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27111_3) <- "Lowland crop n3"
#C1794 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27111_3) <- "Lowland crop n4"
#C1795 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27111_5) <- "Lowland crop n5"
#C1796 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27111_6) <- "Upland crop n1"
#C1797 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27111_7) <- "Upland crop n2"
#C1798 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27111_8) <- "Upland crop n3"
#C1799 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27111_9) <- "Upland crop n4"
#C1800 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27111_10) <- "Upland crop n5"
#C1801 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27111_11) <- "Upland crop n6"
#C1802 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27111_12) <- "Homegarden crop n1"
#C1803 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_11113) <- "Homegarden crop n2"
#C1804 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27111_14) <- "Homegarden crop n3"
#C1805 = FACT, (empty), (table of correspondence),#C1686 = OK, (empty)
#C1806 = OK, (empty)
#C1807 = CHAR, (empty), (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1808 = FACT, (empty), replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_1211) <- "Lowland crop n1"
#C1809 = FACT, (empty), replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_1212) <- "Lowland crop n2"
#C1810 = FACT, (empty), replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_1213) <- "Lowland crop n3"
#C1811 = FACT, (empty), replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_1214) <- "Lowland crop n4"
#C1812 = FACT, (empty), replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_1215) <- "Lowland crop n5"
#C1813 = FACT, (empty), replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_1216) <- "Upland crop n1"
#C1814 = FACT, (empty), replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_1217) <- "Upland crop n2"
#C1815 = FACT, (empty), replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_1218) <- "Upland crop n3"
#C1816 = FACT, (empty), replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_1219) <- "Upland crop n4"
#C1817 = FACT, (empty), replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_12110) <- "Upland crop n5"
#C1818 = FACT, (empty), replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_12111) <- "Upland crop n6"
#C1819 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_12112) <- "Homegarden crop n1"
#C1820 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_12113) <- "Homegarden crop n2"
#C1821 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_12114) <- "Homegarden crop n3"
#C1822 = FACT, (empty), (table of correspondence)
#C1823 = OK, (empty)
#C1824 = FACT, (correspond to crops numbers)
#C1825 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_1311) <- "Lowland crop n1"
#C1826 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_1312) <- "Lowland crop n2"
#C1827 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_1313) <- "Lowland crop n3"
#C1828 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_1314) <- "Lowland crop n4"
#C1829 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_1315) <- "Lowland crop n5"
#C1830 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_1316) <- "Upland crop n1"
#C1831 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_1317) <- "Upland crop n2"
#C1832 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_1318) <- "Upland crop n3"
#C1833 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_1319) <- "Upland crop n4"
#C1834 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_13110) <- "Upland crop n5"
#C1835 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_13111) <- "Upland crop n6"
#C1836 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_13112) <- "Homegarden crop n1"
#C1837 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_13113) <- "Homegarden crop n2"
#C1838 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_13114) <- "Homegarden crop n3"
#C1839 = FACT, (table of correspondence)
#C1840 = OK, (empty)
#C1841 = FACT, (correspond to crops numbers)
#C1842 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_1411) <- "Lowland crop n1"
#C1843 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_1412) <- "Lowland crop n2"
#C1844 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_1413) <- "Lowland crop n3"
#C1845 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_1414) <- "Lowland crop n4"
#C1846 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_1415) <- "Lowland crop n5"
#C1847 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_1416) <- "Upland crop n1"
#C1848 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_1417) <- "Upland crop n2"
#C1849 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_1418) <- "Upland crop n3"
#C1850 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_1419) <- "Upland crop n4"
#C1851 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_14110) <- "Upland crop n5"
#C1852 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_14111) <- "Upland crop n6"
#C1853 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_14112) <- "Homegarden crop n1"
#C1854 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_14113) <- "Homegarden crop n2"
#C1855 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_14114) <- "Homegarden crop n3"
#C1856 = FACT, (table of correspondence), (empty)
#C1857 = OK, (empty)
#C1858 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1859 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_1511) <- "Lowland crop n1"
#C1860 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_1512) <- "Lowland crop n2"
#C1861 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_1513) <- "Lowland crop n3"
#C1862 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_1514) <- "Lowland crop n4"
#C1863 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_1515) <- "Lowland crop n5"
#C1864 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_1516) <- "Upland crop n1"
#C1865 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_1517) <- "Upland crop n2"
#C1866 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_1518) <- "Upland crop n3"
#C1867 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_1519) <- "Upland crop n4"
#C1868 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_15110) <- "Upland crop n5"
#C1869 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_15111) <- "Upland crop n6"
#C1870 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_15112) <- "Homegarden crop n1"
#C1871 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_15113) <- "Homegarden crop n2"
#C1872 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_15114) <- "Homegarden crop n3"
#C1873 = FACT, (table of correspondence)
#C1874 = OK, (empty)
#C1875 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#replace label: "d27_991. for which crop(s) do you use d27_oth?"
var_label(HouseholdCambodia$d27_991) <- "d27_991. for which crop(s) do you use d27_oth?"
#C1876 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d27_9911) <- "Lowland crop n1"
#C1877 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d27_9912) <- "Lowland crop n2"
#C1878 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d27_9913) <- "Lowland crop n3"
#C1879 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d27_9914) <- "Lowland crop n4"
#C1880 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d27_9915) <- "Lowland crop n5"
#C1881 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d27_9916) <- "Upland crop n1"
#C1882 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d27_9917) <- "Upland crop n2"
#C1883 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d27_9918) <- "Upland crop n3"
#C1884 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d27_9919) <- "Upland crop n4"
#C1885 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d27_99110) <- "Upland crop n5"
#C1886 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d27_99111) <- "Upland crop n6"
#C1887 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d27_99112) <- "Homegarden crop n1"
#C1888 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d27_99113) <- "Homegarden crop n2"
#C1889 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d27_99114) <- "Homegarden crop n3"
#C1890 = FACT, (table of correspondence)
#replace label: "d27_992. if yes, what was your main motivation to implement d27_oth practice"
var_label(HouseholdCambodia$d27_992) <- "d27_992. if yes, what was your main motivation to implement d27_oth practice"
#C1891 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1892 = FACT, (table of correspondence), (see answers below)
#I do not know them
#They are too costly
#I have no time to implement them
#I don't want to do things differently from my neighbors
#C1893 = FACT, (table of correspondence), (see answers below)
#Synthetic insecticide and fungicide are used regularly and no other system is used
#Mixed use of synthetic and biological/natural pesticides
#Mixed management with various supporting practices listed above; synthetic insecticide and fungicide are still used
#Mixed management with various supporting practices listed above; no longer use of synthetic insecticide and fungicide
#Do not know
#C1894 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1895 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1896 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d30_21) <- "Lowland crop n1"
#C1897 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d30_22) <- "Lowland crop n2"
#C1898 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d30_23) <- "Lowland crop n3"
#C1899 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d30_24) <- "Lowland crop n4"
#C1900 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d30_25) <- "Lowland crop n5"
#C1901 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d30_26) <- "Upland crop n1"
#C1902 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d30_27) <- "Upland crop n2"
#C1903 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d30_28) <- "Upland crop n3"
#C1904 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d30_29) <- "Upland crop n4"
#C1905 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d30_210) <- "Upland crop n5"
#C1906 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d30_211) <- "Upland crop n6"
#C1907 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d30_212) <- "Homegarden crop n1"
#C1908 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d30_213) <- "Homegarden crop n2"
#C1909 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d30_214) <- "Homegarden crop n3"
#C1910 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1911 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1912 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d30_41) <- "Lowland crop n1"
#C1913 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d30_42) <- "Lowland crop n2"
#C1914 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d30_43) <- "Lowland crop n3"
#C1915 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d30_44) <- "Lowland crop n4"
#C1916 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d30_45) <- "Lowland crop n5"
#C1917 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d30_46) <- "Upland crop n1"
#C1918 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d30_47) <- "Upland crop n2"
#C1919 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d30_48) <- "Upland crop n3"
#C1920 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d30_49) <- "Upland crop n4"
#C1921 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d30_410) <- "Upland crop n5"
#C1922 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d30_411) <- "Upland crop n6"
#C1923 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d30_412) <- "Homegarden crop n1"
#C1924 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d30_413) <- "Homegarden crop n2"
#C1925 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d30_414) <- "Homegarden crop n3"
#C1926 = FACT, (table of correspondence), (no = "0", yes = "1", do not know = "88")
#C1927 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1928 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d30_61) <- "Lowland crop n1"
#C1929 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d30_62) <- "Lowland crop n2"
#C1930 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d30_63) <- "Lowland crop n3"
#C1931 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d30_64) <- "Lowland crop n4"
#C1932 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d30_65) <- "Lowland crop n5"
#C1933 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d30_66) <- "Upland crop n1"
#C1934 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d30_67) <- "Upland crop n2"
#C1935 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d30_68) <- "Upland crop n3"
#C1936 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d30_69) <- "Upland crop n4"
#C1937 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d30_610) <- "Upland crop n5"
#C1938 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d30_611) <- "Upland crop n6"
#C1939 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d30_612) <- "Homegarden crop n1"
#C1940 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d30_613) <- "Homegarden crop n2"
#C1941 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d30_614) <- "Homegarden crop n3"
#C1942 = FACT, (table of correspondence), (bin)
#C1943 = CHAR, (correspond to crops numbers), (Useless answer-MultipleCombined, use the following columns)
#C1944 = FACT, replace label: "Lowland crop n1"
var_label(HouseholdCambodia$d32_11) <- "Lowland crop n1"
#C1945 = FACT, replace label: "Lowland crop n2"
var_label(HouseholdCambodia$d32_12) <- "Lowland crop n2"
#C1946 = FACT, replace label: "Lowland crop n3"
var_label(HouseholdCambodia$d32_13) <- "Lowland crop n3"
#C1947 = FACT, replace label: "Lowland crop n4"
var_label(HouseholdCambodia$d32_14) <- "Lowland crop n4"
#C1948 = FACT, replace label: "Lowland crop n5"
var_label(HouseholdCambodia$d32_15) <- "Lowland crop n5"
#C1949 = FACT, replace label: "Upland crop n1"
var_label(HouseholdCambodia$d32_16) <- "Upland crop n1"
#C1950 = FACT, replace label: "Upland crop n2"
var_label(HouseholdCambodia$d32_17) <- "Upland crop n2"
#C1951 = FACT, replace label: "Upland crop n3"
var_label(HouseholdCambodia$d32_18) <- "Upland crop n3"
#C1952 = FACT, replace label: "Upland crop n4"
var_label(HouseholdCambodia$d32_19) <- "Upland crop n4"
#C1953 = FACT, replace label: "Upland crop n5"
var_label(HouseholdCambodia$d32_110) <- "Upland crop n5"
#C1954 = FACT, replace label: "Upland crop n6"
var_label(HouseholdCambodia$d32_111) <- "Upland crop n6"
#C1955 = FACT, replace label: "Homegarden crop n1"
var_label(HouseholdCambodia$d32_112) <- "Homegarden crop n1"
#C1956 = FACT, replace label: "Homegarden crop n2"
var_label(HouseholdCambodia$d32_113) <- "Homegarden crop n2"
#C1957 = FACT, replace label: "Homegarden crop n3"
var_label(HouseholdCambodia$d32_114) <- "Homegarden crop n3"
#C1958 = FACT, (table of correspondence), (see answers below)
#From the village seller
#From the cooperative 
#From a trader in town 
#From family & friends
#Do not know
#Other
#C1959 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C1960 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#Do not have
#Family members 
#Hired people 
#Mutual help
#C1961 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_10) <- "Do not have"
#C1962 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_11) <- "Family members"
#C1963 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_12) <- "Hired people"
#C1964 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_13) <- "Mutual help"
#C1965 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1966 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_20) <- "Do not have"
#Move the column at the right place
HouseholdCambodia <- HouseholdCambodia %>% relocate(d33_20 , .after = d33_2)
#C1967 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_21) <- "Family members"
#C1968 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_22) <- "Hired people"
#C1969 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_23) <- "Mutual help"
#C1970 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1971 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_30) <- "Do not have"
#C1972 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_31) <- "Family members"
#C1973 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_32) <- "Hired people"
#C1974 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_33) <- "Mutual help"
#C1975 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1976 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_40) <- "Do not have"
#C1977 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_41) <- "Family members"
#C1978 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_42) <- "Hired people"
#C1979 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_43) <- "Mutual help"
#C1980 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1981 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_50) <- "Do not have"
#C1982 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_51) <- "Family members"
#C1983 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_52) <- "Hired people"
#C1984 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_53) <- "Mutual help"
#C1985 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1986 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_60) <- "Do not have"
#C1987 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_61) <- "Family members"
#C1988 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_62) <- "Hired people"
#C1989 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_63) <- "Mutual help"
#C1990 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1991 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_70) <- "Do not have"
#C1992 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_71) <- "Family members"
#C1993 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_72) <- "Hired people"
#C1994 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_73) <- "Mutual help"
#C1995 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C1996 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_80) <- "Do not have"
#C1997 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_81) <- "Family members"
#C1998 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_82) <- "Hired people"
#C1999 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_83) <- "Mutual help"
#C2000 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C2001 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_90) <- "Do not have"
#C2002 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_91) <- "Family members"
#C2003 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_92) <- "Hired people"
#C2004 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_93) <- "Mutual help"
#C2005 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C2006 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_100) <- "Do not have"
#C2007 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_101) <- "Family members"
#C2008 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_102) <- "Hired people"
#C2009 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_103) <- "Mutual help"
#C2010 = CHAR, (Useless answer-MultipleCombined, use the following columns)
#C2011 = FACT, replace label: "Do not have"
var_label(HouseholdCambodia$d33_110) <- "Do not have"
#C2012 = FACT, replace label: "Family members"
var_label(HouseholdCambodia$d33_111) <- "Family members"
#C2013 = FACT, replace label: "Hired people"
var_label(HouseholdCambodia$d33_112) <- "Hired people"
#C2014 = FACT, replace label: "Mutual help"
var_label(HouseholdCambodia$d33_113) <- "Mutual help"
#C2015 = OK
#C2016 = OK, (which currency?)
#C2017 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
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
#C2018 = FACT, replace label: "Land preparation"
var_label(HouseholdCambodia$d361) <- "Land preparation"
#C2019 = FACT, replace label: "Sowing"
var_label(HouseholdCambodia$d362) <- "Sowing"
#C2020 = FACT, replace label: "Fertilization"
var_label(HouseholdCambodia$d363) <- "Fertilization"
#C2021 = FACT, replace label: "Weed management"
var_label(HouseholdCambodia$d364) <- "Weed management"
#C2022 = FACT, replace label: "Pest and disease management"
var_label(HouseholdCambodia$d365) <- "Pest and disease management"
#C2023 = FACT, replace label: "Pruning"
var_label(HouseholdCambodia$d366) <- "Pruning"
#C2024 = FACT, replace label: "Water/irrigation management"
var_label(HouseholdCambodia$d367) <- "Water/irrigation management"
#C2025 = FACT, replace label: "Harvest"
var_label(HouseholdCambodia$d368) <- "Harvest"
#C2026 = FACT, replace label: "Transportation"
var_label(HouseholdCambodia$d369) <- "Transportation"
#C2027 = FACT, replace label: "None of above"
var_label(HouseholdCambodia$d360) <- "None of above"
#C2028 = FACT, replace label: "Other"
var_label(HouseholdCambodia$d3699) <- "Other"
#C2029 = OK, (the answer is still in Khmer, Ky will maybe solve it)

# # #e.
#C2030 = FACT, (bin)
#C2031 = CHAR, (Useless answer-MultipleCombined, use the following columns), see choices below
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
#Other poultry
#C2032 = FACT, (bin)
#C3033 = FACT, (bin)
#C2034 = FACT, (bin)
#C2035 = FACT, (bin)
#C2036 = FACT, (bin)
#C2037 = FACT, (bin)
#C2038 = FACT, (bin)
#C2039 = FACT, (bin)
#C2040 = FACT, (bin)
#C2041 = FACT, (bin)
#C2042 = FACT, (bin)
#C2043 = FACT, (bin)
#C2044 = FACT, (bin)
#C2045 = OK, (empty)
#C2046 = OK, (empty)
#C2047 = OK
#C2048 = OK
#C2049 = OK
#C2050 = OK
#C2051 = OK
#C2052 = OK
#C2053 = OK
#C2054 = OK
#C2055 = OK, (empty)
#C2056 = OK, (empty)
#C2057 = OK
#C2058 = OK
#C2059 = OK
#C2060 = OK
#C2061 = OK
#C2062 = OK, replace label: "e3_98. e2_oth1"
var_label(HouseholdCambodia$e3_98) <- "e3_98. e2_oth1"
#C2063 = OK, replace label: "e3_99. e2_oth2"
var_label(HouseholdCambodia$e3_99) <- "e3_99. e2_oth2"
#C2064 = FACT, (table of correspondence), (see answer below)
#Buffalo
#Cattle
#Pig
#Goat
#Sheep
#Horse
#Rabbit
#Chicken
#Duck or muscovy
#Goose
#Other cattle
#Other poutry
#C2065 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2066 = FACT, (table of correspondence)
#C2067 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2068 = FACT, (table of correspondence)
#C2069 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2070 = OK, replace label: "e5_a. how many breeds of e4_1text does your household have"
var_label(HouseholdCambodia$e5_a) <- "e5_a. how many breeds of e4_1text does your household have"
#C2071 = FACT, (bin),  replace label: "e5_b. do you have any local breeds of e4_1text"
var_label(HouseholdCambodia$e5_b) <- "e5_b. do you have any local breeds of e4_1text"
#C2072 = FACT, (bin)
#C2073 = FACT, (bin),  replace label: "e5_d. do you cross local breeds with other breeds of e4_1text"
var_label(HouseholdCambodia$e5_d) <- "e5_d. do you cross local breeds with other breeds of e4_1text"
#C2074 = REMOVE
#C2075 = OK, replace label: "e5_1. how many e4_1text died in the past 1 year (3 years)"
var_label(HouseholdCambodia$e5_1) <- "e5_1. how many e4_1text died in the past 1 year (3 years)"
#C2076 = OK, replace label: "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e5_2) <- "e5_2. how many e4_1text did you slaughter and self-consume in the past 1 year [3 years]?"
#C2077 = OK, replace label: "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e5_3) <- "e5_3. how many e4_1text did you give to other in the past 1 year [3 years]?"
#C2078 = OK, replace label: "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
var_label(HouseholdCambodia$e5_4) <- "e5_4. how many e4_1text were sold in the 1 year [3 years]?"
#C2079 = OK, replace label: "e5_41. average selling price/kg or head for each e4_1text sold (local currency/kg)"
var_label(HouseholdCambodia$e5_41) <- "e5_41. average selling price/kg or head for each e4_1text sold (local currency/kg)"
#C2080 = OK, replace label: "e5_42. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e5_42) <- "e5_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
#C2081 = OK, replace label: "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e5_5) <- "e5_5. how many e4_1text were bought in the past 1 year [3 years]?"
#C2082 = OK, replace label: "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
var_label(HouseholdCambodia$e5_51) <- "e5_51. average buying price/kg for each e4_1text (local currency/kg)"
#C2083 = OK, replace label: "e5_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e5_52) <- "e5_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
#C2084 = FACT, (table of correspondence), (see choices below) replace label: "b18_1. what is the main outlet/ buyer for e4_1text?"
var_label(HouseholdCambodia$b18_1) <- "b18_1. what is the main outlet/ buyer for e4_1text?"
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
#C2085 = OK, (the answer is still in Khmer, Ky will maybe solve it), (real name)
#C2086 = OK, (the answer is still in Khmer, Ky will maybe solve it), (category name)
#C2087 = FACT, (table of correspondence), (see choices below), replace label: "b19_1. what is the proportion of e4_1text that you sell to b18_1"
var_label(HouseholdCambodia$b19_1) <- "b19_1. what is the proportion of e4_1text that you sell to b18_1"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2088 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see choices below)
#replace label: "b20_1. does  b18_1text provide any of the following?"
var_label(HouseholdCambodia$b20_1) <- "b20_1. does  b18_1text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C2089 = FACT, replace label: "Nothing"
var_label(HouseholdCambodia$b20_10) <- "Nothing"
#C2090 = FACT, replace label: "Inputs (sold)"
var_label(HouseholdCambodia$b20_11) <- "Inputs (sold)"
#C2091 = FACT, replace label: "Inputs on credit"
var_label(HouseholdCambodia$b20_12) <- "Inputs on credit"
#C2092 = FACT, replace label: "Cash credit"
var_label(HouseholdCambodia$b20_13) <- "Cash credit"
#C2093 = FACT, replace label: "Technical advice/training"
var_label(HouseholdCambodia$b20_14) <- "Technical advice/training"
#C2094 = FACT, replace label: "Market information"
var_label(HouseholdCambodia$b20_15) <- "Market information"
#C2095 = FACT, replace label: "Regular sales"
var_label(HouseholdCambodia$b20_16) <- "Regular sales"
#C2096 = FACT, replace label: "Other"
var_label(HouseholdCambodia$b20_199) <- "Other"
#C2097 = FACT, replace label: "Do not know"
var_label(HouseholdCambodia$b20_188) <- "Do not know"
#C2098 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "b20_1oth. specify other provision from b18_1text"
var_label(HouseholdCambodia$b20_1oth) <- "b20_1oth. specify other provision from b18_1text"
#C2099 = FACT, (see choices below), replace label: "b21_1. do you have a contract with b18_1text?"
var_label(HouseholdCambodia$b21_1) <- "b21_1. do you have a contract with b18_1text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C2100 = OK, replace label: "e6_a. how many breeds of e4_2text does your household have"
var_label(HouseholdCambodia$e6_a) <- "e6_a. how many breeds of e4_2text does your household have"
#C2101 = FACT, (bin),  replace label: "e6_b. do you have any local breeds of e4_2text"
var_label(HouseholdCambodia$e6_b) <- "e6_b. do you have any local breeds of e4_2text"
#C2102 = FACT
#C2103 = FACT, (bin),  replace label: "e6_d. do you cross local breeds with other breeds of e4_2text"
var_label(HouseholdCambodia$e6_d) <- "e6_d. do you cross local breeds with other breeds of e4_2text"
#C2104 = REMOVE
#C2105 = OK, replace label: "e6_1. how many e4_2text died in the past 1 year (3 years)"
var_label(HouseholdCambodia$e6_1) <- "e6_1. how many e4_2text died in the past 1 year (3 years)"
#C2106 = OK, replace label: "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e6_2) <- "e6_2. how many e4_2text did you slaughter and self-consume in the past 1 year [3 years]?"
#C2107 = OK, replace label: "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e6_3) <- "e6_3. how many e4_2text did you give to other in the past 1 year [3 years]?"
#C2108 = OK, replace label: "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
var_label(HouseholdCambodia$e6_4) <- "e6_4. how many e4_2text were sold in the 1 year [3 years]?"
#C2109 = OK, replace label: "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
var_label(HouseholdCambodia$e6_41) <- "e6_41. average selling price/kg for each e4_2text sold (local currency/kg)"
#C2110 = OK, replace label: "e6_42. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e6_42) <- "e6_42. average selling price/kg for each e4_2text sold (local currency/kg)"
#C2111 = OK, replace label: "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e6_5) <- "e6_5. how many e4_2text were bought in the past 1 year [3 years]?"
#C2112 = OK, replace label: "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
var_label(HouseholdCambodia$e6_51) <- "e6_51. average buying price/kg for each e4_2text (local currency/kg)"
#C2113 = OK, replace label: "e6_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e6_52) <- "e6_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
#C2114 = FACT, (table of correspondence), (see choices below) replace label: "b18_2. what is the main outlet/ buyer for e4_2text?"
var_label(HouseholdCambodia$b18_2) <- "b18_2. what is the main outlet/ buyer for e4_2text?"
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
#C2115 = OK, (the answer is still in Khmer, Ky will maybe solve it), (real name)
#C2116 = OK, (the answer is still in Khmer, Ky will maybe solve it), (category name)
#C2117 = FACT, (table of correspondence), (see choices below), replace label: "b19_2. what is the proportion of e4_2text that you sell to b18_2"
var_label(HouseholdCambodia$b19_2) <- "b19_2. what is the proportion of e4_2text that you sell to b18_2"
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2118 = FACT, (see choices below)
#replace label: "b20_2. does  b18_2text provide any of the following?"
var_label(HouseholdCambodia$b20_2) <- "b20_2. does  b18_2text provide any of the following?"
#Nothing
#Inputs (sold)
#Inputs on credit
#Cash credit
#Technical advice/training
#Market information
#Regular sales
#Other
#Do not know
#C2119 = OK, (the answer is still in Khmer, Ky will maybe solve it), replace label: "b20_2oth. specify other provision from b18_2text"
var_label(HouseholdCambodia$b20_2oth) <- "b20_2oth. specify other provision from b18_2text"
#C2120 = FACT, (see choices below), replace label: "b21_2. do you have a contract with b18_2text?"
var_label(HouseholdCambodia$b21_2) <- "b21_2. do you have a contract with b18_2text?"
#Formal contract
#Informal contract
#No contract/ no prior arrangements
#Do not know
#C2121 = OK, replace label: "e7_a. how many breeds of e4_3text does your household have"
var_label(HouseholdCambodia$e7_a) <- "e7_a. how many breeds of e4_3text does your household have"
#C2122 = FACT, (bin),  replace label: "e7_b. do you have any local breeds of e4_3text"
var_label(HouseholdCambodia$e7_b) <- "e7_b. do you have any local breeds of e4_3text"
#C2123 = OK
#C2124 = FACT, (bin),  replace label: "e7_d. do you cross local breeds with other breeds of e4_3text"
var_label(HouseholdCambodia$e7_d) <- "e7_d. do you cross local breeds with other breeds of e4_3text"
#C2125 = REMOVE
#C2126 = OK, replace label: "e7_1. how many e4_3text died in the past 1 year (3 years)"
var_label(HouseholdCambodia$e7_1) <- "e7_1. how many e4_3text died in the past 1 year (3 years)"
#C2127 = OK, replace label: "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e7_2) <- "e7_2. how many e4_3text did you slaughter and self-consume in the past 1 year [3 years]?"
#C2128 = OK, replace label: "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e7_3) <- "e7_3. how many e4_3text did you give to other in the past 1 year [3 years]?"
#C2129 = OK, replace label: "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
var_label(HouseholdCambodia$e7_4) <- "e7_4. how many e4_3text were sold in the 1 year [3 years]?"
#C2130 = OK, replace label: "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
var_label(HouseholdCambodia$e7_41) <- "e7_41. average selling price/kg for each e4_3text sold (local currency/kg)"
#C2131 = OK, replace label: "e7_42. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e7_42) <- "e7_42. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
#C2132 = OK, replace label: "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
var_label(HouseholdCambodia$e7_5) <- "e7_5. how many e4_3text were bought in the past 1 year [3 years]?"
#C2133 = OK, replace label: "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
var_label(HouseholdCambodia$e7_51) <- "e7_51. average buying price/kg for each e4_3text (local currency/kg)"
#C2134 = OK, replace label: "e7_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
var_label(HouseholdCambodia$e7_52) <- "e7_52. How many kg/head of e4_1text are sold each year/3years (local currency/kg or head)"
#C2135 = FACT, (bin)
#C2136 = OK, (m2 ?)
#C2137 = OK
#C2138 = FACT, (bin)
#C2139 = OK, (m2 ?)
#C2140 = FACT, (bin)
#C2141 = OK, (m2 ?)
#C2142 = FACT, (bin)
#C2143 = OK, (m2 ?)
#C2144 = FACT, (bin)
#C2145 = FACT, (table of correspondence), (see option below)
#Synthetic fertilizer
#Organic manure 
#Both above
#C2146 = FACT, (bin)
#C2147 = FACT, (bin)
#C2148 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2149 = FACT, (table of correspondence)
#C2150 = FACT, (bin)
#C2151 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
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
#C2152 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e191) <- "Grazing in public area"
#C2153 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e192) <- "Grazing in own pasture area"
#C2154 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e193) <- "Cutting and carry natural grass / vegetables from public area"
#C2155 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e194) <- "Cutting and carry natural grass / vegetables from own area"
#C2156 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e195) <- "Cutting and carry of forage"
#C2157 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e196) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2158 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e197) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2159 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e198) <- "Silage"
#C2160 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e199) <- "Kitchen waste"
#C2161 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e1910) <- "Rice, rice brand, broken rice… bought from market"
#C2162 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e1911) <- "Rice, rice brand, broken rice… bought from own farm"
#C2163 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e1912) <- "Restaurant / Market waste"
#C2164 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e1912) <- "Waste from processing rice wine, soybean milke"
#C2165 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e1999) <- "Other"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e1999, .after = e1912)
#C2166 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e1988) <- "Do not know"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e1988, .after = e1999)
#C2167 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2168 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2169 = FACT, (table of correspondence)
#C2170 = FACT, (bin)
#C2171 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Rice, rice brand, broken rice… bought from market
#Rice, rice brand, broken rice… bought from own farm
#Restaurant / Market waste
#Waste from processing rice wine, soybean milk
#Do not know	
#Other
#C2172 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e241) <- "Grazing in public area"
#C2173 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e242) <- "Grazing in own pasture area"
#C2174 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e243) <- "Cutting and carry natural grass / vegetables from public area"
#C2175 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e244) <- "Cutting and carry natural grass / vegetables from own area"
#C2176 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e245) <- "Cutting and carry of forage"
#C2177 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e246) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2178 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e247) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2179 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e248) <- "Silage"
#C2180 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e249) <- "Kitchen waste"
#C2181 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e2410) <- "Rice, rice brand, broken rice… bought from market"
#C2182 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e2411) <- "Rice, rice brand, broken rice… bought from own farm"
#C2183 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e2412) <- "Restaurant / Market waste"
#C2184 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e2413) <- "Waste from processing rice wine, soybean milk"
#C2185 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e2499) <- "Other"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e2499, .after = e2413)
#C2186 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e2488) <- "Do not know"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e2488, .after = e2499)
#C2187 = OK, (empty)
#C2188 = FACT, (bin)
#C2189 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2190 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdCambodia$e25_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2191 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdCambodia$e25_12) <- "From market: feed / concentrate"
#C2192 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e25_188) <- "Do not know"
#C2193 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e25_199) <- "Other"
#C2194 = FACT, (bin)
#C2195 = FACT, (Table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2196 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2197 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdCambodia$e270) <- "Nothing"
#C2198 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdCambodia$e271) <- "Traditional treatment"
#C2199 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdCambodia$e272) <- "Chemicals (define)"
#C2200 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2201 = OK, (empty)
#C2202 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2203 = OK, (empty)
#C2204 = FACT, (bin)
#C2205 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2206 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdCambodia$e290) <- "I don’t use antibiotics at all"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e290, .after = e29)
#C2207 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdCambodia$e291) <- "For treatment diseases only"
#C2208 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdCambodia$e292) <- "For prevention of diseases only"
#C2209 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdCambodia$e293) <- "For growth promotion"
#c2210 = FACT, (table of correspondence), (see answers below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2211 = OK, (empty)
#c2212 = FACT, (table of correspondence), (see answers below)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2213 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2214 = FACT, (table of correspondence), (see answers below)
#Fattening family raising using only local feed stuffs and kitchen waste
#Fattening family raising, but buying feed from market
#Piglet family raising using only local feed stuffs and kitchen waste
#Piglet family raising, but buying feed from market
#Other
#C2215 = OK, (empty)
#C2216 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2217 = FACT, (table of correspondence)
#C2218 = FACT, (bin)
#C2219 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Rice, rice brand, broken rice… bought from market
#Rice, rice brand, broken rice… bought from own farm
#Restaurant / Market waste
#Waste from processing rice wine, soybean milk
#Do not know	
#Other
#C2220 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e341) <- "Grazing in public area"
#C2221 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e342) <- "Grazing in own pasture area"
#C2222 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e343) <- "Cutting and carry natural grass / vegetables from public area"
#C2223 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e344) <- "Cutting and carry natural grass / vegetables from own area"
#C2224 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e345) <- "Cutting and carry of forage"
#C2225 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e346) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2226 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e347) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2227 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e348) <- "Silage"
#C2228 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e349) <- "Kitchen waste"
#C2229 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e3410) <- "Rice, rice brand, broken rice… bought from market"
#C2230 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e3411) <- "Rice, rice brand, broken rice… bought from own farm"
#C2231 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e3412) <- "Restaurant / Market waste"
#C2232 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e3413) <- "Waste from processing rice wine, soybean milk"
#C2233 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e3499) <- "Other"
HouseholdCambodia <- HouseholdCambodia %>% relocate(e3499, .after = e3413)
#C2234 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e3488) <- "Do not know"
#Move this column at this place:
HouseholdCambodia <- HouseholdCambodia %>% relocate(e3488, .after = e3499)
#C2235 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2236 = FACT, (table of correspondence)
#C2237 = FACT, (bin)
#C2238 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Rice, rice brand, broken rice… bought from market
#Rice, rice brand, broken rice… bought from own farm
#Restaurant / Market waste
#Waste from processing rice wine, soybean milk
#Do not know	
#Other
#C2239 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e381) <- "Grazing in public area"
#C2240 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e382) <- "Grazing in own pasture area"
#C2241 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e383) <- "Cutting and carry natural grass / vegetables from public area"
#C2242 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e384) <- "Cutting and carry natural grass / vegetables from own area"
#C2243 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e385) <- "Cutting and carry of forage"
#C2244 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e386) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2245 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e387) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2246 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e388) <- "Silage"
#C2247 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e389) <- "Kitchen waste"
#C2248 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e3810) <- "Rice, rice brand, broken rice… bought from market"
#C2249 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e3811) <- "Rice, rice brand, broken rice… bought from own farm"
#C2250 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e3812) <- "Restaurant / Market waste"
#C2251 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e3813) <- "Waste from processing rice wine, soybean milk"
#C2252 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e3888) <- "Do not know"
#C2253 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e3899) <- "Other"
#C2254 = OK, (empty)
#C2255 = FACT, (bin)
#C2256 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2257 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdCambodia$e39_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2258 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdCambodia$e39_12) <- "From market: feed / concentrate"
#C2259 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e39_188) <- "Do not know"
#C2260 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e39_199) <- "Other"
#C2261 = FACT, (bin)
#C2262 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2263 = FACT, (table of correspondence), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2264 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdCambodia$e410) <- "Nothing"
#C2265 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdCambodia$e411) <- "Traditional treatment"
#C2266 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdCambodia$e412) <- "Chemicals (define)"
#C2267 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2268 = OK, (empty)
#C2269 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2270 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2271 = FACT, (bin)
#C2272 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2273 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdCambodia$e431) <- "For treatment diseases only"
#C2274 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdCambodia$e432) <- "For prevention of diseases only"
#C2275 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdCambodia$e433) <- "For growth promotion"
#C2276 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdCambodia$e430) <- "I don’t use antibiotics at all"
#C2277 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2278 = OK, (empty)
#C2279 = FACT, (table of correspondence), (see table below)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2280 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2281 = FACT, (table of correspondence), (see answers below)
#Family raising for consumption
#Family raising but larger scale for selling meat
#Family raising but larger scale for selling chick
#Family raising but larger scale for selling eggs
#Industrial (semi-industrial) broiler chicken 
#Industrial (semi-industrial) layer chicken
#Other
#C2282 = OK, (empty)
#C2283 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2284 = FACT, (table of correspondence)
#C2285 = FACT, (bin)
#C2286 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Rice, rice brand, broken rice… bought from market
#Rice, rice brand, broken rice… bought from own farm
#Restaurant / Market waste
#Waste from processing rice wine, soybean milk
#Do not know	
#Other
#C2287 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e481) <- "Grazing in public area"
#C2288 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e482) <- "Grazing in own pasture area"
#C2289 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e483) <- "Cutting and carry natural grass / vegetables from public area"
#C2290 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e484) <- "Cutting and carry natural grass / vegetables from own area"
#C2291 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e485) <- "Cutting and carry of forage"
#C2292 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e486) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2293 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e487) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2294 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e488) <- "Silage"
#C2295 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e489) <- "Kitchen waste"
#C2296 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e4810) <- "Rice, rice brand, broken rice… bought from market"
#C2297 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e4811) <- "Rice, rice brand, broken rice… bought from own farm"
#C2298 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e4812) <- "Restaurant / Market waste"
#C2299 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e4813) <- "Waste from processing rice wine, soybean milk"
#C2300 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e4888) <- "Do not know"
#C2301 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e4899) <- "Other"
#C2302 = FACT, (table of correspondence), (see choices below)
#Confined in a barn
#Confined in a shelter in the grazing area
#Attached in the grazing area
#Grazing with shepherd
#Free grazing without Shepherd
#Do not know
#Other
#C2303 = FACT, (table of correspondence)
#C2304 = FACT, (bin)
#C2305 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Grazing in public area
#Grazing in own pasture area
#Cutting and carry natural grass / vegetables from public area
#Cutting and carry natural grass / vegetables from own area
#Cutting and carry of forage
#Own crop residues (rice straw, maize stem...) grazing after harvest
#Own crop residues (rice straw, maize stem…) collected and stored
#Silage
#Kitchen waste
#Rice, rice brand, broken rice… bought from market
#Rice, rice brand, broken rice… bought from own farm
#Restaurant / Market waste
#Waste from processing rice wine, soybean milk
#Do not know	
#Other
#C2306 = FACT, (bin), replace label: "Grazing in public area"
var_label(HouseholdCambodia$e521) <- "Grazing in public area"
#C2307 = FACT, (bin), replace label: "Grazing in own pasture area"
var_label(HouseholdCambodia$e522) <- "Grazing in own pasture area"
#C2308 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from public area"
var_label(HouseholdCambodia$e523) <- "Cutting and carry natural grass / vegetables from public area"
#C2309 = FACT, (bin), replace label: "Cutting and carry natural grass / vegetables from own area"
var_label(HouseholdCambodia$e524) <- "Cutting and carry natural grass / vegetables from own area"
#C2310 = FACT, (bin), replace label: "Cutting and carry of forage"
var_label(HouseholdCambodia$e525) <- "Cutting and carry of forage"
#C2311 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem...) grazing after harvest"
var_label(HouseholdCambodia$e526) <- "Own crop residues (rice straw, maize stem...) grazing after harvest"
#C2312 = FACT, (bin), replace label: "Own crop residues (rice straw, maize stem…) collected and stored"
var_label(HouseholdCambodia$e527) <- "Own crop residues (rice straw, maize stem…) collected and stored"
#C2313 = FACT, (bin), replace label: "Silage"
var_label(HouseholdCambodia$e528) <- "Silage"
#C2314 = FACT, (bin), replace label: "Kitchen waste"
var_label(HouseholdCambodia$e529) <- "Kitchen waste"
#C2315 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from market"
var_label(HouseholdCambodia$e5210) <- "Rice, rice brand, broken rice… bought from market"
#C2316 = FACT, (bin), replace label: "Rice, rice brand, broken rice… bought from own farm"
var_label(HouseholdCambodia$e5211) <- "Rice, rice brand, broken rice… bought from own farm"
#C2317 = FACT, (bin), replace label: "Restaurant / Market waste"
var_label(HouseholdCambodia$e5212) <- "Restaurant / Market waste"
#C2318 = FACT, (bin), replace label: "Waste from processing rice wine, soybean milk"
var_label(HouseholdCambodia$e5213) <- "Waste from processing rice wine, soybean milk"
#C2319 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e5288) <- "Do not know"
#C2320 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e5299) <- "Other"
#C2321 = OK, (empty)
#C2322 = FACT, (bin)
#C2323 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#From farm: feed (maize meal, paddy rice bran)
#From market: feed / concentrate
#Do not know
#Other
#C2324 = FACT, (bin), replace label: "From farm: feed (maize meal, paddy rice bran)"
var_label(HouseholdCambodia$e53_11) <- "From farm: feed (maize meal, paddy rice bran)"
#C2325 = FACT, (bin), replace label: "From market: feed / concentrate"
var_label(HouseholdCambodia$e53_12) <- "From market: feed / concentrate"
#C2326 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$e53_188) <- "Do not know"
#C2327 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$e53_199) <- "Other"
#C2328 = FACT, (bin)
#C2329 = FACT, (table of correspondence), (see answer below)
#Internal parasites
#Tick
#Worms
#Do not know
#Others
#C2330 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answer below)
#Nothing
#Traditional treatment
#Chemicals (define)
#C2331 = FACT, (bin), replace label: "Nothing"
var_label(HouseholdCambodia$e550) <- "Nothing"
#C2332 = FACT, (bin), replace label: "Traditional treatment"
var_label(HouseholdCambodia$e551) <- "Traditional treatment"
#C2333 = FACT, (bin), replace label: "Chemicals (define)"
var_label(HouseholdCambodia$e552) <- "Chemicals (define)"
#C2334 = FACT, (bin)
#C2335 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2336 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2337 = FACT, (table of correspondence), (see table below)
#Not available on the market
#Too expensive
#Do not know
#Others
#C2338 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2339 = FACT, (bin)
#C2340 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#For treatment diseases only 
#For prevention of diseases only
#For growth promotion 
#I don’t use antibiotics at all
#C2341 = FACT, (bin), replace label: "For treatment diseases only"
var_label(HouseholdCambodia$e571) <- "For treatment diseases only"
#C2342 = FACT, (bin), replace label: "For prevention of diseases only"
var_label(HouseholdCambodia$e572) <- "For prevention of diseases only"
#C2343 = FACT, (bin), replace label: "For growth promotion"
var_label(HouseholdCambodia$e573) <- "For growth promotion"
#C2344 = FACT, (bin), replace label: "I don’t use antibiotics at all"
var_label(HouseholdCambodia$e570) <- "I don’t use antibiotics at all"
#C2345 = FACT, (table of correspondence), (see table below)
#Better for environment/ AE
#Human health
#Do not know
#Others
#C2346 = OK, (empty)
#C2347 = FACT, (empty)
#Not available on the market, 
#Too expensive
#Already include in the feed
#Others
#C2348 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2349 = OK, (bin)
#C2350 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2351 = FACT, (bin), replace label: "Jan"
var_label(HouseholdCambodia$e58_11) <- "Jan"
#C2352 = FACT, (bin), replace label: "Feb"
var_label(HouseholdCambodia$e58_12) <- "Feb"
#C2353 = FACT, (bin), replace label: "Mar"
var_label(HouseholdCambodia$e58_13) <- "Mar"
#C2354 = FACT, (bin), replace label: "Apr"
var_label(HouseholdCambodia$e58_14) <- "Apr"
#C2355 = FACT, (bin), replace label: "May"
var_label(HouseholdCambodia$e58_15) <- "May"
#C2356 = FACT, (bin), replace label: "Jun"
var_label(HouseholdCambodia$e58_16) <- "Jun"
#C2357 = FACT, (bin), replace label: "Jul"
var_label(HouseholdCambodia$e58_17) <- "Jul"
#C2358 = FACT, (bin), replace label: "Aug"
var_label(HouseholdCambodia$e58_18) <- "Aug"
#C2359 = FACT, (bin), replace label: "Sep"
var_label(HouseholdCambodia$e58_19) <- "Sep"
#C2360 = FACT, (bin), replace label: "Oct"
var_label(HouseholdCambodia$e58_110) <- "Oct"
#C2361 = FACT, (bin), replace label: "Nov"
var_label(HouseholdCambodia$e58_111) <- "Nov"
#C2362 = FACT, (bin), replace label: "Dec"
var_label(HouseholdCambodia$e58_112) <- "Dec"
#C2363 = OK, (bin)
#C2364 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#C2365 = FACT, (bin), replace label: "Jan"
var_label(HouseholdCambodia$e59_11) <- "Jan"
#C2366 = FACT, (bin), replace label: "Feb"
var_label(HouseholdCambodia$e59_12) <- "Feb"
#C2367 = FACT, (bin), replace label: "Mar"
var_label(HouseholdCambodia$e59_13) <- "Mar"
#C2368 = FACT, (bin), replace label: "Apr"
var_label(HouseholdCambodia$e59_14) <- "Apr"
#C2369 = FACT, (bin), replace label: "May"
var_label(HouseholdCambodia$e59_15) <- "May"
#C2370 = FACT, (bin), replace label: "Jun"
var_label(HouseholdCambodia$e59_16) <- "Jun"
#C2371 = FACT, (bin), replace label: "Jul"
var_label(HouseholdCambodia$e59_17) <- "Jul"
#C2372 = FACT, (bin), replace label: "Aug"
var_label(HouseholdCambodia$e59_18) <- "Aug"
#C2373 = FACT, (bin), replace label: "Sep"
var_label(HouseholdCambodia$e59_19) <- "Sep"
#C2374 = FACT, (bin), replace label: "Oct"
var_label(HouseholdCambodia$e59_110) <- "Oct"
#C2375 = FACT, (bin), replace label: "Nov"
var_label(HouseholdCambodia$e59_111) <- "Nov"
#C2376 = FACT, (bin), replace label: "Dec"
var_label(HouseholdCambodia$e59_112) <- "Dec"
#C2377 = FACT, (bin)

# # #f.
#C2378 = FACT, (bin)
#C2379 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2380 = FACT, (table of correspondence), (see answers below)
#No time 
#Very little time
#Moderate amount of time
#Almost enough time
#Sufficient amount of time
#Do not know
#C2381 = FACT, (table of correspondence)
#C2382 = REMOVE, (text from the questionnaire)

# # #g.
#C2383 = FACT, (table of correspondence), (see answers below)
#Myself alone
#Me in consultation with spouse/other family members
#My spouse/other family members
#Do not know
#C2384 = FACT, (table of correspondence)
#C2385 = FACT, (table of correspondence)
#C2386 = FACT, (table of correspondence)
#C2387 = FACT, (bin)
#C2388 = FACT, (bin)


# # #h. 
#C2389 = REMOVE, text from the questionnaire
#C2390 = FACT, (table of correspondence), (see answers below)
#Yes, strongly
#Yes, maybe
#They should emigrate if they had the chance
#No, agriculture is not a good job
#Do not know
#C2391 = FACT, (bin)
#C2392 = FACT, (bin)
#C2393 = FACT, (bin)
#C2394 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see table below)
#Other farmers/farmer group/cooperative
#Technical advisors
#Researchers
#Buyers
#Other
#Do not know
#C2395 = FACT, (bin), replace label: "Other farmers/farmer group/cooperative"
var_label(HouseholdCambodia$h4_11) <- "Other farmers/farmer group/cooperative"
#C2396 = FACT, (bin), replace label: "Technical advisors"
var_label(HouseholdCambodia$h4_12) <- "Technical advisors"
#C2397 = FACT, (bin), replace label: "Researchers"
var_label(HouseholdCambodia$h4_13) <- "Researchers"
#C2398 = FACT, (bin), replace label: "Buyers"
var_label(HouseholdCambodia$h4_14) <- "Buyers"
#C2399 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$h4_199) <- "Other"
#C2400 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$h4_188) <- "Do not know"
#C2401 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2402 = FACT, (bin)
#C2403 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2404 = OK, (the answer is still in Khmer, Ky will maybe solve it)

# # #i.
#C2405 = REMOVE, (text from questionnaire)
#C2406 = FACT, (table of correspondence), (see answer below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2407 = FACT, (bin)
#C2408 = CHAR, (Useless answer-MultipleCombined, use the following columns),
#C2409 = FACT, (bin), replace label: "Jan"
var_label(HouseholdCambodia$i31) <- "Jan"
#C2410 = FACT, (bin), replace label: "Feb"
var_label(HouseholdCambodia$i32) <- "Feb"
#C2411 = FACT, (bin), replace label: "Mar"
var_label(HouseholdCambodia$i33) <- "Mar"
#C2412 = FACT, (bin), replace label: "Apr"
var_label(HouseholdCambodia$i34) <- "Apr"
#C2413 = FACT, (bin), replace label: "May"
var_label(HouseholdCambodia$i35) <- "May"
#C2414 = FACT, (bin), replace label: "Jun"
var_label(HouseholdCambodia$i36) <- "Jun"
#C2415 = FACT, (bin), replace label: "Jul"
var_label(HouseholdCambodia$i37) <- "Jul"
#C2416 = FACT, (bin), replace label: "Aug"
var_label(HouseholdCambodia$i38) <- "Aug"
#C2417 = FACT, (bin), replace label: "Sep"
var_label(HouseholdCambodia$i39) <- "Sep"
#C2418 = FACT, (bin), replace label: "Oct"
var_label(HouseholdCambodia$i310) <- "Oct"
#C2419 = FACT, (bin), replace label: "Nov"
var_label(HouseholdCambodia$i311) <- "Nov"
#C2420 = FACT, (bin), replace label: "Dec"
var_label(HouseholdCambodia$i312) <- "Dec"
#C2421 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Climate (drought, floods) 
#Pest damages
#Animal disease  
#No buyers for your produce
#Declining selling prices for your produce
#Need to reimburse credits 
#Increasing prices of food (rice…)
#Other
#C2422 = FACT, (bin), replace label: "Climate (drought, floods)"
var_label(HouseholdCambodia$i41) <- "Climate (drought, floods)"
#C2423 = FACT, (bin), replace label: "Pest damages"
var_label(HouseholdCambodia$i42) <- "Pest damages"
#C2424 = FACT, (bin), replace label: "Animal disease"
var_label(HouseholdCambodia$i43) <- "Animal disease"
#C2425 = FACT, (bin), replace label: "No buyers for your produce"
var_label(HouseholdCambodia$i44) <- "No buyers for your produce"
#C2426 = FACT, (bin), replace label: "Declining selling prices for your produce"
var_label(HouseholdCambodia$i45) <- "Declining selling prices for your produce"
#C2427 = FACT, (bin), replace label: "Need to reimburse credits"
var_label(HouseholdCambodia$i46) <- "Need to reimburse credits"
#C2428 = FACT, (bin), replace label: "Increasing prices of food (rice…)"
var_label(HouseholdCambodia$i47) <- "Increasing prices of food (rice…)"
#C2429 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$i499) <- "Other"
#C2430 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2431 = FACT, (table of correspondence), (see answers below)
#Happens every year or most years
#Happens sometimes but not regularly
#It was exceptional

# # #j. 
#C2432 = FACT, (table of correspondence), (see answers below)
#Owned
#Rented
#Share cropping 
#Other
#Do not know
#C2433 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2434 = FACT, (table of correspondence), (see answers below)
#Brick wall
#Concrete wall
#Wooden wall
#Bamboo, Thatch/leaves, Grass
#Galvanized iron or aluminium or other metal sheets 
#Wood or logs
#Other
#C2435 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2436 = FACT, (table of correspondence), (see answers below)
#Thatch/leaves/grass 
#Wood
#Fibrous cement 
#Concrete, cement
#Brick tile roof
#Stone tile roof
#Metal/ tin roof
#Other
#C2437 = OK, (empty)
#C2438 = FACT, (bin)
#C2439 = FACT, (bin)
#C2440 = FACT, (table of correspondence), (see answers below)
#Private tap water
#Public water
#Drill
#Well
#Rain water
#Natural stream
#Water delivery
#Rain water
#Other
#C2441 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2442 = FACT, (table of correspondence), (see answers below)
#Grid electricity 
#Private electricity
#Small hydroelectricity
#Own generator
#Battery
#Solar panel 
#None, using kerosene/ candles
#Other
#C2443 = OK, (other)

# # #k.
#C2444 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
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
#C2445 = FACT, (bin), replace label: "Motorcycle"
var_label(HouseholdCambodia$k11) <- "Motorcycle"
#C2446 = FACT, (bin), replace label: "2-wheel hand tractor"
var_label(HouseholdCambodia$k12) <- "2-wheel hand tractor"
#C2447 = FACT, (bin), replace label: "4-wheel tractor"
var_label(HouseholdCambodia$k13) <- "4-wheel tractor"
#C2448 = FACT, (bin), replace label: "Car"
var_label(HouseholdCambodia$k14) <- "Car"
#C2449 = FACT, (bin), replace label: "Truck"
var_label(HouseholdCambodia$k15) <- "Truck"
#C2450 = FACT, (bin), replace label: "Combine harvester"
var_label(HouseholdCambodia$k16) <- "Combine harvester"
#C2451 = FACT, (bin), replace label: "Land leveler"
var_label(HouseholdCambodia$k17) <- "Land leveler"
#C2452 = FACT, (bin), replace label: "Rice planter"
var_label(HouseholdCambodia$k18) <- "Rice planter"
#C2453 = FACT, (bin), replace label: "Maize planter"
var_label(HouseholdCambodia$k19) <- "Maize planter"
#C2454 = FACT, (bin), replace label: "Cassava disc ridging tool"
var_label(HouseholdCambodia$k110) <- "Cassava disc ridging tool"
#C2455 = FACT, (bin), replace label: "Cassava harvesting tool"
var_label(HouseholdCambodia$k111) <- "Cassava harvesting tool"
#C2456 = FACT, (bin), replace label: "Rice thresher"
var_label(HouseholdCambodia$k112) <- "Rice thresher"
#C2457 = FACT, (bin), replace label: "Rice mill"
var_label(HouseholdCambodia$k113) <- "Rice mill"
#C2458 = FACT, (bin), replace label: "Backpack sprayer"
var_label(HouseholdCambodia$k114) <- "Backpack sprayer"
#C2459 = FACT, (bin), replace label: "Motor pump sprayer"
var_label(HouseholdCambodia$k115) <- "Motor pump sprayer"
#C2460 = FACT, (bin), replace label: "Grass cutter"
var_label(HouseholdCambodia$k116) <- "Grass cutter"
#C2461 = FACT, (bin), replace label: "Grass chopping machine"
var_label(HouseholdCambodia$k117) <- "Grass chopping machine"
#C2462 = FACT, (bin), replace label: "Water pump"
var_label(HouseholdCambodia$k118) <- "Water pump"
#C2463 = FACT, (bin), replace label: "Irrigation system"
var_label(HouseholdCambodia$k119) <- "Irrigation system"
#C2464 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$k199) <- "Other"
#C2465 = OK
#C2466 = OK
#C2467 = OK
#C2468 = OK
#C2469 = OK
#C2470 = OK
#C2471 = OK
#C2472 = OK
#C2473 = OK
#C2474 = OK
#C2475 = OK
#C2476 = OK
#C2477 = OK
#C2478 = OK
#C2479 = OK
#C2480 = OK
#C2481 = OK
#C2482 = OK
#C2483 = OK

# # #l.
#C2484 = REMOVE, (text from the questionaire)
#C2485 = FACT, (bin)
#C2486 = FACT, (table of correspondence), (see answers below)
#Less than 25%
#25-50%
#50-75%
#Over 75%
#Do not know
#C2487 = FACT, (table of correspondence), (see answers below)
#No debt   
#Debt is higher than income
#Debt is more than half of the income. Capacity to reimburse is limited
#Debt is approximately half of the income
#Debt is low and I am capable to reimburse
#C2488 = FACT, (bin)
#C2489 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
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
#C2490 = FACT, (bin), replace label: "To buy food"
var_label(HouseholdCambodia$l51) <- "To buy food"
#C2491 = FACT, (bin), replace label: "For emergency expenses"
var_label(HouseholdCambodia$l52) <- "For emergency expenses"
#C2492 = FACT, (bin), replace label: "To buy animals"
var_label(HouseholdCambodia$l53) <- "To buy animals"
#C2493 = FACT, (bin), replace label: "To buy equipment/machinery for agriculture"
var_label(HouseholdCambodia$l54) <- "To buy equipment/machinery for agriculture"
#C2494 = FACT, (bin), replace label: "To buy seeds"
var_label(HouseholdCambodia$l55) <- "To buy seeds"
#C2495 = FACT, (bin), replace label: "To buy chemical farm inputs"
var_label(HouseholdCambodia$l56) <- "To buy chemical farm inputs"
#C2496 = FACT, (bin), replace label: "To buy organic farm inputs"
var_label(HouseholdCambodia$l57) <- "To buy organic farm inputs"
#C2497 = FACT, (bin), replace label: "To pay for certification"
var_label(HouseholdCambodia$l58) <- "To pay for certification"
#C2498 = FACT, (bin), replace label: "Festivals/celebrations"
var_label(HouseholdCambodia$l59) <- "Festivals/celebrations"
#C2499 = FACT, (bin), replace label: "Child education"
var_label(HouseholdCambodia$l510) <- "Child education"
#C2500 = FACT, (bin), replace label: "Health care"
var_label(HouseholdCambodia$l511) <- "Health care"
#C2501 = FACT, (bin), replace label: "To improve the house"
var_label(HouseholdCambodia$l512) <- "To improve the house"
#C2502 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$l599) <- "Other"
#C2503 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$l588) <- "Do not know"
#C2504 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2505 = CHAR, (Useless answer-MultipleCombined, use the following columns), (see answers below)
#Producer group
#Union (farmer, women, veteran)
#Bank
#Relatives & friends 
#Collector / trader
#Input seller
#Processor
#Other
#Do not know
#C2506 = FACT, (bin), replace label: "Producer group"
var_label(HouseholdCambodia$l61) <- "Producer group"
#C2507 = FACT, (bin), replace label: "Union (farmer, women, veteran)"
var_label(HouseholdCambodia$l62) <- "Union (farmer, women, veteran)"
#C2508 = FACT, (bin), replace label: "Bank"
var_label(HouseholdCambodia$l63) <- "Bank"
#C2509 = FACT, (bin), replace label: "Relatives & friends"
var_label(HouseholdCambodia$l64) <- "Relatives & friends"
#C2510 = FACT, (bin), replace label: "Collector / trader"
var_label(HouseholdCambodia$l65) <- "Collector / trader"
#C2511 = FACT, (bin), replace label: "Input seller"
var_label(HouseholdCambodia$l66) <- "Input seller"
#C2512 = FACT, (bin), replace label: "Processor"
var_label(HouseholdCambodia$l67) <- "Processor"
#C2513 = FACT, (bin), replace label: "Other"
var_label(HouseholdCambodia$l699) <- "Other"
#C2514 = FACT, (bin), replace label: "Do not know"
var_label(HouseholdCambodia$l688) <- "Do not know"
#C2515 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2516 = FACT, (bin)
#C2517 = FACT, (bin)

# # #m.
#C2518 = FACT, (table of correspondence), (see answers below)
#Complete
#Partially complete
#Refuses to participate
#No household member present
#Household moved to a new location
#C2519 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C2520 = OK
#C2521 = OK
#C2520 = REMOVE, (duplicate)
#C2521 = OK
#C2522 = OK
#C2523 = OK
#C2524 = OK

## For each column we determine if necessary to change the name, label or remove it
#C2525 = REMOVE, (text from questionnaire)
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



##2.2. Work on the 2nd database: "HouMemberCambodia" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, ID
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = FACT, (id of Household member in a household)
#C4 = FACT, (both previous id combined)
#C5 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it)
#C6 = FACT, (bin), replace label:"a1_1. is  a1 the survey respondent ?"
var_label(HouMemberCambodia$a1_1) <- "a1_1. is  a1 the survey respondent ?"
#C7 = FACT, (table of correspondence), (see answers below),
#replace label:"a2. what is a1’s relationship to household head ?"
var_label(HouMemberCambodia$a2) <- "a2. what is a1’s relationship to household head ?"
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
#C8 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it)
#C9 = FACT, (table of correspondence), (see answers below), replace label: "a3. what is the sex of a1 ?"
var_label(HouMemberCambodia$a3) <- "a3. what is the sex of a1 ?"
#Male
#Female
#C10 = FACT, (table of correspondence), (see answers below), replace label: "a4. what is a1's solar year of birth ?"
var_label(HouMemberCambodia$a4) <- "a4. what is a1's solar year of birth ?"
#C11 = NUM, replace label: "Age (years)"
var_label(HouMemberCambodia$a4_1) <- "Age (years)"
#C12 = FACT, (table of correspondence), (see answers below), replace label: "a5. what is the highest diploma a1 has obtained ?"
var_label(HouMemberCambodia$a5) <- "a5. what is the highest diploma a1 has obtained ?"
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
#C13 = FACT, (table of correspondence), (see answers below), replace label: "a6. what is a1's main occupation ?"
var_label(HouMemberCambodia$a6) <- "a6. what is a1's main occupation ?"
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
#C14 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it), replace label: "a6_oth. specify other main occupation of a1 in the past 12 months"
var_label(HouMemberCambodia$a6_oth) <- "a6_oth. specify other main occupation of a1 in the past 12 months"
#C15 = FACT, (table of correspondence), (see answers below), replace label: "a7. what is a1's secondary occupation ?"
var_label(HouMemberCambodia$a7) <- "a7. what is a1's secondary occupation ?"
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
#C16 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it), replace label: "a6_oth. specify other second occupation of a1 in the past 12 months"
var_label(HouMemberCambodia$a7_oth) <- "a7_oth. specify other second occupation of a1 in the past 12 months"
#C17 = FACT, (table of correspondence), (see answers below)
#Weaving
#Handicraft,
#Commerce
#Agricultural sector
#Agri. Service on soil preparation and harvesting
#Agri. Service on spraying
#Agri. Service on threshing
#Services/employment in tourism,
#Services/employment in restaurant
#Services/employment in shops
#Transportation,
#Blacksmithing
#Construction work,
#Factory work,
#Other
#Do not know
#Other
#Do not know
#C18 = FACT, (bin), replace label: "Weaving"
var_label(HouMemberCambodia$a81) <- "Weaving"
#C19 = FACT, (bin), replace label: "Handicraft"
var_label(HouMemberCambodia$a82) <- "Handicraft"
#C20 = FACT, (bin), replace label: "Commerce"
var_label(HouMemberCambodia$a83) <- "Commerce"
#C21 = FACT, (bin), replace label: "Agricultural sector"
var_label(HouMemberCambodia$a84) <- "Agricultural sector"
#C22 = FACT, (bin), replace label: "Agri. Service on soil preparation and harvesting"
var_label(HouMemberCambodia$a85) <- "Agri. Service on soil preparation and harvesting"
#C23 = FACT, (bin), replace label: "Agri. Service on spraying"
var_label(HouMemberCambodia$a86) <- "Agri. Service on spraying"
#C24 = FACT, (bin), replace label: "Agri. Service on threshing"
var_label(HouMemberCambodia$a87) <- "Agri. Service on threshing"
#C25 = FACT, (bin), replace label: "Services/employment in tourism"
var_label(HouMemberCambodia$a88) <- "Services/employment in tourism"
#C26 = FACT, (bin), replace label: "Services/employment in restaurant"
var_label(HouMemberCambodia$a89) <- "Services/employment in restaurant"
#C27 = FACT, (bin), replace label: "Services/employment in shops"
var_label(HouMemberCambodia$a810) <- "Services/employment in shops"
#C28 = FACT, (bin), replace label: "Transportation"
var_label(HouMemberCambodia$a811) <- "Transportation"
#C29 = FACT, (bin), replace label: "Blacksmithing"
var_label(HouMemberCambodia$a812) <- "Blacksmithing"
#C30 = FACT, (bin), replace label: "Construction work"
var_label(HouMemberCambodia$a813) <- "Construction work"
#C31 = FACT, (bin), replace label: "Factory work"
var_label(HouMemberCambodia$a814) <- "Factory work"
#C32 = FACT, (bin), replace label: "Other"
var_label(HouMemberCambodia$a899) <- "Other"
#Move this column
HouMemberCambodia <- HouMemberCambodia %>% relocate(a899, .after = a814)
#C33 = FACT, (bin), replace label: "Do not know"
var_label(HouMemberCambodia$a888) <- "Do not know"
#Move this column
HouMemberCambodia <- HouMemberCambodia %>% relocate(a888, .after = a899)
#C34 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it)
#C35 = FACT, (table of correspondence), (see answers below), replace label: "a9. reason why a1  was unable to work in the last 12 months ?"
var_label(HouMemberCambodia$a9) <- "a9. reason why a1  was unable to work in the last 12 months ?"
#Cannot find employment
#Sick
#Handicapped
#Too old
#Too young
#Other reason
#C36 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it), replace label: "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
var_label(HouMemberCambodia$a9_oth) <- "a9_oth. specify other reason why a1 was unable to work in the last 12 months ?"
#C37 = FACT, (bin), replace label: "a10. do a1 migrate seasonally every year ?"
var_label(HouMemberCambodia$a10) <- "a10. do a1 migrate seasonally every year ?"
#C38 = OK, (the answer is still in Cambodiaese, Ky will maybe solve it)
#C39 = FACT, (table of correspondence), (see answers below), replace label: "a12. if yes, what is a1's main occupation when migrating"
var_label(HouMemberCambodia$a12) <- "a12. if yes, what is a1's main occupation when migrating"
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
#C40 = CHAR, (Useless answer-MultipleCombined, use the following columns), replace label: "a13. if yes, what months do a1 migrates ?"
var_label(HouMemberCambodia$a13) <- "a13. if yes, what months do a1 migrates ?"
#C41 = FACT, (bin), replace label: "Jan"
var_label(HouMemberCambodia$a131) <- "Jan"
#C42 = FACT, (bin), replace label: "Feb"
var_label(HouMemberCambodia$a132) <- "Feb"
#C43 = FACT, (bin), replace label: "Mar"
var_label(HouMemberCambodia$a133) <- "Mar"
#C44 = FACT, (bin), replace label: "Apr"
var_label(HouMemberCambodia$a134) <- "Apr"
#C45 = FACT, (bin), replace label: "May"
var_label(HouMemberCambodia$a135) <- "May"
#C46 = FACT, (bin), replace label: "Jun"
var_label(HouMemberCambodia$a136) <- "Jun"
#C47 = FACT, (bin), replace label: "Jul"
var_label(HouMemberCambodia$a137) <- "Jul"
#C48 = FACT, (bin), replace label: "Aug"
var_label(HouMemberCambodia$a138) <- "Aug"
#C49 = FACT, (bin), replace label: "Sep"
var_label(HouMemberCambodia$a139) <- "Sep"
#C50 = FACT, (bin), replace label: "Oct"
var_label(HouMemberCambodia$a1310) <- "Oct"
#C51 = FACT, (bin), replace label: "Nov"
var_label(HouMemberCambodia$a1311) <- "Nov"
#C52 = FACT, (bin), replace label: "Dec"
var_label(HouMemberCambodia$a1312) <- "Dec"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C53 = OK, (member id and name combined), (useless?)
#C54 = FACT, (bin), ??
#C55 = FACT, (bin), ??
#C56 = FACT, (bin), ??
#C57 = FACT, (bin), ??
#C58 = FACT, (bin), ??
#C59 = FACT, (bin), ??
#C60 = OK, (empty)
#C61 = OK, (empty)
#C62 = OK, (empty)
#C63 = OK, (empty)
#C64 = OK, (empty)
#C65 = OK, (empty)
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

##2.3. Work on the third database: "ClowlandCambodia" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, ID
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = FACT, (id of crop per household)
#C4 = FACT, We create a new column here as the combination of crop and household id
ClowlandCambodia$pid <- paste(ClowlandCambodia$hhid_re2,ClowlandCambodia$crop1_now)
#We move this column at the right place
ClowlandCambodia <- ClowlandCambodia %>% relocate(pid, .after = hhid_re2)
#C5 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C6 = FACT, (number corresponding to crops), replace label: "d2_12. select crop crop1_now with keyword searchcrop1"
var_label(ClowlandCambodia$d2_12) <- "d2_12. select crop crop1_now with keyword searchcrop1"
#C7 = OK, (the answer is still in Khmer, Ky will maybe solve it)
#C8 = REMOVE, (empty)
#C9 = OK, (crop name in Khmer) replace label: "d2_13v. Crop name (Khmer)"
var_label(ClowlandCambodia$d2_13v) <- "d2_13v. Crop name (Khmer)"
#C10 = OK, (crop name in English) replace label: "d2_13e. Crop name (English)"
var_label(ClowlandCambodia$d2_13e) <- "d2_13e. Crop name (English)"
#C11 = NUM, replace label: "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(ClowlandCambodia$d2_132) <- "d2_132. what is the total area of all plots where you grow crop_1 (m2) ?"
#C12 = FACT, (table of correspondence), (see option below), replace label: "d2_133. which unit of d2_13v ?"
var_label(ClowlandCambodia$d2_133) <- "d2_133. which unit of d2_13v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C13 = OK, replace label: "d2_134. number of seed units of d2_13v that household used?"
var_label(ClowlandCambodia$d2_134) <- "d2_134. number of seed units of d2_13v that household used?"
#C14 = FACT, (table of correspondence), (see option below), replace label: "d2_135a. which unit of d2_13v ?"
var_label(ClowlandCambodia$d2_135a) <- "d2_135a. which unit of d2_13v ?"
#C15 = FACT, (table of correspondence), (see option below), replace label: "d2_135b. Specify unit for d2_13v ?"
var_label(ClowlandCambodia$d2_135b) <- "d2_135b. Specify unit for d2_13v ?"
#C16 = OK, replace label: "d2_135c. number of unit produced of d2_13v ?"
var_label(ClowlandCambodia$d2_135c) <- "d2_135. number of unit produced of d2_13v ?"
#C17 = FACT, (table of correspondence), (see option below), replace label: "d2_136a. which unit of d2_13v ?"
var_label(ClowlandCambodia$d2_136a) <- "d2_136a. which unit of d2_13v ?"
#C18 = FACT, (table of correspondence), (see option below), replace label: "d2_136b. Specify unit for d2_13v ?"
var_label(ClowlandCambodia$d2_136b) <- "d2_136b. Specify unit for d2_13v ?"
#C19 = OK, replace label: "d2_136c. number of unit produced of d2_13v ?"
var_label(ClowlandCambodia$d2_136c) <- "d2_136c. number of unit produced of d2_13v ?"
#C20 = OK, replace label: "d2_137.number of kg sold of d2_13v"
var_label(ClowlandCambodia$d2_137) <- "d2_137. selling price/ kg of d2_13v"
#C21 = ??
#C22 = OK, replace label: "d2_138. how many species or varieties of d2_13v ?"
var_label(ClowlandCambodia$d2_138) <- "d2_138. how many species or varieties of d2_13v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C23 = (empty)
#C24 = (empty)
#C25 = (empty)
#C26 = FACT, (id)
#C27 = FACT, (id)
#C28 = FACT, (time)
#C29 = FACT, (empty)
#C30 = OK, (empty)
#C31 = OK, (empty)
#C32 = OK, (useless?)
#C33 = OK, (useless?)
#C34 = OK, (empty)

##2.4. Work on the fourth database: "CuplandCambodia" (C = Column)
## For each column we determine if necessary to change the name, label or remove it

#C1 = CHAR, ID
#C2 = FACT, (id of household, to link with other dataframes)
#C3 = FACT, (id of crop per household)
#C4 = ???
#C5 = FACT, (number corresponding to crops), replace label: "d2_22. select crop crop1_now with keyword searchcrop1"
#C6 = OK, (empty)
#C7 = OK, (crop name in Khmer) replace label: "d2_23v. Crop name (Khmer)"
var_label(CuplandCambodia$d2_23v) <- "d2_23v. Crop name (Khmer)"
#C8 = OK, (crop name in English) replace label: "d2_23e. Crop name (English)"
var_label(CuplandCambodia$d2_23e) <- "d2_23e. Crop name (English)"
#C9 = ???
#C10 = NUM, replace label: "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
var_label(CuplandCambodia$d2_232) <- "d2_232. what is the total area of all plots where you grow crop_1 (m2) ?"
#C11 = FACT, (table of correspondence), (see option below), replace label: "d2_233. which unit of d2_23v ?"
var_label(CuplandCambodia$d2_233) <- "d2_233. which unit of d2_23v ?"
#Kg of seed
#Gr of seed
#Number of seedlings
#C12 = OK, replace label: "d2_234. number of seed units of d2_23v that household used?"
var_label(CuplandCambodia$d2_234) <- "d2_234. number of seed units of d2_23v that household used?"
#C13 = FACT, (table of correspondence), (see option below), replace label: "d2_135a. which unit of d2_13v ?"
var_label(CuplandCambodia$d2_235a) <- "d2_235a. which unit of d2_13v ?"
#C14 = FACT, (table of correspondence), (see option below), replace label: "d2_135b. Specify unit for d2_13v ?"
var_label(CuplandCambodia$d2_235b) <- "d2_235b. Specify unit for d2_13v ?"
#C15 = OK, replace label: "d2_235c. number of unit produced of d2_13v ?"
var_label(CuplandCambodia$d2_235c) <- "d2_235c. number of unit produced of d2_13v ?"
#C16 = FACT, (table of correspondence), (see option below), replace label: "d2_136a. which unit of d2_13v ?"
var_label(CuplandCambodia$d2_236a) <- "d2_236a. which unit of d2_13v ?"
#C17 = FACT, (table of correspondence), (see option below), replace label: "d2_136b. Specify unit for d2_13v ?"
var_label(CuplandCambodia$d2_236b) <- "d2_236b. Specify unit for d2_13v ?"
#C18 = OK, replace label: "d2_236c. number of unit produced of d2_13v ?"
var_label(CuplandCambodia$d2_236c) <- "d2_236c. number of unit produced of d2_13v ?"
#C19 = OK, replace label: "d2_237.d2_237. selling price/ kg of d2_23v"
var_label(CuplandCambodia$d2_237a) <- "d2_237a. selling price/ kg of d2_23v"
#C20 = ??
#C21 = OK, replace label: "d2_238. how many species or varieties of d2_23v ?"
var_label(CuplandCambodia$d2_238) <- "d2_238. how many species or varieties of d2_23v ?"

# FOR FOLLOWING COLUMNS, DECIDE TO REMOVE IT OR NOT
#C22 = ??
#C23 = FACT, (CROP id)
#C24 = OK, (useless??)
#C25 = FACT, (parent index)
#C26 = FACT, (id)
#C27 = FACT, (id)
#C28 = FACT, (time)



### 3. Columns data type changing and removing


##3.1 Change column data types base on previous Columns ID
#Convert the tbl_df into a dataframe for each database
HouseholdCambodia_2 <- as.data.frame(HouseholdCambodia)
HouMemberCambodia_2 <- as.data.frame(HouMemberCambodia)
ClowlandCambodia_2 <- as.data.frame(ClowlandCambodia)
CuplandCambodia_2 <- as.data.frame(CuplandCambodia)
#a. FACTOR CONVERSION
#Convert columns to FACTOR - "HouseholdCambodia"
for (i in c(1:3,7:9,11,13:34,36,38:42,48:54,57,68:78,80:86,88:91,93,96:117,120,122,124,131,133:154,
            157,159:171,173:184,186:200,203:211,213:226,228:236,238:246,248:257,260:268,270,274:289,
            295:310,312,316:331,337:352,354,358:373,376,380:395,398,402:417,419,423:438,440,444:459,
            462:463,465:473,475,477,479:480,482:496,498,500:510,512,514,516,519:530,532:535,537:566,
            569:576,578:580,582:589,620:634,639:653,655:663,665:666,671:685,687:695,697,709:724,727,
            730,733,736,739,742,745,748,751,757,760,766,769,771:773,777,779,781,783,785,787,790,793,796,
            800,801:806,809:817,819,821:835,838:852,855:869,872:886,889:903,906:920,923:937,940:954,
            957:965,968:982,985:999,1002:1016,1019:1033,1036:1050,1053:1067,1070:1084,1087:1101,1103,
            1104,1106:1117,1120:1134,1137:1151,1154:1168,1171:1185,1188:1202,1205:1219,1222:1236,1239:1253,
            1256:1270,1273:1287,1290:1304,1307:1321,1323:1324,1326:1340,1343:1357,1359:1374,1377:1391,
            1394:1408,1411:1425,1428:1442,1445:1459,1462:1476,1479:1493,1496:1510,1513:1527,1530:1544,
            1547:1561,1565:1578,1581:1595,1597:1599,1601,1603:1618,1621:1635,1638:1652,1655:1669,1672:1686,
            1689:1703,1706:1720,1723:1737,1740:1754,1757:1771,1774:1788,1791:1805,1808:1822,1824:1839,
            1841:1856,1859:1873,1876:1890,1892:1894,1896:1910,1912:1926,1928:1942,1944:1958,1961:1964,
            1966:1969,1971:1974,1976:1979,1981:1984,1986:1989,1991:1994,1996:1999,2001:2004,2006:2009,
            2011:2014,2018:2028,2030,2032:2044,2064,2066,2068,2071:2073,2084,2087,2089:2097,2099,2101:2103,
            2114,2117,2118,2120,2122,2124,2135,2138,2140,2142,2144:2150,2152:2166,2168:2170,2172:2186,2188,
            2190:2195,2197:2200,2202,2204,2206:2210,2212,2214,2216,2217:2218,2220:2237,2239:2253,2255,
            2257:2267,2269,2271,2273:2277,2279,2281,2283:2285,2287:2304,2306:2320,2322,2324:2329,2331:2335,
            2337,2339,2341:2345,2347,2351:2362,2365:2378,2380:2381,2383:2388,2390:2393,2395:2400,2402,
            2406:2407,2409:2420,2422:2429,2431:2432,2434,2436,2438:2440,2442,2445:2464,2485:2488,2490:2503,
            2506:2514,2516:2518)){
  HouseholdCambodia_2[,i] <- as.factor(HouseholdCambodia_2[,i])
}
#Convert columns to FACTOR - "HouMemberCambodia"
for (i in c(1:4,6:7,9:10,12:13,15,17:33,35,37,39,41:52,54:59)){
  HouMemberCambodia_2[,i] <- as.factor(HouMemberCambodia_2[,i])
}
#Convert columns to FACTOR - "ClowlandCambodia"
for (i in c(2:4,6,12,14:15,17:18,26:29)){
  ClowlandCambodia_2[,i] <- as.factor(ClowlandCambodia_2[,i])
}
#Convert columns to FACTOR - "CuplandCambodia"
for (i in c(2:3,5,11,13:14,16:17,23,25:28)){
  CuplandCambodia_2[,i] <- as.factor(CuplandCambodia_2[,i])
}
#b. NUMERIC CONVERSION
#Convert columns to NUMERIC - "HouseholdCambodia"
for (i in  c(67,127,129,590,598:600,609:611,617:618,702:703,726,729,731:732,734:735,737:738,740:741,
             743:744,747,749:750,752,756,759,761,765,767:768,770,792,795,798)){
  HouseholdCambodia_2[,i] <- as.numeric(HouseholdCambodia_2[,i])
}
#Convert columns to NUMERIC - "HouMemberCambodia"
  HouMemberCambodia_2[,10] <- as.numeric(HouMemberCambodia_2[,11])
#Convert columns to NUMERIC - "ClowlandCambodia"
  ClowlandCambodia_2[,10] <- as.numeric(ClowlandCambodia_2[,11])
#Convert columns to NUMERIC - "CuplandCambodia"
  CuplandCambodia_2[,9] <- as.numeric(CuplandCambodia_2[,10])
#c. CHARACTER CONVERSION
#Convert columns to CHARACTER - "HouseholdCambodia"
  for (i in  c(1,95,132,155,158,172,185,202,227,247,259,273,294,315,335:336,357,379,401,422,443,481,
               499,518,536,568,581,619,637,638,654,669:670,686,708,799,808,820,837,854,871,888,905,922,
               939,956,967,984,1001,1018,1035,1052,1069,1086,1105,1119,1136,1153,1170,1187,1204,1221,1238,
               1255,1272,1289,1306,1325,1342,1376,1393,1410,1427,1444,1461,1478,1495,1512,1529,1546,1563,
               1580,1602,1620,1637,1654,1671,1688,1705,1722,1739,1756,1773,1790,1807,1858,1875,1895,1911,
               1927,1943,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2017,2031,2088,2151,2171,
               2189,2196,2205,2219,2238,2256,2272,2286,2305,2323,2330,2340,2350,2364,2394,2408,2421,2444,
               2489,2505)){
    HouseholdCambodia_2[,i] <- as.character(HouseholdCambodia_2[,i])
  }
#Convert columns to CHARACTER - "HouMemberCambodia"
  for (i in  c(1,40)){
  HouMemberCambodia_2[,i] <- as.factor(HouMemberCambodia_2[,i])
  }
#Convert columns to CHARACTER - "ClowlandCambodia"
  ClowlandCambodia_2[,1] <- as.numeric(ClowlandCambodia_2[,1])
#Convert columns to CHARACTER - "CuplandCambodia"
  CuplandCambodia_2[,1] <- as.numeric(CuplandCambodia_2[,1])


##3.2 REMOVE Unwanted columns (1st Version)
#Remove unwanted columns for each datasets
#HouseholdCambodia_2C <- HouseholdCambodia_2[c(5:6,43:47,58,92,474,597,608,789,2074,2104,2125,2382,
                                              #2389,2405,2484,2520,2525,2526,2527:2574]
#HouMemberCambodia_2C <- HouMemberCambodia_2[,-c()]
#ClowlandCambodia_2C <- ClowlandCambodia_2C[,]

#Add the proper labels to each columns
HouseholdCambodia_2C <- copy_labels(HouseholdCambodia_2, HouseholdCambodia)
HouMemberCambodia_2C <- copy_labels(HouMemberCambodia_2, HouMemberCambodia)
ClowlandCambodia_2C <- copy_labels(ClowlandCambodia_2, ClowlandCambodia)
CuplandCambodia_2 <- copy_labels(CuplandCambodia_2, CuplandCambodia)


### 4. Data Cleaning 1 (Based on Ky comments)

## 4.1 Data cleaning for "HouseholdLaos_2"


# # #a. Duplicates
#Check if all households agreed to participate on the survey (% of "yes" answer)
count_if("1",HouseholdCambodia_2C$consent)/nrow(HouseholdCambodia_2C)
# Check household duplicates, for some of them it correspond to answer from the man
#and from the woman to the survey
#1st we move the household ID column at 1st column
HouseholdCambodia_2C <- HouseholdCambodia_2C %>% relocate(o9 , .before = start_time)
#Check the number and id of duplicates
count_if("TRUE",duplicated(HouseholdCambodia_2C$o9))
Dum <- HouseholdCambodia_2C[duplicated(HouseholdCambodia_2C$o9),]
IdDup <- Dum$o9
#Create a loop to merge all the household values in a same column
for (i in (Dum$o9)){
  Temp <- subset(HouseholdCambodia_2C, o9 == i)
  for (j in 1:ncol(HouseholdCambodia_2C)){
    ifelse(is.na(Temp[1,j]), Temp[1,j] <- Temp[2,j], Temp[1,j] <- Temp[1,j])
  }
  HouseholdCambodia_2C[HouseholdCambodia_2C$o9 == i,] <- Temp[1,]
}
#Select all duplicates rows and delete the unnecessary
Dupli <- HouseholdCambodia_2C[HouseholdCambodia_2C$o9 %in% Dum$o9,]
Dupli <- Dupli[order(Dupli$o9),]
Dupli <- rownames(Dupli)
row_odd <- seq_len(length(Dupli)) %% 2
Dupli <- Dupli[row_odd == 0]
HouseholdCambodia_2C <- HouseholdCambodia_2C[!rownames(HouseholdCambodia_2C) %in% Dupli,]
#There is another duplicate, we remove it:
HouseholdCambodia_2C <- HouseholdCambodia_2C[!rownames(HouseholdCambodia_2C) == "409",]
#Check again the duplicates: 
count_if("TRUE",duplicated(HouseholdCambodia_2C$o9))



# # #b Outliers part 1



# # #c. Corresponding fields
#-	There were 13 households who did no selling agri-products (at b1) but they still selected 3
#main sources of income from crop production and livestock raising (b3). Please review list below
#to know and check record for validating data
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C$b1 <- as.character(HouseholdCambodia_2C$b1)
HouseholdCambodia_2C[c(65,96,119,132,154,166,492),80] <- "1"
HouseholdCambodia_2C[c(93,174),80] <- "2"
HouseholdCambodia_2C[c(67,318),80] <- "3"

#For selling livestock, we based on b1 & b2 to find 308 households selling fresh or processed
#livestock product, but there were 318 ones answered at b17. So, we should delete 10 households
#through check_b10inc_liv (0=no sell livestock products, 1=sold livestock products). Households
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C[424,80] <- "3"
HouseholdCambodia_2C[177,80] <- "0"
HouseholdCambodia_2C$b1 <- as.factor(HouseholdCambodia_2C$b1)
HouseholdCambodia_2C$b1 <- as.character(HouseholdCambodia_2C$b17)
HouseholdCambodia_2C[c(67,93,174,319),242] <- "88"
HouseholdCambodia_2C$b1 <- as.factor(HouseholdCambodia_2C$b17)

#Another problem that we missed data for 45 households at b13_01, we had 362 households sold
#crop products for the 1st buyers (at b12_1), but only 317 households listed crop they sold at
#b13_01. 
#NOTHING TO DO HERE FOR NOW

#Need validate unlogic between d81 (576 respondents) and d81_1a (578 respondents)
HouseholdCambodia_2C$d81_1a <- as.character(HouseholdCambodia_2C$d81_1a)
HouseholdCambodia_2C$d81_1a <- ifelse(is.na(HouseholdCambodia_2C$d81) & !is.na(HouseholdCambodia_2C$d81_1a), NA, HouseholdCambodia_2C$d81_1a)
HouseholdCambodia_2C$d81_1a <- as.factor(HouseholdCambodia_2C$d81_1a)

#Need validate unlogic between d82 (521 respondents) and d82_1a (533 respondents)
HouseholdCambodia_2C$d82_1a <- as.character(HouseholdCambodia_2C$d82_1a)
HouseholdCambodia_2C$d82_1a <- ifelse(is.na(HouseholdCambodia_2C$d82) & !is.na(HouseholdCambodia_2C$d82_1a), NA, HouseholdCambodia_2C$d82_1a)
HouseholdCambodia_2C$d82_1a  <- as.factor(HouseholdCambodia_2C$d82_1a)

#Need validate unlogic between d83 (397 respondents) and d83_1a (414 respondents)
HouseholdCambodia_2C$d83_1a <- as.character(HouseholdCambodia_2C$d83_1a)
HouseholdCambodia_2C$d83_1a <- ifelse(is.na(HouseholdCambodia_2C$d83) & !is.na(HouseholdCambodia_2C$d83_1a), NA, HouseholdCambodia_2C$d83_1a)
HouseholdCambodia_2C$d83_1a  <- as.factor(HouseholdCambodia_2C$d83_1a)

#We have 578 hhs with lowland or upland but d12 on water conservation practice have 581
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C$d12 <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C$d12)
for (i in c(865:874)){
  HouseholdCambodia_2C[,i] <- as.character(HouseholdCambodia_2C[,i])
  HouseholdCambodia_2C[,i] <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C[,i])
  HouseholdCambodia_2C[,i] <- as.factor(HouseholdCambodia_2C[,i])
}

#Check for d131_1 & d131_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d131_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d131_2))
#Check for d133_1 & d133_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d133_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d133_2))
#Check for d134_1 & d134_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d134_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d134_2))
#Check for d135_1 & d135_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d135_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d135_2))
#Check for d136_1 & d136_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d136_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d136_2))
#Check for d137_1 & d137_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d137_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d137_2))
#Check for d138_1 & d138_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d138_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d138_2))

#We have 578 hhs with lowland or upland but d14 on soil conservation practice have 575
#MANUAL, NEED TO MODIFY IT
#1st, there are 2 '0' values for households with no crops, we replace it by ''
HouseholdCambodia_2C$d14 <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C$d14)
for (i in c(989:997)){
  HouseholdCambodia_2C[,i] <- as.character(HouseholdCambodia_2C[,i])
  HouseholdCambodia_2C[,i] <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C[,i])
  HouseholdCambodia_2C[,i] <- as.factor(HouseholdCambodia_2C[,i])
}

#Then, there are 7 values with answers to column corresponding to each practices,
#but not on column 13, we replace '' by '0':
HouseholdCambodia_2C$d14 <- ifelse(HouseholdCambodia_2C$d14 == '' & HouseholdCambodia_2C$d140 == 2, '0', HouseholdCambodia_2C$d14)

#Check for d151_1 & d151_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d151_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d151_2))
#Check for d152_1 & d152_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d152_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d152_2))
#Check for d153_1 & d153_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d153_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d153_2))
#Check for d154_1 & d154_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d154_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d154_2))
#Check for d155_1 & d155_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d155_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d155_2))
#Check for d156_1 & d156_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d156_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d156_2))
#Check for d157_1 & d157_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d157_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d157_2))
#Check for d158_1 & d158_2, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d158_1 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d158_2))

#We have 3 answers for d16 for hhs without lowland or upland
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C$d16 <- as.character(HouseholdCambodia_2C$d16)
HouseholdCambodia_2C$d16 <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C$d16)
HouseholdCambodia_2C$d16 <- as.factor(HouseholdCambodia_2C$d16)

#For d18, there are different issues solved below
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C$d18 <- ifelse(HouseholdCambodia_2C$d181 == '1' & HouseholdCambodia_2C$d18 == '', '1', HouseholdCambodia_2C$d18)
HouseholdCambodia_2C$d18 <- ifelse(HouseholdCambodia_2C$d1899 == '1' & HouseholdCambodia_2C$d18 == '', as.character('99'), HouseholdCambodia_2C$d18)
#For d17, there are 3 '0' values for households with no crops, we replace it by ''
HouseholdCambodia_2C$d17 <- as.character(HouseholdCambodia_2C$d17)
HouseholdCambodia_2C$d17 <- ifelse(HouseholdCambodia_2C$d17 != '2' & !is.na(HouseholdCambodia_2C$d18), '2', HouseholdCambodia_2C$d17)
HouseholdCambodia_2C$d17 <- ifelse(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2), NA, HouseholdCambodia_2C$d17)
HouseholdCambodia_2C$d17 <- as.factor(HouseholdCambodia_2C$d17)

## 3 NA remains for households with crops, see later if necesary to change it
#for "no practice"

#Check for d18_11 & d18_12, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_11 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_12))
#Check for d18_21 & d18_22, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_21 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_22))
#Check for d18_31 & d18_32, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_31 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_32))
#Check for d18_41 & d18_42, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_41 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_42))
#Check for d18_51 & d18_52, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_51 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_52))
#Check for d18_61 & d18_62, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_61 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_62))
#Check for d18_71 & d18_72, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_71 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_72))
#Check for d18_81 & d18_82, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_81 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_82))
#Check for d18_91 & d18_92, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_91 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_92))
#Check for d18_101 & d18_102, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_101 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_102))
#Check for d18_111_a & d18_112, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d18_111_a != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d18_112))

#For d19, one hh answered while he mentionned to have implemented this practice
HouseholdCambodia_2C$d19 <- as.character(HouseholdCambodia_2C$d19)
HouseholdCambodia_2C$d19 <- ifelse(HouseholdCambodia_2C$d17 == '2' & !is.na(HouseholdCambodia_2C$d19), NA, HouseholdCambodia_2C$d19)
HouseholdCambodia_2C$d19 <- as.factor(HouseholdCambodia_2C$d19)

#For d20 & d21, there are different issues solved below
#MANUAL, NEED TO MODIFY IT
HouseholdCambodia_2C$d21 <- ifelse(HouseholdCambodia_2C$d2110 == '1' & HouseholdCambodia_2C$d21 == '', '10', HouseholdCambodia_2C$d21)
#For d20, different issues solved below
HouseholdCambodia_2C$d20 <- as.character(HouseholdCambodia_2C$d20)
HouseholdCambodia_2C$d20 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d20)
x <- paste(HouseholdCambodia_2C$no_crop1,HouseholdCambodia_2C$no_crop2)
HouseholdCambodia_2C$d20 <- ifelse( x != " " & is.na(HouseholdCambodia_2C$d20), '0', HouseholdCambodia_2C$d20)
HouseholdCambodia_2C$d20 <- as.factor(HouseholdCambodia_2C$d20)

#Check for d21_12 & d21_13, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_12 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_13))
#Check for d21_22 & d21_23, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_22 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_23))
#Check for d21_32 & d21_33, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_32 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_33))
#Check for d21_42 & d21_43, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_42 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_43))
#Check for d21_52 & d21_53, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_52 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_53))
#Check for d21_62 & d21_63, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_62 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_63))
#Check for d21_72 & d21_73, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_72 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_73))
#Check for d21_82 & d21_83, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_82 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_83))
#Check for d21_92 & d21_93, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_92 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_93))
#Check for d21_102 & d21_103, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_102 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_103))
#Check for d21_112 & d21_113, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_112 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_113))
#Check for d21_122 & d21_123, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_122 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_123))
#Check for d21_132 & d21_133, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_132 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_133))
#Check for d21_992 & d21_993, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d21_992 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d21_993))

#For d22, one hh answered while he mentionned to have implemented this practice
HouseholdCambodia_2C$d22 <- as.character(HouseholdCambodia_2C$d22)
HouseholdCambodia_2C$d22 <- ifelse(HouseholdCambodia_2C$d20 == '2' & !is.na(HouseholdCambodia_2C$d22), NA, HouseholdCambodia_2C$d22)
HouseholdCambodia_2C$d22 <- as.factor(HouseholdCambodia_2C$d22)

#One household didn't answered to d22 but we don't fulfil it as we don't know his answer

#For d24, different issues solved below
HouseholdCambodia_2C$d24 <- as.character(HouseholdCambodia_2C$d24)
HouseholdCambodia_2C$d24 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d24)
HouseholdCambodia_2C$d24 <- as.factor(HouseholdCambodia_2C$d24)

#For d26, different issues solved below
HouseholdCambodia_2C$d26 <- as.character(HouseholdCambodia_2C$d26)
HouseholdCambodia_2C$d26 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d26)
HouseholdCambodia_2C$d26 <- as.factor(HouseholdCambodia_2C$d26)

#Check for d27_11 & d27_12, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_11 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_12))
#Check for d27_21 & d27_22, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_21 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_22))
#Check for d27_31 & d27_32, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_31 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_32))
#Check for d27_41 & d27_42, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_41 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_42))
#Check for d27_51 & d27_52, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_51 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_52))
#Check for d27_61 & d27_62, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_61 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_62))
#Check for d27_71 & d27_72, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_71 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_72))
#Check for d27_81 & d27_82, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_81 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_82))
#Check for d27_91 & d27_92, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_91 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_92))
#Check for d27_101 & d27_102, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_101 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_102))
#Check for d27_111_a & d27_112_a, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_111_a != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_112_a))
#Check for d27_121 & d27_122, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_121 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_122))
#Check for d27_131 & d27_132, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_131 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_132))
#Check for d27_141 & d27_142, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_141 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_142))
#Check for d27_991 & d27_992, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & HouseholdCambodia_2C$d27_991 != '')
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d27_992))

#For d28, different issues solved below
HouseholdCambodia_2C$d28 <- as.character(HouseholdCambodia_2C$d28)
HouseholdCambodia_2C$d28 <- ifelse(is.na(HouseholdCambodia_2C$d26) & !is.na(HouseholdCambodia_2C$d28), NA, HouseholdCambodia_2C$d28)
HouseholdCambodia_2C$d28 <- as.factor(HouseholdCambodia_2C$d28)

#For d30_1, different issues solved below
HouseholdCambodia_2C$d30_1 <- as.character(HouseholdCambodia_2C$d30_1)
HouseholdCambodia_2C$d30_1 <- ifelse(HouseholdCambodia_2C$d30_2 == '' & HouseholdCambodia_2C$d30_1 == '1' , '0', HouseholdCambodia_2C$d30_1)
HouseholdCambodia_2C$d30_1 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d30_1)
HouseholdCambodia_2C$d30_1 <- as.factor(HouseholdCambodia_2C$d30_1)

#Check for d30_3 & d30_4, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d30_3))
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d30_4))
#Check for d30_5 & d30_6, Crops for which you use this practice and main motivation
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d30_5))
sum(is.na(HouseholdCambodia_2C$no_crop1) & is.na(HouseholdCambodia_2C$no_crop2) & !is.na(HouseholdCambodia_2C$d30_6))

#For d32 & d32_2, different issues solved below
HouseholdCambodia_2C$d32 <- as.character(HouseholdCambodia_2C$d32)
HouseholdCambodia_2C$d32 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d32)
HouseholdCambodia_2C$d32 <- ifelse(HouseholdCambodia_2C$d32_1 == '' & HouseholdCambodia_2C$d32 == '2' , '1', HouseholdCambodia_2C$d32)
HouseholdCambodia_2C$d32 <- as.factor(HouseholdCambodia_2C$d32)
HouseholdCambodia_2C$d32_2 <- as.character(HouseholdCambodia_2C$d32_2)
HouseholdCambodia_2C$d32_2 <- ifelse(HouseholdCambodia_2C$no_crop1 == '' & HouseholdCambodia_2C$no_crop2 == '' , NA, HouseholdCambodia_2C$d32_2)
HouseholdCambodia_2C$d32_2 <- as.factor(HouseholdCambodia_2C$d32_2)

#For module E3. Cattle and buffalo system, households having other kind of cattle 
#didn't fulfil this part

#One household is raising cattle/buffalo and didn't fulfil E3 = Nothing more to do...

#There were 317 households raised buffalo and cattle as important animals. However, I found 328 
#households participated in the module E3. In fact, these households raise pig and the data
#In module E3 is probably the data supposed to be in module E4, so we'll transfer these data at the right place
#First we set columns in the same order
HouseholdCambodia_2C <- HouseholdCambodia_2C %>% relocate(e3899 , .after = e3888)
#Then we extract household id of concerned households
x <- HouseholdCambodia_2C[HouseholdCambodia_2C$e2_1 == '0' & HouseholdCambodia_2C$e2_2 == '0' & !is.na(HouseholdCambodia_2C$e16),]
hid <- x$o9
#And finally we create a loop to replace the values in E4 and remove them from E3
for (i in hid){
  HouseholdCambodia_2C[HouseholdCambodia_2C$o9 == i,c(2044:2102)] <- HouseholdCambodia_2C[HouseholdCambodia_2C$o9 == i,c(1982:1996,1998:2041)]
  HouseholdCambodia_2C[HouseholdCambodia_2C$o9 == i,c(1982:1996,1998:2041)] <- 'NA'
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
HouseholdCambodia_2C[,i] <- ifelse(HouseholdCambodia_2C$e2_1 == '0' & HouseholdCambodia_2C$e2_2 == '0',
                                             NA,HouseholdCambodia_2C[,i])
}

## 4.2C Data cleaning for "HouMemberCambodia_2"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(HouMemberCambodia_2C$pid))
d <- count(HouMemberCambodia_2C, pid)
#Check and remove the real duplicates through automated method
Dum <- HouMemberCambodia_2C[duplicated(HouMemberCambodia_2C$pid),]
idDup <- unique(Dum$hhid_re1)
DumReal <- HouMemberCambodia_2C[HouMemberCambodia_2C$hhid_re1 %in% idDup,]
DumReal$check <- paste(DumReal$p_no, DumReal$hhid_re1, DumReal$a2, DumReal$a4_1)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
HouMemberCambodia_2C <- HouMemberCambodia_2C[!rownames(HouMemberCambodia_2C) %in% Duplic,]
#For some duplicates, we have to do it manually:
#TO ADAPT TO EACH DATABASE
HouMemberCambodia_2C <- HouMemberCambodia_2C[-c(21,2443),]
#Re-attribute household members numbers when the household id is duplicated for different household members
Dum <- HouMemberCambodia_2C[duplicated(HouMemberCambodia_2C$pid),]
idDup <- unique(Dum$hhid_re1)
for (i in idDup){
  HouMemberCambodia_2C$p_no[HouMemberCambodia_2C$hhid_re1 == i] <- 1:sum(HouMemberCambodia_2C$hhid_re1 == i)
}
HouMemberCambodia_2C$pid <- paste(HouMemberCambodia_2C$hhid_re1,HouMemberCambodia_2C$p_no)
#Check again the duplicates: 
count_if("TRUE",duplicated(HouMemberCambodia_2C$pid))

## 4.3 Data cleaning for "ClowlandCambodia_2C"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(ClowlandCambodia_2C$pid))
#Check and remove the real duplicates through automated method
Dum <- ClowlandCambodia_2C[duplicated(ClowlandCambodia_2C$pid),]
idDup <- unique(Dum$hhid_re2)
DumReal <- ClowlandCambodia_2C[ClowlandCambodia_2C$hhid_re2 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_13v, DumReal$d2_132)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
Dupli$pid
ClowlandCambodia_2C <- ClowlandCambodia_2C[!rownames(ClowlandCambodia_2C) %in% Duplic,]
#Check again the duplicates:

count_if("TRUE",duplicated(ClowlandCambodia_2C$pid))

# # #b Outliers part 1
#Check amount of seeds according to Ky excel file

## 4.4 Data cleaning for "CuplandCambodia_2"

# # #a. Duplicates
#Check the number and id of duplicates
count_if("TRUE",duplicated(CuplandCambodia_2$pid))
#Check and remove the real duplicates through automated method
Dum <- CuplandCambodia_2[duplicated(CuplandCambodia_2$pid),]
idDup <- unique(Dum$hhid_re3)
DumReal <- CuplandCambodia_2[CuplandCambodia_2$hhid_re3 %in% idDup,]
DumReal$check <- paste(DumReal$pid, DumReal$d2_13v, DumReal$d2_132)
DumReal <- DumReal %>% relocate(check , .after = pid)
Dupli <- DumReal[duplicated(DumReal$check),]
Duplic <- rownames(Dupli)
CuplandCambodia_2 <- CuplandCambodia_2[!rownames(CuplandCambodia_2) %in% Duplic,]
#Check again the duplicates: 
count_if("TRUE",duplicated(CuplandCambodia_2$pid))

# Virer les NA
# Inclure les données des cultures/ménages ou trouver un moyen de faire le lien
# Corriger les cases contradictoires selon le rapport de Ky

HouseholdCambodia_2C <- copy_labels(HouseholdCambodia_2C, HouseholdCambodia)
HouMemberCambodia_2C <- copy_labels(HouMemberCambodia_2C, HouMemberCambodia)
ClowlandCambodia_2C <- copy_labels(ClowlandCambodia_2C, ClowlandCambodia)
CuplandCambodia_2 <- copy_labels(CuplandCambodia_2, CuplandCambodia)


###5. Data export


##3.3 Export of the database under dta format
saveRDS(HouseholdCambodia_2C, "HouseholdCambodia_2C.rds")
saveRDS(HouMemberCambodia_2C, "HouMemberCambodia_2C.rds")
saveRDS(ClowlandCambodia_2C, "ClowlandCambodia_2C.rds")
saveRDS(CuplandCambodia_2, "CuplandCambodia_2.rds")


#rstudioapi::writeRStudioPreference("data_viewer_max_columns", 30L)
