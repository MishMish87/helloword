# This script is made by: Michele Rocca (FAO-ESS)
# Created: 27th July 2015
# last update: 28th july 2015
# The goal of this code is to reshape and manipulate rawdata from
# Household survey of Afghanistan 2007_08. The final dataframe will have the
# shape of input necessary to implement the softaware ADePT. The data will
# be integrated from previous manipulation made to generate consistent 
# statistics on Food Security analysis.

# Section 0: Upload data and packages --------------------------------

library(foreign)
library(doBy)
library(plyr)
library(Hmisc)
library(gtools)
library(readr)
# it is jus a try

setwd("T:/Team_working_folder/D/TEAM_D/2.FOOD_SECURITY/ADePT COUNTRIES/FINAL OUTPUTS/Afghanistan_2007_08/")

HHinput.df <- read.spss("hh.sav",use.value.labels= T, to.data.frame= T)

HHinput.df <- HHinput.df[,c("hh_no","region","urb_rur","hh_size","hh_wgt","thh_inc")]
names(HHinput.df)[names(HHinput.df) == "hh_no"] <- "hhid"
HHinput.df <- HHinput.df[!HHinput.df$thh_inc==0,]
HHinput.df <- HHinput.df[!is.na(HHinput.df$thh_inc),]

##
HHend <- length(HHinput.df) + 1
##

setwd("T:/Team_working_folder/D/TEAM_D/1.RESTRICTED_AREA/DATA/MICRODATA_ARCHIVING/1. Processed_Surveys/Afghanistan_2007_08_NRVA/1.raw data/")

TransferRaw <- read.dta("S8.dta")
FoodCashRaw <- read.dta("S10.dta")

# Section 1: Sources of HH income -----------------------------------------

# This section contain the amounts of transfers for social insurance,
# non public transfers and international remittances.

OldAgepension.df <- subset(TransferRaw,q_8_1_1 == "Pension")
OldAgepension.df$OldAgePens <- rowSums(OldAgepension.df[,c("q_8_1_2",
                                       "q_8_1_3","q_8_1_4","q_8_1_5","q_8_1_6")],na.rm = T)
OldAgepension.df <- OldAgepension.df[,c("hhid","OldAgePens")]

DomRem.df <- subset(TransferRaw,q_8_1_1 == "Remittances from seasonal migrants")
DomRem.df$DomRem <- rowSums(DomRem.df[,c("q_8_1_2","q_8_1_3","q_8_1_4",
                                         "q_8_1_5","q_8_1_6")],na.rm = T)
DomRem.df <- DomRem.df[,c("hhid","DomRem")]

IntRem.df <- subset(TransferRaw,q_8_1_1 == "Remittances from family members living permanently away")
IntRem.df$IntRem <- rowSums(IntRem.df[,c("q_8_1_2","q_8_1_3","q_8_1_4",
                                         "q_8_1_5","q_8_1_6")],na.rm = T)
IntRem.df <- IntRem.df[,c("hhid","IntRem")]

OthSocAss.df <- subset(TransferRaw,q_8_1_1 == "Other Government benefits")
# This type of transfers contains also the amount received. Nevertheless,
# base on the fact the other two type of transfers cannot permit to determine
# the amount recieved during the entire year, we will consider just the partecipation.
# last but not the least, the numerosity is too low to consider this transfer
# as the entire representation of social assistance in the country.

OthSocAss.df$OthSocAss <- 1
OthSocAss.df <- OthSocAss.df[,c("hhid","OthSocAss")]

# Section 2: Social assistance,cash Transfers----------------------------------------

CashTr.df <- subset(FoodCashRaw,q_10_1 == "Yes",select = c("hhid","q_10_1"))
# The amount of cash-work present a discrepancy between the question of partecipation (year base)
# and the salary rate (daily base). Without the frequency of this  transfers during the
# entire year we cannot determine how much the actually received during the year.
# this transfer will be considered only partecipatory like the others in social assistance.
CashTr.df$CashTr <- 1
CashTr.df <- CashTr.df[,c("hhid","CashTr")]

# Section 3: Social assistance,food aid -----------------------------------

FoodAid.df <- subset(FoodCashRaw,q_10_5 == "Yes",select = c("hhid","q_10_5"))
FoodAid.df$FoodAid <- 1
FoodAid.df <- FoodAid.df[,c("hhid","FoodAid")]

# Section 4: collapse and outliers ----------------------------------------

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/R Standard Scripts/")
source("OutliersMADsp.r")
source("imputeout.r")

## Old Age Pension 

OldAgepension.df <- summaryBy(OldAgePens ~ hhid,data = OldAgepension.df,FUN=sum, keep.names = T)
OldAgepension.df <- OldAgepension.df[!OldAgepension.df$OldAgePens==0,]
OldAgepension.df <- merge(HHinput.df,OldAgepension.df, by = "hhid",all.y = T)
OldAgepension.df <- orderBy(~ region, data = OldAgepension.df)
# Not enough observations, i will do at national level
OldAgepension.df$OldAgePensIMP <- imputeout(OldAgepension.df$OldAgePens)
OldAgepension.df <- OldAgepension.df[,c("hhid","OldAgePens","OldAgePensIMP")]

HHinput.df <- merge(HHinput.df,OldAgepension.df,all.x = T)

# Domestic Remittances

DomRem.df <- summaryBy(DomRem ~ hhid,data = DomRem.df,FUN=sum, keep.names = T)
DomRem.df <- merge(HHinput.df,DomRem.df, by = "hhid",all.y = T)
DomRem.df <- orderBy(~ region, data = DomRem.df )
Tab <- dlply(DomRem.df, .(region),.fun =function(s) OutliersMADsp(s$DomRem))
Tab2 <- unlist(Tab)
DomRem.df$DomRemIMP<- Tab2
DomRem.df <- DomRem.df[,c("hhid","DomRemIMP","DomRem")]
HHinput.df <- merge(HHinput.df,DomRem.df,all.x = T)
rm(Tab)
rm(Tab2)

# International Remittances

IntRem.df <- summaryBy(IntRem ~ hhid,data = IntRem.df,FUN=sum, keep.names = T)
IntRem.df <- merge(HHinput.df,IntRem.df, by = "hhid",all.y = T)
IntRem.df <- orderBy(~ region, data = IntRem.df )
Tab <- dlply(IntRem.df, .(region),.fun =function(s) OutliersMADsp(s$IntRem))
Tab2 <- unlist(Tab)
IntRem.df$IntRemIMP<- Tab2
IntRem.df <- IntRem.df[,c("hhid","IntRemIMP","IntRem")]
HHinput.df <- merge(HHinput.df,IntRem.df,all.x = T)
rm(Tab)
rm(Tab2)

# Section 5: merging dataframes -------------------------------------------

# exchange rate in international costant $ PPP
# mean between 2005 and 2006

Temp <- read.dta("PPPcostant.dta")
Exchange <- subset(Temp, Country_Name == "Afghanistan",select = c("2007","2008"))
Exchange <- as.numeric(Exchange)
Exchange <- mean(Exchange)

HHinput.df <- mutate(HHinput.df, thh_inc = thh_inc/Exchange)
HHinput.df[,HHend:length(HHinput.df)] <- HHinput.df[,HHend:length(HHinput.df)]/365
HHinput.df[,HHend:length(HHinput.df)] <- HHinput.df[,HHend:length(HHinput.df)]/Exchange 

# Here we can merge the partecipatory variable of social assistance
HHinput.df <- merge(HHinput.df,OthSocAss.df,all.x = T)
HHinput.df <- merge(HHinput.df,FoodAid.df,all.x = T)
HHinput.df <- merge(HHinput.df,CashTr.df,all.x = T)

# Section 6: final input  --------------------------------------------------

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/Afghanistan_2007_08/")
write.dta(HHinput.df,"HH_SP_Afghanistan_07_08.dta", version = 12)
rm(list=ls())

# Section 7 : Cross Tabulation --------------------------------------------

# The discrepancy with ASPIRE remittances are due to the absence of
# monetary values for remittances sections.

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/R Standard Scripts/")

Temp <- read.dta("PPPcostant.dta")
Exchange <- subset(Temp, Country_Name == "Afghanistan",select = c("2007","2008"))
Exchange <- as.numeric(Exchange)
Exchange <- mean(Exchange)

setwd("T:/Team_working_folder/D/TEAM_D/2.FOOD_SECURITY/ADePT COUNTRIES/FINAL OUTPUTS/Afghanistan_2007_08/")

HHinput.df <- read.spss("hh.sav",use.value.labels= T, to.data.frame= T)
names(HHinput.df)[names(HHinput.df) == "hh_no"] <- "hhid"
HHinput.df <- mutate(HHinput.df, thh_cexp = thh_inc/Exchange)
HHinput.df <- HHinput.df[!HHinput.df$thh_inc==0,]
HHinput.df <- HHinput.df[!is.na(HHinput.df$thh_inc),]

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/Afghanistan_2007_08/")

input <- read.dta("HH_SP_Afghanistan_07_08.dta")

input$TotBenefit <- rowSums(input[,c("DomRemIMP","IntRemIMP","OldAgePensIMP")],na.rm = T)
input$SocIns <- input$OldAgePensIMP
input$SocIns[is.na(input$SocIns)] <- 0
input$InRemit <- input$IntRemIMP
input$InRemit[is.na(input$InRemit)] <- 0
input$NonPubTr <- input$DomRemIMP
input$NonPubTr[is.na(input$NonPubTr)] <- 0

input$SocialAs <- rowSums(input[,c("OthSocAss","FoodAid","CashTr")],na.rm = T)
input$TotBenefit <- ifelse(input$TotBenefit==0,NA,input$TotBenefit)

input$NTotBene <-  quantcut( input$TotBenefit, seq(0,1,by=1/3), na.rm = TRUE)
input$NTotBene <- as.numeric(input$NTotBene)
input$Quint <-  quantcut(input$thh_inc, seq(0,1,by=0.2), na.rm = TRUE)
input$Quint <- as.numeric(input$Quint)

setwd("T:/Team_working_folder/D/TEAM_D/2.FOOD_SECURITY/ADePT COUNTRIES/FINAL OUTPUTS/Afghanistan_2007_08/")

intermediate <- read.table("household_aggregated.dat",header= T,sep= ",")
names(intermediate)[names(intermediate)== "hh_no"] <- "hhid"

intermediate <- intermediate[!duplicated(intermediate$hhid),]
intermediate <- intermediate[,c("hhid","X_HHGENDER","X_PERCAPDAY_TOTAL_KCAL","X_HHSIZE_CAT",
                                "X_PERCAPDAY_TOTAL_ANIMAL_PRTN","X_PERCAPDAY_TOTAL_MV","X_PCE")]
# We have 4 missing values for geneder, base on the fact that 98% of 
# HHheaded are men, i can substitute the missing with 1 (Male HHheaded)
intermediate$X_HHGENDER[is.na(intermediate$X_HHGENDER)] <- 1

names(intermediate)[names(intermediate)== "X_PERCAPDAY_TOTAL_KCAL"] <- "Kcal"
names(intermediate)[names(intermediate)== "X_PERCAPDAY_TOTAL_ANIMAL_PRTN"] <- "Animal_Prt"
intermediate$Animal_Prt <-intermediate$Animal_Prt*4
intermediate$Sh_An_Prt <- (intermediate$Animal_Prt/intermediate$Kcal)*100

# This function has to revised in order to know which is the correspondet value
# for every level of education
intermediate$X_HHSIZE_CAT <- ifelse(intermediate$X_HHSIZE_CAT==4,1,intermediate$X_HHSIZE_CAT)

names(intermediate)[names(intermediate)=="X_PERCAPDAY_TOTAL_MV"] <- "mv"
intermediate$engel <- intermediate$mv/intermediate$X_PCE


input <- merge(input,intermediate, by = "hhid",all.x = T)

input$SocIns <- ifelse(input$SocIns > 0, 1,2)
input$InRemit <- ifelse(input$InRemit > 0,1,2)
input$SocialAs <- ifelse(input$SocialAs == 0,2,1)
input$NonPubTr <- ifelse(input$NonPubTr > 0,1,2)

input <- input[,c("DomRemIMP","hhid","IntRemIMP","NTotBene",
                  "InRemit","SocIns","OldAgePensIMP","TotBenefit",
                  "X_HHGENDER","SocialAs","CashTr","OthSocAss","FoodAid","NonPubTr","Kcal",
                  "Sh_An_Prt","Quint","engel","X_HHSIZE_CAT")]

HHinput.df <- merge(HHinput.df,input, by = "hhid",all.x = T)
HHinput.df <- mutate(HHinput.df, ExpFact = hh_size * hh_wgt)

HHinput.df$IncTerc <-  quantcut(HHinput.df$thh_inc, seq(0,1,by=1/3), na.rm = TRUE)
HHinput.df$IncTerc <- as.numeric(HHinput.df$IncTerc)
HHinput.df$NTotBene[is.na(HHinput.df$NTotBene)] <- 0

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/Final script and input files")
write.dta(HHinput.df,"DbSPAfghanistan.dta",version = 11)
