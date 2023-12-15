# ==============================================================================
# ================================ Description =================================
# ==============================================================================
# Author: Amirreza Sahebi
# Last modified: 12.01.2023
# Input:
# Output: 
# Purpose: 

# The input of this code is processed ARCOS data from DataProcessing1ARCOSnew.R
# The input has all demographic data at three levels: 1) dispenser 
# 2) distributor 3) manufacturer. The outputs are different data sets by



# ============================================================================ #
# ============================= Required libraries =========================== #
# ============================================================================ #
library(dplyr)
library(tidyr)
library(magrittr)
library(data.table)


# ============================================================================ #
# ================================= Functions ================================ #
# ============================================================================ #
CountyAgg <- function(data, stakeholder){
  summvars <- names(data)[which(grepl("Dosage|Weight|TransactionNo|MME" ,names(data)))]
  
  if (stakeholder == "Buyer"){
    summvars <- setdiff(summvars,names(data)[which(grepl("Seller" ,names(data)))])
    data <- data %>% group_by(across(names(data)[-which(grepl("Dosage|Weight|TransactionNo|MME|Drug|Seller|BusActBuyer" ,
                                                              names(data)))])) %>% summarise(across(summvars, ~ sum(.x, na.rm = TRUE)))
  } else {
    summvars <- setdiff(summvars,names(data)[which(grepl("Buyer" ,names(data)))])
    data <- data %>% group_by(across(names(data)[-which(grepl("Dosage|Weight|TransactionNo|MME|Drug|Buyer|BusActSeller" ,
                                                              names(data)))])) %>% summarise(across(summvars, ~ sum(.x, na.rm = TRUE)))
  }
  data <- as.data.frame(data)
}

StateAgg <- function(data, stakeholder){
  if (stakeholder == "Buyer"){
    summvars <- names(data)[which(grepl("Dosage|Weight|TransactionNo|MME" ,names(data)))]
    summvars <- setdiff(summvars,names(data)[which(grepl("Seller" ,names(data)))])
    timelocvars <- c("Year","Quarter","TimeID","StateBuyer")
    demvars <- names(data)[-which(grepl("Seller|County|BusAct|isBoarder|NeighborState|Drug" ,names(data)))]
    demvars <- setdiff(demvars,summvars)
    demvars <- setdiff(demvars,timelocvars)
    data <- data %>% group_by(TimeID,StateBuyer) %>% mutate(ID = cur_group_id())
  } else {
    summvars <- names(data)[which(grepl("Dosage|Weight|TransactionNo|MME" ,names(data)))]
    summvars <- setdiff(summvars,names(data)[which(grepl("Buyer" ,names(data)))])
    timelocvars <- c("Year","Quarter","TimeID","StateSeller")
    demvars <- names(data)[-which(grepl("Buyer|County|BusAct|isBoarder|NeighborState|Drug" ,names(data)))]
    demvars <- setdiff(demvars,summvars)
    demvars <- setdiff(demvars,timelocvars)
    data <- data %>% group_by(TimeID,StateSeller) %>% mutate(ID = cur_group_id())
  }
  data1 <- data %>% group_by(ID) %>% summarise(across(summvars, ~ sum(.x, na.rm = TRUE)))
  data2 <- data %>% select(c(timelocvars,demvars,"ID"))
  data2 <- distinct(data2,ID,.keep_all = T)
  data <- merge(data2,data1,by = c("ID"))
  rm(data2)
  rm(data1)
  data$ID <- NULL
  data <- data %>% select(c(timelocvars,summvars,demvars))
}


# ============================================================================ #
# ================================== Read Data =============================== #
# ============================================================================ #
pathread <- "G:/My Drive/North Carolina State University/Project - Opioid 3/Data Processed/"
pathwrite <- "G:/My Drive/North Carolina State University/Project - Opioid 3/Data Processed/"
# Dispenser data
Disp <- fread(paste0(pathread,"dfARCOSdispenser.csv"))
Disp <- as.data.frame(Disp)
# Manufacturer data
Man <- fread(paste0(pathread,"dfARCOSman.csv"))
Man <- as.data.frame(Man)


# ============================================================================ #
# ============================= Data Pre-Processing ========================== #
# ============================================================================ #
# Distributors to Dispensers
Dist2Disp <- Disp[which(Disp$BusActSeller %in% c("DISTRIBUTOR",
                                                 "CHEMPACK/SNS DISTRIBUTOR")),]
# Overall
fwrite(CountyAgg(Dist2Disp,"Buyer"),paste0(pathwrite,"Dist2DispBuyerCounty.csv"))
fwrite(StateAgg(Dist2Disp,"Buyer"),paste0(pathwrite,"Dist2DispBuyerState.csv"))
fwrite(CountyAgg(Dist2Disp,"Seller"),paste0(pathwrite,"Dist2DispSellerCounty.csv"))
fwrite(StateAgg(Dist2Disp,"Seller"),paste0(pathwrite,"Dist2DispSellerState.csv"))
# Drug based
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "HYDROCODONE"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerHydCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "HYDROCODONE"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerHydState.csv"))
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "HYDROCODONE"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerHydCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "HYDROCODONE"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerHydState.csv"))
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "OXYCODONE"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerOxyCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "OXYCODONE"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerOxyState.csv"))
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "OXYCODONE"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerOxyCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "OXYCODONE"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerOxyState.csv"))
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "FENTANYL"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerFentCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "FENTANYL"),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerFentState.csv"))
fwrite(CountyAgg(Dist2Disp[which(Dist2Disp$Drug == "FENTANYL"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerFentCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(Dist2Disp$Drug == "FENTANYL"),],"Seller"),
       paste0(pathwrite,"Dist2DispSellerFentState.csv"))
# Business activity based
fwrite(CountyAgg(Dist2Disp[which(grepl("PRACTITIONER", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerPractitionerCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(grepl("PRACTITIONER", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerPractitionerState.csv"))
fwrite(CountyAgg(Dist2Disp[which(grepl("RETAIL PHARMACY", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerRetailPharmacyCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(grepl("RETAIL PHARMACY", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerRetailPharmacyState.csv"))
fwrite(CountyAgg(Dist2Disp[which(grepl("CHAIN PHARMACY", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerChainPharmacyCounty.csv"))
fwrite(StateAgg(Dist2Disp[which(grepl("CHAIN PHARMACY", Dist2Disp$BusActBuyer)),],"Buyer"),
       paste0(pathwrite,"Dist2DispBuyerChainPharmacyState.csv"))

# Manufacturer to Distributors
Man2Dist <- Man[which(Man$BusActBuyer %in% c("DISTRIBUTOR",
                                               "CHEMPACK/SNS DISTRIBUTOR")),]
# Overall
fwrite(CountyAgg(Man2Dist,"Buyer"),paste0(pathwrite,"Man2DistBuyerCounty.csv"))
fwrite(StateAgg(Man2Dist,"Buyer"),paste0(pathwrite,"Man2DistBuyerState.csv"))
fwrite(CountyAgg(Man2Dist,"Seller"),paste0(pathwrite,"Man2DistSellerCounty.csv"))
fwrite(StateAgg(Man2Dist,"Seller"),paste0(pathwrite,"Man2DistSellerState.csv"))
# Drug based
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "HYDROCODONE"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerHydCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "HYDROCODONE"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerHydState.csv"))
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "HYDROCODONE"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerHydCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "HYDROCODONE"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerHydState.csv"))
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "FENTANYL"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerFentCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "FENTANYL"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerFentState.csv"))
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "FENTANYL"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerFentCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "FENTANYL"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerFentState.csv"))
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "OXYCODONE"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerOxyCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "OXYCODONE"),],"Buyer"),
       paste0(pathwrite,"Man2DistBuyerOxyState.csv"))
fwrite(CountyAgg(Man2Dist[which(Man2Dist$Drug == "OXYCODONE"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerOxyCounty.csv"))
fwrite(StateAgg(Man2Dist[which(Man2Dist$Drug == "OXYCODONE"),],"Seller"),
       paste0(pathwrite,"Man2DistSellerOxyState.csv"))
