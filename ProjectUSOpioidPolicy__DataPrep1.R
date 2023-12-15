# ==============================================================================
# ================================ Description =================================
# ==============================================================================
# Author: Amirreza Sahebi
# Last modified: 12.01.2023
# Input:
# Output: 
# Purpose: 


# ============================================================================ #
# ============================= Required libraries =========================== #
# ============================================================================ #
library(data.table)
library(tidyr)
library(dplyr)

# ============================================================================ #
# ================================== Read Data =============================== #
# ============================================================================ #
pathread <- "G:/My Drive/North Carolina State University/Project - Opioid 1/All Datasets/"
pathwrite <- "G:/My Drive/North Carolina State University/Project - Opioid 3/Data Processed/"

# Demographic data
dfDemograp <- fread(paste0(pathread,"State Demographic/cps_state_control_quarter.csv"))
colnames(dfDemograp)[which(names(dfDemograp) == "STATEFIP")] <- "StateFIPS"
dfDemograp$StateFIPS <- as.character(dfDemograp$StateFIPS)
colnames(dfDemograp)[which(names(dfDemograp) == "year")] <- "Year"
colnames(dfDemograp)[which(names(dfDemograp) == "quarter")] <- "Quarter"

# State and County FIPS data
dfStateFIPS <- fread(paste0(pathread,"State Demographic/StateFIPS.csv"))
dfCountyFIPS <- fread(paste0(pathread,"State Demographic/data_county.csv"))
colnames(dfCountyFIPS) <- c("County","State","CountyFIPS")

# Boarder Counties data
dfCounty2BoarderDistance <-  fread(paste0(pathread,"State Demographic/cntydist.txt"))
dfCounty2BoarderDistance$V9 <- NULL
colnames(dfCounty2BoarderDistance)<- c("BoarderIdx", "St1St2Abb", "St1Boarder",
                                       "St2Boarder", "StateFIPS","CountyFIPS",
                                       "Dist","Loc")
dfCounty2BoarderDistance$CountyFIPS <-
  str_pad(dfCounty2BoarderDistance$CountyFIPS,3,pad = "0")
dfCounty2BoarderDistance$StateFIPS <-
  str_pad(dfCounty2BoarderDistance$StateFIPS,2,pad = "0")
dfCounty2BoarderDistance$St1Boarder <-
  substr(dfCounty2BoarderDistance$St1St2Abb,start = 1, stop = 2)
dfCounty2BoarderDistance$St2Boarder <-
  substr(dfCounty2BoarderDistance$St1St2Abb,start = 4, stop = 5)
dfCounty2BoarderDistance$CountyFIPS <-
  paste0(dfCounty2BoarderDistance$StateFIPS,
         dfCounty2BoarderDistance$CountyFIPS,sep="")
dfStateFIPS$StateFIPS <-
  str_pad(dfStateFIPS$StateFIPS,2,pad = "0")
dfCounty2BoarderDistance <- merge(dfCounty2BoarderDistance,dfStateFIPS,by=c("StateFIPS"))
for (ii in 1:nrow(dfCounty2BoarderDistance)){
  tmp <- dfCounty2BoarderDistance[ii,c("St1Boarder","St2Boarder","State")]
  tmp <- as.list(as.data.frame(t(tmp)))
  tmp <- tmp$V1
  dfCounty2BoarderDistance[ii,"StateBoarder"] <- setdiff(tmp[1:2],tmp[3])
}
dfCounty2BoarderDistance[,"NeighborSt"] <- ifelse(dfCounty2BoarderDistance$Dist <= 30,
                                                dfCounty2BoarderDistance$StateBoarder,"")

dfCountyNeighbors <- data.frame()
for (ii in unique(dfCounty2BoarderDistance$CountyFIPS)){
  index <- which(dfCounty2BoarderDistance$CountyFIPS == ii)
  tmp <- dfCounty2BoarderDistance[index,c("CountyFIPS","NeighborSt")]

  tmp2 <- unique(tmp$NeighborSt)
  tmp2 <- tmp2[tmp2 != ""]
  tmp[,"NeighborSt"] <- paste0(tmp2,collapse = "-")
  tmp <- unique(tmp)
  tmp[,"isBoarder"] <- 1
  if (tmp$NeighborSt== ""){
    tmp[,"isBoarder"] <- 0
  }
  dfCountyNeighbors <- rbind(dfCountyNeighbors,tmp)
  tmp <- NULL
}

# ARCOS data
dfARCOS <-  fread(paste0(pathread,"SLCG/ARCOS__SLCG__Monthly__Processed.csv"))

# Population data
dfStatePop <- fread(paste0(pathread,"State Demographic/StatePop.csv"))
colnames(dfStatePop)<- c("State","Year","Pop")
dfCountyPop <- fread(paste0(pathread,"State Demographic/CountyPop.csv"))
dfCountyPop <- dfCountyPop[,c("countyfips","BUYER_STATE","year","population")]
colnames(dfCountyPop) <- c("CountyFIPS","State","Year","Pop")

# Policy data
dflaws <- fread(paste0(pathread,"State Demographic/StateLaws.csv"), header  = T)
dflaws$V1 <- NULL
LawNames <- c("State","PDMPacs","PDMPreq","PrscLim","PillMill","GdSmrt",
              "NlxAcs","SttMedExp","ePDMP_Horwitz","ePDMP_PDAPS","MAPDMP",
              "PDMP_RAND","PDMP_Horwitz_RAND","OPPDMP_RAND","MAPDMP_RAND",
              "ePDMP_RAND")
colnames(dflaws) <- LawNames
dflaws[,"SttMedExp"] <- NULL
LawNames <- LawNames[LawNames != "SttMedExp"]
LawNames <- LawNames[LawNames != "State"]


# ============================================================================ #
# ================================ Data Cleaning ============================= #
# ============================================================================ #
# Remove non-related business activities
removedBusActs <- c("REVERSE DISTRIB","ANALYTICAL LAB",
                    "IMPORTER","IMPORTER (C I,II)" ,
                    "EXPORTER","CHEMICAL EXPORTER",
                    "RESEARCHER (I)","RESEARCHER (II-V)")
dfARCOS <- dfARCOS %>% filter(!BUYER_BUS_ACT %in% removedBusActs &
                              !REPORTER_BUS_ACT %in% removedBusActs)




# ============================================================================ #
# ============================= Data Pre-Processing ========================== #
# ============================================================================ #

# ================ #
# Demographic Data #
# ================ #
# Add State FIPS
dfDemograp <- merge(dfDemograp,dfStateFIPS,by = c("StateFIPS"))
dfDemograp <- dfDemograp %>% relocate(State, .before = StateFIPS)

# Population
dfCountyPop$CountyFIPS <- str_pad(dfCountyPop$CountyFIPS,5,pad = "0")
dfCountyPop <- merge(dfCountyPop,dfCountyNeighbors,by = c("CountyFIPS"))
dfCountyPop[,"PopInland"] <- ifelse(dfCountyPop$isBoarder == 0,dfCountyPop$Pop,0)
dfCountyPop[,"PopBoarder"] <- ifelse(dfCountyPop$isBoarder == 1,dfCountyPop$Pop,0)
dfStatePop2 <- dfCountyPop %>% group_by(State,Year) %>% summarise(PopInland = sum(PopInland),
                                                                  PopBoarder = sum(PopBoarder))
dfStatePop <- merge(dfStatePop,dfStatePop2,by = c("State","Year"),all=T)
dfStatePop$PopInland[which(dfStatePop$State %in% c("AK","HI"))] <- 
  dfStatePop$Pop[which(dfStatePop$State %in% c("AK","HI"))]
dfStatePop$PopBoarder[which(dfStatePop$State %in% c("AK","HI"))] <- 0
rm(dfStatePop2)

# ========== #
# ARCOS Data #
# ========== #
# Add quarter
for (ii in 1:12){
  if (ii %% 3 == 0){
    dfARCOS[which(dfARCOS$month == ii),"Quarter"] <- ii%/%3
  } else {
    dfARCOS[which(dfARCOS$month == ii),"Quarter"] <- (ii%/%3)+1
  }
}

# Add County FIPS
dfARCOS <- merge(dfARCOS,dfCountyFIPS,
                 by.x = c("BUYER_STATE","BUYER_COUNTY"),
                 by.y = c("State","County"))
colnames(dfARCOS)[which(names(dfARCOS) == "CountyFIPS")] <- "BUYER_COUNTYFIPS"
dfARCOS <- merge(dfARCOS,dfCountyFIPS,
                 by.x = c("REPORTER_STATE","REPORTER_COUNTY"),
                 by.y = c("State","County"))
colnames(dfARCOS)[which(names(dfARCOS) == "CountyFIPS")] <- "REPORTER_COUNTYFIPS"

# Filter and summarize over selected columns
dfARCOS <- dfARCOS %>% group_by(year,Quarter,BUYER_STATE,BUYER_COUNTY,BUYER_COUNTYFIPS,BUYER_BUS_ACT,
                                REPORTER_STATE,REPORTER_COUNTY,REPORTER_COUNTYFIPS,REPORTER_BUS_ACT,
                                DRUG_NAME) %>% summarise(across(names(dfARCOS)[which(grepl("Dosage|Weight|MME|Transaction" , names(dfARCOS)))]
                                                                , ~ sum(.x, na.rm = TRUE)))
colnames(dfARCOS) <- c("Year","Quarter","StateBuyer","CountyBuyer","CountyFIPSBuyer","BusActBuyer",
                       "StateSeller","CountySeller","CountyFIPSSeller","BusActSeller",
                       "Drug","Dosage","MME","TransactionNo","Weight")

# Add time id
dfARCOS <- dfARCOS %>% group_by(Year,Quarter) %>% mutate(TimeID = cur_group_id())

# Add boarder counties 
dfARCOS <- merge(dfARCOS,dfCountyNeighbors,
                 by.x = c("CountyFIPSBuyer"), by.y = c("CountyFIPS"))
colnames(dfARCOS)[which(names(dfARCOS) == "isBoarder")] <- "isBoarderBuyer"
colnames(dfARCOS)[which(names(dfARCOS) == "NeighborSt")] <- "NeighborStateBuyer"
dfARCOS <- merge(dfARCOS,dfCountyNeighbors,
                   by.x = c("CountyFIPSSeller"), by.y = c("CountyFIPS"))
colnames(dfARCOS)[which(names(dfARCOS) == "isBoarder")] <- "isBoarderSeller"
colnames(dfARCOS)[which(names(dfARCOS) == "NeighborSt")] <- "NeighborStateSeller"
for (ii in c("Dosage","Weight","MME","TransactionNo")){
  dfARCOS[,paste0(ii,"BoarderBuyer")] <- ifelse(dfARCOS$isBoarderBuyer == 1,
                                                  dfARCOS[,ii],0)
  dfARCOS[,paste0(ii,"BoarderSeller")] <- ifelse(dfARCOS$isBoarderSeller == 1,
                                                   dfARCOS[,ii],0)
  dfARCOS[,paste0(ii,"NonBoarderBuyer")] <- ifelse(dfARCOS$isBoarderBuyer == 0,
                                                  dfARCOS[,ii],0)
  dfARCOS[,paste0(ii,"NonBoarderSeller")] <- ifelse(dfARCOS$isBoarderSeller == 0,
                                                     dfARCOS[,ii],0)
  dfARCOS[,paste0(ii,"InState")] <- ifelse(dfARCOS$StateBuyer == dfARCOS$StateSeller,
                                             dfARCOS[,ii],0)
  dfARCOS[,paste0(ii,"OutState")] <- ifelse(dfARCOS$StateBuyer != dfARCOS$StateSeller,
                                             dfARCOS[,ii],0)
}

# Extract Dispenser, Distributor and Manufacturer Data 
BuyerBusAct <- unique(dfARCOS$BusActBuyer)
DispenserBusAct <- BuyerBusAct[grepl("^MLP.*", BuyerBusAct)]
DispenserBusAct <- cbind(DispenserBusAct,
                         BuyerBusAct[grepl("CLINIC",BuyerBusAct,fixed=T)])
DispenserBusAct <- cbind(DispenserBusAct,
                         BuyerBusAct[grepl("PRACTITIONER",BuyerBusAct,fixed=T)])
DispenserBusAct <- cbind(DispenserBusAct,
                         BuyerBusAct[grepl("MAINT",BuyerBusAct,fixed=T)])
DispenserBusAct <- cbind(DispenserBusAct,
                         BuyerBusAct[grepl("DETOX",BuyerBusAct,fixed=T)])
DispenserBusAct <- cbind(DispenserBusAct,
                         BuyerBusAct[grepl("PHARMACY",BuyerBusAct,fixed=T)])
DispenserBusAct <- cbind(DispenserBusAct,
                         c("AUTOMATED DISPENSING SYSTEM",
                           "CANINE HANDLER", "TEACHING INSTITUTION"))
DispenserBusAct <- unlist(as.list(DispenserBusAct))
DispenserBusAct <- unique(DispenserBusAct)
ManBusAct <- BuyerBusAct[grepl("MANUF",BuyerBusAct,fixed=T)]
DistBusAct <- BuyerBusAct[grepl("DISTRIB",BuyerBusAct,fixed=T)]
dfARCOSdispenser <- dfARCOS[which(dfARCOS$BusActBuyer %in% DispenserBusAct),]
dfARCOSdist <- dfARCOS[which(dfARCOS$BusActSeller %in% DistBusAct),]
dfARCOSman <- dfARCOS[which(dfARCOS$BusActSeller %in% ManBusAct),]
rm(dfARCOS)

# Add demographic
names = c("dfARCOSdispenser","dfARCOSdist","dfARCOSman")
dfARCOSdispenser <- merge(dfARCOSdispenser,dfStatePop,
                        by.x = c("Year","StateBuyer"),
                        by.y = c("Year","State"))
dfARCOSdispenser <- merge(dfARCOSdispenser,dfDemograp,
                        by.x = c("Year","Quarter","StateBuyer"),
                        by.y = c("Year","Quarter","State"))
dfARCOSdist <- merge(dfARCOSdist,dfStatePop,
                   by.x = c("Year","StateSeller"),
                   by.y = c("Year","State"))
dfARCOSdist <- merge(dfARCOSdist,dfDemograp,
                   by.x = c("Year","Quarter","StateSeller"),
                   by.y = c("Year","Quarter","State"))
dfARCOSman <- merge(dfARCOSman,dfStatePop,
                  by.x = c("Year","StateSeller"),
                  by.y = c("Year","State"))
dfARCOSman <- merge(dfARCOSman,dfDemograp,
                  by.x = c("Year","Quarter","StateSeller"),
                  by.y = c("Year","Quarter","State"))

# Add policy data
names = c("dfARCOSdispenser","dfARCOSdist","dfARCOSman")
for (ii in names){
  tmp = get(ii)
  for (law in LawNames){
    tmp[,law] <- 10000
    tmp[,paste0(law,"_tr")] <- 0
    tmp[,paste0(law,"_time")] <- -10000
    tmp[,paste0(law,'_anychange')] <- 0
    tmp[,paste0(law,'_istr')] <- 0
    
  }
  assign(ii,tmp)
  tmp = data.frame()
}
for (ii in names){
  tmp = get(ii)
  if (ii == "dfARCOSdispenser"){
    StateNames <- unique(tmp$StateBuyer)
  } else {
    StateNames <- unique(tmp$StateSeller)
  }
  for (state in StateNames){
    for (law in LawNames){
      lawyear <- as.numeric(substr(dflaws[which(dflaws$State == state),get(law)],1,4))
      if (!is.na(lawyear)){
        if (lawyear > 2012){
          lawyear <- NA
        }
      }
      lawMonth <- as.numeric(substr(dflaws[which(dflaws$State == state),get(law)],6,7))
      if (!is.na(lawyear)){
        if (lawMonth %% 3 == 0){
          lawQuarter <- lawMonth %/% 3
        } else {
          lawQuarter <- (lawMonth %/% 3) + 1
        }
      }
      if (!is.na(lawyear)){
        if (lawyear < 2006){
          lawtime = (lawyear-2006)*4 - lawQuarter
        }
        else{
          lawtime = (lawyear-2006)*4 + lawQuarter
        }
      }
      if (ii == "dfARCOSdispenser"){
        index <-  which(tmp$StateBuyer == state)
      } else {
        index <-  which(tmp$StateSeller == state)
      }
      if (!is.na(lawyear)){
        tmp[index,law] <- lawtime
        if (lawyear < 2006) {
          tmp[index,paste0(law,'_tr')] <- 1
        } else {
          tmp[index,paste0(law,"_tr")] <- ifelse(tmp[index,"TimeID"] <= lawtime, 0, 1)

        }
        tmp[index,paste0(law,'_anychange')] <- 
          ifelse(tmp[index,"TimeID"] == lawtime, 1, 0)
        tmp[index,paste0(law,'_time')] <- 
          tmp[index,"TimeID"] - lawtime
        tmp[index,paste0(law,'_istr')] <- 1
       }
    }
  }
  assign(ii,tmp)
  fwrite(get(ii),paste0(pathwrite,ii,".csv"))
  rm(tmp)
}





