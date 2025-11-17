# Adjust future stream temperature predictions (Siegel model) in reaches below dams for Willamette
# using empirical data from USGS gages, imputed to fill in temporal gaps
# A.H. Fullerton, March 2024

# Setup ----
library(nhdplusTools)
library(sf)
library(dplyr)
library(googledrive)
library(data.table)
library(fst)

source("code/functions.R")
options (scipen = 999)

# Load info about LCM reaches and associated COMIDs
comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")
LCM_Reaches <- sort(unique(comid.dat$LCM_Reach))

# Load COMIDs and HUC info for later sorting
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
COM_HUC$Huc10 <- substr(COM_HUC$Huc12, 1, 10)
huclist <- sort(unique(COM_HUC$Huc6))
huc10list <- sort(unique(COM_HUC$Huc10))
comids <- COM_HUC$COMID[COM_HUC$Huc6 == huclist[17]]; comids <- comids[!is.na(comids)]

# Load dam info
damnms <- c("BIG CLIFF", "FOSTER", "COUGAR", "DEXTER", "Middle_Willamette", "Upper_Willamette")
dams <- st_read("data/shapefiles", "PNW_DAMS")

# Load USGS gage data
gage_info <- read.csv("data/willamette_temperature_gage_info.csv")
gage_info <- gage_info[!is.na(gage_info$Keep),]
gage_data <- read.csv("data/imputed_data_rf_mod_1990.csv")[,-1]
gage_dat <- merge(gage_data, gage_info[gage_info$Keep == 1, c("Site_Numbe", "COMID")], by.x = "site_no", by.y = "Site_Numbe", all.y = T)
gage_dat$Date <- as.Date(gage_dat$Date)
gages <- unique(gage_dat$site_no)
gages <- gages [1:23] #eliminating Willamette @ Newberg and @ Portland because we don't have predictions for these areas

# Set parameters
climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
retro_downloaded <- T
gdrive_downloaded <- T
gcm_combined <- T
if(!exists("streamlines_loaded")) streamlines_loaded <- F
dam_affected <- F #this is the default - for above-dam reaches as well as mainstem Willamette reaches

# Adjust Siegel model predictions below dams based on imputed USGS observations in particular year -----

# Download retrospective predictions
# requires access! data are also available at the Riverscapes Consortium Data Exchange
if(retro_downloaded == F){
  fncDownloadRetro()
}

# Download GCM predictions
# requires access! data are also available at the Riverscapes Consortium Data Exchange
if(gdrive_downloaded == F){
  predictions_path <- "https://drive.google.com/drive/u/1/folders/1kxkPOUbBOVWtEOUhasPOUf7a0qsrkF8B" #data/predictions
  cc_folders <- googledrive::drive_ls(predictions_path)
  cc_folders <- cc_folders[order(cc_folders$name),]; cc_folders <- cc_folders[-10,]
  fncDownloadPreds(cc_folders)
  gdrive_downloaded <- T
}

# Combine GCM data into a wide format
if(gcm_combined == F){
  # Get future stream temperatures into a wide format
  ccdat <- fncGCMsWide()
  #ccdat <- fst::read_fst("data/170900_AllGCMs.fst")
  
  # Merge retro and cc predictions
  retro <- fst::read_fst("data/170900_retrospective.fst")
  retro <- retro[,c("COMID", "tim.date", "prd.stream_temp")]
  
  st_dat <- dplyr::left_join(retro, ccdat, by = c("COMID", "tim.date"))
  
  fst::write_fst(st_dat, "data/170900_AllST.fst")
  gcm_combined <- T
}

# Load NHDv2 streamlines
if(streamlines_loaded == F){
  streams <- fncLoadStreams()
  streamlines_loaded <- T
}

# Read in merged data
st_dat <- fst::read_fst("data/170900_AllST.fst")

# Loop years
alldat <- NULL
for(yy in seq(1990, 2021)){
  print(yy)

  tmp <- NULL
  for(g in 1:length(gages)){
    # Get basic info given USGS gage
    x <- fncGageID2COMID(gageid = gages[g])
    lcm_rch <- x[[1]]
    start_comid <- x[[2]]
    pop <- x[[3]]
    rch_cids <- comid.dat$COMID[comid.dat$LCM_Reach == lcm_rch]
    
    ## Identify relevant reaches and comids
    #lcm_rchs <- LCM_Reaches[grep(pop, LCM_Reaches)]
    #upcomids <- get_UT(streams, start_comid)
    #up_rchs <- unique(comid.dat$LCM_Reach[comid.dat$COMID %in% upcomids])
    #dncomids <- get_DM(streams, start_comid)
    #dn_rchs <- unique(comid.dat$LCM_Reach[comid.dat$COMID %in% dncomids])
    #all_rchs <- union(lcm_rchs, dn_rchs)
    #if(length(grep("Migration", lcm_rchs)) > 0) lcm_rchs <- lcm_rchs[-grep("Migration", lcm_rchs)]
    
    # Get NHDv2 reaches (attributed) downstream from start_comid 
    ds_nhd <- fncDS_NHD(start_comid)
    ds_nhd <- ds_nhd[ds_nhd$COMID %in% rch_cids,]
    
    # Merge ST and NHD data for reaches below start_comid
    st_ds <- fncMerge2NHD(st_dat, yy) #reads: st_dat, ds_nhd, us_nhd
    #plot(st_ds$DistDS, st_ds$pDA)
    
    # Merge in imputed empirical data for COMIDS below selected gage and within LCM reach
    st_ds_adj <- fncAdjbyGage() #reads: st_ds
    #plot(st_ds_adj$tim.date, st_ds_adj$ST_med, cex = 0.2)
    #points(st_ds_adj$tim.date, st_ds_adj$prd.stream_temp, cex = 0.2, col = 4)
    #points(st_ds_adj$tim.date, st_ds_adj$vAdj, cex = 0.2, col = 3)
    #points(st_ds_adj$tim.date, st_ds_adj$Value, cex = 0.2, col = 2)
    
    tmp <- rbind(tmp, st_ds_adj)
  }
  alldat <- rbind(alldat, tmp)
}
fst::write_fst(alldat, "data/alldat.fst", compress = 80)
#alldat <- fst::read_fst("data/alldat.fst")

# Isolate the unmodified data
comids2do <- setdiff(unique(st_dat$COMID), unique(alldat$COMID))
st_dat$RCID <- NA; st_dat$year <- lubridate::year(st_dat$tim.date)
st_dat$Pathlength <- NA; st_dat$WillPL_KM <- NA; st_dat$WillPL_Mi <- NA
st_dat$DistDS <- NA; st_dat$pDA <- NA; st_dat$river_mi <- NA
st_dat$site_no <- NA; st_dat$Value <- NA
st_dat$ST_med <- NA
st_dat$vAdj <- st_dat$prd.stream_temp; st_dat$pAdj <- 1
st_dat <- st_dat[st_dat$COMID %in% comids2do, colnames(alldat)]
gc()
st_dat$ST_med <- apply(st_dat[,colnames(st_dat) %in% climate_scenarios], 1, median, na.rm = T)
fst::write_fst(st_dat, "data/st_dat_wmedian.fst", compress = 80)

# Merge in all the unmodified data
alldat <- fst::read_fst("data/alldat.fst")
final_dat <- rbind(alldat, st_dat)
fst::write_fst(final_dat, "data/final_dat.fst", compress = 80)

