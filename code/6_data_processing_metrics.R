# Get processed stream temperature data prepped for manuscript figures and RShiny app
# A.H. Fullerton, March 2024

# Setup ----
library(dplyr)
library(sf)
library(xts)
library(nhdplusTools)

source("code/functions.R")

# Load streams shapefiles and attributed LCM reaches, fish distributions etc. ----
comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")
LCM_Reaches <- sort(unique(comid.dat$LCM_Reach))
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
huclist <- sort(unique(COM_HUC$Huc6))
comids <- COM_HUC$COMID[COM_HUC$Huc6 == huclist[17]]; comids <- comids[!is.na(comids)]
ms_reaches <- c("Falls2Mouth", "San2Falls_DS", "San2Falls_US", "Mck2San_DS", "Mck2San_US", "Mfk2Mck")
comid.dat$LCM_Reach[comid.dat$LCM_Reach %in% ms_reaches] <- paste0("Mainstem_", comid.dat$LCM_Reach[comid.dat$LCM_Reach %in% ms_reaches])
comid.dat <- merge(comid.dat, COM_HUC, by = "COMID", all.x = T)
comid.dat <- comid.dat[, c("COMID", "GNIS_NAME", "LCM_Reach", "Huc6")]
write.csv(comid.dat, file = "data/comid.dat.csv", row.names = F)

wil_streams <- sf::st_zm(sf::read_sf("data/shapefiles/NHDv2_Willamette_SOgt2.shp"))
wil_ms <- sf::st_zm(wil_streams[wil_streams$GNIS_NAME == "Willamette River",])
wil_ms <- merge(wil_ms, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID", all.x = T)
anad_streams <- sf::st_zm(sf::read_sf("data/shapefiles/NHDv2_Willamette_LCM.shp")) # has LCM reaches and StreamNet fish distributions attributed
nsan_streams <- anad_streams[anad_streams$LCM_Reach %in% c("Falls2Mouth", "San2Falls", anad_streams$LCM_Reach[grep("North Santiam", anad_streams$LCM_Reach)]),]
nsan_streams <- merge(nsan_streams[,!colnames(nsan_streams) %in% "LCM_Reach"], comid.dat[, c("COMID", "LCM_Reach")], by = "COMID", all.x = T)

plot(sf::st_geometry(wil_streams), col = "gray70")
plot(sf::st_geometry(anad_streams), add = T)
plot(sf::st_geometry(wil_ms), col = 3, add = T)
plot(sf::st_geometry(nsan_streams), col = 4, add = T)
plot(sf::st_geometry(nsan_streams[nsan_streams$CH_SP_RE == 1,]), col = 2, add = T)

chinook.enroute <- unique(wil_ms$COMID[wil_ms$LCM_Reach %in% c("Mainstem_Falls2Mouth", "Mainstem_San2Falls_DS", "Mainstem_San2Falls_US")]) #enroute
chinook.spawnrear <- unique(nsan_streams$COMID[!is.na(nsan_streams$COMID) & nsan_streams$CH_SP_RE %in% 1]) #spawners, incubat
chinook.rearmigr <- unique(nsan_streams$COMID[!is.na(nsan_streams$COMID) & nsan_streams$CH_MI_RE %in% 1]) #outmigr1, 2, 3
chinook.rearmigr <- c(chinook.rearmigr, chinook.enroute)
sthead.enroute <- chinook.enroute
sthead.spawnrear <- unique(nsan_streams$COMID[!is.na(nsan_streams$COMID) & nsan_streams$SH_SP_RE %in% 1])
sthead.rearmigr <- unique(nsan_streams$COMID[!is.na(nsan_streams$COMID) & nsan_streams$SH_RE_MI %in% 1])
sthead.rearmigr <- c(sthead.rearmigr, sthead.enroute)

plot(sf::st_geometry(wil_streams), col = "gray70")
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% chinook.enroute,]), col = 4, lwd = 3, add = T)
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% chinook.spawnrear,]), col = 5, lwd = 3, add = T)
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% chinook.rearmigr,]), col = 3, lwd = 1.5, add = T)

plot(sf::st_geometry(wil_streams), col = "gray70")
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% sthead.enroute,]), col = 4, lwd = 3, add = T)
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% sthead.spawnrear,]), col = 5, lwd = 3, add = T)
plot(sf::st_geometry(wil_streams[wil_streams$COMID %in% sthead.rearmigr,]), col = 3, lwd = 1.5, add = T)

nsan_huc <- sf::st_zm(sf::read_sf("data/shapefiles/north-santiam_WBD.shp"))
dams <- sf::st_zm(sf::read_sf("data/shapefiles/PNW_Dams.shp"))
nsan_nhd <- sf::st_zm(sf::read_sf("data/shapefiles/north-santiam_streams.shp"))

save(wil_streams, anad_streams, wil_ms, nsan_streams, nsan_huc, nsan_nhd, dams,
     chinook.enroute, chinook.spawnrear, chinook.rearmigr, sthead.enroute, sthead.rearmigr, sthead.spawnrear, 
     file = "data/nsan_shapefiles.RData")


# RETRO: time series of min/median/max and just medians ----
retro.dat <- fst::read_fst("data/data_1990-2021.fst")
retro.dat <- dplyr::left_join(retro.dat, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID")
retro.dat <- retro.dat[, c("COMID", "tim.date", "prd.stream_temp", "year", "Pathlength", "WillPL_KM", "WillPL_Mi", "DistDS", "pDA", "river_mi", "site_no", "Value", "vAdj", "prd.pAdj", "LCM_Reach")]
retro.dat <- fst::write_fst(retro.dat, "data/data_1990-2021.fst")

pop <- "North Santiam"; pop2 <- gsub(" ", "", pop)

for(yy in 1990:2021){
  dat <- retro.dat[retro.dat$year == yy, c("tim.date", "vAdj", "LCM_Reach")]
  dat <- dat[c(grep("North Santiam", dat$LCM_Reach), grep("Mainstem", dat$LCM_Reach)),]
  dat <- dat[!dat$LCM_Reach %in% c("Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "North Santiam Migration"),]
  dat <- dat %>% 
    group_by(LCM_Reach, tim.date) %>%
    summarise("Median" = median(vAdj, na.rm = T), "Min" = min(vAdj, na.rm = T), "Max" = max(vAdj, na.rm = T))
  dat.mn <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Median")], names_from = "LCM_Reach", values_from = "Median", names_prefix = "Median.")
  dat.mi <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Min")], names_from = "LCM_Reach", values_from = "Min", names_prefix = "Min.")
  dat.mx <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Max")], names_from = "LCM_Reach", values_from = "Max", names_prefix = "Max.")
  dat2 <- cbind(dat.mn, dat.mi[,-1], dat.mx[,-1])
  xdt <- xts::as.xts(dat2)
  assign(paste0("dat.", pop, ".", yy), xdt)
  rm(dat, dat.mn, dat.mi, dat.mx, dat2, xdt)
}
names.list <- paste0("dat.", pop, ".", 1990:2021)
data.list <- lapply(names.list, function(x) get(x))
names(data.list) <- names.list
assign(paste0(pop2, ".retro.data.list"), data.list)
save(list = c(paste0(pop2, ".retro.data.list")), file = paste0("data/", pop2, ".retro.data.list.RData"))
rm(list = ls(pattern = paste0("dat.", pop, "."))); rm(data.list)

dat <- retro.dat[, c("tim.date", "vAdj", "LCM_Reach")]
dat <- dat[c(grep(pop, dat$LCM_Reach), grep("Mainstem", dat$LCM_Reach)),]
dat <- dat[!dat$LCM_Reach %in% c("Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "North Santiam Migration"),]
dat <- dat %>%
  group_by(LCM_Reach, tim.date) %>%
  summarise("Median" = median(vAdj, na.rm = T))
if(any(is.na(dat$LCM_Reach))) dat <- dat[!is.na(dat$LCM_Reach),]
dat <- tidyr::pivot_wider(dat, names_from = "LCM_Reach", values_from = "Median")
assign("NorthSantiam.retro.medians", dat)
save(NorthSantiam.retro.medians, file = paste0("data/", pop2, ".retro.medians.RData"))


# CLIMATE: time series of min/median/max and just medians ----
climate_scenarios <- c("ST_med", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
cc.dat <- fst::read_fst("data/cc-pAdj_cmb.fst")

pop <- "North Santiam"; pop2 <- gsub(" ", "", pop)

for(climate_scenario in climate_scenarios){
  cs <- gsub("-", "_", climate_scenario)
  cc.dat$vAdj <- cc.dat[,climate_scenario] * cc.dat[, paste0("pAdj.", climate_scenario)]
  for(yy in 1950:2099){
    dat <- cc.dat[cc.dat$year == yy, c("tim.date", "vAdj", "LCM_Reach")]
    dat <- dat[c(grep(pop, dat$LCM_Reach), grep("Mainstem", dat$LCM_Reach)),]
    dat <- dat[!dat$LCM_Reach %in% c("Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "North Santiam Migration"),]
    dat <- dat %>% 
      group_by(LCM_Reach, tim.date) %>%
      summarise("Median" = median(vAdj, na.rm = T), "Min" = min(vAdj, na.rm = T), "Max" = max(vAdj, na.rm = T))
    dat.mn <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Median")], names_from = "LCM_Reach", values_from = "Median", names_prefix = "Median.")
    dat.mi <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Min")], names_from = "LCM_Reach", values_from = "Min", names_prefix = "Min.")
    dat.mx <- tidyr::pivot_wider(dat[, c("LCM_Reach", "tim.date", "Max")], names_from = "LCM_Reach", values_from = "Max", names_prefix = "Max.")
    dat2 <- cbind(dat.mn, dat.mi[,-1], dat.mx[,-1])
    xdt <- xts::as.xts(dat2)
    assign(paste0("dat.", pop, ".", yy), xdt)
    rm(dat, dat.mn, dat.mi, dat.mx, dat2, xdt)
  }
  names.list <- paste0("dat.", pop, ".", 1950:2099)
  data.list <- lapply(names.list, function(x) get(x))
  names(data.list) <- names.list
  assign(paste0(pop2, ".", cs, ".data.list"), data.list)
  save(list = c(paste0(pop2, ".", cs, ".data.list")), file = paste0("data/", pop2, ".", climate_scenario, ".data.list.RData"))
  rm(list = ls(pattern = paste0("dat.", pop, "."))); rm(data.list)
}

for(climate_scenario in climate_scenarios){
  cs <- gsub("-", "_", climate_scenario)
  cc.dat$vAdj <- cc.dat[,climate_scenario] * cc.dat[, paste0("pAdj.", climate_scenario)]
  dat <- cc.dat[, c("tim.date", "vAdj", "LCM_Reach")]
  dat <- dat[c(grep("North Santiam", dat$LCM_Reach), grep("Mainstem", dat$LCM_Reach)),]
  dat <- dat[!dat$LCM_Reach %in% c("Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "North Santiam Migration"),]
  dat <- dat %>%
    group_by(LCM_Reach, tim.date) %>%
    summarise("Median" = median(vAdj, na.rm = T))
  dat <- tidyr::pivot_wider(dat, names_from = "LCM_Reach", values_from = "Median")
  if("NA" %in% colnames(dat)) dat <- dat[,!colnames(dat) %in% "NA"]
  assign(paste0(pop2, ".", cs, ".medians"), dat)
  save(list = c(paste0(pop2, ".", cs, ".medians")), file = paste0("data/", pop2, ".", climate_scenario, ".medians.RData"))
  rm(list = ls(pattern = paste0(pop2, ".")))
}






# RETRO: thermal metrics ----

load("data/nsan_shapefiles.RData")
retro.dat <- fst::read_fst("data/data_1990-2021.fst")
subwat <- "north-santiam"
species <- "Omykiss"
ws <- fncSetWatershed(subwat)
pop <- ws[[1]]; pop2 <- gsub(" ", "", pop)
rchs <- ws[[2]] 
subwat_dams <- ws[[3]]
year.range <- c(1990, 2021)

# Process
met.dat <- NULL
life.stages <- c("enroute", "prespawn", "incubat", "outmigr1")
#life.stages <- as.character(lubridate::month(1:12, label = T))
for(life.stage in life.stages){
  for(yy in year.range[1]:year.range[2]){
    
    sls <- fncSetLifestage(species, life.stage, yy, retro.dat, rchs)
    stdat <- sls$stdat; endat <- sls$endat; th <- sls$th
    td <- sls$thedata[, c("LCM_Reach", "tim.date", "COMID", "vAdj")]
    if(any(is.na(td$LCM_Reach))) td <- td[!is.na(td$LCM_Reach),]
    
    thecol <- "vAdj"
    if(nrow(td) > 0){ #if there are data
      if(!all(is.na(td[,thecol]))){
        #for(thresh in c(th, th + 2)){
        thresh <- th
        
        # high temp frequency: proportion of days with temps > threshold
        met1 <- lapply(X = unique(td$COMID), FUN = fnc_pDays.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # high temp duration: longest consecutive stretch with temps > threshold
        met2 <- lapply(X = unique(td$COMID), FUN = fnc_Cum.Days.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # timing, first week too high
        met3 <- lapply(X = unique(td$COMID), FUN = fnc_1stWk.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # duration within suitable thermal range
        met4 <- lapply(X = unique(td$COMID), FUN = fnc_Days.in.Range, frame = td, start.date = stdat, end.date = endat, XX = thresh, YY = 4, st.col = thecol, show.na = F)
        # cumulative exposure, ie degree-days
        met5 <- lapply(X = unique(td$COMID), FUN = fnc_Cum.Exposure, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        # variability
        met6 <- lapply(X = unique(td$COMID), FUN = fnc_Variance, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        if(any(is.na(met6))){idx <- which(is.na(met6));for(i in idx){met6[[i]] <- rep(NA, 5)}}
        met6 <- t(abind::abind(met6, along = 2))
        # weekly min/median/max
        met7 <- lapply(X = unique(td$COMID), FUN = fnc_Weekly, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        if(any(is.na(met7))){idx <- which(is.na(met7));for(i in idx){met7[[i]] <- rep(NA, 6)}}
        met7 <- t(abind::abind(met7, along = 2))
        
        dat <- data.frame("COMID" = unique(td$COMID), "pExc" = unlist(met1), "durExc" = unlist(met2), "first.week" = unlist(met3), "daysSuitable" = unlist(met4),
                          "cum.exp" = unlist(met5), met6, met7,
                          "Population" = pop, "Life.stage" = life.stage, "Start" = stdat, "End" = endat, "Thresh" = thresh, "year" = yy)
        met.dat <- rbind(met.dat, dat)
        #}
      } #end data check 2
    } #end data check 1
  } #end year range
}# end life.stage

met.dat$first.week <- as.Date(met.dat$first.week)
d <- lubridate::day(met.dat$first.week)
m <- lubridate::month(met.dat$first.week)
met.dat$first.week <- as.Date(paste0("2000-", m, "-", d))
met.dat <- met.dat[!is.na(met.dat$AWA),]
met.dat <- dplyr::left_join(met.dat, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID")
data.table::fwrite(met.dat, paste0("data/thermal_metrics_", subwat, "_", species, ".csv"))


# CLIMATE: thermal metrics ----
# use median across GCMs, adjusted below dams
ccd <- fst::read_fst("data/cc-pAdj_cmb.fst")
ccd$vAdj <- ccd$pAdj.ST_med * ccd$ST_med
vaa <- read.csv("data/shapefiles/PlusFlowlineVAA.csv")
ccd <- dplyr::left_join(ccd, vaa[, c("ComID", "Pathlength")], by = c("COMID" = "ComID"))
fst::write_fst(ccd, "data/cc-pAdj_cmb_plus.fst")

subwat <- "north-santiam"
species <- "Omykiss"
ws <- fncSetWatershed(subwat)
pop <- ws[[1]]; pop2 <- gsub(" ", "", pop)
rchs <- ws[[2]] 
subwat_dams <- ws[[3]]
year.range <- c(1950, 2099)

# Process
met.dat <- NULL
life.stages <- c("enroute", "prespawn", "incubat", "outmigr1")
#life.stages <- as.character(lubridate::month(1:12, label = T))
for(life.stage in life.stages){
  for(yy in year.range[1]:year.range[2]){
    
    sls <- fncSetLifestage(species, life.stage, yy, ccd, rchs) #ccd, not retro.dat
    stdat <- sls$stdat; endat <- sls$endat; th <- sls$th
    td <- sls$thedata[, c("LCM_Reach", "tim.date", "COMID", "vAdj")]
    if(any(is.na(td$LCM_Reach))) td <- td[!is.na(td$LCM_Reach),]
    
    thecol <- "vAdj"
    if(nrow(td) > 0){ #if there are data
      if(!all(is.na(td[,thecol]))){
        #for(thresh in c(th, th + 2)){
        thresh <- th
        
        # high temp frequency: proportion of days with temps > threshold
        met1 <- lapply(X = unique(td$COMID), FUN = fnc_pDays.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # high temp duration: longest consecutive stretch with temps > threshold
        met2 <- lapply(X = unique(td$COMID), FUN = fnc_Cum.Days.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # timing, first week too high
        met3 <- lapply(X = unique(td$COMID), FUN = fnc_1stWk.Unsuitable, frame = td, start.date = stdat, end.date = endat, XX = thresh, st.col = thecol, sign = "GT", show.na = F)
        # duration within suitable thermal range
        met4 <- lapply(X = unique(td$COMID), FUN = fnc_Days.in.Range, frame = td, start.date = stdat, end.date = endat, XX = thresh, YY = 4, st.col = thecol, show.na = F)
        # cumulative exposure, ie degree-days
        met5 <- lapply(X = unique(td$COMID), FUN = fnc_Cum.Exposure, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        # variability
        met6 <- lapply(X = unique(td$COMID), FUN = fnc_Variance, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        if(any(is.na(met6))){idx <- which(is.na(met6));for(i in idx){met6[[i]] <- rep(NA, 5)}}
        met6 <- t(abind::abind(met6, along = 2))
        # weekly min/median/max
        met7 <- lapply(X = unique(td$COMID), FUN = fnc_Weekly, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        if(any(is.na(met7))){idx <- which(is.na(met7));for(i in idx){met7[[i]] <- rep(NA, 6)}}
        met7 <- t(abind::abind(met7, along = 2))
        
        dat <- data.frame("COMID" = unique(td$COMID), "pExc" = unlist(met1), "durExc" = unlist(met2), "first.week" = unlist(met3), "daysSuitable" = unlist(met4),
                          "cum.exp" = unlist(met5), met6, met7,
                          "Population" = pop, "Life.stage" = life.stage, "Start" = stdat, "End" = endat, "Thresh" = thresh, "year" = yy)
        met.dat <- rbind(met.dat, dat)
        #}
      } #end data check 2
    } #end data check 1
  } #end year range
}# end life.stage

met.dat$first.week <- as.Date(met.dat$first.week)
d <- lubridate::day(met.dat$first.week)
m <- lubridate::month(met.dat$first.week)
met.dat$first.week <- as.Date(paste0("2000-", m, "-", d))
met.dat <- met.dat[!is.na(met.dat$AWA),]
met.dat <- dplyr::left_join(met.dat, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID")
data.table::fwrite(met.dat, paste0("data/thermal_metrics_", subwat, "_", species, "_cc.csv"))


# Below dam matrices data ----
dat <- fst::read_fst("data/data_1990-2021.fst") # for 2011, 2015
#dat <- fst::read_fst("data/cc-pAdj_cmb.fst") #for 2050, 2080
#dat$vAdj <- dat$ST_med * dat$pAdj.ST_med
dat <- dplyr::left_join(dat, wil_streams[, c("COMID", "Pathlength")], by = "COMID")

dams <- read.csv("data/dams.csv")
dam_nm <- "Detroit"
subnm <- "nsan"
start_comid <- dams$COMID[dams$DAM_NAME == dam_nm]
ms_reaches <- paste0("Mainstem_", c("Falls2Mouth", "San2Falls_DS", "San2Falls_US", "Mck2San_DS", "Mck2San_US", "Mfk2Mck"))

# Use nhdplustools package to get downstream and upstream COMIDs
ds.comids <- get_DM(wil_streams, start_comid)
us.comids <- get_UT(wil_streams, start_comid) 
plot(sf::st_geometry(sf::st_zm(wil_streams)), col = "gray")
plot(sf::st_geometry(sf::st_zm(dplyr::filter(wil_streams, COMID %in% ds.comids))), add = T, col = 4) #downstream
plot(sf::st_geometry(sf::st_zm(dplyr::filter(wil_streams, COMID %in% us.comids))), add = T, col = 3) #upstream

dat <- dat[dat$COMID %in% ds.comids, c("Pathlength", "tim.date", "vAdj"),]
dat$year <- lubridate::year(dat$tim.date)
dat <- dat %>% group_by(Pathlength, tim.date) %>% summarise("vAdj" = median(vAdj, na.rm = T)) #to remove duplicates if any exist
td <- tidyr::pivot_wider(dat, names_from = "tim.date", values_from = "vAdj")
td <- as.data.frame(td[order(td$Pathlength),])
assign(paste0(dam_nm, "_data"), td)

write.csv(get(paste0(dam_nm, "_data")), paste0("data/", dam_nm, "_data.csv"))
# write.csv(get(paste0(dam_nm, "_data")), paste0("data/", dam_nm, "_cc_data.csv"))
