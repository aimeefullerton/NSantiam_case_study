# Predict pAdj for climate change scenarios
# A.H. Fullerton, March 2024

# Setup ----
library(randomForestSRC)
library(dplyr)

# Load info about LCM reaches and associated COMIDs
comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")[,-1]
LCM_Reaches <- sort(unique(comid.dat$LCM_Reach))
COM_HUC <- data.table::fread("data/COMID_to_HUC12.csv")
COM_HUC$Huc6 <- substr(COM_HUC$Huc12, 1, 6)
COM_HUC$Huc10 <- substr(COM_HUC$Huc12, 1, 10)
huclist <- sort(unique(COM_HUC$Huc6))
huc10list <- sort(unique(COM_HUC$Huc10))
comids <- COM_HUC$COMID[COM_HUC$Huc6 == huclist[17]]; comids <- comids[!is.na(comids)]

# Load spatial covariate data
spatial <- data.table::fread("data/spatial_data.csv")
spatial <- spatial[, c("COMID", "cov.elev_ws", "cov.proportion_dam_influenced", "cov.distance_below_dam", "cov.dam_hgt_m")]

# Compile Willamette CC data & compute CC-specific pAdj ----
cc_folders <- dir("../st-cc/data/predictions")
for(i in 1:length(cc_folders)){
  climate_scenario <- cc_folders[i]
  cc_hucs <- dir(paste0("../st-cc/data/predictions/", climate_scenario))
  cc_hucs <- gsub("st_pred_", "", cc_hucs); cc_hucs <- gsub(".csv", "", cc_hucs)
  
  # Get data for Willamette (huclist = 17, "170900")
  ghuc10s <- sort(cc_hucs[grep(huclist[17], cc_hucs)])
  td <- NULL
  for(the_huc in ghuc10s){
    thedata <- data.table::fread(paste0("../st-cc/data/predictions/", climate_scenario, "/st_pred_", the_huc, ".csv"))
    thedata <- thedata[,-1]
    td <- rbind(td, thedata)
  }
  fst::write_fst(td, paste0("../st-cc/data/Willamette/", huclist[17], "_", climate_scenario, ".fst"), compress = 50)
}

# Read in retrospective data including pAdj info
st <- fst::read_fst("data/final_data.fst")

# Make adjusted Value and Proportion for CC data
st$pAdj <- st$vAdj / st$prd.stream_temp

for(c in 4:13){
  vAdj <- (st[,c] * (1- st$pDA)) + (st$Value * st$pDA)
  pAdj <- vAdj / st[,c]
  st <- cbind(st, pAdj)
  colnames(st)[ncol(st)] <- paste0("pAdj.", colnames(st)[c])
}
fst::write_fst(st, "data/st-cc_data.fst", compress = 80)


# Forward predict ----

# Load ST data
st <- fst::read_fst("data/st-cc_data.fst")

climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")
for(climate_scenario in climate_scenarios){

# Load climate scenario data, add streams and spatial covariates
ccdat <- fst::read_fst(paste0("../st-cc/data/Willamette/", huclist[17], "_", climate_scenario, ".fst"))

# Merge with comid.dat to get stream reach info
ccdat <- dplyr::left_join(ccdat, comid.dat[,c("COMID", "LCM_Reach")], by = "COMID")
ccdat$Stream <- "Mainstem"
idx <- grep("North Santiam", ccdat$LCM_Reach)
ccdat$Stream[idx] <- "North Santiam"
idx <- grep("South Santiam", ccdat$LCM_Reach)
ccdat$Stream[idx] <- "South Santiam"
idx <- grep("McKenzie", ccdat$LCM_Reach)
ccdat$Stream[idx] <- "McKenzie"
idx <- grep("Middle Fork", ccdat$LCM_Reach)
ccdat$Stream[idx] <- "Middle Fork"
ccdat$Stream <- as.factor(ccdat$Stream)

# Merge with spatial covariates
ccdat <- dplyr::left_join(ccdat, spatial, by = "COMID")
ccdat$doy <- lubridate::yday(ccdat$tim.date)
ccdat <- ccdat[order(ccdat$COMID, ccdat$tim.date),]

# Add appropriate pAdj column
cols <- c("tim.date", "COMID", climate_scenario, paste0("pAdj.", climate_scenario))
ccdat$tim.date <- as.Date(ccdat$tim.date)

# Merge with ST data (that has computed pAdj for 1990-2021)
ccdat <- dplyr::left_join(ccdat, st[, cols], by = c("COMID", "tim.date"))

# A few adjustments to the data
ccdat$cov.antec_air_temp[is.na(ccdat$cov.antec_air_temp)] <- 5 # fill a few blank days at beginning of 1950
ccdat[,paste0("pAdj.", climate_scenario)][is.infinite(ccdat[,paste0("pAdj.", climate_scenario)])] <- 2 #remove infinite numbers
ccdat[,paste0("pAdj.", climate_scenario)][ccdat[,paste0("pAdj.", climate_scenario)] > 2] <- 2 #remove really high numbers
ccdat$cov.proportion_dam_influenced[is.na(ccdat$cov.proportion_dam_influenced)] <- 0
ccdat$cov.distance_below_dam[is.na(ccdat$cov.distance_below_dam)] <- 250
ccdat$cov.dam_hgt_m[is.na(ccdat$cov.dam_hgt_m)] <- 0

# formula doesn't handle dashes in names, need to adjust:
cnam <- paste0("pAdj.", climate_scenario)
cnam <- gsub("-", "_", cnam)
idx <- which(colnames(ccdat) == paste0("pAdj.", climate_scenario))
colnames(ccdat)[idx] <- cnam

predictors <- c("doy", "cov.antec_air_temp", "cov.distance_below_dam", "cov.elev_ws", "cov.dam_hgt_m",
                "cov.std_mean_flow", "cov.proportion_dam_influenced", "Stream")
the.formula <- as.formula(paste0(cnam, "~", paste(predictors, collapse = "+")))

train.dat <- ccdat[!is.na(ccdat[,cnam]),]
test.dat <- ccdat[is.na(ccdat[,cnam]),]
test.dat[,cnam] <- 1


# Run the model and view results
rf.mod <- rfsrc(the.formula, data = train.dat, ntree = 500, nodedepth = 5) #, importance = T)
rf.mod
#save(rf.mod, file = paste0("data/rf.mod-", climate_scenario, ".RData"))
#load(paste0("data/rf.mod-", climate_scenario, ".RData"))

# Predict for other DATES
test_final <- NULL
x1 <- 1; x2 <- 1000000
while(x2 < nrow(test.dat)){
  test.dat_sub <- test.dat[x1:x2, ]
  preds_cc <- predict.rfsrc(rf.mod, newdata = test.dat_sub)
  test.dat_sub$prd.pAdj <- preds_cc$predicted
  test_final <- rbind(test_final, test.dat_sub)
  x1 <- x1 + 1000000; x2 <- x2 + 1000000
}
#for last bit:
x1 <- nrow(test_final) + 1; x2 <- nrow(test.dat)
test.dat_sub <- test.dat[x1:x2, ]
preds_cc <- predict.rfsrc(rf.mod, newdata = test.dat_sub)
test.dat_sub$prd.pAdj <- preds_cc$predicted
test_final <- rbind(test_final, test.dat_sub)

summary(test_final)
fst::write_fst(test_final, paste0("../st-cc/data/Willamette/", climate_scenario, "_pAdj.fst"))
hist(test_final$prd.pAdj)
}

# Merge back together
test_final <- fst::read_fst(paste0("../st-cc/data/Willamette/", climate_scenario, "_pAdj.fst"))
test_final[,cnam] <- test_final$prd.pAdj
test_final <- test_final[,-ncol(test_final)]

alldat <- rbind(train.dat, test_final)
colnames(alldat)[colnames(alldat) == cnam] <- paste0("pAdj.", climate_scenario)

fst::write_fst(alldat, paste0("../st-cc/data/Willamette/", climate_scenario, "_pAdj.fst"), compress = 80)

# Combine results
cc <- fst::read_fst("data/170900_AllGCMs_wMed.fst")
cc$tim.date <- as.Date(cc$tim.date)
for(climate_scenario in climate_scenarios){
  c1 <- fst::read_fst(paste0("data/", climate_scenario, "_pAdj.fst"))
  cc <- dplyr::left_join(cc, c1[, c("COMID", "tim.date", paste0("pAdj.", climate_scenario)], by = c("COMID", "tim.date"))
}
fst::write_fst(cc, "data/cc-pAdj_cmb.fst", compress = 80)
summary(cc)


# Examine ----
foo <- td[td$COMID == 23751752,]
plot(foo$tim.date, foo[,"GDFL-ESM2M"], type = "l", ylab = "Stream temperature (C)", xlab = "Date")
lines(foo$tim.date, (foo[,"GDFL-ESM2M"] * foo[, "pAdj.GDFL-ESM2M"]), col = 2)

# Boxplots of pAdj
td <- fst::read_fst("data/cc-pAdj_cmb.fst")

climate_scenarios <- c("CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GDFL-ESM2M", "HadGEM2-CC", "HadGEM2-ES", "inmcm4", "IPSL-CM5A-MR", "MIROC")

quantiles <- seq(0, 1, 0.01)
for(c in 1:length(climate_scenarios)){
  climate_scenario <- climate_scenarios[c]
  cnam <- colnames(td)[colnames(td) == paste0("pAdj.", climate_scenario)]
  dat <- td %>% select(all_of(cnam)) %>%
    summarise(x = quantile(!! rlang::sym(cnam), seq(0, 1, 0.01)), prob = seq(0, 1, 0.01)) 
  quantiles <- cbind.data.frame(quantiles, dat$x)
  colnames(quantiles)[c + 1] <- cnam
}
colnames(quantiles) <- c("Q", climate_scenarios)
boxplot(quantiles[2:100,-1], las = 1, ylab = "pAdj")
abline(h = 1, lty = 3)



