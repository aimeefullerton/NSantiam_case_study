# Use random forest regression to predict the proportion to adjust below-dam stream temperatures
# A.H. Fullerton, March 2024

# Setup ----
library(randomForestSRC)

final_dat <- fst::read_fst("data/final_dat.fst")
# this dataset has pAdj values (imputed empirical minus Siegel model predicted) for all reaches with gages, 
# as well as all reaches below gages diluted based on distance downstream from the gage

spatial <- data.table::fread("data/spatial_data.csv")
colnames(spatial)
retro <- fst::read_fst("data/170900_retrospective.fst")

testdat <- merge(final_dat[, c("COMID", "tim.date", "pAdj")], retro[,c(2:5)], by = c("COMID", "tim.date"))
testdat <- merge(testdat, spatial[,c("COMID", "cov.stream_order", "cov.path_length", "cov.tot.da.sqkm", "cov.elev_ws", "cov.BFI_ws", 
                 "cov.precip_ws", "cov.dam_nrm_stor_ws", "cov.proportion_dam_influenced", "cov.distance_below_dam", "cov.dam_hgt_m", 
                 "cov.dam_main_use", "cov.dam_name")], 
                 by = c("COMID"), all.x = T)
testdat$doy <- as.POSIXlt(testdat$tim.date)$yday + 1
fst::write_fst(testdat, "data/pAdj_dataset.fst", compress = 50)


# Prepare data ----
testdat <- fst::read_fst("data/pAdj_dataset.fst")

td <- testdat[testdat$pAdj!=1,]
td <- td[!is.na(td$pAdj),]
quantile(td$pAdj, probs = seq(0,1,0.01))
td <- td[td$pAdj < 2,] #remove really high numbers
td$cov.proportion_dam_influenced[is.na(td$cov.proportion_dam_influenced)] <- 0
td$cov.distance_below_dam[is.na(td$cov.distance_below_dam)] <- 250
td$cov.dam_hgt_m[is.na(td$cov.dam_hgt_m)] <- 0
td <- td[!is.na(td$cov.antec_air_temp),]
td$cov.dam_main_use[td$cov.dam_main_use == ""] <- "No dam"
td$cov.dam_main_use <- as.factor(td$cov.dam_main_use)
sort(unique(td$cov.dam_name))
summary(td)

comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")[,-1]
td <- dplyr::left_join(td, comid.dat[,c("COMID", "LCM_Reach")], by = "COMID")

td$Stream <- "Mainstem"
idx <- grep("North Santiam", td$LCM_Reach)
td$Stream[idx] <- "North Santiam"
idx <- grep("South Santiam", td$LCM_Reach)
td$Stream[idx] <- "South Santiam"
idx <- grep("McKenzie", td$LCM_Reach)
td$Stream[idx] <- "McKenzie"
idx <- grep("Middle Fork", td$LCM_Reach)
td$Stream[idx] <- "Middle Fork"
td$Stream <- as.factor(td$Stream)

fst::write_fst(td, "data/pAdj_dataset_ready.fst", compress = 50)


# Set up model ----
td <- fst::read_fst("data/pAdj_dataset_ready.fst")
predictors <- c("doy", "cov.antec_air_temp", "cov.distance_below_dam", "cov.elev_ws", "cov.dam_hgt_m", 
                "cov.std_mean_flow", "cov.proportion_dam_influenced", "Stream")
the.formula <- as.formula(paste0("pAdj", "~", paste(predictors, collapse = "+")))
lm1 <- lm(the.formula, data = td)
summary(lm1)
# all highly significant, but R2 of model is only 0.36
coeffs <- sort(abs(summary(lm1)$coeff[,1]), decreasing = T)[-1]


# Determine which are best variables to try
xvar.used <- rfsrc(the.formula, td, ntree = 250, nodedepth = 4,
                   var.used="all.trees", mtry = Inf, nsplit = 100)$var.used

# Run the model and view results ----
rf.mod <- rfsrc(the.formula, data = td, ntree = 500, nodedepth = 5) #, importance = T)
rf.mod
save(rf.mod, file = "data/rf.mod.RData")
load("data/rf.mod.RData")

# Results that included total drainage area
#Sample size: 2911970
#Number of trees: 1000
#Forest terminal node size: 5
#Average no. of terminal nodes: 31.886
#No. of variables tried at each split: 3
#Total no. of variables: 8
#Resampling used to grow trees: swor
#Resample size used to grow trees: 1840365
#Analysis: RF-R
#Family: regr
#Splitting rule: mse *random*
#  Number of random split points: 10
#(OOB) R squared: 0.5696821
#(OOB) Requested performance error: 0.01288085

# Results without total drainage area:
#Sample size: 2911970
#Number of trees: 500
#Forest terminal node size: 5
#Average no. of terminal nodes: 31.956
#No. of variables tried at each split: 3
#Total no. of variables: 7
#Resampling used to grow trees: swor
#Resample size used to grow trees: 1840365
#Analysis: RF-R
#Family: regr
#Splitting rule: mse *random*
#  Number of random split points: 10
#(OOB) R squared: 0.57082237
#(OOB) Requested performance error: 0.01284672

# Results without DA but with 5 factor levels of Stream
# Sample size: 2911970
# Number of trees: 500
# Forest terminal node size: 5
# Average no. of terminal nodes: 31.92
# No. of variables tried at each split: 3
# Total no. of variables: 8
# Resampling used to grow trees: swor
# Resample size used to grow trees: 1840365
# Analysis: RF-R
# Family: regr
# Splitting rule: mse *random*
#   Number of random split points: 10
# (OOB) R squared: 0.56958301
# (OOB) Requested performance error: 0.01288382

#rfsrc.fast()

# Try modeling subset of data ----
d <- sample(nrow(td), size = (nrow(td)/3))
dat <- td[d,]
rf.mod2 <- rfsrc(the.formula, data = dat, ntree = 500, nodedepth = 5)

# Variable importance scores (needed subsample to get this to run)
v <- vimp(rf.mod2, importance = T)$importance
#doy            cov.antec_air_temp        cov.distance_below_dam          cov.elev_ws 
#0.025302692    0.022712330               0.002663567                     0.001110134 
#cov.dam_hgt_m    cov.std_mean_flow       cov.proportion_dam_influenced   Stream 
#0.005786641      0.004049345             0.001848680                     0.002387106 


# Evaluate ----
# Plot partial effects
plot.variable(rf.mod, partial = TRUE, smooth.lines = TRUE)

# Get variable importance
oo <- subsample(rf.mod2, verbose = F)
vimpCI <- extract.subsample(oo)$var.jk.sel.Z
plot(subsample(oo))

# Identify interactions
find.interactions(rf.mod2)

# Predict ----
predicted_pAdj <- predict.rfsrc(rf.mod)
save(predicted_pAdj, file = "data/predicted_pAdj.RData")

prd_pAdj <- predicted_pAdj$predicted
obs_pAdj <- predicted_pAdj$yvar
plot(obs_pAdj, prd_pAdj)
write.csv(prd_pAdj, "data/prd_pAdj.csv")

td <- cbind(td, prd_pAdj)
fst::write_fst(td, "data/pAdj_dataset_done.fst")


# Predict all values
newdat <- fst::read_fst("data/pAdj_dataset.fst")
newdat <- newdat[newdat$pAdj < 2,] #remove really high numbers
newdat$cov.proportion_dam_influenced[is.na(newdat$cov.proportion_dam_influenced)] <- 0
newdat$cov.distance_below_dam[is.na(newdat$cov.distance_below_dam)] <- 250
newdat$cov.dam_hgt_m[is.na(newdat$cov.dam_hgt_m)] <- 0
newdat <- newdat[!is.na(newdat$cov.antec_air_temp),]
newdat$cov.dam_main_use[newdat$cov.dam_main_use == ""] <- "No dam"
newdat$cov.dam_main_use <- as.factor(newdat$cov.dam_main_use)

comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")[,-1]
newdat <- dplyr::left_join(newdat, comid.dat[,c("COMID", "LCM_Reach")], by = "COMID")

newdat$Stream <- "Mainstem"
idx <- grep("North Santiam", newdat$LCM_Reach)
newdat$Stream[idx] <- "North Santiam"
idx <- grep("South Santiam", newdat$LCM_Reach)
newdat$Stream[idx] <- "South Santiam"
idx <- grep("McKenzie", newdat$LCM_Reach)
newdat$Stream[idx] <- "McKenzie"
idx <- grep("Middle Fork", newdat$LCM_Reach)
newdat$Stream[idx] <- "Middle Fork"
newdat$Stream <- as.factor(newdat$Stream)
fst::write_fst(newdat, "data/newdat.fst", compress = 80)


load("data/rf.mod.RData")
newdat <- fst::read_fst("data/newdat.fst")

final <- NULL
x1 <- 1; x2 <- 500000
while(x2 <= nrow(newdat)){
  sub <- newdat[x1:x2, c("COMID", "tim.date", "pAdj", predictors)]
  preds_sub <- predict.rfsrc(rf.mod, newdata = sub)
  sub$predicted <- preds_sub$predicted
  x1 <- x1 + 500000
  x2 <- x2 + 500000
  final <- rbind(final, sub[,c("COMID", "tim.date", "pAdj", "predicted")])
  rm(sub)
}

# Save result
fst::write_fst(final, "data/FinalpAdj.fst")
plot(final$pAdj, final$predicted)

png("check_pAdj.png", width = 6, height = 6, units = "in", res = 300)
plot(final_dat$prd.stream_temp * final_dat$prd.pAdj, final_dat$vAdj)
dev.off()

# Check downstream from Foster Dam - shows that the vAdj is more reasonable than pAdj*ST
st_col <- "prd.stream_temp" #"ST_med"
thecomids <- unique(gage_info$COMID[gage_info$Site_Numbe %in% gages])
start_comid <- 23785717
start_comid <- 23751940
the.date <- as.Date(paste0(yy, "-08-01"))
ds_nhd <- fncDS_NHD(start_comid)
cids <- unique(ds_nhd$COMID)
dat <- final_dat[final_dat$COMID %in% cids,]; dat <- dat[dat$tim.date == the.date,]
par(mfrow = c(2,1), mar = c(3,4,1,1), las = 1)
plot(dat$DistDS, (dat$prd.stream_temp * dat$prd.pAdj), ylab = "Stream temperature (C)", xlab = "Distance downstream from dam (km)", ylim = c(0,25), cex = 0.2)
points(dat$DistDS, dat$vAdj, cex = 0.2, col = 2)
plot(dat$pDA, (dat$prd.stream_temp * dat$prd.pAdj), ylab = "Stream temperature (C)", xlab = "Proportional dam influence", ylim = c(0,25), cex = 0.2)
points(dat$pDA, dat$vAdj, cex = 0.2, col = 2)



# Add results from random forest model to full dataset ----
final_dat <- fst::read_fst("data/final_dat.fst") 
pAdj_dat <- fst::read_fst("data/FinalpAdj.fst") #from above script
final_dat <- dplyr::left_join(final_dat, pAdj_dat, by = c("COMID", "tim.date"))
final_dat <- final_dat[, -c(1,27)]
colnames(final_dat)[22] <- "Imputed"
colnames(final_dat)[25] <- "pAdj"
colnames(final_dat)[26] <- "prd.pAdj"
fst::write_fst(final_dat, "data/170900_retro_adj.fst", compress = 80)
