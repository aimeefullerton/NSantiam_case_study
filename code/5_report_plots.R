# Willamette Report figures
# A.H. Fullerton, March 2024

# Setup ----
library(sf)
library(dataRetrieval)

source("code/functions.R")

final_dat <- fst::read_fst("data/final_data.fst")
st_col <- "ST_med" #"prd.stream_temp"
comids <- unique(final_dat$COMID)
streams <- fncLoadStreams() # load NHDv2 streamlines
bg <- st_zm(streams[streams$COMID %in% comids,])
fl <- streams[streams$COMID %in% comids,]

# Load USGS gage data
gage_info <- read.csv("data/willamette_temperature_gage_info.csv")[,c(3,4,6,7,10,13:18)]
gage_info <- gage_info[!is.na(gage_info$Keep),]
gage_data <- read.csv("data/imputed_data_rf_mod_1990.csv")[,-1]
gage_dat <- merge(gage_data, gage_info[gage_info$Keep == 1, c("Site_Numbe", "COMID")], by.x = "site_no", by.y = "Site_Numbe", all.y = T)
gage_dat$Date <- as.Date(gage_dat$Date)
gages <- unique(gage_dat$site_no)
gages <- gages [1:23] #eliminating Willamette @ Newberg and @ Portland because we don't have predictions for these areas


# Report figures ----
# Fig 1 - compare imputed to gage data ----
parameterCd <- "00010" # Temperature
statCd<-"00003" #mean (see pg of DataRetrieval documentation for other options: max="00001")
startDate <- "" #earliest date available
endDate <- ""   #latest date available

png(paste0("Willamette_plots/gage_vs_imputed.png"), width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(3,4,1,1), las = 1)
sites <- c(14145500, 14166000)
for(s in 1:length(sites)){
  site_no <- sites[s]
  wil_temp <- readNWISdv(site_no, parameterCd, startDate, endDate,statCd=statCd)
  wil_temp <- renameNWISColumns(wil_temp)
  colnames(wil_temp)[3] <- "tim.date"
  cid <- gage_info$COMID[gage_info$Site_Numbe == site_no]
  wdat <- dplyr::left_join(final_dat[final_dat$COMID == cid,], wil_temp[, c(2:4)], by = "tim.date")
  
  plot(wdat$tim.date, wdat$Value, cex = 0.3, ylab = "Stream temperature (C)", xlab = "Date", ylim = c(0,28), col = 2)
  points(wdat$tim.date, wdat$Wtemp, cex = 0.3, col = 1)
  if(s == 2) legend("topright", legend = c("observed", "imputed"), pch = 19, col = c(1,2), bty = 'n')
  legend("topleft", legend = paste0(site_no, ": ", gage_info$Site_Name[gage_info$Site_Numbe == site_no]), bty = 'n', cex = 0.8)
  
}
dev.off()

# Fig 2 - compare adjusted to modeled ----
site_no <- 14166000 # 14145500
cid <- gage_info$COMID[gage_info$Site_Numbe == site_no]
png(paste0("Willamette_plots/adjusted_vs_modeled_", site_no, ".png"), width = 6, height = 6, units = "in", res = 300)
par(mfrow = c(2,1), mar = c(3,4,1,1), las = 1)
plot(final_dat$tim.date[final_dat$COMID == cid], final_dat[,st_col][final_dat$COMID == cid], cex = 0.3, ylab = "Stream temperature (C)", xlab = "", ylim = c(0,28))
points(final_dat$tim.date[final_dat$COMID == cid], (final_dat$Value)[final_dat$COMID == cid], cex = 0.3, col = 4)
legend("topleft", legend = paste0(site_no, ": ", gage_info$Site_Name[gage_info$Site_Numbe == site_no]), bty = 'n', cex = 0.8)
#points(final_dat$tim.date[final_dat$COMID == cid], (final_dat[,st_col]*final_dat[,"prd.pAdj"])[final_dat$COMID == cid], cex = 0.3, col = 4)
yy <- 2018
plot(final_dat$tim.date[final_dat$COMID == cid & final_dat$year == yy], final_dat[,st_col][final_dat$COMID == cid & final_dat$year == yy], type = 'l', ylab = "Stream temperature (C)", xlab = "", ylim = c(0,25))
lines(final_dat$tim.date[final_dat$COMID == cid & final_dat$year == yy], final_dat$Value[final_dat$COMID == cid & final_dat$year == yy], col = 4)
#lines(final_dat$tim.date[final_dat$COMID == cid & final_dat$year == yy], (final_dat[,st_col]*final_dat[,"prd.pAdj"])[final_dat$COMID == cid & final_dat$year == yy], col = 4)
legend("topleft", legend = c("modeled", "adjusted"), lty = 1, col = c(1,4), bty = 'n')
dev.off()

# Fig 3 - all gage sites, modeled, adjusted and imputed, in a downstream direction from top/left to bottom/right ----
# $Value and #vAdj should be identical at gage locations; will differ at other reaches moving downstream
start_comid <- 23751940 #gage 14145500
ds_nhd <- fncDS_NHD(start_comid)
thecomids <- ds_nhd$COMID[1:25]
yy <- 2018
td <- final_dat[final_dat$COMID %in% thecomids & final_dat$year == yy,]
thecomids <- unique(td$COMID)
foo <- td[td$tim.date == as.Date(paste0(yy, "-01-01")),c("COMID", "DistDS")]
ci <- foo$COMID[order(foo$DistDS)]

png(paste0("Willamette_plots/adj_gage_ts.png"), width = 11, height = 12, units = "in", res = 300)
par(mfrow = c(4,3), mar = c(3,4,2,2), las = 1, cex = 0.9)
for(i in 1:12){
  plot(td$tim.date[td$COMID == ci[i]], td[,st_col][td$COMID == ci[i]], cex = 0.3, 
       ylab = "Stream temperature (C)", xlab = "", ylim = c(0,25), type = 'l', 
       main = paste0(gage_info$Site_Numbe[gage_info$COMID == ci[i]], " ", substr(gage_info$Site_Name[gage_info$COMID == ci[i]], 1,32)), cex.main = 0.8)
  lines(td$tim.date[td$COMID == ci[i]], td$vAdj[td$COMID == ci[i]], cex = 0.3, col = 4)
  #lines(td$tim.date[td$COMID == ci[i]], (td[,st_col]*td[,"prd.pAdj"])[td$COMID == ci[i]], cex = 0.3, col = 4)
  lines(td$tim.date[td$COMID == ci[i]], td$Value[td$COMID == ci[i]], cex = 0.3, col = 2)
  if(i == 1) legend("topleft", legend = c("modeled", "adjusted", "imputed"), lty = 1, col = c(1,4,2), bty = 'n')
  legend("topright", legend = paste0("COMID ", ci[i]), bty = 'n')
}
dev.off()


# Fig 4 - partial effects plots from random forest ----
  # See 3_predict_pAdj_retro.R
  # plot.variable(rf.mod, partial = TRUE, smooth.lines = TRUE)

# Fig 5 - frequency distribution of pAdj across reaches and days for GCMs ----
ByYear <- dat[!is.na(dat$STcc),] %>%
  group_by(year) %>%
  summarise(mean(vAdj), mean(STcc))
ByYear2 <- dat[!is.na(dat$STcc),] %>%
  group_by(year) %>%
  summarise(mean(pAdj.CanESM2))
plot(ByYear$year, ByYear$`mean(STcc)`, type = 'l', las = 1, ylab = "Stream temperature (C)", xlab = "")
lines(ByYear$year, ByYear$`mean(vAdj)`, col = 2)

boxplot(ByYear2$`mean(pAdj.CanESM2)` ~ ByYear2$year, las = 1, ylab = "pAdj")

# Fig 6 - as above for CanESM2 only ----
pbs <- seq(0, 1, 0.1)
ByYear3 <- dat[!is.na(dat$STcc),] %>%
  group_by(year) %>%
  summarise(quantile(vAdj, pbs), quantile(STcc, pbs), quantile(pAdj.CanESM2, pbs), prob = pbs)
boxplot(ByYear3$`quantile(pAdj.CanESM2, pbs)` ~ ByYear3$year, las = 1, ylab = "pAdj")

plotdata <- ByYear3[ByYear3$year > 1990 & ByYear3$year < 2022,]
boxplot(plotdata$`quantile(pAdj.CanESM2, pbs)` ~ plotdata$year, las = 1, ylab = "pAdj", ylim = c(0.8, 1.25), xlab = "")

# Fig 7 - adjusted predictions by site and year ----
# By population
td <- final_dat[final_dat$COMID %in% thecomids & final_dat$year == yy,]
td <- dplyr::left_join(td, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID")
td$year <- lubridate::year(td$tim.date)
cnam <- colnames(td)[colnames(td) == paste0("pAdj.", climate_scenario)]
dat <- td[, c("COMID", "LCM_Reach", "tim.date", "year", climate_scenario, cnam)]
dat$vAdj <- dat[,5] * dat[,6]
colnames(dat)[5] <- "STcc" 

for(rch in sort(unique(dat$LCM_Reach))){
  png(paste0("plots/Vadj_", climate_scenario, "_", rch, ".png"), width = 5, height = 4, units = "in", res = 300)
  par(mar = c(3,4,2,2), las = 1, cex = 0.9)
  plotdat <- dat[dat$year == 2015 & dat$LCM_Reach == rch,]
  plotdat <- plotdat[order(plotdat$tim.date),]
  ylm <- c(min(plotdat$STcc, plotdat$vAdj, na.rm = T), max(plotdat$STcc, plotdat$vAdj, na.rm = T))
  plot(plotdat$tim.date, plotdat$STcc, type = 'l', las = 1, ylab = "Stream temperature (C)", xlab = "", main = rch, ylim = ylm)
  lines(plotdat$tim.date, plotdat$vAdj, col = "#eaad44c8")
  dev.off()
}

# Fig 8 - maps of adjusted predicted temps across Willamette ----
the.date <- as.Date(paste0(yy, "-08-01"))
dat <- final_dat[final_dat$tim.date == the.date,]
dat$adjusted <- dat[,st_col] * dat$prd.pAdj
fl <- dplyr::left_join(fl, dat, by = "COMID")
fl <- st_zm(fl)
fl.dat <- as.data.frame(fl)
color_range <- c(0,30)
color_range[1] <- floor(color_range[1] - 0.05 * color_range[1]); color_range[2] <- ceiling(color_range[2] + 0.05 * color_range[2])
if(color_range[1] == 0) color_range[1] <- 1
col_by <- round((color_range[2] - color_range[1] + 1) / 13)
colscheme <- viridis::plasma(length(seq(color_range[1], color_range[2], by = col_by)) - 1) #violet to pink to yellow

png(paste0("Willamette_plots/map_", st_col, "_", the.date, ".png"), width = 5, height = 8, units = "in", res = 300)
par(oma = rep(0,4), mar = rep(1,4))
plot(sf::st_geometry(bg), col = "gray70", key.pos = NULL, reset = F, main = the.date, cex = 0.8)
plot(fl[,"adjusted"], breaks = seq(color_range[1], color_range[2], by = col_by), pal = colscheme, lwd = fl$StreamOrde/2, add = T)
addLegendToSFPlot(value_range = color_range, num_cats = length(colscheme), palette = colscheme, adjX = 0.05, adjY = 0.05)
dev.off()

# Summarize by LCM reach ----
agg <- dat %>% 
  group_by(LCM_Reach, tim.date) %>%
  summarise(mean(vAdj), mean(STcc))
data.table::fwrite(agg, paste0("data/", climate_scenario, "_meanByRchDay.csv")) 

agg$year <- lubridate::year(agg$tim.date)
plotdat <- agg[agg$year == 2015 & agg$LCM_Reach == "McKenzie Reach A",]
plotdat <- plotdat[order(plotdat$tim.date),]
plot(plotdat$tim.date, plotdat$`mean(STcc)`, type = 'l', las = 1, ylab = "Stream temperature (C)", xlab = "")
lines(plotdat$tim.date, plotdat$`mean(vAdj)`, col = 2)

#pbs <- seq(0, 1, 0.1)
#agg2 <- agg %>%
#  group_by(LCM_Reach, Date) %>%
#  summarize(quantile(`mean(ST_adj)`, pbs), prob = pbs)
#data.table::fwrite(agg2, paste0("data/Willamette_ST4LCM/", yr, "/ST_", pop, "_meanByRch_QbyGCM.csv")) #193 MB

