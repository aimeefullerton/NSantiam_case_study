# Figures for Fullerton et al. Future stream temperatures manuscript
# 9/12/24

library(dplyr)
source("code/functions.R")

# Figure 5 North Santiam ----

# Annual Time Series
library(xts)
library(dygraphs)

pop <- "North Santiam"; pop2 <- gsub(" ", "", pop)
load(paste0("data/", pop2, ".retro.data.list.RData")) # for 2011, 2015
#load(paste0("data/", pop2, ".ST_median.data.list.RData")) # for 2050, 2080
data.list <- get(paste0(pop2, ".retro.data.list")) # for 2011, 2015
#data.list <- get(paste0(pop2, ".ST_median.data.list")) # for 2050, 2080
yy = 2015
df <- as.data.frame(data.list[[paste0("dat.", pop, ".", yy)]])
idx <- c("Reach A", "Reach B", "Reach F")
x <- NULL; for(i in 1:length(idx)) {x <- c(x, grep(idx[i], colnames(df)))}
df <- df[,x]
xdt <- xts::as.xts(df)
reaches <- colnames(xdt); reaches <- gsub("Min.", "", reaches); reaches <- gsub("Median.", "", reaches); reaches <- gsub("Max.", "", reaches); reaches <- unique(reaches)
for(r in 1:length(reaches)){
  #vv <- c(paste0("Min.", reaches[r]), paste0("Mean.", reaches[r]), paste0("Max.", reaches[r]))
  vv <- colnames(xdt)[grep(reaches[r], colnames(xdt))][c(2,1,3)]
  assign(paste0("vv",r), vv)
  ll <- gsub(pop, "", vv); ll <- gsub(" Reach", "", ll); ll <- gsub("Min.", "", ll); ll <- gsub("Median.", "", ll); ll <- gsub("Max.", "", ll)
  ll <- gsub(" ", "", ll); ll <- gsub(".*_", "", ll); ll <- unique(ll)
  assign(paste0("ll",r), ll)
}

dygraph(xdt) %>%
  dySeries(.,vv1, label = ll1) %>%
  {if(length(reaches) > 1) dySeries(.,vv2, label = ll2) else .} %>%
  {if(length(reaches) > 2) dySeries(.,vv3, label = ll3) else .} %>%
  {if(length(reaches) > 3) dySeries(.,vv4, label = ll4) else .} %>%
  {if(length(reaches) > 4) dySeries(.,vv5, label = ll5) else .} %>%
  {if(length(reaches) > 5) dySeries(.,vv6, label = ll6) else .} %>%
  {if(length(reaches) > 6) dySeries(.,vv7, label = ll7) else .} %>%
  {if(length(reaches) > 7) dySeries(.,vv8, label = ll8) else .} %>%
  dyRoller(showRoller = T, rollPeriod = 7) %>%
  dyRangeSelector() %>%
  dyLegend(width = 160, labelsSeparateLines = T) %>%
  dyAxis("y", label = "Stream temperature (C)")
# Save manually
rm(list = ls(pattern = "ll")); rm(list = ls(pattern = "vv")); rm(r, x, i, idx, data.list)



# Below Dams Heatmap
dam_data <- read.csv("data/Detroit_data.csv")

yy = 2020
cols2keep <- colnames(dam_data)[grep(yy, colnames(dam_data))]
dat <- dam_data[,c("Pathlength", cols2keep), drop = F]
mat <- as.matrix(dat)

png(paste0("plots/heatmap", yy, ".png"), width = 6.5, height = 5, units = "in", res = 600)
  colr <- hcl.colors(14, "YlOrRd", rev = TRUE)
  par(las = 1, mar = c(4,4,2,2))
  fields::image.plot(t(mat), ylab = "River kilometer", xlab = "Julian day", axes = F, col = colr, zlim = c(0,28)); box()
  axis(1, at = seq(0, 1, length.out = 12), labels = round(seq(0, 365, length.out = 12)))
  axis(2, at = seq(0, max(dat$Pathlength, na.rm = T), length.out = 10) / max(dat$Pathlength, na.rm = T), 
       labels = round(seq(min(dat$Pathlength, na.rm = T), max(dat$Pathlength, na.rm = T), length.out = 10)))
dev.off()
rm(dat, colr, cols2keep)


# Figure 6 North Santiam ----

# Thermal metrics
library(dplyr)

species <- "Chinook" # "Omykiss"
subwat <- "north-santiam"

thermal_metrics <- data.table::fread(paste0("data/thermal_metrics_", subwat, "_", species, "_cc.csv"))
thermal_metrics$abv_dams <- F
thermal_metrics$abv_dams[thermal_metrics$LCM_Reach %in% c("North Santiam Reach D", "North Santiam Reach E")] <- T

dat <- as.data.frame(thermal_metrics)
ls <- "prespawn"
abv_dams <- F
smooth <- T
add_legend <- F
thetitle <- fncTitle(species, life.stage = ls)
col2use <- c("#ABC9CD96", "#4A929BC8", "#33797f")
if(species == "Omykiss") col2use <- c("#D7BDCB", "#985E7D","#5E2041")
ad <- ifelse(abv_dams == T, "AboveDams", "BelowDams")

# Select and summarize data for plot
dat <- dat[dat$abv_dams == abv_dams & dat$Life.stage == ls,] 
# Transform days suitable into proportion of time period suitable
dat$pSuitable <- dat$daysSuitable / max(dat$daysSuitable)
dat <- dat[,c("COMID", "LCM_Reach", "abv_dams", "year", "Life.stage", "pSuitable")]

rch_data <-
  dat %>% group_by(year) %>%
  summarise(Met = quantile(pSuitable, c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = T), prob = c(0.1, 0.25, 0.5, 0.75, 0.9))
the.years <- unique(rch_data$year)

# Plot
png(paste0("plots/", species, "_", ls, ad, ".png"), width = 6.5, height = 5, units = "in", res = 600)
  par(mar = c(5,5.5,3,1))
  plot(the.years, rch_data$Met[rch_data$prob == 0.5], type = 'n', ylab = "Proportion of reaches thermally suitable", xlab = "Year", las = 1, ylim = c(0, 1.1))
  if(smooth == T){
    l1 <- loess(as.numeric(rch_data$Met[rch_data$prob == 0.1]) ~ as.numeric(rch_data$year[rch_data$prob == 0.1]))
    l2 <- loess(as.numeric(rev(rch_data$Met[rch_data$prob == 0.9])) ~ as.numeric(rev(rch_data$year[rch_data$prob == 0.9])))
    l3 <- loess(as.numeric(rch_data$Met[rch_data$prob == 0.25]) ~ as.numeric(rch_data$year[rch_data$prob == 0.25]))
    l4 <- loess(as.numeric(rev(rch_data$Met[rch_data$prob == 0.75])) ~ as.numeric(rev(rch_data$year[rch_data$prob == 0.75])))
    l5 <- loess(as.numeric(rch_data$Met[rch_data$prob == 0.5]) ~ as.numeric(rch_data$year[rch_data$prob == 0.5]))
    polygon(c(the.years, rev(the.years)), c(l1$fitted, l2$fitted), border = NA, col = col2use[1])
    polygon(c(the.years, rev(the.years)), c(l3$fitted, l4$fitted), border = NA, col = col2use[2])
    lines(the.years, l5$fitted, lwd = 2, col = col2use[3])
  }else{ #jagged edges
    polygon(c(the.years, rev(the.years)), c(rch_data$Met[rch_data$prob == 0.1], rev(rch_data$Met[rch_data$prob == 0.9])), border = NA, col = col2use[1])
    polygon(c(the.years, rev(the.years)), c(rch_data$Met[rch_data$prob == 0.25], rev(rch_data$Met[rch_data$prob == 0.75])), border = NA, col = col2use[2])
    lines(the.years, rch_data$Met[rch_data$prob == 0.5], lwd = 2, col = col2use[3])
  }
  abline(v = c(1950, 2000, 2050, 2100), lty = 3)
  abline(h = c(0.2, 0.4, 0.6, 0.8), lty = 3)
  
  # Legend
  if(add_legend == T){
    legend_text_lines <- strwrap(thetitle, width = 25)  # Split the legend text into multiple lines & adjust the width as needed
    num_lines <- length(legend_text_lines) # Calculate the number of lines in the legend
    legend_x <- 1950  # Adjust the x-coordinate
    legend_y <- 0.2 # Adjust the y-coordinate
    legend(legend_x, legend_y, 
           legend = legend_text_lines[1], cex = 1, lwd = 2, col = col2use[3], bty = "n")
    legend(legend_x, legend_y - 0.07,
           legend = legend_text_lines[2], cex = 1, lwd = 2, col = NA, bg = NA, bty = "n")
  }
dev.off()

# Mapped metric
library(sf)
library(mapsf)

load("data/nsan_shapefiles.RData")
subwat <- "north-santiam"
species <- "Chinook" # "Omykiss"
thermal_metrics <- data.table::fread(paste0("data/thermal_metrics_", subwat, "_", species, "_cc.csv"))
thermal_metrics$abv_dams <- F
thermal_metrics$abv_dams[thermal_metrics$LCM_Reach %in% c("North Santiam Reach D", "North Santiam Reach E")] <- T

yy <- 2020
met <- "pSuitable"
ls = "prespawn"

streams <- nsan_streams
dat <- as.data.frame(thermal_metrics)
# Select and summarize data for plot
dat <- dat[dat$Life.stage == ls & dat$year == yy,] 
# Transform days suitable into proportion of time period suitable
dat$pSuitable <- dat$daysSuitable / max(dat$daysSuitable)
dat <- dat[,c("COMID", "LCM_Reach", "year", "Life.stage", "pSuitable")]

# Merge NHD lines and predictions based on COMID
streams$sortby <- as.numeric(rownames(streams))
dd <- merge(streams, dat, by = "COMID", all.x = T, sort = F)
dd <- dd[, c("COMID", "pSuitable", "sortby", "geometry")]
dd <- dd[order(dd$sortby),] #need to do this because the COMIDs with no predictions get shunted to the bottom and messes up sorting
streams <- dd[!is.na(dd$pSuitable),]

# Plot maps
png(paste0("plots/map_", species, "_", ls, "_", yy, ".png"), width = 6.5, height = 5, units = "in", res = 600)

  par(las = 1, cex = 0.9)

  # Color schemes
  #color_range <- range(dat$pSuitable)
  color_range <- c(0,1)
  col_by = 0.1
  if(length(col_by) < 5) col_by = col_by/2
  colscheme <- (viridis::mako(20))[12:20] #seagreen to blue to black
  if(species == "Omykiss") colscheme <- (viridis::magma(20))[9:17] # purple to orange
  #image(matrix(1:20,1:20), col = colscheme)
  #image(matrix(1:9,1:9), col = colscheme)
  
  streams <- sf::st_zm(streams)
  # Plot basin and streams colored by metric
  plot(sf::st_geometry(nsan_huc), border = "gray30", col = "gray30", axes = F)
  plot(sf::st_geometry(sf::st_zm(nsan_nhd)), col = "gray70", key.pos = NULL, reset = F, add = T)
  mf_map(x = streams, col = "gray80", lwd = 5, leg_pos = NA, add = T)
  mf_map(x = streams, var = "pSuitable", type = "choro", breaks = seq(color_range[1], color_range[2], length.out = length(colscheme)), pal = colscheme, lwd = 3, leg_pos = NA, add = T)
  
  # legend
  addLegendToSFPlot(value_range = round(color_range, 1), num_cats = length(colscheme), palette = colscheme, adjX = 0.05, adjY = 0.05)
  
  # add dam location(s) for reference
  subwat_dams <- c("Detroit", "BIG CLIFF")
  plot(sf::st_geometry(dams[dams$DAM_NAME == subwat_dams[1],]), pch = 24, bg = "yellow", cex= 1.2, add = TRUE)
  plot(sf::st_geometry(dams[dams$DAM_NAME == subwat_dams[2],]), pch = 25, bg = "yellow", cex = 1.2, add = TRUE)
  legend("bottom", legend = c(paste(stringr::str_to_title(subwat_dams[1]), "dam"), paste(stringr::str_to_title(subwat_dams[2]), "dam")), pch = c(24, 25), pt.bg = "yellow", bty ='n')

  # Add cartographic details
  mf_arrow("bottomright")
  #mf_scale(pos = "bottom", cex = 1)
  
dev.off()



# Table 6 North Santiam ----
ccd <- fst::read_fst("data/cc-pAdj_cmb_plus.fst")
subwat <- "north-santiam"; pop <- "North Santiam"; pop2 <- "NorthSantiam"
rchs <- c("North Santiam Reach A", "North Santiam Reach B", "North Santiam Reach C", "North Santiam Reach D", "North Santiam Reach E",
          "North Santiam Reach F", "North Santiam_Santiam", "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
comid.dat <- read.csv("data/Willamette_LCM_COMIDs.csv")[,-1]
subwat_dams <- c("Detroit", "BIG CLIFF")
year.range <- c(1950, 2099)

# Process
met.dat.cc <- NULL
the.months <- as.character(lubridate::month(1:12, label = T))
life.stages <- c("enroute", "prespawn", "incubat", "outmigr1", "outmigr2", "outmigr3", the.months)
for(life.stage in life.stages){
  for(yy in year.range[1]:year.range[2]){
    
    sls <- fncSetLifestage("Chinook", life.stage, yy, ccd, rchs) #ccd, not retro.dat
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
        # weekly min/mean/max
        met7 <- lapply(X = unique(td$COMID), FUN = fnc_Weekly, frame = td, start.date = stdat, end.date = endat, st.col = thecol, show.na = F)
        if(any(is.na(met7))){idx <- which(is.na(met7));for(i in idx){met7[[i]] <- rep(NA, 6)}}
        met7 <- t(abind::abind(met7, along = 2))
        
        dat <- data.frame("COMID" = unique(td$COMID), "pExc" = unlist(met1), "durExc" = unlist(met2), "first.week" = unlist(met3), "daysSuitable" = unlist(met4),
                          "cum.exp" = unlist(met5), met6, met7,
                          "Population" = pop, "Life.stage" = life.stage, "Start" = stdat, "End" = endat, "Thresh" = thresh, "year" = yy)
        met.dat.cc <- rbind(met.dat.cc, dat)
        #}
      } #end data check 2
    } #end data check 1
  } #end year range
}# end life.stage
met.dat.cc$first.week <- as.Date(met.dat.cc$first.week)
d <- lubridate::day(met.dat.cc$first.week)
m <- lubridate::month(met.dat.cc$first.week)
met.dat.cc$first.week <- as.Date(paste0("2000-", m, "-", d))
met.dat.cc$first.week[met.dat.cc$first.week == as.Date("2000-01-01")] <- NA
met.dat.cc <- met.dat.cc[!is.na(met.dat.cc$AWA),]
met.dat.cc <- dplyr::left_join(met.dat.cc, comid.dat[, c("COMID", "LCM_Reach")], by = "COMID")
met.dat.cc$LCM_Reach[met.dat.cc$LCM_Reach == "Santiam"] <- "North Santiam_Santiam"
data.table::fwrite(met.dat.cc, paste0("data/thermal_metrics_", subwat, "_cc.csv"))

#met.dat.cc <- data.table::fread("data/thermal_metrics_north-santiam_cc.csv")

dat20s <- 
  met.dat.cc %>% filter(year >= 2020 & year <= 2029) %>% 
  group_by(Life.stage) %>%
  summarise(IWI = mean(IWI, na.rm = T), AWA = mean(AWA, na.rm = T), MWM = mean(MWM, na.rm = T), MWV = mean(MWV, na.rm = T),
            RNG = mean(RNG, na.rm = T), pExc = mean(pExc, na.rm = T), first.week = mean(first.week, na.rm = T),
            daysSuitable = mean(daysSuitable, na.rm = T), cum.exp = mean(cum.exp, na.rm = T))
dat50s <- 
  met.dat.cc %>% filter(year >= 2050 & year <= 2059) %>% 
  group_by(Life.stage) %>%
  summarise(IWI = mean(IWI, na.rm = T), AWA = mean(AWA, na.rm = T), MWM = mean(MWM, na.rm = T), MWV = mean(MWV, na.rm = T),
            RNG = mean(RNG, na.rm = T), pExc = mean(pExc, na.rm = T), first.week = mean(first.week, na.rm = T),
            daysSuitable = mean(daysSuitable, na.rm = T), cum.exp = mean(cum.exp, na.rm = T))
dat80s <- 
  met.dat.cc %>% filter(year >= 2080 & year <= 2089) %>% 
  group_by(Life.stage) %>%
  summarise(IWI = mean(IWI, na.rm = T), AWA = mean(AWA, na.rm = T), MWM = mean(MWM, na.rm = T), MWV = mean(MWV, na.rm = T),
            RNG = mean(RNG, na.rm = T), pExc = mean(pExc, na.rm = T), first.week = mean(first.week, na.rm = T),
            daysSuitable = mean(daysSuitable, na.rm = T), cum.exp = mean(cum.exp, na.rm = T))
dat <- rbind(cbind(dat20s, "Period" = 2020), cbind(dat50s, "Period" = 2050), cbind(dat80s, "Period" = 2080))

dat2 <- subset(dat, Life.stage %in% c("enroute", "incubat", "prespawn", "outmigr1", "outmigr2", "outmigr3"))
dat2[,c(2:7,9:10)] <- round(dat2[,c(2:7,9:10)],1)

write.csv(dat2, "data/thermal_metrics_20s50s80s.csv")
