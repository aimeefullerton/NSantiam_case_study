# Functions for Willamette case study


# Set watershed-specific variables
fncSetWatershed <- function(subwat){
  
  if(subwat == "north-santiam") {
    subwat_dams <- c("Detroit", "BIG CLIFF")
    pop <- "North Santiam"
    rchs <- sort(LCM_Reaches[grep(pop, LCM_Reaches)])
    rchs <- c(rchs, "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
    subnm <- "nsan"
  }
  if(subwat == "south-santiam") {
    subwat_dams <- c("Green Peter", "Foster")
    pop <- "South Santiam"
    rchs <- sort(LCM_Reaches[grep(pop, LCM_Reaches)])
    rchs <- c(rchs, "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
    subnm <- "ssan"
  }
  if(subwat == "mckenzie") {
    subwat_dams <- c("Cougar", "Blue River Dam")
    pop <- "McKenzie"
    rchs <- sort(LCM_Reaches[grep(pop, LCM_Reaches)])
    rchs <- c(rchs, "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
    subnm <- "mckz"
  }
  if(subwat == "mf-willamette") {
    subwat_dams <- c("Hills Creek", "DEXTER")
    pop <- "Middle Fork"
    rchs <- sort(LCM_Reaches[grep(pop, LCM_Reaches)])
    rchs <- c(rchs, "Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
    subnm <- "mfrk"
  }
  if(subwat == "mainstem"){
    subwat_dams <- NA
    pop <- "Mainstem"
    rchs <- c("Mainstem_Mfk2Mck", "Mainstem_Mck2San_US", "Mainstem_Mck2San_DS", "Mainstem_San2Falls_US", "Mainstem_San2Falls_DS", "Mainstem_Falls2Mouth")
    subnm <- "mstm"
  }
  
  if(length(grep("Migration", rchs)) > 0) rchs <- rchs[-grep("Migration", rchs)] #remove Dam reaches
  
  out <- list(pop, rchs, subwat_dams, subnm)
  names(out) <- c("pop", "rchs", "dams", "subnm")
  return(out)
  
}

# Set life-stage/species specific variables
fncSetLifestage <- function(species = "Chinook", life.stage, yy, thedata, rchs){
  
if(species == "Chinook"){
  if(life.stage == "enroute") {
    stdat = as.Date(paste0(yy,"-04-15"))
    endat = as.Date(paste0(yy,"-07-31"))
    th = 20
    thetitle <-  paste0(species, " adult migration 15 Apr - 31 Jul, >", th, "C")
    ls.rchs <- rchs[-grep(pop, rchs)]
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% ls.rchs])
  } else if(life.stage == "prespawn") {
    stdat = as.Date(paste0(yy,"-07-01"))
    endat = as.Date(paste0(yy,"-09-15"))
    th = 16
    thetitle <-  paste0(species, " prespawn holding 1 Jul - 15 Sep, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$CH_SP_RE == 1])
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "incubat") {
    stdat = as.Date(paste0(yy,"-09-15"))
    endat = as.Date(paste0(yy + 1,"-01-31"))
    th = 13
    thetitle <- paste0(species, " egg incubation 15 Sep - 31 Jan, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$CH_SP_RE == 1])
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "outmigr1") { #subyearling spring/summer
    stdat = as.Date(paste0(yy + 1,"-02-01"))
    endat = as.Date(paste0(yy + 1,"-06-30"))
    th = 18 
    thetitle <-  paste0(species, " subyearling smolt migration 1 Feb - 30 Jun, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & (nsan_streams$CH_MI_RE == 1 | nsan_streams$CH_SP_RE == 1)])
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "outmigr2") { #subyearling fall
    stdat = as.Date(paste0(yy + 1,"-07-01"))
    endat = as.Date(paste0(yy + 1,"-12-31"))
    th = 18 
    thetitle <-  paste0(species, " subyearling smolt migration 1 Jul - 31 Dec, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & (nsan_streams$CH_MI_RE == 1 | nsan_streams$CH_SP_RE == 1)])
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "outmigr3") { #yearlings following spring
    stdat = as.Date(paste0(yy + 2,"-01-01"))
    endat = as.Date(paste0(yy + 2,"-06-30"))
    th = 18 
    thetitle <-  paste0(species, " yearling smolt migration 1 Jan - 30 Jun, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & (nsan_streams$CH_MI_RE == 1 | nsan_streams$CH_SP_RE == 1)])
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  }
}
if(species == "Omykiss"){
  #https://www.st.nmfs.noaa.gov/data-and-tools/Salmon_CVA/pdf/Salmon_CVA_Name_Upper_Willamette_River_steelhead.pdf
  if(life.stage == "enroute") {
    stdat = as.Date(paste0(yy,"-03-01"))
    endat = as.Date(paste0(yy,"-04-30"))
    th = 20
    thetitle <-  paste0(species, " adult migration 1 Mar - 30 Apr, >", th, "C")
    ls.rchs <- rchs[-grep(pop, rchs)]
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% ls.rchs])
  } else if(life.stage == "prespawn") {
    stdat = as.Date(paste0(yy,"-04-01"))
    endat = as.Date(paste0(yy,"-06-30"))
    th = 16
    thetitle <-  paste0(species, " prespawn holding 1 Apr - 30 Jun, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$SH_SP_RE == 1])
    # adding above dams where Chinook are to see habitat quality there in case of passage:
    idx <- c(idx, unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$CH_SP_RE == 1 & nsan_streams$LCM_Reach %in% c("North Santiam Reach D", "North Santiam Reach E")]))
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "incubat") {
    stdat = as.Date(paste0(yy,"-04-15"))
    endat = as.Date(paste0(yy + 1,"-08-15"))
    th = 13
    thetitle <- paste0(species, " egg incubation 15 Apr - 15 Aug, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$SH_SP_RE == 1])
    # adding above dams where Chinook are to see habitat quality there in case of passage:
    idx <- c(idx, unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$CH_SP_RE == 1 & nsan_streams$LCM_Reach %in% c("North Santiam Reach D", "North Santiam Reach E")]))
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  } else if(life.stage == "outmigr1") { #subyearling spring/summer
    stdat = as.Date(paste0(yy + 2,"-02-01"))
    endat = as.Date(paste0(yy + 2,"-06-30"))
    th = 18 
    thetitle <-  paste0(species, " subyearling smolt migration 1 Feb - 30 Jun, >", th, "C")
    idx <- unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & (nsan_streams$SH_RE_MI == 1 | nsan_streams$SH_SP_RE == 1)])
    # adding above dams where Chinook are to see habitat quality there in case of passage:
    idx <- c(idx, unique(nsan_streams$COMID[nsan_streams$LCM_Reach %in% rchs & nsan_streams$CH_MI_RE == 1 & nsan_streams$LCM_Reach %in% c("North Santiam Reach D", "North Santiam Reach E")]))
    ls.rchs <- unique(nsan_streams$LCM_Reach[nsan_streams$COMID %in% idx])
  }
    # Need to add outmigration windows...
}

if(!is.null(thedata)) thedata <- thedata[thedata$COMID %in% idx & thedata$tim.date >= stdat & thedata$tim.date <= endat,]

out <- list(stdat, endat, th, ls.rchs, thetitle, thedata)
names(out) <- c("stdat", "endat", "th", "ls.rchs", "thetitle", "thedata")
return(out)
}

# Set title
fncTitle <- function(species = "Chinook", life.stage){
  
  if(species == "Chinook"){
    if(life.stage == "enroute") {
      thetitle <-  paste0(species, " adult migration 15 Apr - 31 Jul") #20C
    } else if(life.stage == "prespawn") {
      thetitle <-  paste0(species, " prespawn holding 1 Jul - 15 Sep") #16C
    } else if(life.stage == "incubat") {
      thetitle <- paste0(species, " egg incubation 15 Sep - 31 Jan") #13C
    } else if(life.stage == "outmigr1") { #subyearling spring/summer
      thetitle <-  paste0(species, " subyearling smolt migration 1 Feb - 30 Jun") #18C
    } else if(life.stage == "outmigr2") { #subyearling fall
      thetitle <-  paste0(species, " subyearling smolt migration 1 Jul - 31 Dec") #18C
    } else if(life.stage == "outmigr3") { #yearlings following spring
      thetitle <-  paste0(species, " yearling smolt migration 1 Jan - 30 Jun,") #18C
    }
  }
  if(species == "Omykiss"){
    #https://www.st.nmfs.noaa.gov/data-and-tools/Salmon_CVA/pdf/Salmon_CVA_Name_Upper_Willamette_River_steelhead.pdf
    if(life.stage == "enroute") {
      thetitle <-  paste0(species, " adult migration 1 Mar - 30 Apr")
    } else if(life.stage == "prespawn") {
      thetitle <-  paste0(species, " prespawn holding 1 Apr - 30 Jun")
    } else if(life.stage == "incubat") {
      thetitle <- paste0(species, " egg incubation 15 Apr - 15 Aug")
    } else if(life.stage == "outmigr1") { #subyearling spring/summer
      thetitle <-  paste0(species, " subyearling smolt migration 1 Feb - 30 Jun")
    }
    # Need to add outmigration windows...
  }
  
  return(thetitle)
}

# # Plot map legend function for use with sf plots
addLegendToSFPlot <- function(value_range = c(0, 1), num_cats = 2, palette = c("blue", "red"), adjX = 0, adjY = 0, rnd = 1, ...){
  #modified from: https://stackoverflow.com/questions/52975447/reorganize-sf-multi-plot-and-add-a-legend
  # Get the axis limits and calculate size
  axisLimits <- par()$usr
  xLength <- axisLimits[2] - axisLimits[1]
  yLength <- axisLimits[4] - axisLimits[3]
  adjX <- adjX * xLength
  adjY <- adjY * yLength
  
  # Define values cuts and make pretty labels
  values <- seq(value_range[1], value_range[2], length.out = num_cats)
  if(max(values) >= 1){
    labels <- round(values, rnd)
  } else if(class(values) == "Date"){
    labels <- paste0(lubridate::month(values,label = T), " ", lubridate::day(values))
  } else {
    labels <- paste(" ", sprintf("%02d", round(seq(value_range[1], value_range[2], length.out = num_cats))))
    labels2 <- paste(" ", sprintf("%02d", seq(value_range[1], value_range[2], by = 5)))
    if(length(labels) > num_cats) labels <- labels2
    if(any(duplicated(labels))) labels <- round(seq(value_range[1], value_range[2], length.out = num_cats),1)
  }
  
  # Define the colour palette
  colourPalette <- leaflet::colorNumeric(palette, range(values))
  
  # Add the legend
  plotrix::color.legend(xl = axisLimits[1] + adjX - 0.025 * xLength, xr = axisLimits[1] + adjX,
                        yb = axisLimits[3] + adjY, yt = axisLimits[3] + 0.4 * yLength + adjY,
                        legend = labels, rect.col = colourPalette(values), 
                        gradient = "y", align = 'rb', cex = 0.8)
} 

# Weekly Metrics
fnc_Weekly <- function(frame, site, start.date, end.date, st.col, site.col = "COMID", date.col = "tim.date", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[,date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[,st.col]))){ #proceed only if there are data
    stlist <- frame[,st.col]
    minlist <- meanlist <- maxlist <- rep(0, (length(datelist) - 6))
    for (i in 1:(length(datelist) - 6)){
      if(any(!is.na(stlist[i:(i + 6)]))){
        minlist[i] <- min(stlist[i:(i + 6)], na.rm = T)
        meanlist[i] <- mean(stlist[i:(i + 6)], na.rm = T)
        maxlist[i] <- max(stlist[i:(i + 6)], na.rm = T)
      } else {minlist[i] <- meanlist[i] <- maxlist[i] <- NA}
    }
    iwit <- min(minlist, na.rm = T) # minimum weekly minimum
    awat <- mean(meanlist, na.rm = T) # average weekly average
    mwat <- max(meanlist, na.rm = T) # maximum weekly average
    awmt <- mean(maxlist, na.rm = T) # average weekly maximum; this is 7DADM
    mwmt <- max(maxlist, na.rm = T) # maximum weekly maximum
    awit <- mean(minlist, na.rm = T) # average weekly minimum
    
    out <- c(iwit, awat, mwat, awmt, mwmt, awit)
    names(out) <- c("IWI", "AWA", "MWA", "AWM", "MWM", "AWI")
    return(out)	
  } else{ return (NA)}
}

# Variance Metrics
fnc_Variance <- function(frame, site, start.date, end.date, st.col, site.col = "COMID", date.col = "tim.date", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[,date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[,st.col]))){ #proceed only if there are data
    stlist <- frame[,st.col]
    varlist <- rep(0, (length(datelist) - 6))
    for (i in 1:(length(datelist) - 6)){
      if(any(!is.na(stlist[i:(i + 6)]))){
        varlist[i] <- var(stlist[i:(i + 6)], na.rm = T)
      } else {varlist[i] <- NA}
    }
    iwv <- min(varlist, na.rm = T) # minimum weekly variance
    if(is.na(iwv) | is.infinite(iwv)) iwv <- NA
    awv <- mean(varlist, na.rm = T) # average weekly variance
    if(is.na(awv) | is.infinite(awv)) awv <- NA
    mwv <- max(varlist, na.rm = T) # maximum weekly variance
    if(is.na(mwv) | is.infinite(mwv)) mwv <- NA
    var <- var(stlist, na.rm = T) # raw variance
    rng <- range(stlist, na.rm = T)[2] - range(stlist, na.rm = T)[1] #range of raw variance
    
    out <- c(iwv, awv, mwv, var, rng)
    names(out) <- c("IWV", "AWV", "MWV", "VAR", "RNG")
    return(out)	
  } else{ return (NA)}
}

# No. of days with values above or below a threshold 
fnc_pDays.Unsuitable <- function(frame, site, start.date, end.date, XX, st.col, site.col = "COMID", date.col = "tim.date", sign = "GT", show.na = T){
  
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  period <- as.numeric(end.date - start.date) + 1
  
  if(any(!is.na(frame[, st.col]))){ #proceed only if there are data
    if(sign == "GT"){
      dayslist <- frame[!is.na(frame[, st.col]) & frame[, st.col] >= XX,]
    } else if(sign == "LT"){
      dayslist <- frame[!is.na(frame[, st.col]) & frame[, st.col] <= XX,]
    }
    
    return(length(unique(dayslist[, date.col]))/period)
  } else{ return(NA)}
}

# The first week to sustain values above or below a threshold
fnc_1stWk.Unsuitable <- function(frame, site, start.date, end.date, XX, st.col, site.col = "COMID", date.col = "tim.date", sign = "GT", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[,st.col]))){ #proceed only if there are data
    #for each day in datelist, count number of records above XX degrees
    daysabovelist <- rep(0, length(datelist))
    XXlist = rep(0, (length(datelist) - 6))
    for (i in 1:length(datelist)){
      if(sign == "GT"){
        daysabovelist[i] <- sum(frame[frame[, date.col] == datelist[i], st.col] > XX, na.rm = T) 
      } else if(sign == "LT") {
        daysabovelist[i] <- sum(frame[frame[, date.col] == datelist[i], st.col] < XX, na.rm = T) 
      }
    }
    for (i in 1:(length(datelist) - 6)){
      XXlist[i] <- sum(daysabovelist[i:(i + 6)])
    }
    
    if(any(XXlist > 0)){
      return(datelist[which.max(XXlist)])
    } else{ return (as.Date("1900-01-01"))}
    
  } else{ return (as.Date("1900-01-01"))}
}

# Date at which cumulative exposure surpasses or goes below a threshold
fnc_Date.Unsuitable <- function(frame, site, start.date, end.date, XX, st.col, site.col = "COMID", date.col = "tim.date", sign = "GT", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[, st.col]))){ #proceed only if there are data
    cumexp <- 0; i = 1
    if(sign == "GT"){
      while(cumexp < XX & i <= length(datelist)){
        cumexp <- sum(cumexp, frame[frame[, date.col] == datelist[i], st.col], na.rm = T) 
        i <- i + 1
      }
      if(cumexp > XX) return(datelist[i]) else return (as.Date("1900-01-01"))
    } else if(sign == "LT"){
      while(cumexp > XX & i <= length(datelist)){
        cumexp <- sum(cumexp, frame[frame[, date.col] == datelist[i], st.col], na.rm = T) 
        i <- i + 1
      }
      if(cumexp < XX) return(datelist[i]) else return (as.Date("1900-01-01"))
    }
    
  } else{ return (as.Date("1900-01-01"))}
}

# Cumulative exposure (for temperature, this is degrees-days)
fnc_Cum.Exposure <- function(frame, site, start.date, end.date, st.col, site.col = "COMID", date.col = "tim.date", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[, st.col]))){ #proceed only if there are data
    cumexp <- aggregate(frame[,st.col], list(frame$COMID), sum, na.rm = T)
    colnames(cumexp) <- c("COMID", st.col)
    
    return(cumexp[, st.col])
  } else{ return (NA)}
}

# Cumulate days in unsuitable range
fnc_Cum.Days.Unsuitable <- function(frame, site, start.date, end.date, XX, st.col, site.col = "COMID", date.col = "tim.date", sign = "GT", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])

  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[, st.col]))){ #proceed only if there are data
    if(sign == "GT"){
      # Find indices where values exceed threshold
      exceed_indices <- which(frame[,st.col] > XX)
      
    } else if(sign == "LT"){
      # Find indices where values exceed threshold
      exceed_indices <- which(frame[,st.col] < XX)
    }
    
    # Calculate consecutive durations of exceedances
    consecutive_durations <- rle(exceed_indices)$lengths
    longest_duration <- sum(consecutive_durations)
    
    return(longest_duration)
  } else {return(0)}
}

#No. of days with temperatures within temperature range XX to YY during facet period
fnc_Days.in.Range = function(frame, site, start.date, end.date, XX, YY, st.col, site.col = "COMID", date.col = "tim.date", show.na = T){
  frame <- as.data.frame(frame)
  frame <- frame[frame[, site.col] == site,]
  frame <- frame[frame[, date.col] >= start.date & frame[, date.col] <= end.date,]
  frame <- frame[order(frame[, date.col]),]
  datelist <- sort(unique(frame[, date.col]))
  na.list <- which(is.na(frame[, st.col]))
  na.datelist <- unique(frame[, date.col][na.list])
  
  if(show.na){cat("There are", length(na.datelist),"missing days  \n", as.character(na.datelist), "\n")}
  
  if(any(!is.na(frame[, st.col]))){ #proceed only if there are data
    frame <- frame[!is.na(frame[,st.col]),]
    return(nrow(frame[frame[,st.col] >= YY & frame[,st.col] <= XX,]))
  } else {
    return(0)
  }
}

# COLORS 
fncColors4Quantiles <- function(){
  #Min/max
  c1.0<- rgb(226,226,226,150,NULL,255) #hex #e2e2e2, light gray
  c1.1<- rgb(252,217,156,150,NULL,255) #hex #fcd99c96 orange
  c1.2<- rgb(247,246,187,150,NULL,255) #hex #f7f6bb96 yellow
  c1.3<- rgb(176,191,252,150,NULL,255) #hex #b0bffc96 blue
  c1.4<- rgb(209,167,207,150,NULL,255) #hex #d1a7cf purple
  c1.5<- rgb(196,237,197,150,NULL,255) #hex #e2e2e2 green
  c1.6<- rgb(171,201,205,150,NULL,255) #hex #abc9cd aqua
  
  #Q1/Q3
  c2.0<- rgb(142,142,142,200,NULL,255) #hex #8e8e8e, gray
  c2.1<- rgb(234,173,68,200,NULL,255) #hex #eaad44c8 orange
  c2.2<- rgb(239,237,95,200,NULL,255) #hex #efed5fc8 yellow
  c2.3<- rgb(128,153,252,200,NULL,255) #hex #809ffcc8 blue
  c2.4<- rgb(137,101,136,200,NULL,255) #hex #896588 purple
  c2.5<- rgb(81,198,83,200,NULL,255) #hex #8e8e8e green
  c2.6<- rgb(74,146,155,200,NULL,255) #hex #4a929b aqua
  
  #Median
  c3.0<- rgb(5,5,5,255,NULL,255) #black
  c3.1<- rgb(244,155,2,255,NULL,255) #hex #f49b02ff orange
  c3.2<- rgb(206,193,8,255,NULL,255) # hex #cec108ff yellow
  c3.3<- rgb(3,38,178,255,NULL,255) #hex #0326b2ff blue
  c3.4<- rgb(97,18,104,255,NULL,255) #hex #611268 purple
  c3.5<- rgb(1,137,3,255,NULL,255) #green
  c3.6<- rgb(66,110,130,255,NULL,255) #hex #426e82 aqua
  
  c1 <- c(c1.0, c2.0, c3.0)
  c2 <- c(c1.1, c2.1, c3.1)
  c3 <- c(c1.2, c2.2, c3.2)
  c4 <- c(c1.3, c2.3, c3.3)
  c5 <- c(c1.4, c2.4, c3.4)
  c6 <- c(c1.5, c2.5, c3.5)
  c7 <- c(c1.6, c2.6, c3.6)

  return(list(c1, c2, c3, c4, c5, c6, c7))
}

# Wrap long legend
fncWrapLegend <- function(legend_text, legwidth, col, cx = 1){
  
  #plot(1:5, 1:5, type = 'n', axes = F, xlab = "", ylab = "")
  
  # Split the legend text into multiple lines
  legend_text_lines <- strwrap(legend_text, width = legwidth)  # Adjust the width as needed
  
  # Calculate the number of lines in the legend
  num_lines <- length(legend_text_lines)
  
  # Calculate the coordinates for the legend
  legend_x <- 1950  # Adjust the x-coordinate
  legend_y <- 1.25 # Adjust the y-coordinate
  line_height <- 0.1  # Adjust the height between lines
  
  # Add each line of the legend
  legend(legend_x, legend_y, 
         legend = legend_text_lines[1], cex = cx, lwd = 2, col = col, bty = "n")
  for (i in 2:num_lines) {
    legend(legend_x, legend_y - (i - 1) * line_height, 
           legend = legend_text_lines[i], cex = cx, lwd = 2, col = NA, bg = NA, bty = "n")
  }
}

# Plot downstream and upstream reaches from a starting COMID
fncPlotReaches <- function(streams, start_comid, mouth_ds_segs){
  #library(nhdplusTools); library(sf)
  
  # Use nhdplustools package to get downstream and upstream COMIDs
  ds.comids <- c(get_DM(streams, start_comid), mouth_ds_segs)
  us.comids <- get_UT(streams, start_comid) 
  
  # Map
  plot(sf::st_geometry(sf::st_zm(streams)), col = "gray")
  plot(sf::st_geometry(sf::st_zm(dplyr::filter(streams, COMID %in% ds.comids))), add = T, col = 4) #downstream
  plot(sf::st_geometry(sf::st_zm(dplyr::filter(streams, COMID %in% us.comids))), add = T, col = 3) #upstream
  
  return(ds.comids)
}


