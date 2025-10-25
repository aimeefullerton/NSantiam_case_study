# Impute missing water temperature values from USGS-monitored time series
# Morgan Bond, March 2024

# Setup ----
library(dataRetrieval)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)
library(missForest)
library(misspi)
library(data.table)
library(neuralnet)

options (scipen = 999)

# Load temperature data for selected USGS gages ----
sites <- read_sf("data/shapefiles/Willamette_gauge_locations.shp")
site_no <- sites$site_no
site_no2 <- site_no[1:185]

parameterCd <- "00010" # Temperature
statCd<-"00003" #mean (see pg of DataRetrieval documentation for other options: max="00001")
startDate <- "" #earliest date available
endDate <- ""   #latest date available

wil_temp <- readNWISdv(site_no2, parameterCd, startDate, endDate,statCd=statCd)
wil_temp <- renameNWISColumns(wil_temp)
wil_temp$Month <- months(wil_temp$Date)
wil_temp$Year <- format(wil_temp$Date,format="%Y")

sites_all <- table(wil_temp$site_no)
sites_all <- as.data.frame(sites_all)

# Prepare data ----

# Move the old location temps to the regular temps for site 14192015
wil_temp$Wtemp[wil_temp$site_no == 14192015] <- wil_temp$Old.Location_Wtemp[wil_temp$site_no == 14192015]
wil_temp[wil_temp == -999999] <- NA

# Number of sites after 1990
wil_1990 <- subset(wil_temp, Date > "1990-01-01")
wil_1990 <- subset(wil_1990, !(is.na(Wtemp)))
wil_1990 <- select(wil_1990, site_no, Date, Wtemp)

# Look at the early data
wil_all <- subset(wil_temp, !(is.na(Wtemp)))

# Keep only some columns
wil_all <- select(wil_all, site_no, Date, Wtemp)

# Remove sites with < 365 data points
sites <- table(wil_all$site_no)
sites <-as.data.frame(sites) 
sites <- sites[sites$Freq > 300,]
hist(sites$Freq, breaks = 20)

# Merge to remove site data with < 365
wil_wil_all <- merge(wil_all,sites, by.x = c("site_no"), by.y = c("Var1"), all.x = T)
wil_all <- wil_all[complete.cases(wil_all$Freq),]
wil_all <- select(wil_all, site_no, Date, Wtemp)

# Long to wide
wwid <- reshape(wil_1990, idvar = c("Date"), timevar = "site_no", direction = "wide")
wwid <- wwid[order(wwid$Date),]
wwid_d <- subset(wwid, select = -c(Date) )
wwid_m <- as.matrix(wwid_d)

# Replace -999999 with NA
wwid_m[wwid_m == -999999] <- NA

# Imputation ----

iris.imp <- misspi(wwid_m, ncore = 32)

wwid_imp <- as.data.frame(iris.imp$x.imputed)
wwid_imp$Date <- wwid$Date
wwid_imp <- wwid_imp[order(wwid_imp$Date),]

imputed <- wwid_imp
head(imputed)
str(imputed)

# Output imputed data frame
# Identify columns starting with "Wtemp."
wtemp_cols <- grep("^Wtemp.", names(imputed))

# Melt the data frame using reshape
melted_df <- melt(as.data.table(imputed), 
                  id.vars = c(names(imputed)[-wtemp_cols]), 
                  measure.vars = wtemp_cols, 
                  var.name = "site_no", 
                  value.name = "Value")
# Cleam up site names
melted_df$site_no <- gsub("^.{0,6}", "", melted_df$variable)
# (Optional) Rename the "Variable" column to a more descriptive name (e.g., "Timepoint")
# melted_df$Variable <- "Timepoint"

# Final data set with imputed data
wil_1990_imputed<-select(melted_df, site_no, Date, Value)

# Plot one of the sites
# Subset McKenzie river sites 14162500,14159110
mcsub<-subset(wil_1990_imputed, site_no==c("14148000"))

mcsub %>%
  ggplot( aes(x=Date, y=Value, group=site_no, color=site_no)) +
  geom_line()

# Imputation, linear model approach ----
iris.imp_lm <- misspi(wwid_m, ncore = 32, model.train = lm)

wwid_imp_lm <- as.data.frame(iris.imp_lm$x.imputed)
wwid_imp_lm$Date <- wwid$Date
wwid_imp_lm <- wwid_imp_lm[order(wwid_imp_lm$Date),]

imputed_lm <- wwid_imp_lm
head(imputed_lm)
str(imputed_lm)

# Output imputed data frame
# Identify columns starting with "Wtemp."
wtemp_cols <- grep("^Wtemp.", names(imputed_lm))

# Melt the data frame using reshape
melted_df_lm <- melt(as.data.table(imputed_lm), 
                  id.vars = c(names(imputed_lm)[-wtemp_cols]), 
                  measure.vars = wtemp_cols, 
                  var.name = "site_no", 
                  value.name = "Value")

# Cleam up site names
melted_df_lm$site_no <- gsub("^.{0,6}", "", melted_df_lm$variable)
# (Optional) Rename the "Variable" column to a more descriptive name (e.g., "Timepoint")
# melted_df$Variable <- "Timepoint"
 
# Final data set with imputed data
wil_1990_imputed <- select(melted_df_lm, site_no, Date, Value)
write.csv(wil_1990_imputed, "data/imputed_data_rf_mod_1990.csv")

# Plot one of the sites
# Subset McKenzie river sites 14162500,14159110
mcsub <- subset(wil_1990_imputed, site_no == c("14148000"))
mcsub %>%
  ggplot( aes(x = Date, y = Value, group= site_no, color = site_no)) +
  geom_line()

summary(wil_1990_imputed)
sort(as.numeric(unique(site_no)))
