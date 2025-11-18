# North Santiam case study: thermal habitats above and below dams

### Aimee H Fullerton, Morgan Bond, and Jeff Jorgensen

aimee.fullerton\@noaa.gov

This repository includes scripts used to modify daily predictions of past and future stream temperatures for the North Santiam River (Willamette) and compute thermal habitats for salmon above and below dams using data from Siegel et al. 2023, PLoS Water 2(8) 30000119 and Fullerton et al. in prep. Below are listed each script and the associated the data files called by the script, followed by the data's source (listed only on its the first appearance). Data necessary for running scripts are available and described at https://doi.org/10.5281/zenodo.17635460.

### 1_impute_ST_usgs_gages.R

shapefiles/Willamette_gauge_locations.shp : USGS National Water Dashboard

### 2_adjust_ST_below_dams.R

COMID_to_HUC12.csv : Siegel et al. (2023)

shapefiles/PNW_DAMS : National Dam Inventory

willamette_temperature_gage_info.csv : USGS National Water Dashboard

NHDPlus17/NHDSnapshot/Hydrography/NHDFlowline.shp  & …/PlusFlowlineVAA.dbf : National Hydrography Dataset v2

Willamette_LCM_COMIDs.csv : Subset of NHD v2 streamlines used in NOAA Fisheries salmon life cycle models

Retrospective & future stream temperatures predicted for HUC 170900 : Riverscape Data Exchange https://data.riverscapes.net/pt/streamtemp

imputed_data_rf_mod_1990.csv : Produced in step 1

### 3_predict_pAdj_retro.R

spatial_data.csv : Siegel et al. (2023)

170900_retrospective.fst : Produced in step 2

### 4_predict_pAdj_GCMs.R

170900_retro_adj.fst :  Produced in step 3

### 5_data_processing_metrics.R

shapefiles/NHDv2_Willamette_SOgt2.shp : subset of NHD v2 streamlines

shapefiles/NHDv2_Willamette_LCM.shp : life cycle modeling boundaries, M. Bond, pers com

shapefiles/north-santiam_WBD.shp : NHD v2 Watershed Boundary Dataset

shapefiles/north-santiam_streams.shp : NHD v2 Hydrography

170900_retro_adj.fst : Produced in step 2

170900_cc_adj.fst : Produced in step 4

### 6_report_plots.R

### 7_manuscript_plots.R

Detroit_data.csv and Detroit_cc_data.csv : Produced in step 5

thermal_metrics_north-santiam.csv, thermal_metrics_north-santiam_cc.csv : Produced in step 5

thermal_metrics_north-santiam_Chinook.csv, thermal_metrics_north-santiam_Chinook_cc.csv : Produced in step 5

thermal_metrics_north-santiam_Omykiss.csv, thermal_metrics_north-santiam_Omykiss_cc.csv : Produced in step 5

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.
