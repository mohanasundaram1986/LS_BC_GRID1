library(raster)
library(rgdal)
library(sf)
library(dplyr)
library(spData)
library(gstat)
library(tmap)
library(maptools)
library(readxl)
library(raster)
library(ggplot2)
library(rasterVis)
library(gridExtra)
library(ncdf4)

library(raster)
library(gstat)
library(sp)

#study basin for clipping
basin1 <- readOGR("F:/OneDrive/AIT/papers/rainfall_biascorrection/gis/test.shp")

#observation(APHRODITE precipitation)
#link for Aphrodite precipitation data (https://climatedataguide.ucar.edu/climate-data/aphrodite-asian-precipitation-highly-resolved-observational-data-integration-towards)

p1 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/aphrodite/data/"
R1 <- list.files(p1, pattern = "nc$")
ap_rain <- raster::stack(file.path(p1, R1), varname = "precip")

#gcm data(MIROC6 Historical)
#link for GCM MIROC6 data (https://esgf-node.llnl.gov/projects/cmip6/)
p2 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/cmip6_gcm/monthly/MIROC6/pr_hist/"
R2 <- list.files(p2, pattern = "nc$")
gcm1_rain_hist <- raster::stack(file.path(p2, R2), varname = "pr")

#crop to study area
s1_crop <- crop(ap_rain,basin1)

#prepare data from 1970-2005
s1_ap = s1_crop[[6941:20089]]
s1_ap1 = s1_ap

#convert daily data to monthly
idx_1_a <- seq(as.Date('1970-01-01'), as.Date('2005-12-31'), 'day')
idx_2_a <- seq(as.Date('1970-01-01'), as.Date('2005-12-31'), 'month')
idx_3_a <- seq(1,12)

names(s1_ap1)=idx_1_a

indices_a <- format(as.Date(names(s1_ap1), format = "X%Y.%m.%d"), format = "%y.%m")
indices_a <- as.numeric(indices_a)

#sum layers
month_ap<- stackApply(s1_ap1, indices_a, fun = sum)
names(month_ap) <- idx_2_a

#GCM monthly rain
s1_gcm1 = gcm1_rain_hist[[241:672]]*86400*30

#resampling empty list
month_gcm1_r =list();

# resampling
for (i in 1:432){
  #241:432
  print(i)
  month_gcm1_r[[i]] = resample(s1_gcm1[[i]],month_ap[[1]],'bilinear')

}

#assign gcm and obs to xx and yy varaibles
yy_p3 = stack(month_ap)
xx_p3 = stack(month_gcm1_r)

#monthly_indices
jun = seq(6, 240, 12)
jul = seq(7, 240, 12)
aug = seq(8, 240, 12)
sep = seq(9, 240, 12)
jan = seq(1, 240, 12)
oct = seq(10, 240, 12)
nov = seq(11, 240, 12)
dec = seq(12, 240, 12)
feb = seq(2, 240, 12)
mar = seq(3, 240, 12)
apl = seq(4, 240, 12)
may = seq(5, 240, 12)


#Bias correct
bc_ls1 = ls_bc(xx_p3,yy_p3)
