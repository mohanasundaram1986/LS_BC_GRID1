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
# library(qmap)

#study basin for clipping
basin1 <- readOGR("F:/OneDrive/AIT/papers/rainfall_biascorrection/gis/test.shp")
#Example data download link: https://1drv.ms/f/s!Al-1aCrnZtO1g_E7sgJxDYOQmd3SDA?e=cTtg0O

#observation(APHRODITE precipitation)
p1 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/aphrodite/data/"
#Example aphrodite data download link: https://1drv.ms/f/s!Al-1aCrnZtO1iq56T6YjoJuNOWL5fQ?e=Yzt8fJ
R1 <- list.files(p1, pattern = "nc$")
ap_rain <- raster::stack(file.path(p1, R1), varname = "precip")

#gcm data(MIROC6 Historical)
p2 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/cmip6_gcm/monthly/MIROC6/pr_hist/"
#Example gcm data download link:https://1drv.ms/f/s!Al-1aCrnZtO1i9coiEsKYke10vAXgw?e=H60dG0
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

#obs1: observed data for fitting; obs2:observed data for testing; mod1: modelled data for fitting; mod2: modelled data for testing
obs1 = yy_p3[[1:240]]
obs2 = yy_p3[[241:432]]

mod1 = xx_p3[[1:240]]
mod2 = xx_p3[[241:432]]

#number of layers in fitting data
nlay1 = dim(obs1)[3]

#monthly_indices
jun = seq(6, nlay1, 12)
jul = seq(7, nlay1, 12)
aug = seq(8, nlay1, 12)
sep = seq(9, nlay1, 12)
jan = seq(1, nlay1, 12)
oct = seq(10, nlay1, 12)
nov = seq(11, nlay1, 12)
dec = seq(12, nlay1, 12)
feb = seq(2, nlay1, 12)
mar = seq(3, nlay1, 12)
apl = seq(4, nlay1, 12)
may = seq(5, nlay1, 12)

#data splitting for fitting and testing
obs1 = yy_p3[[1:240]]
obs2 = yy_p3[[241:432]]
mod1 = xx_p3[[1:240]]
mod2 = xx_p3[[241:432]]

#bias correction
ls_bc1= ls_bc(mod1, obs1)

#----------------------------#

