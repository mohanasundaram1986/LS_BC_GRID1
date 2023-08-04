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

#study basin
basin1 <- readOGR("F:/OneDrive/AIT/papers/rainfall_biascorrection/gis/test.shp")
dem = raster('F:/OneDrive/AIT/papers/rainfall_biascorrection/data/cmip6_gcm/dem/elev.0.25-deg.nc')

#dem
dem_c <- crop(dem,basin1)
dem_m = mask(dem_c, basin1)
# dem_m_p1 =dem_m

#moving window
w=c(5,5)


# basin2 <- readOGR("F:/OneDrive/AIT/papers/rainfall_biascorrection/gis/mon_asia3_p1_n2.shp")
# basin1 <- readOGR("E:/Cloud/OneDrive/AIT/papers/rainfall_biascorrection/gis/ma_s3.shp")
# basin2 <- readOGR("C:/Users/ezhil/OneDrive/AIT/papers/rainfall_biascorrection/gis/mon_asia3_seg_sw.shp")

#observation
p1 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/aphrodite/data/"
R1 <- list.files(p1, pattern = "nc$")

ap_rain <- raster::stack(file.path(p1, R1), varname = "precip")

#gcm data
p2 <- "F:/OneDrive/AIT/papers/rainfall_biascorrection/data/cmip6_gcm/monthly/MIROC6/pr_hist/"
R2 <- list.files(p2, pattern = "nc$")

gcm1_rain_hist <- raster::stack(file.path(p2, R2), varname = "pr")

#crop to study area
s1_crop <- crop(ap_rain,basin1)
s1_ap = s1_crop[[6941:20089]]
s1_ap1 = s1_ap

#get the date from the names of the layers and extract the month
idx_1_a <- seq(as.Date('1970-01-01'), as.Date('2005-12-31'), 'day')
idx_2_a <- seq(as.Date('1970-01-01'), as.Date('2005-12-31'), 'month')
idx_3_a <- seq(1,12)

names(s1_ap1)=idx_1_a

indices_a <- format(as.Date(names(s1_ap1), format = "X%Y.%m.%d"), format = "%y.%m")
indices_a <- as.numeric(indices_a)

#sum layers
month_ap<- stackApply(s1_ap1, indices_a, fun = sum)
names(month_ap) <- idx_2_a

#mean_month
indices1_a <- format(as.Date(names(month_ap), format = "X%Y.%m.%d"), format = "%m")
indices1_a <- as.numeric(indices1_a)

month_ap1<- stackApply(month_ap, indices1_a, fun = mean)
names(month_ap1) = idx_3_a

indices2_a = names(month_ap1)
# indices2 = as.numeric(indices2)

#yearly rainfall
# year_ap = sum(month_ap1)

#GCM monthly rain
s1_gcm1 = gcm1_rain_hist[[241:672]]*86400*30
#
month_gcm1_r_p1 =list();
month_ap_r_p1 = list();
month_gcm1_r = list();
dem_m_r = resample(dem_m, month_ap[[1]],'bilinear')
dem_m1_p1 = stack(replicate(432,dem_m_r))

# dem_m1_p1= list();
# plot(month_ap_c_p1[[1]])
for (i in 1:432){
  #241:432
  print(i)
  # dem_m1_p1[[i]]=resample(dem_m_p1,dem_m_p1,'bilinear')
  # month_ap_r_p1[[i]] = resample(month_ap[[i]],dem_m_p1,'bilinear')
  # month_gcm1_r_p1[[i]] = resample(s1_gcm1[[i]],month_ap[[1]],'ngb')
  month_gcm1_r[[i]] = resample(s1_gcm1[[i]],month_ap[[1]],'bilinear')

}

yy_p3 = list()
xx_p3 = list()
yy_p3 = stack(month_ap)
xx_p3 = stack(month_gcm1_r)


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