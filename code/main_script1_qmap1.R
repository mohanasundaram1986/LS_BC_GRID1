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
library(qmap)

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

#data splitting for fitting and testing
obs1 = yy_p3[[1:240]]
obs2 = yy_p3[[241:432]]
mod1 = xx_p3[[1:240]]
mod2 = xx_p3[[241:432]]

#quantile mapping method: ("Quant", "PTF","DIST","RQUANT","SSPLIN")
#Quant - Empirical cumulative distribution function
#PTF - Parametric quantile-quantile method
#Dist - Theoretical distribution
#Rquant - quantile-quantile relation by linear regression
#SSPLIN - smoothing spline quantile-quantile

qm.fit <- fitQmap((t(obs1[])), (t(mod1[])), method="RQUANT",qstep=0.1)

#type: interpolation types
#linear - linear interpolation
#tricub - spline interpolation

bias_corrected_qm <- doQmap(t(mod2[]), qm.fit, type="tricub")

bias_corrected_qm_arr <- as.array(t(bias_corrected_qm))

#reshape array
nrow1 = dim(mod2)[1]
ncol1 = dim(mod2)[2]
nlay1 = dim(mod2)[3]

dim(bias_corrected_qm_arr) <- c(nrow1,ncol1,nlay1)

# convert to rasterbrick (output- bias corrected layers)
mod_Bcorrected_qm <- setValues(brick(mod2,values=FALSE),bias_corrected_qm_arr)

#compare
plot(mod_Bcorrected_qm[[8]])

nlay1 = dim(obs2)[3]

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

#obs data for validation
obs_jan = obs2[[jan]]
obs_feb = obs2[[feb]]
obs_mar = obs2[[mar]]
obs_apl = obs2[[apl]]
obs_may = obs2[[may]]
obs_jun = obs2[[jun]]
obs_jul = obs2[[jul]]
obs_aug = obs2[[aug]]
obs_sep = obs2[[sep]]
obs_oct = obs2[[oct]]
obs_nov = obs2[[nov]]
obs_dec = obs2[[dec]]

#raw gcm data for validation
mod_jan = mod2[[jan]]
mod_feb = mod2[[feb]]
mod_mar = mod2[[mar]]
mod_apl = mod2[[apl]]
mod_may = mod2[[may]]
mod_jun = mod2[[jun]]
mod_jul = mod2[[jul]]
mod_aug = mod2[[aug]]
mod_sep = mod2[[sep]]
mod_oct = mod2[[oct]]
mod_nov = mod2[[nov]]
mod_dec = mod2[[dec]]

#biascorrected data for validation
bc_jan = mod_Bcorrected_qm[[jan]]
bc_feb = mod_Bcorrected_qm[[feb]]
bc_mar = mod_Bcorrected_qm[[mar]]
bc_apl = mod_Bcorrected_qm[[apl]]
bc_may = mod_Bcorrected_qm[[may]]
bc_jun = mod_Bcorrected_qm[[jun]]
bc_jul = mod_Bcorrected_qm[[jul]]
bc_aug = mod_Bcorrected_qm[[aug]]
bc_sep = mod_Bcorrected_qm[[sep]]
bc_oct = mod_Bcorrected_qm[[oct]]
bc_nov = mod_Bcorrected_qm[[nov]]
bc_dec = mod_Bcorrected_qm[[dec]]

#mean monthly average calculation
obs_jan1 = calc(obs_jan, mean)
obs_feb1 = calc(obs_feb, mean)
obs_mar1 = calc(obs_mar, mean)
obs_apl1 = calc(obs_apl, mean)
obs_may1 = calc(obs_may, mean)
obs_jun1 = calc(obs_jun, mean)
obs_jul1 = calc(obs_jul, mean)
obs_aug1 = calc(obs_aug, mean)
obs_sep1 = calc(obs_sep, mean)
obs_oct1 = calc(obs_oct, mean)
obs_nov1 = calc(obs_nov, mean)
obs_dec1 = calc(obs_dec, mean)

mod_jan1 = calc(mod_jan, mean)
mod_feb1 = calc(mod_feb, mean)
mod_mar1 = calc(mod_mar, mean)
mod_apl1 = calc(mod_apl, mean)
mod_may1 = calc(mod_may, mean)
mod_jun1 = calc(mod_jun, mean)
mod_jul1 = calc(mod_jul, mean)
mod_aug1 = calc(mod_aug, mean)
mod_sep1 = calc(mod_sep, mean)
mod_oct1 = calc(mod_oct, mean)
mod_nov1 = calc(mod_nov, mean)
mod_dec1 = calc(mod_dec, mean)

bc_jan1 = calc(bc_jan, mean)
bc_feb1 = calc(bc_feb, mean)
bc_mar1 = calc(bc_mar, mean)
bc_apl1 = calc(bc_apl, mean)
bc_may1 = calc(bc_may, mean)
bc_jun1 = calc(bc_jun, mean)
bc_jul1 = calc(bc_jul, mean)
bc_aug1 = calc(bc_aug, mean)
bc_sep1 = calc(bc_sep, mean)
bc_oct1 = calc(bc_oct, mean)
bc_nov1 = calc(bc_nov, mean)
bc_dec1 = calc(bc_dec, mean)

#Extracting spatial averages
mean_jan <- data.frame(obs.mean=cellStats(obs_jan1, "mean"),mod.mean=cellStats(mod_jan1, "mean"),bc.mean=cellStats(bc_jan1, "mean"))
mean_feb <- data.frame(obs.mean=cellStats(obs_feb1, "mean"),mod.mean=cellStats(mod_feb1, "mean"),bc.mean=cellStats(bc_feb1, "mean"))
mean_mar <- data.frame(obs.mean=cellStats(obs_mar1, "mean"),mod.mean=cellStats(mod_mar1, "mean"),bc.mean=cellStats(bc_mar1, "mean"))
mean_apl<- data.frame(obs.mean=cellStats(obs_apl1, "mean"),mod.mean=cellStats(mod_apl1, "mean"),bc.mean=cellStats(bc_apl1, "mean"))
mean_may <- data.frame(obs.mean=cellStats(obs_may1, "mean"),mod.mean=cellStats(mod_may1, "mean"),bc.mean=cellStats(bc_may1, "mean"))
mean_jun <- data.frame(obs.mean=cellStats(obs_jun1, "mean"),mod.mean=cellStats(mod_jun1, "mean"),bc.mean=cellStats(bc_jun1, "mean"))
mean_jul <- data.frame(obs.mean=cellStats(obs_jul1, "mean"),mod.mean=cellStats(mod_jul1, "mean"),bc.mean=cellStats(bc_jul1, "mean"))
mean_aug <- data.frame(obs.mean=cellStats(obs_aug1, "mean"),mod.mean=cellStats(mod_aug1, "mean"),bc.mean=cellStats(bc_aug1, "mean"))
mean_sep <- data.frame(obs.mean=cellStats(obs_sep1, "mean"),mod.mean=cellStats(mod_sep1, "mean"),bc.mean=cellStats(bc_sep1, "mean"))
mean_oct <- data.frame(obs.mean=cellStats(obs_oct1, "mean"),mod.mean=cellStats(mod_oct1, "mean"),bc.mean=cellStats(bc_oct1, "mean"))
mean_nov <- data.frame(obs.mean=cellStats(obs_nov1, "mean"),mod.mean=cellStats(mod_nov1, "mean"),bc.mean=cellStats(bc_nov1, "mean"))
mean_dec <- data.frame(obs.mean=cellStats(obs_dec1, "mean"),mod.mean=cellStats(mod_dec1, "mean"),bc.mean=cellStats(bc_dec1, "mean"))

mean_val = rbind(mean_jan,mean_feb,mean_mar,mean_apl,mean_may,mean_jun,
                 mean_jul,mean_aug,mean_sep,mean_oct,mean_nov,mean_dec)

#plotting for comparison
colors = topo.colors(20)
par(mfrow=c(1,2), tcl=-0.5, family="serif",oma = c(4,2,2,2))

par(mai=c(0.4,0.4,0.4,0.4))
# qqline(mean_val$obs.mean, col = "steelblue", lwd = 2)
plot(mean_val$obs.mean, mean_val$mod.mean,xlim = c(0,50),ylim = c(0,50),pch=21,  bg="black")
abline(0,1, col='red',lwd=2)
mtext(side=3, text="Raw GCM", line=1, cex = 1.0)
mtext(side=1, text="Observed Precipitation [mm/month]", line=2, cex = 1.0)
mtext(side=2, text="Raw Precipitation [mm/month]", line=2, cex = 1.0)

plot(mean_val$obs.mean, mean_val$bc.mean,xlim = c(0,50),ylim = c(0,50),pch=21,  bg="black")
abline(0,1, col='red',lwd=2)
mtext(side=3, text="QMap Bias corrected", line=1, cex = 1.0)
mtext(side=1, text="Observed Precipitation [mm/month]", line=2, cex = 1.0)
mtext(side=2, text="Bias corrected Precipitation [mm/month]", line=2, cex = 1.0)

#----------------------------#




