ls_bc = function(xx_p3,yy_p3){

n = nlayers(xx_p3)
 # xx_p3_bi = stack(month_gcm1_r_p1_bi)

 jun = seq(6, n, 12)
 jul = seq(7, n, 12)
 aug = seq(8, n, 12)
 sep = seq(9, n, 12)
 jan = seq(1, n, 12)
 oct = seq(10, n, 12)
 nov = seq(11, n, 12)
 dec = seq(12, n, 12)
 feb = seq(2, n, 12)
 mar = seq(3, n, 12)
 apl = seq(4, n, 12)
 may = seq(5, n, 12)


 xx_p3_jan = xx_p3[[jan]]
 xx_p3_feb = xx_p3[[feb]]
 xx_p3_mar = xx_p3[[mar]]
 xx_p3_apl = xx_p3[[apl]]
 xx_p3_may = xx_p3[[may]]
 xx_p3_jun = xx_p3[[jun]]
 xx_p3_jul = xx_p3[[jul]]
 xx_p3_aug = xx_p3[[aug]]
 xx_p3_sep = xx_p3[[sep]]
 xx_p3_oct = xx_p3[[oct]]
 xx_p3_nov = xx_p3[[nov]]
 xx_p3_dec = xx_p3[[dec]]

 xx_p3_jan1 <- calc(xx_p3_jan, fun = mean)
 xx_p3_feb1 <- calc(xx_p3_feb, fun = mean)
 xx_p3_mar1 <- calc(xx_p3_mar, fun = mean)
 xx_p3_apl1 <- calc(xx_p3_apl, fun = mean)
 xx_p3_may1 <- calc(xx_p3_may, fun = mean)
 xx_p3_jun1 <- calc(xx_p3_jun, fun = mean)
 xx_p3_jul1 <- calc(xx_p3_jul, fun = mean)
 xx_p3_aug1 <- calc(xx_p3_aug, fun = mean)
 xx_p3_sep1 <- calc(xx_p3_sep, fun = mean)
 xx_p3_oct1 <- calc(xx_p3_oct, fun = mean)
 xx_p3_nov1 <- calc(xx_p3_nov, fun = mean)
 xx_p3_dec1 <- calc(xx_p3_dec, fun = mean)

 #----------------
 yy_p3_jan = yy_p3[[jan]]
 yy_p3_feb = yy_p3[[feb]]
 yy_p3_mar = yy_p3[[mar]]
 yy_p3_apl = yy_p3[[apl]]
 yy_p3_may = yy_p3[[may]]
 yy_p3_jun = yy_p3[[jun]]
 yy_p3_jul = yy_p3[[jul]]
 yy_p3_aug = yy_p3[[aug]]
 yy_p3_sep = yy_p3[[sep]]
 yy_p3_oct = yy_p3[[oct]]
 yy_p3_nov = yy_p3[[nov]]
 yy_p3_dec = yy_p3[[dec]]

 yy_p3_jan1 <- calc(yy_p3_jan, fun = mean)
 yy_p3_feb1 <- calc(yy_p3_feb, fun = mean)
 yy_p3_mar1 <- calc(yy_p3_mar, fun = mean)
 yy_p3_apl1 <- calc(yy_p3_apl, fun = mean)
 yy_p3_may1 <- calc(yy_p3_may, fun = mean)
 yy_p3_jun1 <- calc(yy_p3_jun, fun = mean)
 yy_p3_jul1 <- calc(yy_p3_jul, fun = mean)
 yy_p3_aug1 <- calc(yy_p3_aug, fun = mean)
 yy_p3_sep1 <- calc(yy_p3_sep, fun = mean)
 yy_p3_oct1 <- calc(yy_p3_oct, fun = mean)
 yy_p3_nov1 <- calc(yy_p3_nov, fun = mean)
 yy_p3_dec1 <- calc(yy_p3_dec, fun = mean)

 #corection factors------------

 ls_jan = yy_p3_jan1/xx_p3_jan1
 ls_feb = yy_p3_feb1/xx_p3_feb1
 ls_mar = yy_p3_mar1/xx_p3_mar1
 ls_apl = yy_p3_apl1/xx_p3_apl1
 ls_may = yy_p3_may1/xx_p3_may1
 ls_jun = yy_p3_jun1/xx_p3_jun1
 ls_jul = yy_p3_jul1/xx_p3_jul1
 ls_aug = yy_p3_aug1/xx_p3_aug1
 ls_sep = yy_p3_sep1/xx_p3_sep1
 ls_oct = yy_p3_oct1/xx_p3_oct1
 ls_nov = yy_p3_nov1/xx_p3_nov1
 ls_dec = yy_p3_dec1/xx_p3_dec1

 xx_p3_bias_ls = list();
 plot(ls_dec)

 for (i in 1:n){
   #241:432
   print(i)
   # rcmv_r[[i]] = resample(month_rcm[[i]],r3,'bilinear')
   # apv_r[[i]] = resample(month_ap[[i]],r3,'bilinear')

   if(i%%12 == 1){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_jan

   } else if(i%%12 == 2){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_feb

   } else if(i%%12 == 3){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_mar

   } else if(i%%12 == 4){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_apl

   } else if(i%%12 == 5){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_may

   } else if(i%%12 == 6){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_jun

   } else if(i%%12 == 7){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_jul

   } else if(i%%12 == 8){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_aug

   } else if(i%%12 == 9){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_sep

   } else if(i%%12 == 10){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_oct

   } else if(i%%12 == 11){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_nov

   } else if(i%%12 == 0){
     xx_p3_bias_ls[[i]] = xx_p3[[i]] * ls_dec

   }  else {
     NULL
   }

 }
 xx_p3_bias_ls = stack(xx_p3_bias_ls)


 ls = stack(ls_jan,ls_feb,ls_mar,ls_apl,ls_may,ls_jun,ls_jul,ls_aug,ls_sep,ls_oct,ls_nov,ls_dec)

 out = stack(xx_p3_bias_ls)

 return(out)
}
