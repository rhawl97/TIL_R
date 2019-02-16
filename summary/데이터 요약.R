HW6 = function( Input ){
  n = nrow(Input)
  p = ncol(Input)
  mean = median = quartile1 = range = IQR = variance = rep(0,ncol(Input))
  for(i in 1:ncol(Input)){
    if(is.numeric(Input[,i])==T){
      mean[i] = mean(Input[,i],na.rm=T)
      median[i] = median(Input[,i],na.rm = T)
      quartile1[i] = quantile(Input[,i],0.25,na.rm = T)
      range[i] = max(Input[,i])-min(Input[,i])
      IQR[i] = IQR(Input[,i],na.rm = T)
      variance[i] = var(Input[,i],na.rm=T)
    }
  }
  return(
    list(n = n,
         p = p,
         center = rbind(mean = mean, median = median, quartile1 = quartile1),
         dispersion = rbind( range = range, IQR = IQR, variance = variance)
    )
  )
}


f = function(x){
  a = x+2
  return(a)
}
f(2)
