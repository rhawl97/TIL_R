coinfunction = function(ITER){
  y = rep(NA, ITER)
  for(iter in 1:ITER){
    x_old = sample( c(0, 1), 1)
    x_new = sample( c(0, 1), 1)
    num_of_iter = 2
    while( x_old - x_new != 1 ){
      num_of_iter = num_of_iter + 1
      x_old = x_new
      x_new = sample( c(0,1), 1)
    }
    y[iter] = num_of_iter
  }
  return( mean(y) )
}

coinfunction(10^3)
coinfunction(10^4)
coinfunction(10^5)
coinfunction(10^6)