#Q1.
HW21Function = function(ITER){
  y = rep(NA, ITER)
  for(iter in 1:ITER){
    x_one = sample( c(0, 1), 1)
    x_two = sample( c(0, 1), 1)
    x_three = sample( c(0,1), 1)
    num_of_iter = 3
    while( x_one + x_two + x_three != 3 ){
      num_of_iter = num_of_iter + 1
      x_one = x_two
      x_two = x_three
      x_three = sample( c(0, 1), 1)
    }
    y[iter] = num_of_iter
  }
  return( mean(y) )
}


#Q2.
HW22Function = function(ITER){
  y = rep(NA, ITER)
  for(iter in 1:ITER){
    x_one = sample( c(0, 1), 1)
    x_two = sample( c(0, 1), 1)
    x_three = sample( c(0,1), 1)
    num_of_iter = 3
    while( x_one + x_three - x_two != 2 ){
      num_of_iter = num_of_iter + 1
      x_one = x_two
      x_two = x_three
      x_three = sample( c(0, 1), 1)
    }
    y[iter] = num_of_iter
  }
  return( mean(y) )
}