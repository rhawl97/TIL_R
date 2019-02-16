HW5 = function(Input){
  
  if(Input == 0){
    Output = 0
  }else{
      
    n = floor(log(Input,base = 2))
    a = rep(0,(n+1))
    
    for(i in 1:(n+1)){
      a[i] = Input %/% 2^(n + 1 -i)
      Input = Input %% 2^(n + 1 -i)
    }
    
    Output = 0
    
    for(k in 1:(n+1)){
      Output = Output + a[k]*10^(n+1-k)
    }
    
  }
  return(Output)
}

