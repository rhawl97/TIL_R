HW4 = function( Input ){
  
  n = floor(log(Input, base = 10))  #make 'n' the integer part of 'Input'
  
  a = rep(NA, n+1)  #make an empty vector, 'a'
  
  if(n>=1){
    
    for(i in 1:n){
      
      a[i] = Input%/%(10^(n-i+1)) # give 'a' the quotient of Input 
      
      Input = Input%%( 10^(n-i+1))  #In the next loop, 'Input' become the remainder of the previous 'Input'
      
    }
    
    a[n+1] = Input  #The final 'Input' of the above loop become the last element of 'a'
    
    l = length(a)
    
    Output = 0   #give 'Output' the initial value, 0
    
    for(i in 1:l){
      
      Output = Output + a[i]*3^(l-i)  #each element of 'a' multiplied by 3^0, 3^1, 3^2,..then total sum of it become 'Output'
      
    }
    
  return( Output )
  
  }
  
}
