##숫자 추측해서 맞히기
numguess_function = function(lower=1,upper=50){  #default지정
  x = sample(lower:upper, 1)
  y = x-1  #x와 절대 안 같아지도록 초기 넘버 지정
  num_of_guess = 0
  
  while(x!=y){
    print(paste("Guess Any Number between",lower,"and",upper))
    
    y = scan(n=1)
    num_of_guess = num_of_guess+1
    if( x < y ){
      cat(y,"is larger")
      upper = (y-1)
    }else if(x > y){
      cat(y,"is smaller")
      lower = (y+1)
    }else{
      cat("correct")
    }
  }
  return(num_of_guess)
}
numguess_function(1,50)