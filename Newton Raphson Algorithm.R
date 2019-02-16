#뉴튼랩슨은 여러 해 중 하나의 해를 부정확하게 찾아줌!
#무한루프 돌지 않기 위해 maximum iteration 필요
#x가 특정 범위를 벗어나지 않아야 함
#근에 가기 전에 기울기가 0 인 x 등장할 수 있음
#while 조건 : maximum iteration, x_n+1과 x_n의 차이가 작을 경우, x_n의 기울기가 0일 경우
## condition 1: f(X) == 0
## condition 2 : abs(x.new - x.old) is small
## condition 3 : iterations < 10^6

f = function(x){
  5*x^5 + 3*x^3 + 2*x^2 + x + 1
}
df = function(x){
  25*x^4 + 9*x^2 + 4*x + 1
}


newton_method = function(f,df,x.old,max_iter = 10^6,threshold =(0.1)^10){
  
  x.new = x.old - f(x.old)/df(x.old)
  iter = 1
  while( f(x.new) != 0 & abs(x.old-x.new) > threshold & iter < max_iter  ){  
    x.new = x.old - f(x.old)/df(x.old)
    iter = iter+1
    
  } 
  print(paste("result of f(x) = ", f(x.new)))
  return( c( x.new, iteration = iter ) )
}
newton_method(f, df, x.old = 100, threshold = 0) #근이 두 개인 경우 initial value를 바꿔가며 근을 찾을 수 있음


#최종 newton method
newton_method <- function(f, df, x_0, stop_criteria = .Machine$double.eps, max_iter = 10^3, left_b = -Inf, right_b = Inf){ 
  
  # criteria for starting point x_0 
  
  if (left_b > x_0 | right_b < x_0){ 
    
    warning("your starting point is out of bound, please try point in the bounds") 
    
    return(NA) 
    
  } 
  
  
  
  # variable initialization 
  
  iters <- 0 
  
  #x_0 <- sample(1:100, 1) 
  
  distance <- Inf 
  
  # do while until # 1) x does not move 
  
  # 2) exceed the maximun iteration 
  
  # 3) the function value become close to zero 
  
  while ( 
    
    (distance > stop_criteria) 
    
    & (iters < max_iter) 
    
    & (abs(f(x_0)) > stop_criteria) 
    
  ){ 
    
    # newton method 
    
    x_1 <- x_0 - ( f(x_0) / df(x_0) ) 
    
    
    
    # out of bound case 
    
    if (x_1 <= left_b) { 
      
      x_1 <- left_b 
      
    } 
    
    if (right_b <= x_1){ 
      
      x_1 <- right_b 
      
    } 
    
    distance <- abs(x_0 - x_1) 
    
    iters <- iters + 1 
    
    x_0 <- x_1 
    
  } 
  
  # return the final point 
  
  if (x_0 == left_b | x_0 == right_b){ 
    
    warning("\n", "Final point is on the boundary", "\n") 
    
  } 
  
  if (iters > max_iter){ 
    
    warning("\n", "Cannot find the root during the iteration.", "\n") 
    
  } 
  
  cat("Final value of function:", f(x_0), "\n") 
  
  return(  c(root = x_0, num_of_iterations = iters )  )
  
}  
#뉴튼 랩슨 method는 항상 input으로 f(x), df(x), x0(initial value)가 필요 


##최솟값찾기###
f = function(x){
  (x-1)^6+3*(x-1)^4+8*(x-1)^2+1
}
df = function(x){
  6*(x-1)^5+12*(x-1)^3+16*(x-1)
}

df2 = function(x){
  30*(x-1)^4+36*(x-1)^2+16
}
newton_method(df,df2, 1000)

##f가 sin function일 때
f = function(x){
  sin(x)
}
df = function(x){
  cos(x)
}

df2 = function(x){
  -sin(x)
}
newton_method(df,df2, 1)  #cos function 일차 근사함수가 정확히 표현을 못해 0은 안 나옴
#미분값이 0이 되는 것만 고려했기 때문에 root 가 maximize값으로 잘못 찾아줌 (pi/2 = 1.570796)
newton_method(df,df2, 4, left_b = pi, right_b = 2*pi)  #boundary설정해주니 잡아줌 (pi*3/2 = 4.712389), initial value 중요함
#f(1.570796), f(4.712389) 처럼 대입하면 최솟값인지 최대값인지 알 수 있음