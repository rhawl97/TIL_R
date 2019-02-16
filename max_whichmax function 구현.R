#max함수
my_max = function(x){
  n = length(x)
  for (i in 1:n) {
    if( sum( x[i]>x[-i] ) == length(x)-1 ){
      result = x[i]
    }
  }
  return(result)
}

#which.max함수
my_which.max = function(x){
  n = length(x)
  for (i in 1:n) {
    if( sum( x[i]>x[-i] ) == length(x)-1 ){
      result = i
    }
  }
  return(result)
}

my_which.max = function(x){
  n = length(x)
  max_ind = 1
  max_value = x[1]
  for (i in 2:n) {
    if(x[i] > max_value) {
      max_ind = i
      max_value = x[i]
    }
  }
  return(c(max_ind,max_value))
}

#n번째 max함수 
my_n_max = function(x, idx){
  n = length(x)
  for (i in 1:n) {
    if( sum( x[i]>x[-i] ) == length(x)-idx ){
      result = x[i]
    }
  }
  return(result)
}

my_n_max = function(x, idx){
  for (i in 1:idx) {
    max_value = my_max(x)
    x = x[x != max_value]
  }
  return(max_value)
}
set.seed(1)
x = rnorm(10)
my_max(x)
max(x)

my_which.max(x)
which.max(x)

my_n_max(x,10)
min(x)

##grid search algorithm
f = function(x){
  answer = -x^2+2*x-1
  return(answer)
}
# -10 < x < 10 에서 최댓값 찾기

x = seq(-10,10,length.out = 1e+5)
fx_value = f(x)
x[which.max(fx_value)]

###unique함수 짜기 
a = c("a", "b", "c", "a", "b")

my_unique = function(x){
  n = length(x)
  unique_value = x[1]
  
  for(i in 2:n){
    d = 1
    for(j in 1 : (i-1)){
      if( x[i] == x[j] ){
        d = 0  #같은 값이 있으면 d = 0이며 unique_value에 들어가지 않음
        break  #같은 값이 하나라도 있으면 멈춰주어야 알고리즘이 불필요한 루프를 돌지 않음
      }
    }
    if(d == 1){  #같은 값이 없으면 계속 d = 1 나옴
      unique_value = append(unique_value, x[i])
    }
  }
  return(unique_value)
}
my_unique(a)