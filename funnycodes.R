#최대공약수 function
gcd = function(a,b){
  d = min(a,b)
  cond = 1 
  while (cond) {
    if( (a%%d == 0) && (b%%d == 0) ){
      cond = 0
      result = d
    } else {
      d = d - 1
    }
  }
  return(result)
}


gcd(10, 6)

#################
##뉴클리드 호제법..?##
gcd2 = function(a,b){
  if (b == 0){
    return(a)
  }else{
    gcd2(b, a%%b)
  }
}


gcd2(10,6)


## ex.
f = function(){
  print("hellow")
  f()
}

###########재귀함수 연습######

my_factorial = function(x){
  if(x == 1){
    return(x)
  }else{
    return(x * my_factorial(x-1))
  }
  
}

my_factorial(5)

###서로소이용하여 최소공배수
lcm = function(a,b){
  
  gcd_v = gcd(a,b)
  v1 = a/ gcd_v
  v2 = b/ gcd_v
  
  return(gcd_v * v1 * v2)
}

lcm(10,12)

#############
##하노이의 탑##

hanoi = function(n, from, to, aux){
  if(n ==1 ){
    cat(from, "->", to, "\n")
    return(NULL)
  }
  hanoi(n-1, from, aux, to)
  cat(from, "->", aux, "\n")
  hanoi(n-1, aux, to, from)
}

hanoi(3,1,3,2)