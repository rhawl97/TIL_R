#####1등 확률#####
lotto_function = function(){   
  S=1:45
  x = sample(S, 6, replace = F)
  y = sample(S, 6, replace = F)
  
  time4 = proc.time()[3]
  outcome4 = (sum( x %in% y) == 6)
  time4 = proc.time()[3] - time4
  
  outcome = c( prob = outcome4, runtime = time4 )
  return( outcome )
}

ITER = 10^7  #1등 확률이기 때문에 적어도 10^7번 돌려야 함
prob_result = time_result = matrix(0, nrow = ITER, ncol = 1) 
lotto_function()
for( iter in 1: ITER){
  result = lotto_function()
  prob_result[iter, ] = result[1]
  time_result[iter, ] = result[2]
}
sum(prob_result)/ITER #최종 확률 구하기

###4,5등 결과 한번에 저장하기(cashing)###
lotto_function = function(rank){ 
  if(rank == 1){
    matching_num = 6
  }else{
    matching_num = 8-rank   #수식적 규칙
  }
  
  S=1:45
  x = sample(S, 6, replace = F)
  y = sample(S, 6, replace = F)
  
  
  
  time4 = proc.time()[3]
  outcome4 = (sum( x %in% y) == matching_num)
  time4 = proc.time()[3] - time4
  
  outcome = c( prob = outcome4, runtime = time4 )
  return( outcome )
}

ITER = 10^4 
prob_result = matrix(0, nrow = ITER, ncol = 2) 
for( iter in 1: ITER){
  result4 = lotto_function(rank = 4)
  result5 = lotto_function(rank = 5)
  prob_result[iter,1] = result4[1]   #4,5등 결과 한번에 저장
  prob_result[iter,2] = result5[1]
}
sum(prob_result[,1])/ITER
sum(prob_result[,2])/ITER


#####알고리즘 속도 줄이기(optimization)#######
lotto_function = function(){ 
  
  S=1:45
  x = sample(S, 6, replace = F)
  y = sample(S, 6, replace = F)
  
  matched_num = sum( x%in%y ) # in 함수를 한번만 계산해도 됨 - 2배 줄임
  
  time3 = proc.time()[3]
  outcome3 = (matched_num == 4)
  time3 = proc.time()[3] - time3
  
  time4 = proc.time()[3]
  outcome4 = (matched_num == 3)
  time4 = proc.time()[3] - time4
  
  outcome = c( prob3 = outcome3, prob4 = outcome4 )
  return( outcome )
}

ITER = 10^4 
prob_result = matrix(0, nrow = ITER, ncol = 2) 
for( iter in 1: ITER){
  result4 = lotto_function()
  prob_result[iter,1:2] = result4
} #첫번째 컬럼에는 4등, 두번째 컬럼에는 5등 확률 저장됨

sum(prob_result[,1])/ITER
sum(prob_result[,2])/ITER
