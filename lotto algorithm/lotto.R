LottoFunction = function(N){
  outcome = rep(NA, N)     #샘플이 저장될 빈 벡터 생성
  S=1:45
  for (iter in 1:N) {
    x = sample(S, 6, replace = F)    #로또 당첨 번호 랜덤 생성 
    y = sample(S, 6, replace = F)    #내가 뽑은 6개 랜덤 넘버
    bonus = sample(which( ( S %in% x )==F ), 1, replace = F) 	 #x를 제외한 숫자 중 1개의 보너스 번호 랜덤생성
    
    outcome[iter] = ( sum( y %in% x ) == 5 & sum( bonus %in% y ) == 1 ) 
    #y가 x와 5개만 일치하면서 동시에 나머지 1개는 보너스 번호와 일치하면 TRUE(1), 아니면 FALSE(0)을 outcome의 iter번째 원소에 저장 
  }  
  Out  = sum(outcome)/N 	#2등에 당첨될 사건 수를 전부 더한 후 전체 샘플 수로 나누어 구한 2등 당첨 확률을 Out에 저장
  return( Out )
}