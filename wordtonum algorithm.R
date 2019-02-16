### simple example for 0-9 ###
word_to_num001 <- function(word){
  out = NULL
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5, 
                     six=6, seven=7, eight=8, nine=9) #한자리 숫자에 대한 사전을 저장
  
  if( word %in% names(one_digits) ){ #word가 names~~의 원소에 속하냐/0에 대한 name : zero
    out = as.numeric(one_digits[word]) #list나 문자로 나올지도 모르니 asnumeric
  }
  return(list(word,out))
}

word_to_num001("for") #if문 실행 안되면 NULL값  
word_to_num001("seven")

word = "sixty one"
### simple example for 1 - 99 ###
word_to_num010 <- function(word){
  
  wsplit <- strsplit(tolower(word)," ")[[1]] #tolower 대소문자
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50, #고유한 단어는 다 타이핑 할 수 밖에!
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(one_digits, teens, ten_digits) #하나의 사전으로 합치기
  out <- 0 # 결과out을 더한 값
  length_of_word = length(wsplit) #두번째 원소가 없는데 읽으라고 하면 error발생하므로!
  for(i in 1:length_of_word){ 
    if(wsplit[i] %in% names(doubles)){ 
      temp <- as.numeric(doubles[wsplit[i]])
    }
    out <- out + temp
  }
  return(list(word,out))
}
word_to_num010(word)
word_to_num010("twenty three")
word_to_num010("eighty four")
word_to_num010("twenty")
word_to_num010("ten")
########################

word = "one hundred sixty one"
### simple example for 1 - 999 ###
word_to_num100 <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(one_digits, teens, ten_digits)
  #단어 하나라도 오타가 발생하면 전부 에러 띄우기 #기존 사전 단어 + "hundred"
  if( sum( !(wsplit %in% c(names( doubles ), "hundred") ) ) > 0 ){
    stop( paste0("Error:",word ) )
  } 
  temp = rep(0,length(wsplit))
  for(i in 1:length(wsplit)){
    if(wsplit[i] %in% names(doubles)){
      temp[i] <- as.numeric(doubles[wsplit[i]])
    }else if(wsplit[i] == "hundred"){
      temp[i-1] = temp[i-1]*100
    }
    out <- sum(temp)
  }
  return(list(word,out))
}
word = "one hundred sixty one"
word_to_num100(word)
word = "five hundred forty one"
word_to_num100(word)
word = "five hundred fourty one"
word_to_num100(word)
word_to_num100("hundred hundred") #error떠야 하는데 temp에 다 0을 넣어서 0 도출--코드약점
word_to_num100("twelve one") #erorr떠야하는데 안 나옴--연달아나오면 에러 나오게 하도록





### simple example for 1 - 9999 ###

word_to_num1000 = function(word)
{
  wsplit = strsplit(tolower(word), " ")[[1]] #리턴값 형태가 리스트이므로 [[1]]
  one_digits = list(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5,
                    six = 6, seven = 7, eight = 8, nine = 9, ten = 10)
  teens = list(eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15,
               sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19)
  ten_digits = list(twenty = 20, thirty = 30, forty = 40, fifty = 50,
                    sixty = 60, seventy = 70, eighty = 80, ninety = 90)
  doubles = c(one_digits, teens, ten_digits) #세 개 리스트가 묶임
  if (sum(!(wsplit %in% c(names(doubles), "hundred", "thousand"))) > 0) {
    stop(paste("Error:", word)) #사전에 없으면 변환할 수 없는 문자
  }
  
  tmp = numeric(length(wsplit))#괄호 안 만큼의 길이를 가진 영벡터 생성
  cc = 0
  for (i in 1:length(wsplit)) {
    if (wsplit[i] %in% names(doubles)) {
      tmp[i] = as.numeric(doubles[wsplit[i]])  #사전에 있는 문자로 매칭
    } else if (wsplit[i] == "hundred") {
      tmp[i - 1] = tmp[i - 1] * 100
      cc = cc + 1
    } else if (wsplit[i] == "thousand") {
      tmp[i - 1] = tmp[i - 1] * 1000
      cc = cc + 1
    }  #hundred와 thousand 자리에는 무조건 0이 생김 --> count해서 0이 아니게 만들어줌
  }
  out = sum(tmp)
  
  #unlist는 list를 vector로 변환
  #teens안에 두번 이상 들어간다 or ten_digits안에 두번 이상 들어간다 -> error #9999이상 나오면 빼야 함
  if (sum(tmp %in% unlist(teens)) > 1 | sum(tmp %in% unlist(ten_digits)) > 1) {stop(paste("Error:", word))}
  #hundred와 thousand 에 나오는 cc + 원래 일의 자리에 나올 수 있는 cc개수 1
  if (sum(tmp %in% unlist(one_digits)) > (cc + 1)) {stop(paste("Error:", word))}
  #"thousand hundred"(단위만 들어간 경우) error 띄우려고
  if ((length(wsplit) > 1) & (out == 0)) {stop(paste("Error:", word))}
  if ((length(wsplit) > 1) & (tmp[1] == 0)) {stop(paste("Error:", word))}
  return(list(word, out))
}

word_to_num1000("thirteen one")
word_to_num1000(word = "one hundred thirty one")
word_to_num1000(word = "three thousand one hundred thirty one")
word_to_num1000(word = "twelve twelve")
word_to_num1000(word = "thirteen twelve")
word_to_num1000(word = "ten one")


####문자를 숫자로 연산!
word_to_num = function(word)
{
  wsplit = strsplit(tolower(word)[[1]], " ")[[1]]
  operators = list(plus = "+", minus = "-", multiplication = "*", division = "/")
  
  if (sum(wsplit %in% names(operators)) < 1) {  #단어 안에 operator가 없다면 #즉, 숫자만 있다면
    result_num = word_to_num1000(word)
    result = result_num
  } else if (sum(wsplit %in% names(operators)) > 0) {  #단어 안에 operator가 있다면
    oper_ind = which(wsplit %in% names(operators))  #어느 위치에 operator가 있는지 인덱스를 찾음
    oper = operators[wsplit[oper_ind]]   #인덱스에 해당하는 수식 기호가 oper에 들어감
    # plus라는 단어 기준으로 양옆을 쪼개고자 함
    wsplit2 = strsplit(tolower(word)[[1]], #strsplit(단어,쪼개는 기준)  
                       paste(paste("", wsplit[oper_ind], ""),  #(plus)기준이 아니라 ( plus )기준으로 쪼개도록 공백 추가
                             collapse = "|"))[[1]] #operator가 두 개 들어갈 경우를 위해 paste 2번 with collapse = "|"--plus 또는 minus 기준으로 쪼개라
    
    #num을 vector로 2번째 원소 아예 받아서 하는 방법도 있음 ! -- 그게 더 편할 듯,, 
    num = vector(mode = "list", length = length(wsplit2)) #빈 리스트 생성
    for (i in 1:length(wsplit2)) {
      num[[i]] = word_to_num1000(wsplit2[i]) #word_to_num1000의 결과가 리스트 형태이기 때문에 리스트로 결과값 받음
    }
    
    #num 이라는 리스트 중 각 리스트의 두번째 값이 필요함 
    if (length(oper_ind) == 1) { #operator가 1개면
      result_num = Reduce(unlist(oper), sapply(num, "[[", 2)) #num[[1]][[]] 함수 안에 2가 들어가겠다 #sapply는 for문과 같음! #"[["가 함수 역할 
      #oper도 리스트! unlist는 list가 아니게 해줌 #Reduce(함수, 적용시킬 대상 원소 차례대로 적용) #Reduce("+", c(1,2,3)) --> 1+2+3 
      result = list(word, result_num)
    } else {
      num = sapply(num, "[[", 2) #num 이라는 리스트 중 각 리스트의 두번째 값이 필요함
      tmp = paste(num, unlist(oper)) #일단 다 붙힘 paste는 개수 짝이 안 맞으면 앞으로 돌아가서 한 번 더 paste하는게 문제
      result_tmp = paste(paste(tmp[-length(tmp)], collapse = " "), num[length(num)]) #마지막 쓸데없는 연산 빼고 다 합친 다음 마지막 숫자값 paste해줌
      result_num = eval(parse(text = result_tmp))  #parse():캐릭터를 실행할 수 있는 상태로 변환해줌"31+45-1"->31+45-1 #eval()은 실행시켜라
      result = list(word, result_num)
    }
  }
  return(result)
}
