data = iris[1:4]
myk = function(data, centers, iter.max = 50){
  colclass = sapply(data,function(x)class(x))
  colind = colclass %in% c("numeric")
  
  if(sum(!(colind))>0){
    stop("Kmeans does not allow characters")
  }
  
  data = as.matrix(data)
  n = nrow(data)
  p = ncol(data)
  
  cluster = sample(1:centers, n, replace = T)
  center_mat = matrix(nrow = centers, ncol = p)
  
  for (i in 1:centers) {
    center_mat[i,] = colMeans(data[cluster == i, , drop =F])
  }
  
  
  iter = 0
  center_mat_old = center_mat
  dist = matrix(nrow = n, ncol = centers)
  while ((iter = iter + 1) < iter.max) {
    for (i in 1:centers) {
      dist[,i] = apply(data, 1, function(x)(sqrt(sum(x-center_mat[i, , drop = F])^2)))
    }
    cluster = apply(dist, 1, which.min)
    for (i in 1:centers) {
      center_mat[i,] = colMeans(data[cluster == i, , drop =F])
    }
    
    if(sum(is.nan(center_mat))>0){
      for (i in 1:centers) {
        center_mat[i,] = colMeans(data[cluster == i, , drop =F])
      }
      cluster = sample(1:centers, n, replace = T)
    }
    
    if(sqrt(sum(center_mat-center_mat_old)^2) < 1e-10){
      break
    }else{
      center_mat_old = center_mat
    }
  }
  result = list(cluster = cluster, centers = center_mat, iterations = iter)
  return(result)
}
myk(iris[1:4],3)



wordtonum = function(word){
  wordtonum1000 = function(word){
    wsplit = strsplit(tolower(word), " ")[[1]]
    one_digits = list(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5,
                      six = 6, seven = 7, eight = 8, nine = 9)
    teens = list(eleven = 11, twelve = 12, thirteen = 13, fourteen = 14, fifteen = 15,
                 sixteen = 16, seventeen = 17, eighteen = 18, nineteen = 19)
    ten_digits = list(ten = 10, twenty = 20, thirty = 30, forty = 40, fifty = 50,
                      sixty = 60, seventy = 70, eighty = 80, ninety = 90)
    doubles = c(one_digits, teens, ten_digits)
    
    if(sum(!(wsplit %in% c(names(doubles), "hundred", "thousand"))) > 0){
      stop(paste("Error",word))
    }
    
    tmp = numeric(length(wsplit))
    cc = 0
    for (i in 1:length(wsplit)) {
      if(wsplit[i] %in% names(doubles)){
        tmp[i] = as.numeric(doubles[wsplit[i]])
      }else if(wsplit[i] %in% c("hundred")){
        tmp[i-1] = tmp[i-1] *100
        cc = cc + 1
      }else if(wsplit[i] %in% c("thousand")){
        tmp[i-1] = tmp[i-1] *1000
        cc = cc + 1
      }  
    }
    
    out = sum(tmp)
    
    if(sum(tmp %in% unlist(teens)) > 1 | sum(tmp %in% unlist(ten_digits)) > 1){
      stop(paste("Error:", word))
    }
    if(sum(tmp %in% one_digits) > cc + 1){ stop(paste("Error:",word))}
    
    return(list(word, out))
  }
  wsplit = strsplit(tolower(word), " ")[[1]]
  operators = list(plus = "+", minus = "-", multiplication = "*", division = "/")
  
  oper_ind = which(wsplit %in% names(operators))
  oper = operators[wsplit[oper_ind]]
  
  wsplit2 = strsplit(tolower(word)[[1]],paste(paste("", wsplit[oper_ind], ""), collapse = "|"))[[1]]
  num = rep(0,length(wsplit2))
  if(sum(wsplit %in% names(operators)) < 1){
    result = wordtonum1000(word)[[2]]
    out = result
  }else if(sum(wsplit %in% names(operators)) > 0){
    for (i in 1:length(wsplit2)) {
      num[i] = wordtonum1000(wsplit2[i])[[2]]
    }
    tmp = paste(num, unlist(oper))
    result_num = paste(paste(tmp[-length(tmp)],collapse=""),num[length(num)])
    out = eval(parse(text = result_num))
  }
  return(list(word,out))
}


wordtonum("thirty one")