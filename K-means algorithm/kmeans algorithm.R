 
HW7 = function(data, centers, iter.max = 50){
  dist_fun = function(x, y) 
  {
    return(colSums(abs(x - y)))
  }
 
  colClass = sapply(data, function(x) class(x))  
  colClass_ind = colClass %in% c("numeric")  
  
  if (all(colClass_ind) == FALSE) {   
    stop("K-means does not allow characters") 
  }
  
  if (sum(!colClass_ind) > 0) { 
    warn_text = paste("Types of ", paste(which(colClass_ind == FALSE), collapse = ","),  
                      " columns are ", paste(colClass[colClass_ind == FALSE], collapse = ","), 
                      ". ", "Converts to numeric", sep = "")
    data[,colClass_ind == FALSE] = apply(data[,colClass_ind == FALSE], 2, as.numeric)  
    warning(warn_text)
  }
  
  data_mat = as.matrix(data)
  n = nrow(data_mat)
  p = ncol(data_mat)
  
  center_mat = matrix(nrow = centers, ncol = p)  
  cluster = sample(1:centers, n, replace = TRUE)   
  
  for (i in 1:centers) {   
    center_mat[i, ] = colMeans(data_mat[cluster == i, , drop = FALSE])
  }
  
  dist = matrix(nrow = n, ncol = centers)  
  
  iter = 0
  
  center_mat_old = center_mat   
  
  while (((iter = iter + 1) < iter.max)) {   
    
     
    for (i in 1:centers) {   
      dist[, i] = dist_fun(t(data_mat), center_mat[i,]) 
    }
    
    cluster <- apply(dist, 1, which.min)
    
    
    for(i in 1:centers){
      center_mat[i, ] = colMeans(data_mat[cluster == i, , drop = FALSE])
    } 
    
    if (sqrt(sum((center_mat_old - center_mat)^2)) < 1e-10) { 
      break  
    } else {
      center_mat_old = center_mat
    }
  }
  result = cluster
  return(result)
}

