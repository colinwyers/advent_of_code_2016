library(openssl)
library(lambda.r)

library(dplyr)

seq.gen(start, stop=Inf, step=1) %as%
{
  first <- value <- start - step
  function(reset=FALSE) {
    if (reset) { value <<- first; return(invisible()) }
    if (value >= stop) return(NULL)
    
    value <<- value + step
    return(value)
  }
}

num.fn <- seq.gen(0,Inf)

#ptm <- proc.time()

num.fn(reset=TRUE)
result_list <- list()
cnt <- 0

repeat{
  n <- num.fn()
  hash <- md5(paste0('wtnhxymk',n))
  
  if(substring(hash,1,5)=='00000') {
    result_list <- append(result_list,list(list(value=substring(hash,6,6),count=n)))
    cnt <- cnt+1
  }
  
  if(cnt==8){
    break
  }
  
}

#proc.time() - ptm

paste0('Answer 1: ',paste0(sapply(result_list, function(x) { x$value } ), collapse = ''))

ptm <- proc.time()

num.fn(reset=TRUE)
result_list <- list()
cnt <- 0

repeat{
  n <- num.fn()
  hash <- md5(paste0('wtnhxymk',n))
  
  if(substring(hash,1,5)=='00000' & !is.na(as.numeric(substring(hash,6,6))) & as.numeric(substring(hash,6,6)) >= 0 & as.numeric(substring(hash,6,6)) <= 7) {
    result_list <- append(result_list,list(list(pos=substring(hash,6,6),value=substring(hash,7,7),count=n)))
    cnt <- cnt+1
  }
  
  if(cnt==8){
    break
  }
  
}

proc.time() - ptm

x <- sapply(result_list, function(x) { x$value })
y <- sapply(result_list, function(x) { x$pos })

x
y

paste0('Answer 2: ',paste0(x[order(y)], collapse = ''))